# install.packages(c("tidyverse", "systemfit", "micEconAids", "readxl"))
library(readxl)
library(tidyverse)
library(systemfit)
library(micEconAids)

# ============================================================
# ЗАГРУЗКА И ПОДГОТОВКА ДАННЫХ

file_path <- "HA 1 2026.xlsx"
sheet_name <- "Вар.32"
raw_data <- read_excel(file_path, sheet = sheet_name, skip = 1, col_names = TRUE)

# Столбцы 1-14 - наблюдения ДО
df_before <- raw_data[, 1:14]
# Столбцы 16-29 - наблюдения ПОСЛЕ
df_after  <- raw_data[, 16:29]

var_names <- c(
  "N", "Pfood", "Pclothing", "Pinsulin", "Pinhaler", "Pservices", "P_other_goods",
  "M", "Dfood", "Dclothing", "Dservices", "D_other_goods", "Dinsulin", "Dinhaler"
)
names(df_before) <- var_names
names(df_after)  <- var_names
df_before <- df_before %>% mutate(across(everything(), as.numeric))
df_after  <- df_after  %>% mutate(across(everything(), as.numeric))
df_before <- df_before %>% mutate(post = 0L)
df_after  <- df_after  %>% mutate(post = 1L)

# Объединяем в один датафрейм
df <- bind_rows(df_before, df_after) %>%
  mutate(
    treat = as.integer(N <= 1500)
  )

print(table(df$treat, df$post))




# ==============================================================
# РАСХОДЫ И ПРОВЕРКА БЮДЖЕТНОГО ОГРАНИЧЕНИЯ

# Считаем расходы на каждый товар
df <- df %>%
  mutate(
    exp_food = Pfood * Dfood,
    exp_clothing = Pclothing * Dclothing,
    exp_insulin = Pinsulin * Dinsulin,
    exp_inhaler = Pinhaler * Dinhaler,
    exp_services = Pservices * Dservices,
    exp_other_goods = P_other_goods * D_other_goods,
    exp_O = exp_clothing + exp_services + exp_other_goods,
    exp_L = exp_food + exp_insulin + exp_inhaler,
    exp_total = exp_L + exp_O,
    diff_M    = exp_total - M
  )

cat("\nПроверка бюджетного ограничения |sum(P*D) - M|:\n")
print(summary(abs(df$diff_M)))
cat("Максимальное расхождение:", round(max(abs(df$diff_M), na.rm = TRUE), 2), "руб.\n")
# Получаем что расхождение не превышает 5 рублев при расходах в тысячи, то есть погрешность незначительная



# ============================================================
# ПУНКТ А.

# Используем метод разности разностей, чтобы выявить чистый эффект изменения НДС

# НДС, уплаченный потребителем, считается как доля от расходов:

# Ставка t_lgt зависит от города и периода:
#   — ДО реформы везде: 10%
#   — ПОСЛЕ в городе 1 (treat=1): 15%  ← реформа
#   — ПОСЛЕ в городе 2 (treat=0): 10%  ← без изменений

# Добавляем к каждому наблюдению соответствующее значение ставки НДС
# # vat — оценка суммы НДС, уплаченной потребителем.
df <- df %>% mutate(
  t_lgt = case_when(
    post == 0 ~ 0.10,
    post == 1 & treat == 1 ~ 0.15,
    post == 1 & treat == 0 ~ 0.10
  ),
  vat = (exp_food + exp_insulin + exp_inhaler) * t_lgt / (t_lgt + 1) +
    (exp_clothing + exp_services + exp_other_goods) * 0.20 / (1.20)
)

did_table <- df %>%
  group_by(treat, post) %>%
  summarise(mean_vat = mean(vat), .groups = "drop")

# разность разностей
city1_after  <- did_table$mean_vat[did_table$treat == 1 & did_table$post == 1]
city1_before <- did_table$mean_vat[did_table$treat == 1 & did_table$post == 0]
city2_after  <- did_table$mean_vat[did_table$treat == 0 & did_table$post == 1]
city2_before <- did_table$mean_vat[did_table$treat == 0 & did_table$post == 0]

DiD <- (city1_after - city1_before) - (city2_after - city2_before)

# Изменение НДС на покупателя
round(DiD, 2)
# Суммарное изменение НДС для города 1
round(DiD * 1500, 0)





# ============================================================
# Пункт Б.
# Для оценки изменения благосостояния используем компенсирующую вариацию.

# Чтобы получить матрицу Γ, оцениваем модель AIDS.
# Для этого вычислим доли товаров в потреблении.

goods <- c("food", "clothing", "insulin", "inhaler", "services", "other_goods")

# Доли расходов и индекс цен Стоуна
df <- df %>%
  mutate(
    w_food = exp_food / M,
    w_clothing = exp_clothing / M,
    w_insulin = exp_insulin / M,
    w_inhaler = exp_inhaler / M,
    w_services = exp_services / M,
    w_other_goods = exp_other_goods / M,
    
    lnP_stone = w_food * log(Pfood) +
      w_clothing * log(Pclothing) +
      w_insulin * log(Pinsulin) +
      w_inhaler * log(Pinhaler) +
      w_services * log(Pservices) +
      w_other_goods * log(P_other_goods),
    
    lnM_adj = log(M) - lnP_stone
  )

# Оцениваем тоолько на контрольной группе ДО реформы, так как нам нужны
# параметры предпочтений потребителей, не искажённые изменением налогов.

df_ctrl <- df %>% filter(treat == 0, post == 0)

# Проблема нулевых долей: большая часть потребителей не покупала лекарства поэтому w_insulin=0 или w_inhaler=0. 
# Это вызывает проблемы в aidsEst. Заменяем точные нули на очень малое число.
eps <- 1e-8

df_ctrl <- df_ctrl %>%
  mutate(
    w_insulin_adj = pmax(w_insulin, eps),
    w_inhaler_adj = pmax(w_inhaler, eps),
    lnP_stone_adj = w_food * log(Pfood) +
      w_clothing * log(Pclothing) +
      w_insulin_adj * log(Pinsulin) +
      w_inhaler_adj * log(Pinhaler) +
      w_services * log(Pservices) +
      w_other_goods * log(P_other_goods),
    lnM_adj_adj = log(M) - lnP_stone_adj
  )

# aidsEst требует чистый dataframe.
# Явно выбираем только нужные переменные и принудительно приводим к numeric.
aids_data <- as.data.frame(df_ctrl[, c(
  "Pfood", "Pclothing", "Pinsulin", "Pinhaler", "Pservices", "P_other_goods",
  "w_food","w_clothing","w_insulin_adj","w_inhaler_adj","w_services","w_other_goods","M"
)])
aids_data[] <- lapply(aids_data, function(x) as.numeric(as.character(x)))

aids_model <- aidsEst(
  priceNames = c("Pfood", "Pclothing", "Pinsulin", "Pinhaler", "Pservices", "P_other_goods"),
  shareNames = c("w_food", "w_clothing", "w_insulin_adj", "w_inhaler_adj",
                 "w_services", "w_other_goods"),
  totExpName = "M",
  data       = aids_data,
  method     = "LA", # метод линейного приближения
  hom        = TRUE, # ограничение однородности
  sym        = TRUE # симметричность
)

summary(aids_model)
# В выводе:
# alpha_i — константы уравнений долей
# beta_i  — коэффициенты при реальных расходах (знак: нормальный/неполноценный товар)
# gamma_ij — матрица ценовых параметров (нужна для CV)



# Матрица gamma — нужна для второго приближения CV
cf <- coef(aids_model)
print(cf$gamma)

gamma_sym <- matrix(0, nrow = 6, ncol = 6)
gnames <- c("food", "clothing", "insulin", "inhaler", "services", "other_goods")
rownames(gamma_sym) <- colnames(gamma_sym) <- gnames
gamma_sym <- cf$gamma
print(round(gamma_sym, 6))



# Расчёт CV для каждого потребителя города 1

# Определяем num_cols
num_cols <- c("w_food", "w_clothing", "w_insulin", "w_inhaler",
              "w_services", "w_other_goods",
              "Pfood", "Pclothing", "Pinsulin", "Pinhaler",
              "Pservices", "P_other_goods", "M")

# df_t0 — город 1 ДО реформы
# df_t1 — город 1 ПОСЛЕ реформы
df_t0 <- df %>% filter(treat == 1, post == 0) %>%
  mutate(across(all_of(num_cols), as.numeric))
df_t1 <- df %>% filter(treat == 1, post == 1) %>%
  mutate(across(all_of(num_cols), as.numeric))
# Считаем, что строки упорядочены одинаково, то есть i-я строка в каждой таблице описывает
# одного и того же потребителя.

p_cols <- c("Pfood", "Pclothing", "Pinsulin", "Pinhaler", "Pservices", "P_other_goods")
w_cols <- c("w_food", "w_clothing", "w_insulin", "w_inhaler", "w_services", "w_other_goods")

cv_vec     <- numeric(nrow(df_t0))
first_vec  <- numeric(nrow(df_t0))
second_vec <- numeric(nrow(df_t0))

for (i in seq_len(nrow(df_t0))) {
  # Изменение логцен: dP = ln P1 − ln P0 = ln(P1/P0)
  # Для товаров, цена которых не изменилась, dP_i = 0.
  # В городе 1 после реформы выросли цены на продовольствие, инсулин и ингалятор
  # из-за роста НДС с 10% до 15%. Остальные цены не изменились.
  lnP0 <- log(as.numeric(df_t0[i, p_cols]))
  lnP1 <- log(as.numeric(df_t1[i, p_cols]))
  dP   <- lnP1 - lnP0
  
  # Доли расходов ДО реформы — используем исходную корзину потребителя
  w0 <- as.numeric(df_t0[i, w_cols])
  
  # Совокупные расходы ДО реформы
  M0 <- df_t0$M[i]
  
  # Первый член
  first_order  <- sum(w0 * dP)
  
  # Второй член
  second_order <- 0.5 * as.numeric(t(dP) %*% gamma_sym %*% dP)
  
  # Знак минус: dP > 0 при росте цен, w0'·dP > 0,
  # поэтому CV = −M0·(...) > 0 — потребитель несёт потери.
  cv_vec[i]     <- M0 * (first_order + second_order)
  first_vec[i]  <- -M0 * first_order
  second_vec[i] <- -M0 * second_order
}

df_t0$cv <- cv_vec

cat("\n--- Результаты CV (благосостояние) ---\n")
cat("Среднее CV на человека :", round(mean(cv_vec), 2), "руб.\n")
cat("Медиана CV             :", round(median(cv_vec), 2), "руб.\n")
cat("Суммарное CV (город 1) :", round(sum(cv_vec), 0), "руб.\n")
cat("  из них первый член   :", round(mean(first_vec), 2), "руб. в среднем\n")
cat("  из них второй член   :", round(mean(second_vec), 2), "руб. в среднем\n")
# Соотношение первого и второго членов показывает, насколько важен учёт
# поведенческой реакции потребителей. Если второй член мал — достаточно
# первого приближения. Если значителен — второе приближение оправдано.


# CV по децилям дохода
# cv_share_pct = CV/M × 100% — налоговая нагрузка как % от расходов.
cat("\nCV по децилям дохода:\n")
df_t0 %>%
  mutate(decile = ntile(M, 10)) %>%
  group_by(decile) %>%
  summarise(
    mean_M        = round(mean(M), 0),
    mean_cv       = round(mean(cv), 2),
    cv_share_pct  = round(mean(cv / M) * 100, 3)
  ) %>%
  print()

# Если cv_share_pct убывает с ростом дециля — налог регрессивен:
# бедные тратят большуюю долю дохода на льготные товары.





# ============================================================
# ПУНКТ В.

# Правительство предлагает компенсировать потребителям полную стоимость лекарств.
# Субсидия = exp_insulin + exp_inhaler ПОСЛЕ реформы (по ценам с новым НДС).
# Это денежный трансфер, который напрямую снижает CV:
# CV_с_субсидией = CV_без_субсидии − субсидия

# Стоимость субсидии на одного потребителя города 1 после реформы
subsidy_per_person <- df_t1$exp_insulin + df_t1$exp_inhaler

# Средняя субсидия на человека 
round(mean(subsidy_per_person), 2)

# Суммарная стоимость субсидии
round(sum(subsidy_per_person), 2)

# Сравниваем с суммарным приростом НДС из пункта А.
# Если прирост НДС > стоимости субсидии — у правительства остаётся профицит.
vat_gain_total <- DiD * 1500
# Суммарный прирост НДС
round(vat_gain_total, 0)
# Профицит/Дефицит 
round(vat_gain_total - sum(subsidy_per_person), 0)

# Субсидия полностью компенсирует exp_insulin + exp_inhaler после реформы,
# т.е. эффективно снижает чистые расходы потребителя.
# Считаем новый CV: к доходу после реформы добавляем субсидию

cv_subsidy_vec <- numeric(nrow(df_t0))

# Теоретический шок НДС (с 10% до 15%)
delta <- log(1.15 / 1.10)
# Вектор шока строго в порядке: food, clothing, insulin, inhaler, services, other_goods
theoretical_dP <- c(delta, 0, delta, delta, 0, 0)

for (i in seq_len(nrow(df_t0))) {
  # Заменяем эмпирический dP на чистый теоретический налоговый шок
  dP <- theoretical_dP
  
  w0 <- as.numeric(df_t0[i, w_cols])
  M0 <- df_t0$M[i]
  
  # Расчет эффектов
  first_order  <- sum(w0 * dP) 
  second_order <- 0.5 * as.numeric(t(dP) %*% gamma_sym %*% dP)
  
  # Базовый CV 
  cv_vec[i]     <- M0 * (first_order + second_order)
  first_vec[i]  <- M0 * first_order
  second_vec[i] <- M0 * second_order
  
  # Субсидия
  sub_i <- subsidy_per_person[i]
  cv_subsidy_vec[i] <- cv_vec[i] - sub_i
}

df_t0$cv <- cv_vec
df_t0$cv_subsidy <- cv_subsidy_vec

# Среднее CV без субсидии 
round(mean(cv_vec), 2)
# Среднее CV с субсидией 
round(mean(cv_subsidy_vec), 2)
# Суммарное CV без субсидии 
round(sum(cv_vec), 2)
# Суммарное CV с субсидией 
round(sum(cv_subsidy_vec), 2)

# CV с субсидией по децилям дохода
# share_meddrugs - доля потребителей, покупающих лекарства — у них субсидия ненулевая.
# burden_base - налоговая нагрузка (CV/M) без субсидии по децилям.
# burden_sub -  налоговая нагрузка с субсидией.
df_t0 %>%
  mutate(decile = ntile(M, 10)) %>%
  group_by(decile) %>%
  summarise(
    mean_M         = round(mean(M), 0),
    cv_base        = round(mean(cv), 2),
    cv_with_sub    = round(mean(cv_subsidy), 2),
    share_meddrugs = round(mean((exp_insulin + exp_inhaler) > 0) * 100, 1),
    burden_base    = round(mean(cv / M) * 100, 3),
    burden_sub     = round(mean(cv_subsidy / M) * 100, 3)
  ) %>%
  print()


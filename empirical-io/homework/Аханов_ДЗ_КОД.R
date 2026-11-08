# Домашняя работа по курсу 'Эмпирические отраслевые рынки'
# Аханов А.Д.

# ПАКЕТЫ
packages <- c("tidyverse", "ggplot2", "MASS", "frontier", "readxl")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {install.packages(pkg)}
}

library(tidyverse)
library(ggplot2)
library(MASS)       # порядковый пробит / логит
library(frontier)   # стохастический фронтир
library(readxl)


################################################################################
#  ЧАСТЬ 1

# ЗАГРУЗКА ДАННЫХ
shops <- read.csv("repair_shops.csv", stringsAsFactors = FALSE)
print(head(shops))
nrow(shops)

# ОПИСАТЕЛЬНАЯ СТАТИСТИКА
summary(shops)

# Стандартные отклонения для числовых метрик
numeric_vars <- c("population", "housing", "est811111", "est811112", "est811113")
sapply(shops[numeric_vars], sd, na.rm = TRUE)

# Метрики по автотрассам
mean(shops$autoroute)
sum(shops$autoroute)

# РАСПРЕДЕЛЕНИЕ ЧИСЛА МАСТЕРСКИХ ПО РАЗМЕРУ РЫНКОВ

# Согласно BR (1990), число фирм N на рынке — порядковая переменная.
# Поэтому смотрим на распределение N.

table(shops$est811112) # трансмиссии
table(shops$est811113) # выхлопные системы

# Квартильные группы по населению
shops <- shops %>% mutate(pop_quartile = ntile(population, 4))

shops %>%
  group_by(pop_quartile) %>%
  summarise(
    n_cities = n(),
    pop_min = min(population),
    pop_max = max(population),
    mean_trans = mean(est811112),
    mean_exhaust = mean(est811113),
    mean_general = mean(est811111)
  ) %>%
  print()

# Визуализация распределений
ggplot(shops, aes(x = factor(est811112))) +
  geom_bar(fill = "blue", color = "white") +
  labs(title = "Распределение мастерских по ремонту трансмиссий",
       x = "Число мастерских (est811112)", y = "Число городов") +
  theme_minimal(base_size = 13)

ggplot(shops, aes(x = factor(est811113))) +
  geom_bar(fill = "blue", color = "white") +
  labs(title = "Распределение мастерских по ремонту выхлопных систем",
       x = "Число мастерских (est811113)", y = "Число городов") +
  theme_minimal(base_size = 13)

ggplot(shops, aes(x = population, y = est811112)) +
  geom_jitter(alpha = 0.4, color = "blue", width = 0, height = 0.2) +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  labs(title = "Число трансмиссионных мастерских и население города",
       x = "Население (чел.)", y = "Число мастерских") +
  theme_minimal(base_size = 13)



# 1.4 ЛИНЕЙНЫЕ РЕГРЕССИИ И КОРРЕЛЯЦИЯ ОСТАТКОВ
# Хотим оценить, связан ли размер рынка с числом мастерских в каждой нише.

# Регрессия 1
lm_trans <- lm(est811112 ~ population, data = shops)
summary(lm_trans)

# Регрессия 2:
lm_exhaust <- lm(est811113 ~ population, data = shops)
summary(lm_exhaust)

# Проверка корреляции остатков на наличие общих ненаблюдаемых факторов (OMV)
resid_trans <- residuals(lm_trans)
resid_exhaust <- residuals(lm_exhaust)
corr_resid <- cor(resid_trans, resid_exhaust)
corr_resid

# Корреляция остатков существенна (> 0.3)
# Это свидетельствует о наличии общих ненаб факт, влияющих на оба сектора одновременно.

# Визуализация корреляции остатков
ggplot(data.frame(resid_trans, resid_exhaust), aes(x = resid_trans, y = resid_exhaust)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Корреляция остатков двух регрессий",
       x = "Остатки трансмиссии", y = "Остатки выхлопные системы") +
  theme_minimal(base_size = 13)



# ПОРЯДКОВЫЙ ПРОБИТ

# Подготовка данных
# Усекаем зависимую переменную на уровне 5+ как в оригинальной методологии BR
shops_probit <- shops %>%
  mutate(
    n_trans_trunc = pmin(est811112, 5),
    log_pop       = log(population),
    log_housing   = log(housing)
  )

# Распределение зависимой переменной (усечённой)
table(shops_probit$n_trans_trunc)
shops_probit$n_trans_ord <- factor(shops_probit$n_trans_trunc, levels = 0:5, ordered = TRUE)

# Оценка модели
m_probit <- polr(
  n_trans_ord ~ log_pop + log_housing + autoroute,
  data   = shops_probit,
  method = "probit",
  Hess   = TRUE
)
summary(m_probit)

# Расчет p-values для коэффициентов
coef_table <- coef(summary(m_probit))
p_values <- pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE) * 2
result_table <- cbind(coef_table, "p-value" = round(p_values, 4))
result_table


# Предсказание вероятностей для медианного города (без автотрассы)
median_city <- data.frame(
  log_pop     = median(shops_probit$log_pop),
  log_housing = median(shops_probit$log_housing),
  autoroute   = 0
)

# Характеристики медианного города (autoroute = 0)
print(median_city)
pred_probs <- predict(m_probit, newdata = median_city, type = "probs")
pred_df <- data.frame(
  N_firms      = paste0(0:5, c(rep("", 5), "+")),
  Probability  = round(as.numeric(pred_probs), 4)
)
print(pred_df)

# Предсказанные вероятности для города с автотрассой
median_city_route <- median_city
median_city_route$autoroute <- 1

pred_probs_route <- predict(m_probit, newdata = median_city_route, type = "probs")
data.frame(N_firms = paste0(0:5, c(rep("", 5), "+")), Probability = round(as.numeric(pred_probs_route), 4))







################################################################################
# ЧАСТЬ 2


# ЗАГРУЗКА ДАННЫХ
ind <- read_excel("homework_industries.xlsx", sheet = "Аханов А.Д.")
orig_names <- names(ind) 
names(ind) <- c("Y", "Cash", "K", "Debt", "Materials", "L", "Profit")


# ОПИСАТЕЛЬНАЯ СТАТИСТИКА
summary(ind)
mean(ind$Profit < 0, na.rm = TRUE) # Доля убыточных фирм

# Визуализация: распределения ключевых переменных
ind %>%
  dplyr::select(Y, K, L) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 40, fill = "blue", color = "white", alpha = 0.8) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Распределение выпуска, капитала и труда", x = "Значение", y = "Частота") +
  theme_minimal(base_size = 12)




# ПЕРЕМЕННЫЕ ДЛЯ ОЦЕНКИ РЕЗУЛЬТАТИВНОСТИ

# Подготовка переменных. Проверяем положительность для логарифмирования
all(min(ind$Y, na.rm = TRUE) > 0, min(ind$K, na.rm = TRUE) > 0, min(ind$L, na.rm = TRUE) > 0)
ind <- ind %>% mutate(ln_Y = log(Y), ln_K = log(K), ln_L = log(L)) # Создаём логарифмы




# Спецификация Кобба-Дугласа через OLS
m_ols <- lm(ln_Y ~ ln_K + ln_L, data = ind)
summary(m_ols)

# Сумма коэффициентов = эффект масштаба
beta_K_ols <- coef(m_ols)["ln_K"]
beta_L_ols <- coef(m_ols)["ln_L"]
rts_ols <- beta_K_ols + beta_L_ols

beta_K_ols # эластичность по капиталу (0.3785)
beta_L_ols # эластичность по труду (0.1545)
rts_ols # эффект масштаба (0.533)
# Отдача от масштаба у нас убывающая
beta_K_ols / rts_ols # Относительный вклад капитала - 0.7101
beta_L_ols / rts_ols # Относительный вклад труда - 0.2899

# Диагностика OLS остатков
ind$ols_fitted <- fitted(m_ols)

ggplot(ind, aes(x = ols_fitted, y = ln_Y)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Фактический и предсказанный ln(Y) по OLS",
       x = "Предсказанный ln(Y)",
       y = "Фактический ln(Y)") +
  theme_minimal(base_size = 12)




# СТОХАСТИЧЕСКИЙ ФРОНТИР

# Оцениваем ту же спецификацию Кобба-Дугласа, что и в OLS
m_sfa <- sfa(ln_Y ~ ln_K + ln_L, data = ind)
summary(m_sfa)

# Сопоставление результатов OLS и SFA
sfa_coefs   <- coef(m_sfa)
beta_K_sfa  <- sfa_coefs["ln_K"]
beta_L_sfa  <- sfa_coefs["ln_L"]
rts_sfa     <- beta_K_sfa + beta_L_sfa
rts_sfa

data.frame(
  Параметр    = c("β_K (капитал)", "β_L (труд)", "β_K + β_L (эффект масштаба)"),
  OLS         = round(c(beta_K_ols, beta_L_ols, rts_ols), 4),
  SFA         = round(c(beta_K_sfa, beta_L_sfa, rts_sfa), 4)
)

# ИНТЕРПРЕТАЦИЯ ИЗМЕНЕНИЙ
# OLS смещает коэффициент по труду вверх и по капиталу вниз, т.к. более 
# производительные фирмы нанимают больше труда (одновременная эндогенность). 
# SFA, выделяя неэффективность в отдельный компонент ошибки, даёт более чистые оценки беты

# Примечание: Разница обусловлена тем, что OLS игнорирует асимметричную неэффективность
# и абсорбирует её в общую ошибку, смещая оценки эластичности факторов.


# Анализ дисперсии ошибки через параметр Гамма

# frontier хранит: sigmaSq = σ²_u + σ²_v,  gamma = σ²_u / (σ²_u + σ²_v)
gamma_val <- sfa_coefs["gamma"]
gamma_val # Доля дисперсии, объясняемая технической неэффективностью предприятий
# 0,4872
# То есть шум и неэффективность примерно одинаковый вклад вносят в вариацию
# Тем не менее SFA остается методологически правильным подходом

sigmaSq <- sfa_coefs["sigmaSq"]
sigmaSq




# Индексы технической эффективности (TE)
ind$TE <- as.numeric(efficiencies(m_sfa))

# Описательные статистики TE
data.frame(
  Метрика = c("Min", "Q1", "Median", "Mean", "Q3", "Max"),
  TE      = round(c(min(ind$TE), quantile(ind$TE, c(0.25, 0.5)), mean(ind$TE), quantile(ind$TE, 0.75), max(ind$TE)), 4)
)

mean(ind$TE > 0.8) # Доля высокоэффективных компаний
mean(ind$TE < 0.5) # Доля низкоэффективных компаний

# Распределение TE
ggplot(ind, aes(x = TE)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(TE)), color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Распределение индексов технической эффективности (TE)",
       x = "Индекс эффективности (TE)", y = "Число предприятий") +
  scale_x_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 13)

# Сравнение предсказаний OLS и производственного фронтира SFA
ind$sfa_fitted <- exp(fitted(m_sfa))
ind$ols_fitted_Y <- exp(ind$ols_fitted)

ggplot(ind, aes(x = K)) +
  geom_point(aes(y = Y), alpha = 0.3, color = "grey", size = 1) +
  geom_smooth(aes(y = ols_fitted_Y, color = "OLS"), method = "loess", se = FALSE, linewidth = 1.2) +
  geom_smooth(aes(y = sfa_fitted, color = "SFA (frontier)"), method = "loess", se = FALSE, linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("OLS" = "steelblue", "SFA (frontier)" = "darkred")) +
  coord_cartesian(xlim = quantile(ind$K, c(0.01, 0.99), na.rm = TRUE),
                  ylim = quantile(ind$Y, c(0.01, 0.99), na.rm = TRUE)) +
  labs(title = "Фактический выпуск, OLS и производственная граница SFA",
       x = "Капитал (K)", y = "Выпуск (Y)", color = "Модель") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Среднеотраслевой индекс технической эффективности
mean(ind$TE, na.rm = TRUE)
# То есть предприятия могли бы при идеальной эффективности и при тех же ресурсах
# Увеличить выпуск почти на 70%


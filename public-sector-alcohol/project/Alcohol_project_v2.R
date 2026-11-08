# =====================================================================
#  Оценка эффективности акцизного регулирования рынка алкоголя РФ
#  Система спроса (LA-AIDS) -> эластичности -> DWL / благосостояние
#
#  ВЕРСИЯ 2. Что изменено относительно Alcohol_project.R и ПОЧЕМУ —
#  отмечено тегами [ИЗМЕНЕНО], [ДОБАВЛЕНО], [ВНИМАНИЕ] по тексту.
#
#  ВАЖНО ПРО НАЗВАНИЕ МОДЕЛИ:
#  micEconAids::aidsEst() оценивает (LA-)AIDS, а НЕ QUAIDS.
#  Квадратичного члена по log расходов здесь нет. Поэтому по всему
#  коду модель честно называется AIDS / LA-AIDS. Если в тексте проекта
#  заявлен QUAIDS — либо переименуйте текст в AIDS (+ тест квадратики,
#  см. блок 8), либо считайте настоящий QUAIDS в Stata (quaids, Poi 2012)
#  или aidsills с квадратикой (Lecocq & Robin 2015).
# =====================================================================


# Packages ----------------------------------------------------------------
library(micEconAids)   # оценка AIDS (SUR / 3SLS-IV), эластичности
library(tidyverse)
library(readxl)

options(scipen = 999)


# Data --------------------------------------------------------------------
# [ВНИМАНИЕ] лист "wide": 1 строка = регион-квартал; колонки w_*, price_real_*,
# excise_*, total_exp, q1/q2/q3 уже готовы в master_panel.xlsx.

df <- read_excel("master_panel.xlsx", sheet = "wide") %>%
  as.data.frame() %>%
  arrange(region_id, year, quarter_num) %>%
  mutate(
    date_q = (year - 2017) * 4 + quarter_num,   # сквозной номер квартала 1..20

    okrug = case_when(
      region_id %in% c(5, 6, 7, 10, 16, 20, 25, 29, 31, 11, 33, 41, 63, 68, 70, 71, 73, 83) ~ "Центральный",
      region_id %in% c(3, 9, 19, 52, 53, 30, 34, 35, 37, 45, 12) ~ "Северо-Западный",
      region_id %in% c(46, 4, 8, 51, 26, 54, 62, 13) ~ "Южный",
      region_id %in% c(50, 18, 22, 58, 69) ~ "Северо-Кавказский",
      region_id %in% c(48, 24, 55, 56, 36, 40, 42, 43, 64, 65, 59, 75, 76, 80) ~ "Приволжский",
      region_id %in% c(28, 67, 74, 78, 79, 82) ~ "Уральский",
      region_id %in% c(1, 47, 17, 23, 27, 38, 39, 72, 60, 61) ~ "Сибирский",
      region_id %in% c(2, 49, 14, 15, 21, 32, 44, 57, 66, 77, 81) ~ "Дальневосточный",
      TRUE ~ NA_character_
    )
  )

# [ДОБАВЛЕНО] Жёсткая проверка: если регион не попал в маппинг округов,
# его инструмент Хаусмана станет NA, и наблюдение МОЛЧА выпадет из 3SLS.
# Лучше упасть с понятной ошибкой, чем потерять данные незаметно.
stopifnot(!anyNA(df$okrug))


# =====================================================================
#  Хелпер: инструменты Хаусмана + экзогенные расходы
#  [ИЗМЕНЕНО] вынес повторяющийся код в функцию, чтобы 5-cat и 4-cat
#  строились одинаково и без копипасты.
#
#  Инструмент Хаусмана z_g для цены товара g в регионе r, квартал t:
#  = средний log-цены товара g по всем регионам ВНЕ своего округа в t.
#  Идея: издержечные шоки общие между рынками (-> коррелируют с ценой),
#  шоки спроса локальны (-> не коррелируют со спросом в r).
#  Исключаем весь округ (а не только свой регион) — консервативнее
#  к пространственной корреляции шоков спроса.
# =====================================================================
add_hausman_iv <- function(data, goods) {
  lp <- paste0("lp_", goods)        # имена log-цен
  z  <- paste0("z_",  goods)        # имена инструментов

  # log реальных цен
  for (g in goods) data[[paste0("lp_", g)]] <- log(data[[paste0("price_real_", g)]])

  data <- data %>%
    group_by(date_q) %>%
    mutate(across(all_of(lp), ~ sum(.x), .names = "gs_{.col}"),  # сумма по стране
           gn = n()) %>%
    group_by(date_q, okrug) %>%
    mutate(across(all_of(lp), ~ sum(.x), .names = "os_{.col}"),  # сумма по округу
           on = n()) %>%
    ungroup()

  # z = (сумма по стране - сумма по округу) / (кол-во регионов вне округа)
  for (g in goods) {
    gs <- paste0("gs_lp_", g); os <- paste0("os_lp_", g)
    data[[paste0("z_", g)]] <- (data[[gs]] - data[[os]]) / (data$gn - data$on)
  }
  data %>% select(-starts_with("gs_lp_"), -starts_with("os_lp_"), -gn, -on)
}

# Хелпер: нормировка средних долей к единице (для расчёта эластичностей)
norm_shares <- function(w) { w <- w / sum(w); w[length(w)] <- 1 - sum(w[-length(w)]); w }


# =====================================================================
#  ДАННЫЕ: 5 категорий (ОСНОВНАЯ спецификация)
#  [ОБОСНОВАНИЕ] вино и игристое РАЗДЕЛЕНЫ — это литературно сильнее
#  (Fogarty 2010: "вино" неоднородно). Объединение (4-cat) держим как
#  robustness, а не как способ спрятать положительную эластичность игристого.
# =====================================================================
goods5 <- c("beer", "vodka", "cognac", "sparkling", "wine")

df_aids5 <- df %>%
  mutate(w_sum = w_beer + w_vodka + w_cognac + w_sparkling + w_wine,
         across(c(w_beer, w_vodka, w_cognac, w_sparkling, w_wine), ~ .x / w_sum)) %>%
  select(-w_sum)
df_aids5 <- add_hausman_iv(df_aids5, goods5)

pMean5 <- colMeans(df_aids5[paste0("price_real_", goods5)])
wMean5 <- norm_shares(colMeans(df_aids5[paste0("w_", goods5)]))

# [ПОЯСНЕНИЕ] lxtr_exog — "экзогенная" версия log совокупных расходов:
# log(total_exp) минус ценовой индекс с ФИКСИРОВАННЫМИ (средними) долями.
# Используется как инструмент для эндогенного члена реальных расходов
# (расходы и доли определяются одновременно).
df_aids5$lxtr_exog <- log(df_aids5$total_exp) -
  as.numeric(log(as.matrix(df_aids5[paste0("price_real_", goods5)])) %*% as.numeric(wMean5))

# Фиксированные эффекты: регион + квартал (шифтеры демографии/сезона/тренда)
# [ВНИМАНИЕ] factor(date_q) поглощает И сезонность, И НАЦИОНАЛЬНЫЙ АКЦИЗ
# (акциз меняется только во времени). Это нормально для эластичностей,
# но значит: эластичности идентифицируются МЕЖРЕГИОНАЛЬНОЙ вариацией цен,
# а не вариацией акциза. Для DWL акциз войдёт ниже как известный клин.
fe5 <- model.matrix(~ factor(date_q) + factor(region_id), data = df_aids5)[, -1, drop = FALSE]
colnames(fe5) <- paste0("fe", seq_len(ncol(fe5)))
df_aids5 <- cbind(df_aids5, as.data.frame(fe5))
sh5 <- colnames(fe5); shMean5 <- colMeans(fe5)


# =====================================================================
#  ДАННЫЕ: 4 категории (ROBUSTNESS: вино + игристое объединены)
#  Объединённая цена = unit value комбинированной корзины:
#  p_wine_all = (расходы вина+игристого) / (кол-во вина+игристого).
# =====================================================================
goods4 <- c("beer", "vodka", "cognac", "wine_all")

df_aids4 <- df %>%
  mutate(
    q_wine      = w_wine      * total_exp / price_real_wine,
    q_sparkling = w_sparkling * total_exp / price_real_sparkling,
    w_wine_all  = w_wine + w_sparkling,
    price_real_wine_all = w_wine_all * total_exp / (q_wine + q_sparkling),
    w_sum = w_beer + w_vodka + w_cognac + w_wine_all,
    across(c(w_beer, w_vodka, w_cognac, w_wine_all), ~ .x / w_sum)
  ) %>%
  select(-q_wine, -q_sparkling, -w_sum)
df_aids4 <- add_hausman_iv(df_aids4, goods4)

pMean4 <- colMeans(df_aids4[paste0("price_real_", goods4)])
wMean4 <- norm_shares(colMeans(df_aids4[paste0("w_", goods4)]))
df_aids4$lxtr_exog <- log(df_aids4$total_exp) -
  as.numeric(log(as.matrix(df_aids4[paste0("price_real_", goods4)])) %*% as.numeric(wMean4))

fe4 <- model.matrix(~ factor(date_q) + factor(region_id), data = df_aids4)[, -1, drop = FALSE]
colnames(fe4) <- paste0("fe", seq_len(ncol(fe4)))
df_aids4 <- cbind(df_aids4, as.data.frame(fe4))
sh4 <- colnames(fe4); shMean4 <- colMeans(fe4)


# =====================================================================
#  Хелпер для оценки AIDS (чтобы не повторять длинный вызов 4 раза)
# =====================================================================
fit_aids <- function(data, goods, shifters, wMean, method = c("SUR", "3SLS")) {
  method <- match.arg(method)
  args <- list(
    priceNames = paste0("price_real_", goods),
    shareNames = paste0("w_", goods),
    totExpName = "total_exp",
    data       = data,
    method     = "LA",            # линеаризованный AIDS
    priceIndex = "Ls",            # Stone-индекс...
    pxBase     = list(shares = as.numeric(wMean)),  # ...скорректированный базовыми долями
    hom = TRUE, sym = TRUE,       # однородность + симметрия Слуцкого
    shifterNames = shifters,
    estMethod  = method
  )
  if (method == "3SLS")
    args$instNames <- c(paste0("z_", goods), "lxtr_exog", shifters)  # инструменты Хаусмана + экз. расходы
  do.call(aidsEst, args)
}

# [ДОБАВЛЕНО] Хелпер для эластичностей СО СТАНДАРТНЫМИ ОШИБКАМИ.
# В исходном коде SE не считались (aidsElas вызывался без coefCov),
# поэтому нельзя было сказать, отличается ли эластичность от 0 / от -1.
# Передаём vcov(model) -> дельта-метод даёт SE эластичностей.
elas_se <- function(model, prices, shares, shifterValues = NULL) {
  aidsElas(coef(model), prices = prices, shares = shares,
           shifterValues = shifterValues, coefCov = vcov(model))
}


# =====================================================================
#  ОЦЕНКА 4 ОСНОВНЫХ МОДЕЛЕЙ
# =====================================================================
aids5_sur <- fit_aids(df_aids5, goods5, sh5, wMean5, "SUR")
aids5_iv  <- fit_aids(df_aids5, goods5, sh5, wMean5, "3SLS")
aids4_sur <- fit_aids(df_aids4, goods4, sh4, wMean4, "SUR")
aids4_iv  <- fit_aids(df_aids4, goods4, sh4, wMean4, "3SLS")

summary(aids5_iv)   # основная спецификация

elas5_sur <- elas_se(aids5_sur, pMean5, wMean5, shMean5)
elas5_iv  <- elas_se(aids5_iv,  pMean5, wMean5, shMean5)
elas4_sur <- elas_se(aids4_sur, pMean4, wMean4, shMean4)
elas4_iv  <- elas_se(aids4_iv,  pMean4, wMean4, shMean4)

print(elas5_iv)


# =====================================================================
#  [ДОБАВЛЕНО] ПРОВЕРКА ВОГНУТОСТИ (negativity)
#  Чтобы формально показать то, о чём вы пишете словами в §5:
#  положительная собственная эластичность игристого/вина — это НЕ Гиффен,
#  а НАРУШЕНИЕ negativity (матрица Слуцкого должна быть отрицательно
#  полуопределённой => все собственные значения <= 0).
#
#  Восстанавливаем матрицу Слуцкого из эластичностей Хикса:
#  S_ij = w_i * (hicks_ij) ... (в терминах долей; знак и масштаб
#  достаточны для проверки полуопределённости).
# =====================================================================
check_concavity <- function(elas, shares, goods) {
  H <- as.matrix(elas$hicks)                 # компенсированные эластичности
  S <- diag(shares) %*% H                    # ~ матрица замещения (до симметризации)
  S <- (S + t(S)) / 2                        # симметризуем (теоретически симметрична)
  ev <- eigen(S, symmetric = TRUE)$values
  cat("Собственные значения матрицы замещения (", paste(goods, collapse=", "), "):\n")
  print(round(ev, 5))
  cat(if (all(ev <= 1e-8)) "OK: negativity выполняется (все <= 0)\n"
      else "НАРУШЕНИЕ negativity: есть положительное собств. значение -> специфик./измерит. проблема\n")
  invisible(ev)
}
check_concavity(elas5_iv, wMean5, goods5)
check_concavity(elas4_iv, wMean4, goods4)


# =====================================================================
#  Сводная таблица: 4 модели (exp / собств. Маршалл / собств. Хикс)
# =====================================================================
row_from_elas <- function(elas, model, goods) {
  tibble(model = model, good = goods,
         exp = as.numeric(elas$exp),
         marshall_own = diag(as.matrix(elas$marshall)),
         hicks_own    = diag(as.matrix(elas$hicks)))
}
table_models <- bind_rows(
  row_from_elas(elas5_sur, "5 cat SUR",  goods5),
  row_from_elas(elas5_iv,  "5 cat 3SLS", goods5),
  row_from_elas(elas4_sur, "4 cat SUR",  goods4),
  row_from_elas(elas4_iv,  "4 cat 3SLS", goods4)
) %>% mutate(across(c(exp, marshall_own, hicks_own), ~ round(.x, 3)))
print(table_models)


# =====================================================================
#  [ДОБАВЛЕНО — КЛЮЧЕВОЙ НЕДОСТАЮЩИЙ БЛОК] DWL И ЭФФЕКТИВНОСТЬ
#  Это та половина проекта, которой не было в коде вообще.
#  Цель: из эластичностей + известного акцизного клина оценить
#  (а) DWL действующих акцизов, (б) изменение потребления чистого спирта.
#
#  Подход (приближение Харбергера, частичное равновесие):
#  Для системы спроса DWL от набора специфических налогов t (руб/л)
#  приближается квадратичной формой по КОМПЕНСИРОВАННЫМ (Хикс) реакциям:
#
#     DWL ≈ -1/2 * Σ_i Σ_j  t_i * t_j * (∂q_i/∂p_j)|комп
#
#  где (∂q_i/∂p_j) = hicks_ij * q_i / p_j  (перевод эластичности в производную).
#  Знак: матрица замещения отрицательно полуопределена => DWL >= 0.
#
#  [ВНИМАНИЕ] это ИЛЛЮСТРАТИВНЫЙ расчёт «на среднем регионе-квартале».
#  Для финального проекта: (1) считать по каждой ячейке регион-квартал и
#  агрегировать; (2) корректно перевести акциз крепкого в руб/л продукта
#  (он уже так в данных, excise_*); (3) обсудить ограничения (нелегальный
#  рынок, частичное равновесие).
# =====================================================================
dwl_harberger <- function(elas, shares, prices, excise, total_exp_mean, goods) {
  H <- as.matrix(elas$hicks)                     # компенсированные эластичности
  q <- shares * total_exp_mean / prices          # средние кол-ва (л/душу) по категориям
  # матрица компенсированных производных ∂q_i/∂p_j = H_ij * q_i / p_j
  D <- outer(seq_along(goods), seq_along(goods),
             Vectorize(function(i, j) H[i, j] * q[i] / prices[j]))
  D <- (D + t(D)) / 2                            # симметризация (теория)
  dwl <- -0.5 * as.numeric(t(excise) %*% D %*% excise)
  list(dwl_total = dwl,
       revenue   = sum(excise * q),              # фискальный фон (трансферт, не благосостояние)
       q         = q)
}

# крепость для перевода в чистый спирт (ABV); подправьте под свои определения
abv <- c(beer = 0.045, vodka = 0.40, cognac = 0.40, sparkling = 0.115, wine = 0.12)

excise5 <- colMeans(df[paste0("excise_", goods5)])     # средний акцизный клин, руб/л продукта
res_dwl <- dwl_harberger(elas5_iv, wMean5, pMean5, as.numeric(excise5),
                         mean(df$total_exp), goods5)

cat("\n--- Эффективность (иллюстративно, средняя ячейка) ---\n")
cat("DWL действующих акцизов (руб/душу/квартал):", round(res_dwl$dwl_total, 3), "\n")
cat("Поступления (руб/душу/квартал, ОПИСАТЕЛЬНО):", round(res_dwl$revenue, 3), "\n")

# Потребление ЧИСТОГО СПИРТА на душу (л/квартал) — прокси вреда (критерий "минус")
pure_ethanol <- sum(res_dwl$q * abv[goods5])
cat("Чистый спирт, л/душу/квартал:", round(pure_ethanol, 3),
    " (в год ~", round(pure_ethanol * 4, 2), "л)\n")

# [КАК СЧИТАТЬ КОНТРФАКТ] Чтобы оценить эффект ИЗМЕНЕНИЯ акциза (например,
# +10% к водке), сдвиньте цену соответствующей категории на пасс-тру акциза,
# пересчитайте q через эластичности (Маршалл для объёмов, Хикс для DWL),
# и сравните: Δ(чистый спирт) = снижение вреда; Δ(DWL) = рост искажения.
# Здесь критичны ПЕРЕКРЁСТНЫЕ члены: рост акциза на водку может погнать
# спрос в крепленое вино/нелегальный сектор (Goryakin et al. 2015:
# водка и крепленое — субституты) => снижение вреда переоценивается.


# =====================================================================
#  [БЛОК-ЗАГЛУШКА] Как сделать НАСТОЯЩИЙ QUAIDS (если нужно по тексту)
#  micEconAids не умеет квадратику. Варианты:
#  1) Stata: ssc install quaids;  quaids w1-w5, prices(...) expenditure(...)
#     anot(...) ; затем estat ... для эластичностей со SE (Poi 2012).
#  2) Stata + эндогенность: aidsills / расширения с IV (Lecocq & Robin 2015).
#  3) R вручную: нелинейный SUR через systemfit::nlsystemfit или GMM,
#     добавив член λ_i * [ln(x/P)]^2 / b(p). Готового зрелого пакета нет.
#  Если остаётесь на AIDS — добавьте тест значимости квадратичного члена
#  (оцените QUAIDS в Stata разово и проверьте H0: λ_i = 0), чтобы честно
#  написать "квадратика незначима, используем AIDS".
# =====================================================================


# =====================================================================
#  [ОПЦИОНАЛЬНО] Sensitivity к доле импортного пива s.
#  Цена пива = (1-s)*p_отеч + s*p_имп. В master_panel уже зашит s=0.36.
#  Для проверки s=0.25/0.45 нужно либо пересобрать price_real_beer в
#  build_master.py с этими s, либо иметь в данных раздельные p_отеч/p_имп.
#  Здесь оставлено как напоминание — переоценить aids5_iv при каждом s и
#  показать, что собственные эластичности пива/водки/коньяка устойчивы.
# =====================================================================

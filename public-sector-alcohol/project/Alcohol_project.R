# Packages ----------------------------------------------------------------

library(micEconAids)
library(tidyverse)
library(readxl)

options(scipen = 999)


# Data --------------------------------------------------------------------

df <- read_excel("master_panel.xlsx", sheet = "wide") %>%
  as.data.frame() %>%
  arrange(region_id, year, quarter_num) %>%
  mutate(
    date_q = (year - 2017) * 4 + quarter_num,
    
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


# Данные для модели: 5 категорий -----------------------------------------

df_aids5 <- df %>%
  mutate(
    w_sum = w_beer + w_vodka + w_cognac + w_sparkling + w_wine,
    
    w_beer      = w_beer / w_sum,
    w_vodka     = w_vodka / w_sum,
    w_cognac    = w_cognac / w_sum,
    w_sparkling = w_sparkling / w_sum,
    w_wine      = w_wine / w_sum,
    
    lp_beer      = log(price_real_beer),
    lp_vodka     = log(price_real_vodka),
    lp_cognac    = log(price_real_cognac),
    lp_sparkling = log(price_real_sparkling),
    lp_wine      = log(price_real_wine)
  ) %>%
  group_by(date_q) %>%
  mutate(
    gs_beer      = sum(lp_beer),
    gs_vodka     = sum(lp_vodka),
    gs_cognac    = sum(lp_cognac),
    gs_sparkling = sum(lp_sparkling),
    gs_wine      = sum(lp_wine),
    gn           = n()
  ) %>%
  group_by(date_q, okrug) %>%
  mutate(
    os_beer      = sum(lp_beer),
    os_vodka     = sum(lp_vodka),
    os_cognac    = sum(lp_cognac),
    os_sparkling = sum(lp_sparkling),
    os_wine      = sum(lp_wine),
    on           = n(),
    
    z_beer      = (gs_beer - os_beer) / (gn - on),
    z_vodka     = (gs_vodka - os_vodka) / (gn - on),
    z_cognac    = (gs_cognac - os_cognac) / (gn - on),
    z_sparkling = (gs_sparkling - os_sparkling) / (gn - on),
    z_wine      = (gs_wine - os_wine) / (gn - on)
  ) %>%
  ungroup() %>%
  select(-w_sum, -starts_with("gs_"), -starts_with("os_"), -gn, -on)

pMean5 <- colMeans(df_aids5[c(
  "price_real_beer",
  "price_real_vodka",
  "price_real_cognac",
  "price_real_sparkling",
  "price_real_wine"
)])

wMean5 <- colMeans(df_aids5[c(
  "w_beer",
  "w_vodka",
  "w_cognac",
  "w_sparkling",
  "w_wine"
)])

wMean5 <- wMean5 / sum(wMean5)
wMean5[5] <- 1 - sum(wMean5[1:4])

df_aids5$lxtr_exog <- log(df_aids5$total_exp) -
  as.numeric(
    log(as.matrix(df_aids5[c(
      "price_real_beer",
      "price_real_vodka",
      "price_real_cognac",
      "price_real_sparkling",
      "price_real_wine"
    )])) %*% as.numeric(wMean5)
  )

fe5 <- model.matrix(
  ~ factor(date_q) + factor(region_id),
  data = df_aids5
)[, -1, drop = FALSE]

colnames(fe5) <- paste0("fe", seq_len(ncol(fe5)))

df_aids5 <- cbind(df_aids5, as.data.frame(fe5))

sh5 <- colnames(fe5)
shMean5 <- colMeans(fe5)


# Данные для модели: 4 категории -----------------------------------------

df_aids4 <- df %>%
  mutate(
    q_wine      = w_wine * total_exp / price_real_wine,
    q_sparkling = w_sparkling * total_exp / price_real_sparkling,
    
    w_wine_all = w_wine + w_sparkling,
    price_real_wine_all = w_wine_all * total_exp / (q_wine + q_sparkling),
    
    w_sum = w_beer + w_vodka + w_cognac + w_wine_all,
    
    w_beer     = w_beer / w_sum,
    w_vodka    = w_vodka / w_sum,
    w_cognac   = w_cognac / w_sum,
    w_wine_all = w_wine_all / w_sum,
    
    lp_beer     = log(price_real_beer),
    lp_vodka    = log(price_real_vodka),
    lp_cognac   = log(price_real_cognac),
    lp_wine_all = log(price_real_wine_all)
  ) %>%
  group_by(date_q) %>%
  mutate(
    gs_beer     = sum(lp_beer),
    gs_vodka    = sum(lp_vodka),
    gs_cognac   = sum(lp_cognac),
    gs_wine_all = sum(lp_wine_all),
    gn          = n()
  ) %>%
  group_by(date_q, okrug) %>%
  mutate(
    os_beer     = sum(lp_beer),
    os_vodka    = sum(lp_vodka),
    os_cognac   = sum(lp_cognac),
    os_wine_all = sum(lp_wine_all),
    on          = n(),
    
    z_beer     = (gs_beer - os_beer) / (gn - on),
    z_vodka    = (gs_vodka - os_vodka) / (gn - on),
    z_cognac   = (gs_cognac - os_cognac) / (gn - on),
    z_wine_all = (gs_wine_all - os_wine_all) / (gn - on)
  ) %>%
  ungroup() %>%
  select(-q_wine, -q_sparkling, -w_sum, -starts_with("gs_"), -starts_with("os_"), -gn, -on)

pMean4 <- colMeans(df_aids4[c(
  "price_real_beer",
  "price_real_vodka",
  "price_real_cognac",
  "price_real_wine_all"
)])

wMean4 <- colMeans(df_aids4[c(
  "w_beer",
  "w_vodka",
  "w_cognac",
  "w_wine_all"
)])

wMean4 <- wMean4 / sum(wMean4)
wMean4[4] <- 1 - sum(wMean4[1:3])

df_aids4$lxtr_exog <- log(df_aids4$total_exp) -
  as.numeric(
    log(as.matrix(df_aids4[c(
      "price_real_beer",
      "price_real_vodka",
      "price_real_cognac",
      "price_real_wine_all"
    )])) %*% as.numeric(wMean4)
  )

# Фиксированные эффекты для разных спецификаций ---------------------------

fe4_all <- model.matrix(
  ~ factor(date_q) + factor(region_id),
  data = df_aids4
)[, -1, drop = FALSE]

colnames(fe4_all) <- paste0("fe_all_", seq_len(ncol(fe4_all)))

df_aids4 <- cbind(df_aids4, as.data.frame(fe4_all))

sh4_all <- colnames(fe4_all)
shMean4_all <- colMeans(fe4_all)


fe4_region <- model.matrix(
  ~ factor(region_id),
  data = df_aids4
)[, -1, drop = FALSE]

colnames(fe4_region) <- paste0("fe_region_", seq_len(ncol(fe4_region)))

df_aids4 <- cbind(df_aids4, as.data.frame(fe4_region))

sh4_region <- colnames(fe4_region)
shMean4_region <- colMeans(fe4_region)


fe4_quarter <- model.matrix(
  ~ factor(date_q),
  data = df_aids4
)[, -1, drop = FALSE]

colnames(fe4_quarter) <- paste0("fe_quarter_", seq_len(ncol(fe4_quarter)))

df_aids4 <- cbind(df_aids4, as.data.frame(fe4_quarter))

sh4_quarter <- colnames(fe4_quarter)
shMean4_quarter <- colMeans(fe4_quarter)


# Ссредние доли --------------------------------------------------

round(wMean5, 4)
round(wMean4, 4)

sum(wMean5)
sum(wMean4)


# Модель 5 категорий: SUR ---------------------------------------------

aids5_sur <- aidsEst(
  priceNames = c(
    "price_real_beer",
    "price_real_vodka",
    "price_real_cognac",
    "price_real_sparkling",
    "price_real_wine"
  ),
  shareNames = c(
    "w_beer",
    "w_vodka",
    "w_cognac",
    "w_sparkling",
    "w_wine"
  ),
  totExpName = "total_exp",
  data = df_aids5,
  method = "LA",
  priceIndex = "Ls",
  pxBase = list(shares = as.numeric(wMean5)),
  hom = TRUE,
  sym = TRUE,
  shifterNames = sh5,
  estMethod = "SUR"
)

summary(aids5_sur)

elas5_sur <- aidsElas(
  coef(aids5_sur),
  prices = pMean5,
  shares = wMean5,
  shifterValues = shMean5
)

elas5_sur


# 2. Модель 5 категорий: 3SLS Hausman-IV ---------------------------------

aids5_iv <- aidsEst(
  priceNames = c(
    "price_real_beer",
    "price_real_vodka",
    "price_real_cognac",
    "price_real_sparkling",
    "price_real_wine"
  ),
  shareNames = c(
    "w_beer",
    "w_vodka",
    "w_cognac",
    "w_sparkling",
    "w_wine"
  ),
  totExpName = "total_exp",
  data = df_aids5,
  method = "LA",
  priceIndex = "Ls",
  pxBase = list(shares = as.numeric(wMean5)),
  hom = TRUE,
  sym = TRUE,
  shifterNames = sh5,
  estMethod = "3SLS",
  instNames = c(
    "z_beer",
    "z_vodka",
    "z_cognac",
    "z_sparkling",
    "z_wine",
    "lxtr_exog",
    sh5
  )
)

summary(aids5_iv)

elas5_iv <- aidsElas(
  coef(aids5_iv),
  prices = pMean5,
  shares = wMean5,
  shifterValues = shMean5
)

elas5_iv


# 3. Модель 4 категории: SUR ---------------------------------------------

aids4_sur <- aidsEst(
  priceNames = c(
    "price_real_beer",
    "price_real_vodka",
    "price_real_cognac",
    "price_real_wine_all"
  ),
  shareNames = c(
    "w_beer",
    "w_vodka",
    "w_cognac",
    "w_wine_all"
  ),
  totExpName = "total_exp",
  data = df_aids4,
  method = "LA",
  priceIndex = "Ls",
  pxBase = list(shares = as.numeric(wMean4)),
  hom = TRUE,
  sym = TRUE,
  shifterNames = sh4_all,
  estMethod = "SUR"
)

summary(aids4_sur)

elas4_sur <- aidsElas(
  coef(aids4_sur),
  prices = pMean4,
  shares = wMean4,
  shifterValues = shMean4_all
)

elas4_sur


# Модель 4 категории: 3SLS Hausman-IV ---------------------------------

aids4_iv <- aidsEst(
  priceNames = c(
    "price_real_beer",
    "price_real_vodka",
    "price_real_cognac",
    "price_real_wine_all"
  ),
  shareNames = c(
    "w_beer",
    "w_vodka",
    "w_cognac",
    "w_wine_all"
  ),
  totExpName = "total_exp",
  data = df_aids4,
  method = "LA",
  priceIndex = "Ls",
  pxBase = list(shares = as.numeric(wMean4)),
  hom = TRUE,
  sym = TRUE,
  shifterNames = sh4_all,
  estMethod = "3SLS",
  instNames = c(
    "z_beer",
    "z_vodka",
    "z_cognac",
    "z_wine_all",
    "lxtr_exog",
    sh4_all
  )
)

summary(aids4_iv)

elas4_iv <- aidsElas(
  coef(aids4_iv),
  prices = pMean4,
  shares = wMean4,
  shifterValues = shMean4_all
)

elas4_iv



# 4.1. Модель 4 категории: 3SLS, регион + квартал -------------------------

aids4_iv_all <- aidsEst(
  priceNames = c(
    "price_real_beer",
    "price_real_vodka",
    "price_real_cognac",
    "price_real_wine_all"
  ),
  shareNames = c(
    "w_beer",
    "w_vodka",
    "w_cognac",
    "w_wine_all"
  ),
  totExpName = "total_exp",
  data = df_aids4,
  method = "LA",
  priceIndex = "Ls",
  pxBase = list(shares = as.numeric(wMean4)),
  hom = TRUE,
  sym = TRUE,
  shifterNames = sh4_all,
  estMethod = "3SLS",
  instNames = c(
    "z_beer",
    "z_vodka",
    "z_cognac",
    "z_wine_all",
    "lxtr_exog",
    sh4_all
  )
)

summary(aids4_iv_all)

elas4_iv_all <- aidsElas(
  coef(aids4_iv_all),
  prices = pMean4,
  shares = wMean4,
  shifterValues = shMean4_all
)

elas4_iv_all


# 4.2. Модель 4 категории: 3SLS, только региональные эффекты --------------

aids4_iv_region <- aidsEst(
  priceNames = c(
    "price_real_beer",
    "price_real_vodka",
    "price_real_cognac",
    "price_real_wine_all"
  ),
  shareNames = c(
    "w_beer",
    "w_vodka",
    "w_cognac",
    "w_wine_all"
  ),
  totExpName = "total_exp",
  data = df_aids4,
  method = "LA",
  priceIndex = "Ls",
  pxBase = list(shares = as.numeric(wMean4)),
  hom = TRUE,
  sym = TRUE,
  shifterNames = sh4_region,
  estMethod = "3SLS",
  instNames = c(
    "z_beer",
    "z_vodka",
    "z_cognac",
    "z_wine_all",
    "lxtr_exog",
    sh4_region
  )
)

summary(aids4_iv_region)

elas4_iv_region <- aidsElas(
  coef(aids4_iv_region),
  prices = pMean4,
  shares = wMean4,
  shifterValues = shMean4_region
)

elas4_iv_region


# 4.3. Модель 4 категории: 3SLS, только кварталы --------------------------

aids4_iv_quarter <- aidsEst(
  priceNames = c(
    "price_real_beer",
    "price_real_vodka",
    "price_real_cognac",
    "price_real_wine_all"
  ),
  shareNames = c(
    "w_beer",
    "w_vodka",
    "w_cognac",
    "w_wine_all"
  ),
  totExpName = "total_exp",
  data = df_aids4,
  method = "LA",
  priceIndex = "Ls",
  pxBase = list(shares = as.numeric(wMean4)),
  hom = TRUE,
  sym = TRUE,
  shifterNames = sh4_quarter,
  estMethod = "3SLS",
  instNames = c(
    "z_beer",
    "z_vodka",
    "z_cognac",
    "z_wine_all",
    "lxtr_exog",
    sh4_quarter
  )
)

summary(aids4_iv_quarter)

elas4_iv_quarter <- aidsElas(
  coef(aids4_iv_quarter),
  prices = pMean4,
  shares = wMean4,
  shifterValues = shMean4_quarter
)

elas4_iv_quarter


# 4.4. Модель 4 категории: 3SLS, без регионов и без кварталов -------------

aids4_iv_none <- aidsEst(
  priceNames = c(
    "price_real_beer",
    "price_real_vodka",
    "price_real_cognac",
    "price_real_wine_all"
  ),
  shareNames = c(
    "w_beer",
    "w_vodka",
    "w_cognac",
    "w_wine_all"
  ),
  totExpName = "total_exp",
  data = df_aids4,
  method = "LA",
  priceIndex = "Ls",
  pxBase = list(shares = as.numeric(wMean4)),
  hom = TRUE,
  sym = TRUE,
  estMethod = "3SLS",
  instNames = c(
    "z_beer",
    "z_vodka",
    "z_cognac",
    "z_wine_all",
    "lxtr_exog"
  )
)

summary(aids4_iv_none)

elas4_iv_none <- aidsElas(
  coef(aids4_iv_none),
  prices = pMean4,
  shares = wMean4
)

elas4_iv_none




# Таблица 1: сравнение 4 основных моделей --------------------------------

goods5 <- c("beer", "vodka", "cognac", "sparkling", "wine")
goods4 <- c("beer", "vodka", "cognac", "wine_all")

table_models <- bind_rows(
  tibble(
    model = "5 cat SUR",
    good = goods5,
    exp = as.numeric(elas5_sur$exp),
    marshall_own = diag(as.matrix(elas5_sur$marshall)),
    hicks_own = diag(as.matrix(elas5_sur$hicks))
  ),
  tibble(
    model = "5 cat 3SLS",
    good = goods5,
    exp = as.numeric(elas5_iv$exp),
    marshall_own = diag(as.matrix(elas5_iv$marshall)),
    hicks_own = diag(as.matrix(elas5_iv$hicks))
  ),
  tibble(
    model = "4 cat SUR",
    good = goods4,
    exp = as.numeric(elas4_sur$exp),
    marshall_own = diag(as.matrix(elas4_sur$marshall)),
    hicks_own = diag(as.matrix(elas4_sur$hicks))
  ),
  tibble(
    model = "4 cat 3SLS",
    good = goods4,
    exp = as.numeric(elas4_iv$exp),
    marshall_own = diag(as.matrix(elas4_iv$marshall)),
    hicks_own = diag(as.matrix(elas4_iv$hicks))
  )
) %>%
  mutate(across(c(exp, marshall_own, hicks_own), ~ round(.x, 3)))

table_models


# Таблица 2: сравнение спецификаций 4-категорийной 3SLS -------------------

table_specs4 <- bind_rows(
  tibble(
    spec = "region + quarter",
    good = goods4,
    exp = as.numeric(elas4_iv_all$exp),
    marshall_own = diag(as.matrix(elas4_iv_all$marshall)),
    hicks_own = diag(as.matrix(elas4_iv_all$hicks))
  ),
  tibble(
    spec = "region only",
    good = goods4,
    exp = as.numeric(elas4_iv_region$exp),
    marshall_own = diag(as.matrix(elas4_iv_region$marshall)),
    hicks_own = diag(as.matrix(elas4_iv_region$hicks))
  ),
  tibble(
    spec = "quarter only",
    good = goods4,
    exp = as.numeric(elas4_iv_quarter$exp),
    marshall_own = diag(as.matrix(elas4_iv_quarter$marshall)),
    hicks_own = diag(as.matrix(elas4_iv_quarter$hicks))
  ),
  tibble(
    spec = "no FE",
    good = goods4,
    exp = as.numeric(elas4_iv_none$exp),
    marshall_own = diag(as.matrix(elas4_iv_none$marshall)),
    hicks_own = diag(as.matrix(elas4_iv_none$hicks))
  )
) %>%
  mutate(across(c(exp, marshall_own, hicks_own), ~ round(.x, 3)))

table_specs4

View(table_models)
View(table_specs4)
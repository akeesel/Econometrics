install.packages("zoo", "xts", "lubridate", "urca", "quantmod", "dplyr", "ggplot2", "forecast", "tseries", "Metrics")

library("zoo") # 
library("xts")
library("lubridate")
library("urca")
library("quantmod")
library("dplyr")
library("ggplot2")
library("forecast")
library("tseries")
library("Metrics")

####################################
# Задание 1.
####################################

# Загрузка временных рядов
mytimeseries <- read.csv("C:/Users/tempadmin/Desktop/ЭВР_ДЗ/mytimeseris.csv", sep=";", dec=",", header=TRUE)
X25 <- mytimeseries[, c('period', 'X25')]
X19 <- mytimeseries[, c('period', 'X19')]
X10 <- mytimeseries[, c('period', 'X10')]
y25 <- X25$X25
y19 <- X19$X19
y10 <- X10$X10

summary(y25)

# График ряда
y25_ts <- ts(y25, start=1, frequency=1)
autoplot(y25_ts) + ggtitle("Временной ряд y25") + xlab("Время") + ylab("Значение ряда")
plot.ts(y25, main = "Временной ряд X25")

acf(y25)
pacf(y25)
tsdisplay(y25) 



##### ПУНКТ А.2 ТЕСТИРОВАНИЕ СТАЦИОНАРНОСТИ --------------------------------------------------------------------

# Расширенный тест Дики-Фулера ADF (Augmented Dickey-Fuller Test)
adf.test(y25) # H0: ряд нестационарен
adf_test <- adf.test(y25) # H0: нестационарность. малое p => отвергаем H0
adf_test$statistic
adf_test$p.value

# KPSS-тест H0: ряд стационарен -> малое p => отвергаем H0 => нестационарно
kpss.test(y25, null="Level")  # проверка стационарности уровня
kpss.test(y25, null="Trend")  # проверка стационарности с трендом
kpss_test_level <- kpss.test(y25, null="Level")
kpss_test_trend <- kpss.test(y25, null="Trend")
kpss_test_level$statistic
kpss_test_level$p.value



##### Пункт А.3 --------------------------------------------------------------------
Acf(y25, main = "ACF y25")
Pacf(y25, main = "PACF y25")

# порог значимости:
1.96 / sqrt(length(y25))

acf(y25, plot=FALSE)$acf[2:7]   # лаги 1–6
pacf(y25, plot=FALSE)$acf[1:6]  # лаги 1–6

# Для ACF значимы лаги 1, 2 
# Для PACF значимы лаги 1, 2, 4, 5 



##### ПУНКТ А.4 ПОДБОР МОДЕЛЕЙ ARIMA ДЛЯ ВРЕМЕННЫХ РЯДОВ -----------------------------------------------------------------------------
# Рекомендация: p+q <= 3

# Критерии сравнения: 
# AIC/BIC - Меньше — лучш
# Остатки должны быть белым шумом


# Для ряда X25:

ar1  = Arima(y25, order=c(1,0,0))
summary(ar1)
acf(ar1$residuals) # не подходит

ar2  = Arima(y25, order=c(2,0,0))
summary(ar2)
acf(ar2$residuals) # 

ar3 = Arima(y25, order=c(3,0,0))
summary(ar3)
acf(ar3$residuals) #

ma1 = Arima(y25, order=c(0,0,1))
summary(ma1)
acf(ma1$residuals) # не подходит

ma2 = Arima(y25, order=c(0,0,2))
summary(ma2)
acf(ma2$residuals) # 

ma3 = Arima(y25, order=c(0,0,3))
summary(ma3)
acf(ma3$residuals) # 

arma11 = Arima(y25, order=c(1,0,1))
summary(arma11)
acf(arma11$residuals) #

arma21 = Arima(y25, order=c(2,0,1))
summary(arma21)
acf(arma21$residuals) # не подходит
plot(arma21$residuals)

arma12 = Arima(y25, order=c(1,0,2))
summary(arma11)
plot(arma12$residuals)
acf(arma12$residuals) #



best_model <- ma3
summary(best_model)
checkresiduals(best_model)


# Оценка остаточных ошибок модели с помощью теста Льюинга-Бокса
# H0: остатки — белый шум (нет автокорреляции) p-val > 0,05 принимаем
# H1: в остатках есть автокорреляция (модель плохая) p-val < 0,05
Box.test(best_model$residuals, fitdf = 1, lag = 30)
Box.test(residuals(best_model), lag=30, type="Ljung-Box")



##### ПУНКТ A.6 Псевдо-вневыборочный прогноз + сравнение моделей
train_n <- floor(0.85 * length(y25)) # Выбираем 85% выборки, округление к меньшему
train <- y25[1:train_n]
test  <- y25[(train_n+1):length(y25)]
h <- length(test)

best_fit <- Arima(train, model = best_model)  # обучаем заново MA(3) на train
forecast_1 <- forecast(best_fit, h=h)
best_pred <- forecast_1$mean

other_fit <- Arima(train, order=c(2,0,0)) # Другая модель AR(2)
forecast_2  <- forecast(other_fit, h=h)
other_pred <- forecast_2$mean

# Метрики прогнозов
best_metrics <- data.frame(RMSE = rmse(test, best_pred),
                           MAE  = mae(test, best_pred),
                           MAPE = mape(test, best_pred) )

other_metrics <- data.frame(RMSE = rmse(test, other_pred),
                            MAE  = mae(test, other_pred),
                            MAPE = mape(test, other_pred) )

best_metrics
other_metrics



##### ПУНКТ Б --------------------------------------------------------------
# best_model - Arima(y_train, order = c(2,0,1))
forecast_best <- forecast(best_model, h = 5) # Прогноз на 5 периодов вперед

# Показать прогнозные значения
forecast_best

# Построить график прогноза
autoplot(forecast_best)

autoplot(forecast_best) +
  xlim(length(y25) - 40, length(y25) + h) +
  ggtitle("Прогноз (увеличенный фрагмент)")



##### ПУНКТ В --------------------------------------------------------------

# Второй ряд
model_y19 <- auto.arima(y19)
summary(model_y19)

# Третий ряд
model_y10 <- auto.arima(y10)
summary(model_y10)



##### ЗАДАНИЕ 2 ----------------------------------------------------------------
install.packages("mFilter")
library(mFilter)
library(TTR)

gdp <- read.csv("C:/Users/tempadmin/Desktop/ЭВР_ДЗ/TASK_2_MYVAR.csv", sep=";", dec=",", header=TRUE)
gdp_ts <- ts(gdp$gdp, start = 1995, frequency = 1)   # если годовые данные

autoplot(gdp_ts) + ggtitle("gdp") + xlab("year") + ylab("value") # График ВВП


# 1) Скользящее среднее по 5 годам (moving average)
# MA(5)
gdp_ma5 <- SMA(gdp_ts, n = 5)
# цикл = фактический минус тренд (MA)
cycle_ma5 <- gdp_ts - gdp_ma5

# 2) Фильтр Ходрика–Прескотта (HP-filter)
# Для годовых данных λ = 100 (так принято)
hp <- hpfilter(gdp_ts, freq = 100)
gdp_hp_trend <- hp$trend
gdp_hp_cycle <- hp$cycle

cycle_ma5
gdp_hp_cycle
gdp_hp_trend
gdp_ma5
gdp_ts

# 3) ГРАФИКИ

# a) Исходный ряд + тренды
autoplot(gdp_ts, series = "GDP") +
  autolayer(gdp_ma5, series = "MA(5)", color="blue") +
  autolayer(gdp_hp_trend, series = "HP trend", color="red") +
  ggtitle("ВВП: исходный ряд и тренды (MA и HP)") +
  ylab("ВВП") + xlab("Год")


# b) Циклы
autoplot(cycle_ma5, series = "Cycle MA(5)", color="black") +
  autolayer(gdp_hp_cycle, series = "Cycle HP", color="blue") +
  ggtitle("Циклическая компонента ВВП (MA vs HP)") +
  ylab("Отклонение от тренда") + xlab("Год")


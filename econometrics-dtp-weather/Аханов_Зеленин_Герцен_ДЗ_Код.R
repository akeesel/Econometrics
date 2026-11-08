library(readxl)
library(plm)

Data <- read_excel("данные эконометрика.xlsx")
colnames(Data) <- c("city", "Year", "month", "Temp", "Precip", "Precipdays", "PrecipIntensity",
                    "DTP_rate", "Sunlight", "Alcohol", "Wind", "Pressure", "Humidity", "Holidays")

cor(Data[, c("Temp", "PrecipIntensity", "Sunlight", "Alcohol", "Wind" )])
#оказалось, что светлые часы сильно коррелирую с температурой (cor=0,87) => уберем светлые часы 

install.packages("car")
library(car)
model <- lm(DTP_rate ~ PrecipIntensity + Temp + Alcohol + Wind, data = Data)
vif(model)  #проверка мультиколлинеарности. все VIF < 2 => все хорошо

hist(Data$PrecipIntensity)
hist(Data$DTP_rate) #данные сильно смещены => будем логарифмировать для смегчения выбросов
hist(Data$Temp)
hist(Data$Alcohol)

#проведем первичный анализ

model00 <-  plm(DTP_rate ~  + PrecipIntensity + Temp + Alcohol + Wind, data = Data, 
                index = c("city", "month"), model = "pooling")
summary(model00)

#оказалось, что интенсивность осадков не значима. предполагаем, что  переменная является эндогенной 
#из-за пропуска существенной переменной - загруженности дорог (интенсивност осадков влияет на принятие
#решения о том, стоит ли ехать в принципе, что, в свою очередь, влияет на число ДТП)

install.packages("AER")
install.packages("lmtest")
install.packages("sandwich")
library(AER)       # для ivreg() 
library(lmtest)    
library(sandwich)

iv_model <- ivreg(DTP_rate ~ PrecipIntensity + Temp + Alcohol + Wind |
                     Humidity + Temp + Alcohol + Wind, data = Data)

summary(iv_model, diagnostics = TRUE)

#подтвердилась эндогенность интенсивности осадков на 5% уровне значимости
#влажность - релевантный (F статистика > 10 => сильный инструмент) 
#и экзогенный (напрямую не влияет на дтп) инструмент для оценки интенсивности осадков

#построим модели с оцененной интенсивностью дождя
library(fixest)

#Первая стадия: прогнозируем интенсивность осадков
first_stage <- lm(PrecipIntensity ~ Humidity + Temp + Alcohol + Wind, data = Data)

# Сохраняем предсказанные значения в новую переменную
Data$PrecipInt_new <- pmax(predict(first_stage), 0)
cor(Data$PrecipIntensity, Data$PrecipInt_new)
hist(Data$PrecipInt_new)

model01 <-  plm(DTP_rate ~  + PrecipInt_new + Temp + Alcohol + Wind, data = Data, 
                index = c("city", "month"), model = "pooling")
summary(model01)

model02 <- plm(DTP_rate ~  + PrecipInt_new + Temp + Alcohol + Wind,
               data = Data, index = c("city", "month"), model = "within", effect = "individual")
summary(model02)

model03 <- plm(DTP_rate ~  + PrecipInt_new + Temp + Alcohol + Wind,
               data = Data, index = c("city", "month"), model = "within", effect = "twoways")
summary(model03)

#интенсивность осадков стала значимой

#теперь вспомним, что данные ДТП смещены. проверим, подходит ли функциональная форма 
#используем тест Рамсея

library(lmtest)

#построим сначала модель множественной регрессии
model_ols <- lm(DTP_rate ~ PrecipInt_new + Temp + Wind + Alcohol, data = Data)

#проведем тест Рамсея на правильность функциональной формы
resettest(model_ols, power = 2:3)
#получили, что функциональная форма на 5% уровне значимости не является корректной

library(ggplot2)
ggplot(Data, aes(x = PrecipInt_new, y = DTP_rate)) +
  geom_point() 

#предполагаем, что интенсивность осадков сначала увеличивает количество ДТП, но с определенного момента 
#снижает. добавим квадрат интенсивности

model_nice1 <-  plm(DTP_rate ~  + PrecipInt_new + I(PrecipInt_new^2) +Temp + Alcohol + Wind, 
                data = Data, index = c("city", "month"), model = "within", effect = "individual")
summary(model_nice1)
#уже что-то

#рассмотрим несколько видов моделей

model_nice2 <- plm(log(DTP_rate) ~ PrecipInt_new + I(PrecipInt_new^2) + Temp + Wind + log(Alcohol), 
              data = Data, index = c("city", "month"), model = "within", effect = "individual")
summary(model_nice2)
#берем


model_nice3 <- plm(DTP_rate ~ PrecipInt_new + I(PrecipInt_new^2) + Temp + Wind + log(Alcohol), 
                   data = Data, index = c("city", "month"), model = "within", effect = "individual")
summary(model_nice3)
#берем


Data$TempXPrecipInt_new <- Data$PrecipInt_new*Data$Temp
Data$TempXWind <- Data$Wind*Data$Temp
#добавим попарные произведения
model_XX <- plm(DTP_rate ~ PrecipInt_new + I(PrecipInt_new^2) + log(Alcohol) + Temp +
              Wind + TempXWind + TempXPrecipInt_new, data = Data, index = c("city", "month"),
              model = "within",effect = "individual")
summary(model_XX)
#не берем

#проверим правильность спецификации 3 выбранных моделей
test_model1 <- lm(DTP_rate ~ PrecipInt_new + I(PrecipInt_new^2) + Alcohol + Temp + Wind, data = Data)
resettest(test_model1)

test_model2 <- lm(log(DTP_rate) ~ PrecipInt_new + I(PrecipInt_new^2) + log(Alcohol) + Temp + Wind, data = Data)
resettest(test_model2)

test_model3 <- lm(DTP_rate ~ PrecipInt_new + I(PrecipInt_new^2) + log(Alcohol) + Temp + Wind, data = Data)
resettest(test_model3)

#результаты показывают, что модель 1 имеет неверную спецификацию на любом разумном уровне значимости,
#модели 2 и 3 имеют верную спецификацию на любом разумном уровне значимости, однако p-значние модели 2
#значительно превышает p-значение модели 3

model_nice4 <- plm(log(DTP_rate) ~ PrecipInt_new + I(PrecipInt_new^2) + Temp + Wind + log(Alcohol) + 
                   log(Pressure) + Holidays,  data = Data, index = c("city", "month"), model = "within", effect = "individual")
summary(model_nice4)
#берем


#добавим в модель новые переменные Holidays и Pressure

cor(Data[, c("Temp", "PrecipIntensity", "Sunlight", "Alcohol", "Wind", "Holidays", "Pressure" )])

modelWW <- lm(DTP_rate ~ PrecipIntensity + Temp + Alcohol + Wind + Holidays + Pressure, data = Data)
vif(modelWW)

#Первая стадия: прогнозируем интенсивность осадков
first_stage2 <- lm(PrecipIntensity ~ Humidity + Temp + Alcohol + Wind + Holidays + Pressure, data = Data)
Data$PrecipInt_new2 <- pmax(predict(first_stage2), 0)

model_nice11 <-  plm(DTP_rate ~  + PrecipInt_new2 + I(PrecipInt_new2^2) + Temp + Alcohol + Wind + 
                       Holidays + log(Pressure), 
                    data = Data, index = c("city", "month"), model = "within", effect = "individual")
summary(model_nice11)
test_model11 <- lm(DTP_rate ~  + PrecipInt_new2 + I(PrecipInt_new2^2) + Temp + Alcohol + Wind + 
                     Holidays + log(Pressure), data = Data)
resettest(test_model11)
#хорощая модель и спецификация

model_nice22 <- plm(DTP_rate ~  + PrecipInt_new2 + I(PrecipInt_new2^2) + Temp + log(Alcohol) + Wind + Holidays + log(Pressure), 
                    data = Data, index = c("city", "month"), model = "within", effect = "individual")
summary(model_nice22)
test_model22 <- lm(DTP_rate ~  + PrecipInt_new2 + I(PrecipInt_new2^2) + Temp + log(Alcohol) + Wind + Holidays + log(Pressure), data = Data)
resettest(test_model22)
#неверная спецификация


model_nice33 <-  plm(log(DTP_rate) ~  + PrecipInt_new2 + I(PrecipInt_new2^2) + Temp + log(Alcohol) + Wind + Holidays + log(Pressure), 
                     data = Data, index = c("city", "month"), model = "within", effect = "individual")
summary(model_nice33)
test_model33 <- lm(log(DTP_rate) ~  + PrecipInt_new2 + I(PrecipInt_new2^2) + Temp + log(Alcohol) + Wind + Holidays + log(Pressure), data = Data)
resettest(test_model33)
#верная спецификация, но с меньшим p-value. подойдет в качестве подтверждения нелинейного эффекта

#Дружелюбная эконометрика. Глава 3

# Если вам нужно очистить рабочее пространство от старых объектов:
rm(list = ls())

#Устанавливаем и подключаем сет нужных пакетов
#Напоминаю, что инсталлировать достаточно один раз,
#а подключать нужно во время каждой сессии

# install.packages("lmtest")
# install.packages("stargazer")
# install.packages("ggplot2")

library("ggplot2")   #Графики
library("stargazer") #Красивые таблички
library("lmtest")    #Статистические тесты
library('readxl')
# В диаграммах пакета qplot 
# можно менять размер шрифта (default is 12)
theme_set(theme_gray(base_size = 18))

# D <- read.csv("Students.csv", sep=";", dec=",", header=TRUE)
D <- read_excel('Students.xlsx')

#Коэффициенты корреляции между переменными
cor(D)

#Графики для предварительного анализа данных: 
#Диаграмма для переменной CLASS
qplot(data=D, CLASS)
#Диаграмма рассеяния для переменных CLASS и TEST 
qplot(data=D, CLASS, TEST)

#Модель №1
model_1 <- lm(data = D, TEST ~ CLASS)
summary(model_1)

#Модель №2
model_2 <- lm(data = D, TEST ~ CLASS+EXPN)
model_2
summary(model_2)

#Модель №3
model_3 <- lm(data = D, TEST ~ CLASS+EXPN+INCOME)
summary(model_3)

#Сводная табличка
stargazer(model_1, model_2, model_3,
          title="Simple cross-section regressions", type="text", 
          column.labels=c("Модель 1", "Модель 2", "Модель 3"), 
          df=FALSE, digits=2)

#Сводная табличка (со степенями свободы)
stargazer(model_1, model_2, model_3,
          title="Simple cross-section regressions", type="text", 
          column.labels=c("Модель 1", "Модель 2", "Модель 3"), 
          df=TRUE, digits=2)

#Сравнение двух моделей при помощи теста на линейное ограничение
# Проверяем гипотезу о равенстве нулю кэфов при добавленных переменных
waldtest(model_1,model_3)

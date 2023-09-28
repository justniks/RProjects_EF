#Дружелюбная эконометрика. Глава 4

# Если вам нужно очистить рабочее пространство от старых объектов:
rm(list = ls())

library("ggplot2")   #Графики
library("stargazer") #Красивые таблички
library("lmtest")    #Статистические тесты
library('readxl')

# В диаграммах пакета qplot 
# можно менять размер шрифта (default is 12)
theme_set(theme_gray(base_size = 18))

# D <- read.csv("Сottage.csv", sep=";", dec=",", header=TRUE)
D <- read_excel('Cottage.xlsx')

#Описательные статистики
summary(D)
#Коэффициенты корреляции между переменными
cor(D)


#Модель №1
model_1 <- lm(data = D, price ~ living_area+total_area+land+dist)
summary(model_1)


# VIF
install.packages("car")
library(car)
vif(model_1)

#Вспомогательная регрессия для исследования мультиколлинеарности
model_0 <- lm(data = D, total_area ~ living_area+land+dist)
summary(model_0)
1/(1-0.9276)

#Модель №2
model_2 <- lm(data = D, price ~ total_area+land+dist)
summary(model_2)

#Модель №3
model_3 <- lm(data = D, log(price) ~ log(total_area)+log(land)+log(dist))
summary(model_3)

#Модель №4
model_4 <- lm(data = D, log(price) ~ log(total_area)+log(land)+log(dist)+lake)
summary(model_4)

#Сводная табличка
stargazer(model_1, model_2, model_3, model_4,
          title="Модели цены коттеджа", type="text", 
          column.labels=c("Модель 1", "Модель 2", "Модель 3", "Модель 4"), 
          df=FALSE, digits=2)


#Модель №5
model_5 <- lm(data = D, log(price) ~ log(total_area)+log(land)+log(dist)+lake:log(total_area))
summary(model_5)

#Тест Рамсея для модели №5
reset(model_5)


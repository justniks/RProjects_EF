#Глава 5
#Гетероскедастичность


library("sandwich")
library("lmtest")
library("stargazer") 



#Функции для робастных стандартных ошибок:
#Робастные к гетероскедастичности стандартные ошибки в случае использования МНК:
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}


#Загружаем массив данных
D <- read.csv("Agriculture.csv", sep=";", dec=",", header=TRUE)


#Используем для начала обычный МНК
model1 <- lm(data=D, PRODP~FUNG1+FUNG2+YDOB1+YDOB2+GIRB+INSEC+LABOUR)
summary(model1)

stargazer(model1,   
          title="Model1 с обычными стандартными ошибками", type="text", 
          column.labels=c("OLS"), 
          df=FALSE, digits=4)

stargazer(model1,   
          se=list(cse(model1)), 
          title="Model1 с робастными стандартными ошибками", type="text", 
          column.labels=c("OLS"), 
          df=FALSE, digits=4)

#Тест Бреуша -- Пагана на отсутствие гетероскедастичности
bptest(model1)

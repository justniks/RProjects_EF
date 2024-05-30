install.packages('dplyr','ggplot2')
install.packages('frontier','readxl')

library(dplyr)
library(frontier)
library(ggplot2)
library(readxl)

data("riceProdPhil")

riceProdPhil %>% ggplot(aes(LABOR,PROD)) + geom_point()

m0 <- lm(log(PROD/AREA)~log(LABOR/AREA), riceProdPhil)
summary(m0)

riceProdPhil %>% ggplot(aes(x=LABOR/AREA, y=PROD/AREA)) + geom_point() +
  geom_function(fun = function(x) exp(m0$coefficients[1]+log(x)*m0$coefficients[2]))+
  geom_function(fun = function(x) exp(m0$coefficients[1]+log(x)*m0$coefficients[2]+max(m0$residuals)))

m1 <- sfa(log(PROD/AREA)~log(LABOR/AREA), riceProdPhil, ineffDecrease = T)
summary(m1)

m1$olsParam
m1$mleParam
m1$olsResid

riceProdPhil %>% ggplot(aes(x=LABOR/AREA, y=PROD/AREA)) + geom_point() +
  geom_function(fun = function(x) exp(m1$olsParam[1]+log(x)*m1$olsParam[2]), lty='dotted')+
  geom_function(fun = function(x) exp(m1$olsParam[1]+log(x)*m1$olsParam[2]+max(m1$olsResid)))+
  geom_function(fun = function(x) exp(m1$mleParam[1]+log(x)*m1$mleParam[2]), lty='dashed')

efficiencies(m1) %>% hist()

###
textile <- read_xlsx('textile.xlsx')

m2 <- sfa(log(value)~log(os)+log(wage), textile)
summary(m2)

textile$predict.sfa <- predict(m2)

textile %>% ggplot(aes(x=log(os))) +
  geom_point(aes(y=log(value)), alpha=0.1) +
  geom_smooth(aes(y=predict.sfa), color='red')
  
textile$eff.sfa <- efficiencies(m2)

textile %>% ggplot(aes(eff.sfa, color=`Получатель поддержки`)) + geom_density()
textile %>% ggplot(aes(eff.sfa, color=factor(gov))) + geom_density()
textile %>% ggplot(aes(y=eff.sfa, x=`HHI (Региональный)`)) + geom_smooth()

tobacco <- read_xlsx('tobacco.xlsx')

m3 <- sfa(log(value)~log(os)+log(wage), tobacco)
summary(m3)

tobacco$predict.sfa <- predict(m3)

tobacco %>% ggplot(aes(x=log(os)))+
  geom_point(aes(y=log(value)), alpha=0.1)+
  geom_smooth(aes(y=predict.sfa), color='red')

tobacco$eff.sfa <- efficiencies(m3)

hist(tobacco$eff.sfa)

tobacco %>% ggplot(aes(y=eff.sfa, color=sobs)) + geom_boxplot()

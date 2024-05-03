#install.packages('dplyr','ggplot2')
# install.packages('frontier','readxl')

library(dplyr)
library(frontier)
library(ggplot2)
library(readxl)

data("riceProdPhil")
# textile <- read_xlsx('textile.xlsx')
# tobacco <- read_xlsx('tobacco.xlsx') 
riceProdPhil %>%
  ggplot(aes(PROD)) + geom_density()

riceProdPhil %>%
  ggplot( aes(log(PROD)) ) + geom_density()

riceProdPhil %>%
  ggplot( aes(LABOR) ) + geom_density()

riceProdPhil %>%
  ggplot( aes(log(LABOR)) ) + geom_density()

riceProdPhil %>%
  ggplot( aes(NPK) ) + geom_histogram()

riceProdPhil %>%
  ggplot( aes(log(NPK)) ) + geom_histogram()

###
m0 <- lm( log(PROD) ~ log(AREA), data=riceProdPhil )
summary(m0)

riceProdPhil %>% 
  ggplot( aes(x=AREA, y=PROD) ) + geom_point() + geom_smooth(method='lm')

riceProdPhil %>%
  ggplot( aes(x=AREA, y=PROD) ) + geom_point() + geom_function(fun=function(x) exp(m0$coefficients[1] + m0$coefficients[2] * log(x)))

m1 <- lm(log(PROD) ~ log(AREA) + log(LABOR) + log(NPK), data=riceProdPhil)
summary(m1)

m1$residuals %>% hist()

m2 <- sfa(log(PROD) ~ log(AREA) + log(LABOR) + log(NPK), data=riceProdPhil, ineffDecrease=T)
summary(m2)
summary(m2, extraPar=T)
m2$mleParam[2] + m2$mleParam[3] + m2$mleParam[4]

m2_1 <- sfa(log(PROD) ~ log(AREA), data=riceProdPhil, ineffDecrease=T)
summary(m2_1)

riceProdPhil %>% 
  ggplot (aes(x=AREA, y=PROD) ) + geom_point() +
  geom_function(fun=function(x) exp(m2_1$olsParam[1] + m2_1$olsParam[2] * log(x)), col='red') +
  geom_function(fun=function(x) exp(m2_1$mleParam[1] + m2_1$mleParam[2] * log(x)), col='blue')

efficiencies(m2) %>% hist()

riceProdPhil$eff.sfa <- efficiencies(m2)
riceProdPhil %>% ggplot( aes(eff.sfa, col=factor(YEARDUM)) ) + geom_density()
                        


install.packages('dplyr', 'ggplot2')
install.packages('frontier', 'readxl')
library('dplyr')
library('ggplot2')
library('frontier')
library('readxl')

#### ЧАСТЬ 1 ---------------------------------------------------------------------------------
data('riceProdPhil')
summary(riceProdPhil)
help(riceProdPhil)

#Production
riceProdPhil %>%
  ggplot(aes(PROD)) + geom_density()
riceProdPhil %>%
  ggplot(aes(log(PROD))) + geom_density()

#Labor
riceProdPhil %>%
  ggplot(aes(LABOR)) + geom_density()
riceProdPhil %>%
  ggplot(aes(log(LABOR))) + geom_density()

#NPK
riceProdPhil %>%
  ggplot(aes(NPK)) + geom_density()
riceProdPhil %>%
  ggplot(aes(log(NPK))) + geom_density()

#AREA
riceProdPhil %>%
  ggplot(aes(AREA)) + geom_density()
riceProdPhil %>%
  ggplot(aes(log(AREA))) + geom_density()

#Cobb-Douglas ordinary regression -------------------------
riceProdPhil %>%
  ggplot(aes(LABOR, PROD)) + geom_point()

m0 <- lm(log(PROD) ~ log(LABOR), riceProdPhil)
summary(m0)

riceProdPhil %>%
  ggplot(aes(LABOR, PROD)) + geom_point() + 
  geom_function(fun = function(x) exp(m0$coefficients[1] + log(x) * m0$coefficients[2]))

m1 <- lm(log(PROD) ~ log(LABOR) + log(NPK) + log(AREA), riceProdPhil)
summary(m1)

m1$residuals %>% hist()
  

#Переходим к фронтиру
m2 <- sfa(log(PROD) ~ log(LABOR) + log(NPK) + log(AREA), riceProdPhil, ineffDecrease = T) 
summary(m2)
#sigmaSq - sum of the parameterised variance of v and u
# Gamma = share of the variance of u in the total variance sigmaSq 
#gamma = 0 => inefficiency term u is irrelevant => лучше OLS
# еслм gamma=1 => noise term v is irrelevant - все объясняется technical inefficiency 
# ineffDecrease = T - для оценки эффективности, ищем по верхней грани, F - для издержек
m2_1 <- sfa(log(PROD) ~ log(LABOR) + log(NPK) + log(AREA), riceProdPhil, ineffDecrease = F) 
summary(m2_1)

#Добавим подвал
summary(m2, extraPar = TRUE)
#gammaVar - сколько вариации объясняется неэффективностью

m1$coefficients[2] + m1$coefficients[3] + m1$coefficients[4]  
m2$mleParam[2] + m2$mleParam[3] + m2$mleParam[4]  

efficiencies(m2) %>% hist()

m2_2 <- sfa(log(PROD) ~ log(LABOR), riceProdPhil, ineffDecrease = T) 
summary(m2_2)

riceProdPhil %>%
  ggplot(aes(x = LABOR, y = PROD)) + geom_point() + 
  geom_function(fun = function(x) exp(m2_2$olsParam[1] + log(x) * m2_2$olsParam[2]), lty = 'dotted') +
  #geom_function(fun = function(x) exp(m2_2$olsParam[1] + log(x) * m2_2$olsParam[2] + max(m2_2$olsResid))) + 
  geom_function(fun = function(x) exp(m2_2$mleParam[1] + log(x) * m2_2$mleParam[2]), lty = 'dashed')


#### ЧАСТЬ 2 ---------------------------------------------------------------------------------
setwd('/Users/olga/Downloads/SFA_data')
textile <- read_xlsx('textile.xlsx')

#active - актив баланса
#os - ОС
#value - выручка
#chprib - чистая прибыль
#sebest - себестоимость
#subsidy - объем полученных субсидий
#wage - среднемесячная начисленная з/п в регионе регистрации компании (росстат)
#HHI (Региональный) - по доле выручки всех компаний в пределах этого оквэд зарегистрированных в данном регионе
#gov - принадлежность к госсобственности - есть ли в названии формы собственности слова муниципалитет/субъект/РФ - т.е. это в том числе все ГУПЫ, ФГУПЫ и прочее. В том числе в долевой собственности государства и еще кого-то. 1 - есть государство, 0 - нет.
#Получатель поддержки - бинарочка да/нет, по смыслу то же, что и subsidy>0

summary(textile)

textile$"wage" %>% hist()
textile$"os" %>% hist()
textile$"value" %>% hist()
textile$"active" %>% hist()

textile %>% ggplot(aes(x= os, y = value)) + geom_point()
textile %>% ggplot(aes(x= wage, y = value)) + geom_point()

textile %>% ggplot(aes(x= log(os), y = log(value))) + geom_point()
textile %>% ggplot(aes(x= log(wage), y = log(value))) + geom_point()


m3 <- sfa(log(value) ~ log(wage) + log(os), textile)
summary(m3)
textile$eff.sfa <- efficiencies(m3)

textile %>% ggplot(aes(y=eff.sfa, x=log(value))) + 
  geom_point() + geom_smooth(method='lm', formula= y~x)

textile %>% ggplot(aes(eff.sfa, color = `Получатель поддержки`)) + geom_density()

textile %>% ggplot(aes(eff.sfa, color = factor(gov))) + geom_density()

textile %>% ggplot(aes(y = eff.sfa, x = `HHI (Региональный)`)) + geom_smooth() 



#### ЧАСТЬ 3 ---------------------------------------------------------------------------------
tobacco <- read_xlsx('tobacco.xlsx')
m4 <- sfa(log(value) ~ log(wage) + log(os), tobacco)
summary(m4)
tobacco$eff.sfa <- efficiencies(m4)

tobacco$eff.sfa %>% hist()


tobacco %>% ggplot(aes(y = eff.sfa, x = active)) + geom_smooth() 

tobacco %>% ggplot(aes(eff.sfa, color = sobs)) + geom_density()

tobacco %>% ggplot(aes(y = eff.sfa, color = sobs)) + geom_boxplot()


tobacco_short <- subset(tobacco, tobacco$sobs != "Совместная частная и иностранная собственность")
tobacco_short %>% ggplot(aes(eff.sfa, color = sobs)) + geom_density()

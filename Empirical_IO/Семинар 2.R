# setwd("/Users/olgamarkova/Yandex.Disk.localized/EIO/ЭОР_бакалавриат")

#libraries
library(magrittr)
library(dplyr)
library(Hmisc) #summary stats
library(stargazer)
library("modelsummary")
library("ivreg")
library(plm)
library(ggplot2)

#### EX1 ----------------------------------------------------------
#data
data <- read.csv("Семинар 2_dataset.csv")

ggplot(data, aes(x=data$servings_sold, y=data$price_per_serving)) + 
  geom_point()

#look at the data

#define market
data$'market' <- paste('C', data$city, 'Q', data$quarter)
data$'market_size' <- data$'city_population' * 70
data$'market_share' <- data$'servings_sold' / data$'market_size'
 
data <- data %>%
  group_by(market) %>%
  mutate(outside_share = 1 - sum(market_share))
summary(data[,c("market_share","outside_share")])

ggplot(data, aes(x=data$market_share, y=data$price_per_serving)) + 
  geom_point()

### EX 2 ----------------------------------------------------------

data$'logit_delta' <- log(data$'market_share' / data$'outside_share')

m1 = lm(logit_delta ~ price_per_serving + mushy + sugar, data=data) 
summary(m1) 
msummary(m1, vcov='HC0', stars=TRUE) # с поправкой на гетероскедастичность

### EX 3  ----------------------------------------------------------

m2_1 = lm(
  logit_delta ~ price_per_serving + mushy + sugar + as.factor(market) + as.factor(product), 
  data=data
) 
summary(m2_1)

m2 <- plm(
  logit_delta ~ price_per_serving + mushy + sugar, 
  data=data,
  index=c("market", "product"), 
  model="within", effect='twoways'
)
summary(m2)
msummary(m2, vcov='HC0', stars=TRUE) 

ggplot(data, aes(x=data$price_instrument, y=data$price_per_serving)) + 
  geom_point()

m3_1 = lm(price_per_serving ~ price_instrument + mushy + sugar, data=data) 
summary(m3_1)

m3 = ivreg(
  logit_delta ~ price_per_serving + mushy + sugar | price_instrument + mushy + sugar, 
  data=data
) 
summary(m3) # инструменты релевантны + МНК-оценки несостоятельны
msummary(m3, vcov='HC0', stars=TRUE) 



## Делаем инструмент Хаусмана
# for (i in 1:nrow(data)) {
#   subdf <- data %>%
#     filter(city != data$city[i]) %>%
#     filter(quarter == data$quarter[i]) %>%
#     filter(product == data$product[i])
#   data$hausman[i] <- mean(subdf$price_per_serving)
# }
for (i in 1:nrow(data)){
  subdf <- data %>%
    filter(city != data$city[i]) %>%
    filter(quarter == data$quarter[i]) %>%
    filter(product_ids == data$product_ids[i])
  data$hausman[i] <- mean(subdf$price_per_serving)
}

ggplot(data, aes(x=data$hausman, y=data$price_per_serving)) + 
  geom_point()

m4_1 = lm(price_per_serving ~ hausman + mushy + sugar, data=data) 
summary(m4_1)

m4 = ivreg(
  logit_delta ~ price_per_serving + mushy + sugar | hausman + mushy + sugar, 
  data=data
) 
summary(m4) # инструменты релевантны + МНК-оценки несостоятельны
msummary(m4, vcov='HC0', stars=TRUE)





cor(data$price_per_serving, data$hausman)
cor(data$price_per_serving, data$price_instrument)


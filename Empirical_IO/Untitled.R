library('magrittr')
library('dplyr')
library('ivreg')
library('stargazer')
library('ggplot2')

## Есть продуктовые, временные и географические границы
data <- read.csv('Cereal_dataset.csv')
View(data)

data$'market_size' <- data$'city_population' * 91
data$'market_share' <- data$'servings_sold' / data$'market_size'
summary(data$'market_share') # >1 => люди потребляют >1 пачки на харю

data <- data %>%
  group_by(market_ids) %>%
  mutate(outside_share = 1 - sum(market_share))
summary(data[, c('market_share', 'outside_share')])

###
data$'logit_delta' <- log(data$'market_share' / data$'outside_share')
m1 <- lm(logit_delta ~ price_per_serving + mushy + sugar, data=data)
summary(m1)
# есть эндогенность из-за двусторонней причинно-следственной связи: 
# цена влияет на логит-дельта и наоборот

m2_1 <- lm(logit_delta ~ price_per_serving + mushy + sugar + 
             as.factor(market_ids) + as.factor(product_ids), data=data)
summary(m2_1) # фиксированные эффекты могли съесть все шоки
# фиксированные эффекты борются со всеми пропущенными переменными
# но не спасают от эндогенности из-за двусторонней причинно-следственной связи

m3 <- ivreg(logit_delta ~ price_per_serving + mushy + 
              sugar | price_instrument + mushy + sugar, data=data)
summary(m3)

stargazer(m1, m2_1, m3, type='text')

### инструменты Хаусмана (посмотреть на цены на др. рынках)
for (i in 1:nrow(data)){
  subdf <- data %>%
    filter(city != data$city[i]) %>%
    filter(quarter == data$quarter[i]) %>%
    filter(product_ids == data$product_ids[i])
  data$hausman[i] <- mean(subdf$price_per_serving)
}

m4 <- ivreg(logit_delta ~ price_per_serving + mushy + 
              sugar | hausman + mushy + sugar, data=data)
summary(m4)

ggplot(data, aes(x=data$price_per_serving, y=data$price_instrument)) + geom_point()
ggplot(data, aes(x=data$price_per_serving, y=data$hausman)) + geom_point()

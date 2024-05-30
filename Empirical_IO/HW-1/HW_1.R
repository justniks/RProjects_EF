##################### TASK 1 #####################

library(fixest)
library(readxl)
library(dplyr)
library(xtable)
library(stargazer)
library(ivreg)
library(modelsummary)

data <- read_excel('cars.xlsx')
df <- data.frame(data$qu, data$eurpr, data$ye, data$brand, data$ma, data$cla)
df <- select(data, qu, eurpr, ye, brand, ma, cla)
df_drop <- na.omit(df)

df$market <- paste(df$cla, df$brand, sep=', ')
df$market <- paste(df$cla, df$ma, sep=', ')
data$market <- paste('C', data$cla, ', M', data$ma, sep='')
data$market1 <- paste('C', data$cla, ', M', data$ma, ', Y', data$ye, sep='')
data$market2 <- paste('M', data$ma, ', Y', data$ye, sep='')



## Product-space approach
res1 = lm(
  log(qu) ~ log(eurpr), 
  data=data
)
summary(res1)

# Call:
#   lm(formula = log(qu) ~ log(eurpr), data = data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.8279 -1.1271 -0.0056  1.1214  4.3239 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   11.32158    0.20207   56.03   <2e-16 ***
#   log(eurpr)    -0.29603    0.02284  -12.96   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.615 on 11547 degrees of freedom
# Multiple R-squared:  0.01434,	Adjusted R-squared:  0.01426 
# F-statistic:   168 on 1 and 11547 DF,  p-value: < 2.2e-16

xtable(res1, type = "latex", strip=TRUE, first.strip=FALSE)
# print(xtable(res1, type = "html"))
# export2md(xtable(res1, type = "html"), strip=TRUE, first.strip=FALSE)



############################
res2 = feols(
  log(qu) ~ 1 + log(eurpr) | ye + brand + ma, 
  data=data,
  panel.id=~ye+brand+ma
)
summary(res2)

# OLS estimation, Dep. Var.: log(qu)
# Observations: 11,549
# Fixed-effects: ye: 30,  brand: 38,  ma: 5
# Standard-errors: Clustered (ye) 
#   Estimate Std. Error  t value  Pr(>|t|)    
#   log(eurpr)    -1.57892   0.042521 -37.1329 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# RMSE: 1.21632     Adj. R2: 0.4375 
# Within R2: 0.14924



res2 = feols(
  log(qu) ~ 1 + log(eurpr) | ye + brand + ma, 
  data=data,
  panel.id=~ye+ma
)
summary(res2)

# OLS estimation, Dep. Var.: log(qu)
# Observations: 11,549
# Fixed-effects: ye: 30,  brand: 38,  ma: 5
# Standard-errors: Clustered (ye) 
# Estimate Std. Error  t value  Pr(>|t|)    
# log(eurpr) -1.57892   0.042521 -37.1329 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# RMSE: 1.21632     Adj. R2: 0.4375 
# Within R2: 0.14924



res2 = feols(
  log(qu) ~ 1 + log(eurpr) | ye + brand + ma + cla, 
  data=data,
  panel.id=~ye+ma
)
summary(res2)

# OLS estimation, Dep. Var.: log(qu)
# Observations: 11,549
# Fixed-effects: ye: 30,  brand: 38,  ma: 5,  cla: 5
# Standard-errors: Clustered (ye) 
# Estimate Std. Error t value  Pr(>|t|)    
# log(eurpr) -1.63796   0.096634  -16.95 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# RMSE: 1.2002     Adj. R2: 0.452114
# Within R2: 0.049101



res2 = feols(
  log(qu) ~ 1 + log(eurpr), 
  data=data,
  panel.id=~ye+brand+market
)
summary(res2)

# OLS estimation, Dep. Var.: log(qu)
# Observations: 11,549
# Standard-errors: Clustered (ye) 
#   Estimate Std. Error  t value   Pr(>|t|)    
#   (Intercept)   11.321579   0.285197 39.69741  < 2.2e-16 ***
#   log(eurpr)    -0.296028   0.031209 -9.48549 2.1575e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# RMSE: 1.61506   Adj. R2: 0.014258





res2 = lm(
  log(qu) ~ log(eurpr) + as.factor(ye) + as.factor(brand) + as.factor(ma), 
  data=data
)
summary(res2)

# Call:
#   lm(formula = log(qu) ~ log(eurpr) + as.factor(ye) + as.factor(brand) + 
#        as.factor(ma), data = data)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   19.11461    0.29414  64.985  < 2e-16 ***
#   log(eurpr)    -1.57892    0.03519 -44.870  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.22 on 11477 degrees of freedom
# Multiple R-squared:  0.441,	Adjusted R-squared:  0.4375 
# F-statistic: 127.5 on 71 and 11477 DF,  p-value: < 2.2e-16



res2 = lm(
  log(qu) ~ log(eurpr) + as.factor(ye) + as.factor(brand) + as.factor(ma) + as.factor(cla), 
  data=data
)
summary(res2)

# Call:
#   lm(formula = log(qu) ~ log(eurpr) + as.factor(ye) + as.factor(brand) + 
#        as.factor(ma) + as.factor(cla), data = data)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   19.29917    0.50539  38.186  < 2e-16 ***
#   log(eurpr)    -1.63796    0.06730 -24.340  < 2e-16 ***
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.204 on 11473 degrees of freedom
# Multiple R-squared:  0.4557,	Adjusted R-squared:  0.4521 
# F-statistic: 128.1 on 75 and 11473 DF,  p-value: < 2.2e-16



res2 = lm(
  log(qu) ~ log(eurpr) + as.factor(ye) + as.factor(brand) + as.factor(market), 
  data=data
)
summary(res2)

# Call:
#   lm(formula = log(qu) ~ log(eurpr) + as.factor(ye) + as.factor(brand) + 
#        as.factor(market), data = data)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   18.60524    0.50111  37.128  < 2e-16 ***
#   log(eurpr)    -1.56278    0.06666 -23.445  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.187 on 11457 degrees of freedom
# Multiple R-squared:  0.4715,	Adjusted R-squared:  0.4673 
# F-statistic: 112.3 on 91 and 11457 DF,  p-value: < 2.2e-16



res2 = lm(
  log(qu) ~ log(eurpr) + as.factor(ye) + as.factor(brand) + as.factor(market1), 
  data=data
)
summary(res2)
xtable(res2, type = "latex", strip=TRUE, first.strip=FALSE)

# Call:
#   lm(formula = log(qu) ~ log(eurpr) + as.factor(ye) + as.factor(brand) + 
#        as.factor(market1), data = data)
# 
# Coefficients: (29 not defined because of singularities)
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   21.781528   0.617650  35.265  < 2e-16 ***
#   log(eurpr)    -1.886185   0.075142 -25.101  < 2e-16 ***
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.183 on 10761 degrees of freedom
# Multiple R-squared:  0.5068,	Adjusted R-squared:  0.4708 
# F-statistic: 14.05 on 787 and 10761 DF,  p-value: < 2.2e-16



res2 = lm(
  log(qu) ~ log(eurpr) + as.factor(ye) + as.factor(brand) + as.factor(market2), 
  data=data
)
summary(res2)

# Call:
#   lm(formula = log(qu) ~ log(eurpr) + as.factor(ye) + as.factor(brand) + 
#        as.factor(market2), data = data)
# 
# Coefficients: (29 not defined because of singularities)
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   20.083171   0.326896  61.436  < 2e-16 ***
#   log(eurpr)    -1.661173   0.035604 -46.657  < 2e-16 ***
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.212 on 11361 degrees of freedom
# Multiple R-squared:  0.4535,	Adjusted R-squared:  0.4445 
# F-statistic: 50.42 on 187 and 11361 DF,  p-value: < 2.2e-16



res2 = lm(
  log(qu) ~ log(eurpr) + as.factor(brand) + as.factor(market1), 
  data=data
)
summary(res2)

# Call:
#   lm(formula = log(qu) ~ log(eurpr) + as.factor(brand) + as.factor(market1), 
#      data = data)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   21.781528   0.617650  35.265  < 2e-16 ***
#   log(eurpr)    -1.886185   0.075142 -25.101  < 2e-16 ***
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.183 on 10761 degrees of freedom
# Multiple R-squared:  0.5068,	Adjusted R-squared:  0.4708 
# F-statistic: 14.05 on 787 and 10761 DF,  p-value: < 2.2e-16





## Characteristic-space approach
data$'market_size' <- data$'pop' * 0.18
data$'market_share' <- data$'qu' / data$'market_size'

data <- data %>%
  group_by(market1) %>%
  mutate(outside_share = 1 - sum(market_share))
summary(data[,c("market_share","outside_share")])

data$'logit_delta' <- log(data$'market_share' / data$'outside_share')
res3 <- lm(logit_delta ~ eurpr, data=data)
summary(res3)
xtable(res3, type = "latex", strip=FALSE, first.strip=FALSE)

# Call:
#   lm(formula = logit_delta ~ eurpr, data = data)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   -6.526e+00  2.478e-02 -263.30   <2e-16 ***
#   eurpr         -5.833e-05  2.473e-06  -23.59   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.472 on 11547 degrees of freedom
# Multiple R-squared:  0.04598,	Adjusted R-squared:  0.0459 
# F-statistic: 556.5 on 1 and 11547 DF,  p-value: < 2.2e-16



res3 <- lm(
  logit_delta ~ eurpr + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org), 
  data=data
)
summary(res3)
xtable(res3, type = "latex", strip=FALSE, first.strip=FALSE)

# Call:
#   lm(formula = logit_delta ~ eurpr + li + ac + engdpc + cy + as.factor(ma) + 
#        as.factor(brand) + as.factor(cla) + as.factor(org), data = data)
# 
# Coefficients: (4 not defined because of singularities)
#   Estimate     Std. Error t value Pr(>|t|)    
#   (Intercept)       -6.155e+00  6.466e-01  -9.518  < 2e-16 ***
#   eurpr             -5.919e-05  5.672e-06 -10.436  < 2e-16 ***
#   li                -7.274e-02  1.364e-02  -5.335 9.79e-08 ***
#   ac                -1.354e-02  3.760e-03  -3.601 0.000319 ***
#   engdpc            6.896e-05  5.562e-06  12.399  < 2e-16 ***
#   cy                -1.001e-03  6.929e-05 -14.448  < 2e-16 ***
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.197 on 9178 degrees of freedom
# (2317 observations deleted due to missingness)
# Multiple R-squared:  0.3837,	Adjusted R-squared:  0.3802 
# F-statistic: 107.8 on 53 and 9178 DF,  p-value: < 2.2e-16

ggplot(data, aes(y=eurpr, x=tax)) + geom_point()

for (i in 1:nrow(data)){
  subdf <- data %>%
    filter(ma != data$ma[i]) %>%
    filter(ye == data$ye[i]) %>%
    filter(cla == data$cla[i])
  data$hausman[i] <- mean(subdf$eurpr)
}

ggplot(data, aes(x=hausman, y=eurpr)) + geom_point()
cor(data$hausman, data$eurpr) # 0.9280908



res3iv1 = ivreg(
  logit_delta ~ eurpr + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org) | hausman + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org), 
  data=data
) 
summary(res3iv1) # инструменты релевантны + МНК-оценки несостоятельны
msummary(res3iv1, vcov='HC0', stars=TRUE)
stargazer(res3iv1, type = "latex")

# Call:
#   ivreg(formula = logit_delta ~ eurpr + li + ac + engdpc + cy + 
#           as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org) | 
#           hausman + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + 
#           as.factor(cla) + as.factor(org), data = data)
# 
# Coefficients:
#   Estimate    Std. Error t value Pr(>|t|)    
#   (Intercept)      -6.036e+00  6.482e-01  -9.312  < 2e-16 ***
#   eurpr            -2.893e-05  8.837e-06  -3.274 0.001065 ** 
#   li               -7.681e-02  1.369e-02  -5.612 2.06e-08 ***
#   ac               -1.473e-02  3.775e-03  -3.902 9.61e-05 ***
#   engdpc           4.550e-05  7.652e-06   5.947 2.84e-09 ***
#   cy               -1.094e-03  7.247e-05 -15.102  < 2e-16 ***
# 
# Diagnostic tests:
#   df1  df2 statistic  p-value    
#   Weak instruments    1 9178    6465.8  < 2e-16 ***
#   Wu-Hausman          1 9177      20.1 7.45e-06 ***
#   Sargan              0   NA        NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.199 on 9178 degrees of freedom
# Multiple R-Squared: 0.3818,	Adjusted R-squared: 0.3783 
# Wald test: 105.7 on 53 and 9178 DF,  p-value: < 2.2e-16



res3iv2 = ivreg(
  logit_delta ~ eurpr + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org) | tax + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org), 
  data=data
) 
summary(res3iv2) # инструменты релевантны + МНК-оценки состоятельны?
msummary(res3iv2, vcov='HC0', stars=TRUE)
stargazer(res3iv2, type = "latex")

# Call:
#   ivreg(formula = logit_delta ~ eurpr + li + ac + engdpc + cy + 
#           as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org) | 
#           tax + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + 
#           as.factor(cla) + as.factor(org), data = data)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                 -5.581e+00  9.477e-01  -5.889 4.02e-09 ***
#   eurpr                        8.664e-05  1.707e-04   0.508 0.611711    
#   li                          -9.234e-02  2.693e-02  -3.430 0.000607 ***
#   ac                          -1.929e-02  7.766e-03  -2.483 0.013039 *  
#   engdpc                      -4.407e-05  1.323e-04  -0.333 0.739100    
#   cy                          -1.451e-03  5.310e-04  -2.732 0.006300 ** 
# 
# Diagnostic tests:
#   df1  df2 statistic  p-value    
#   Weak instruments    1 9178    10.882 0.000975 ***
#   Wu-Hausman          1 9177     0.784 0.376049    
#   Sargan              0   NA        NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.239 on 9178 degrees of freedom
# Multiple R-Squared: 0.3394,	Adjusted R-squared: 0.3355 
# Wald test: 98.68 on 53 and 9178 DF,  p-value: < 2.2e-16 



res3iv3 = ivreg(
  logit_delta ~ eurpr + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org) | lag(eurpr) + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org), 
  data=data
) 
summary(res3iv3) # инструменты релевантны + МНК-оценки несостоятельны
msummary(res3iv3, vcov='HC0', stars=TRUE)
stargazer(res3iv3, type='text')

# Call:
#   ivreg(formula = logit_delta ~ eurpr + li + ac + engdpc + cy + 
#           as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org) | 
#           lag(eurpr) + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + 
#           as.factor(cla) + as.factor(org), data = data)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                 -6.155e+00  6.466e-01  -9.518  < 2e-16 ***
#   eurpr                       -5.919e-05  5.672e-06 -10.436  < 2e-16 ***
#   li                          -7.274e-02  1.364e-02  -5.335 9.79e-08 ***
#   ac                          -1.354e-02  3.760e-03  -3.601 0.000319 ***
#   engdpc                       6.896e-05  5.562e-06  12.399  < 2e-16 ***
#   cy                          -1.001e-03  6.929e-05 -14.448  < 2e-16 ***
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.197 on 9178 degrees of freedom
# Multiple R-Squared: 0.3837,	Adjusted R-squared: 0.3802 
# Wald test: 107.8 on 53 and 9178 DF,  p-value: < 2.2e-16





# log on log

res4 <- lm(logit_delta ~ log(eurpr), data=data)
summary(res4)

# Call:
#   lm(formula = logit_delta ~ log(eurpr), data = data)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   -3.23417    0.18526  -17.46   <2e-16 ***
#   log(eurpr)    -0.42823    0.02094  -20.45   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.481 on 11547 degrees of freedom
# Multiple R-squared:  0.03496,	Adjusted R-squared:  0.03488 
# F-statistic: 418.3 on 1 and 11547 DF,  p-value: < 2.2e-16


res4 <- lm(
  logit_delta ~ log(eurpr) + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org), 
  data=data
)
summary(res4)

# Call:
#   lm(formula = logit_delta ~ log(eurpr) + li + ac + engdpc + cy + 
#        as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org), 
#      data = data)
# 
# Coefficients: (4 not defined because of singularities)
#   Estimate    Std. Error t value Pr(>|t|)    
#   (Intercept)      -1.856e+00  8.591e-01  -2.161 0.030748 *  
#   log(eurpr)       -5.331e-01  7.393e-02  -7.211 5.99e-13 ***
#   li               -9.502e-02  1.380e-02  -6.886 6.13e-12 ***
#   ac               -2.116e-02  3.836e-03  -5.516 3.57e-08 ***
#   engdpc           7.203e-05  7.599e-06   9.478  < 2e-16 ***
#   cy               -1.005e-03  7.168e-05 -14.020  < 2e-16 ***
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.2 on 9178 degrees of freedom
# (2317 observations deleted due to missingness)
# Multiple R-squared:  0.3799,	Adjusted R-squared:  0.3764 
# F-statistic: 106.1 on 53 and 9178 DF,  p-value: < 2.2e-16


res4iv1 = ivreg(
  logit_delta ~ log(eurpr) + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org) | log(hausman) + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org), 
  data=data
) 
summary(res4iv1) # инструменты релевантны + МНК-оценки несостоятельны
msummary(res4iv1, vcov='HC0', stars=TRUE)

# Call:
#   ivreg(formula = logit_delta ~ log(eurpr) + li + ac + engdpc + 
#           cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + 
#           as.factor(org) | log(hausman) + li + ac + engdpc + cy + as.factor(ma) + 
#           as.factor(brand) + as.factor(cla) + as.factor(org), data = data)
# 
# Coefficients:
#   Estimate    Std. Error t value Pr(>|t|)    
#   (Intercept)      -6.127e+00  1.111e+00  -5.513 3.62e-08 ***
#   log(eurpr)       2.685e-02  1.182e-01   0.227 0.820274    
#   li               -7.998e-02  1.406e-02  -5.688 1.33e-08 ***
#   ac               -1.561e-02  3.954e-03  -3.946 7.99e-05 ***
#   engdpc           2.062e-05  1.138e-05   1.812 0.070035 .  
#   cy               -1.193e-03  7.824e-05 -15.244  < 2e-16 ***
# 
# Diagnostic tests:
#   df1  df2 statistic  p-value    
#   Weak instruments    1 9178   5964.59  < 2e-16 ***
#   Wu-Hausman          1 9177     37.43 9.86e-10 ***
#   Sargan              0   NA        NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.204 on 9178 degrees of freedom
# Multiple R-Squared: 0.3761,	Adjusted R-squared: 0.3725 
# Wald test: 104.5 on 53 and 9178 DF,  p-value: < 2.2e-16 


res4iv2 = ivreg(
  logit_delta ~ log(eurpr) + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org) | log(tax) + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org), 
  data=data
) 
summary(res4iv2) # инструменты релевантны
msummary(res4iv2, vcov='HC0', stars=TRUE)

# Call:
#   ivreg(formula = logit_delta ~ log(eurpr) + li + ac + engdpc + 
#           cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + 
#           as.factor(org) | log(tax) + li + ac + engdpc + cy + as.factor(ma) + 
#           as.factor(brand) + as.factor(cla) + as.factor(org), data = data)
# 
# Coefficients:
#   Estimate    Std. Error t value Pr(>|t|)    
#   (Intercept)      -6.002e+00  3.052e+00  -1.967 0.049256 *  
#   log(eurpr)       1.049e-02  3.910e-01   0.027 0.978606    
#   li               -8.042e-02  1.726e-02  -4.660 3.21e-06 ***
#   ac               -1.577e-02  5.412e-03  -2.914 0.003581 ** 
#   engdpc           2.212e-05  3.606e-05   0.613 0.539686    
#   cy               -1.187e-03  1.475e-04  -8.051 9.21e-16 ***
# 
# Diagnostic tests:
#   df1  df2 statistic p-value    
#   Weak instruments    1 9178   342.378  <2e-16 ***
#   Wu-Hausman          1 9177     2.017   0.156    
#   Sargan              0   NA        NA      NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.204 on 9178 degrees of freedom
# Multiple R-Squared: 0.3763,	Adjusted R-squared: 0.3727 
# Wald test: 104.5 on 53 and 9178 DF,  p-value: < 2.2e-16


res4iv3 = ivreg(
  logit_delta ~ log(eurpr) + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org) | lag( log(eurpr) ) + li + ac + engdpc + cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org), 
  data=data
) 
summary(res4iv3) # инструменты релевантны + МНК-оценки несостоятельны
msummary(res4iv3, vcov='HC0', stars=TRUE)

# Call:
#   ivreg(formula = logit_delta ~ log(eurpr) + li + ac + engdpc + 
#           cy + as.factor(ma) + as.factor(brand) + as.factor(cla) + 
#           as.factor(org) | lag(log(eurpr)) + li + ac + engdpc + cy + 
#           as.factor(ma) + as.factor(brand) + as.factor(cla) + as.factor(org), 
#         data = data)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                 -1.856e+00  8.591e-01  -2.161 0.030748 *  
#   log(eurpr)                  -5.331e-01  7.393e-02  -7.211 5.99e-13 ***
#   li                          -9.502e-02  1.380e-02  -6.886 6.13e-12 ***
#   ac                          -2.116e-02  3.836e-03  -5.516 3.57e-08 ***
#   engdpc                       7.203e-05  7.599e-06   9.478  < 2e-16 ***
#   cy                          -1.005e-03  7.168e-05 -14.020  < 2e-16 ***
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.2 on 9178 degrees of freedom
# Multiple R-Squared: 0.3799,	Adjusted R-squared: 0.3764 
# Wald test: 106.1 on 53 and 9178 DF,  p-value: < 2.2e-16 



##################### TASK 2 #####################
library(frontier)
library(ggplot2)

spark_data <- read_excel('Spark_data.xlsx', sheet='report')
spark_data_mod <- subset(spark_data, select=-c(`№`, `Регистрационный номер`, `2019, Вид деятельности/отрасль`))
colnames(spark_data_mod) <- c('name', 'L', 'K', 'Q')
# is.na(spark_data_mod)

# which_nas <- apply(spark_data_mod, 1, function(X) any(is.na(X)))
# length(which(which_nas)) # 878
# length(which(which_nas)) / nrow(spark_data_mod)

spark_data_drop <- na.omit(spark_data_mod)
spark_data_drop$L <- as.numeric(spark_data_drop$L)
spark_data_drop$K <- as.numeric(spark_data_drop$K)
spark_data_drop$Q <- as.numeric(spark_data_drop$Q)

summary( spark_data_drop[c('L', 'K', 'Q')] )
stargazer(
  xtable(
    spark_data_drop[c('L', 'K', 'Q')]
  )
)



ols_res <- lm( log(Q) ~ log(L) + log(K), data=spark_data_drop )
summary(ols_res)
stargazer(ols_res)

# spark_data_drop %>% ggplot(aes(x=L, y=Q)) + geom_point() +
#   geom_function( fun = function(x) exp(ols_res$coefficients[1] + log(x) * ols_res$coefficients[2]) ) +
#   geom_function( fun = function(x) exp(ols_res$coefficients[1] + log(x) * ols_res$coefficients[2] + max(ols_res$residuals)) )
# 
# spark_data_drop %>% ggplot(aes(x=K, y=Q)) + geom_point() +
#   geom_function( fun = function(x) exp(ols_res$coefficients[1] + log(x) * ols_res$coefficients[2]) ) +
#   geom_function( fun = function(x) exp(ols_res$coefficients[1] + log(x) * ols_res$coefficients[2] + max(ols_res$residuals)) )

ols_res$residuals



sfa_res <- sfa( log(Q) ~ log(L) + log(K), data=spark_data_drop )
summary(sfa_res)
sfa_res$olsParam
stargazer(sfa_res$mleParam)

spark_data_drop$predict_sfa <- predict(sfa_res)

spark_data_drop %>% ggplot (aes(x=log(L)) ) +
  geom_point( aes(y=log(Q)), alpha=0.1 ) +
  geom_smooth( aes(y=predict_sfa), color='red' )

spark_data_drop %>% ggplot (aes(x=log(K)) ) +
  geom_point( aes(y=log(Q)), alpha=0.1 ) +
  geom_smooth( aes(y=predict_sfa), color='red' )

spark_data_drop$eff <- efficiencies(sfa_res)
spark_data_drop$eff %>% hist()

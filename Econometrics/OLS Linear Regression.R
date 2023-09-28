library(readxl)
D <- read_excel("Price2012 (1).xlsx")

OLS_model <- lm(data=D, Price~TotalArea)
OLS_model

# Call:
#   lm(formula = Price ~ TotalArea, data = D)
# 
# Coefficients:
#   (Intercept)    TotalArea  
# 786.5        135.3  
# => \hat{\beta_1} = 786.5, \hat{\beta_2} = 135.3

summary(OLS_model)
# Call:
#   lm(formula = Price ~ TotalArea, data = D)
## 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1282.66  -429.49   -89.36   334.77  2273.95 
## 
# Coefficients:
#             Estimate  Std. Error  t-value  Pr(>|t|)    
# (Intercept)   786.46     583.05   1.349     0.18    
# TotalArea     135.32      16.51   8.194  3.3e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 636.5 on 119 degrees of freedom
# Multiple R-squared:  0.3607,	Adjusted R-squared:  0.3553 
# F-statistic: 67.14 on 1 and 119 DF,  p-value: 3.297e-13
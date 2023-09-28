#Дружелюбная эконометрика. Глава 3

# Если вам нужно очистить рабочее пространство от старых объектов:
rm(list = ls())

#Устанавливаем и подключаем сет нужных пакетов
#Напоминаю, что инсталлировать достаточно один раз,
#а подключать нужно во время каждой сессии

install.packages("lmtest")
install.packages("stargazer")
install.packages("ggplot2")

library("ggplot2")   #Графики
library("stargazer") #Красивые таблички
library("lmtest")    #Статистические тесты
library(readxl)

# В диаграммах пакета qplot 
# можно менять размер шрифта (default is 12)
theme_set(theme_gray(base_size = 18))

D <- read_excel('Students.xlsx')
D

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
# Call:
#   lm(formula = TEST ~ CLASS, data = D)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -53.09 -13.63   0.73  14.55  48.38 
# 
# Coefficients:
#              Estimate   Std.Error  t value       Pr(>|t|)    
# (Intercept)   83.655      6.279     13.324       < 2e-16 ***
#   CLASS       -1.309      0.281     -4.659   ((( 5.82e-06 *** )))
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 20.34 on 198 degrees of freedom
# Multiple R-squared:  ((( 0.0988 ))),	Adjusted R-squared:  0.09425 
# F-statistic: 21.71 on 1 and 198 DF,  p-value: 5.819e-06

#Модель №2
model_2 <- lm(data = D, TEST ~ CLASS+EXPN)
summary(model_2)
# Call:
#   lm(formula = TEST ~ CLASS + EXPN, data = D)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -47.557 -14.823   1.934  13.878  43.707 
# 
# Coefficients:
#               Estimate  Std. Error t value       Pr(>|t|)    
# (Intercept)   60.1560     7.9712   7.547         1.62e-12 ***
#   CLASS        -0.9088     0.2829  -3.212    ((( 0.00154 ** )))
#   EXPN          2.5371     0.5669   4.475        1.29e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 19.42 on 197 degrees of freedom
# Multiple R-squared:  ((( 0.182 ))),	Adjusted R-squared:  0.1737 
# F-statistic: 21.91 on 2 and 197 DF,  p-value: 2.56e-09

#Модель №3
model_3 <- lm(data = D, TEST ~ CLASS+EXPN+INCOME)
summary(model_3)
# Call:
#   lm(formula = TEST ~ CLASS + EXPN + INCOME, data = D)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -31.743  -7.726   0.452   7.485  24.708 
# 
# Coefficients:
#              Estimate Std. Error t value        Pr(>|t|)    
# (Intercept)  29.1786     4.8472   6.020         8.46e-09 ***
#   CLASS        -1.0725     0.1632  -6.573   ((( 4.36e-10 *** )))
#   EXPN          2.0502     0.3275   6.260       2.37e-09 ***
#   INCOME        1.0490     0.0526  19.943       < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 11.19 on 196 degrees of freedom
# Multiple R-squared:  ((( 0.7299 ))),	Adjusted R-squared:  ((( 0.7258 )))
# F-statistic: 176.6 on 3 and 196 DF,  ((( p-value: < 2.2e-16 )))
# => ур-ние в целом значимо


#Сводная табличка
stargazer(model_1, model_2, model_3,
          title="Simple cross-section regressions", type="text", 
          column.labels=c("Модель 1", "Модель 2", "Модель 3"), 
          df=FALSE, digits=2)

# Simple cross-section regressions
# =================================================
#   Dependent variable:     
#   -----------------------------
#   TEST             
# Модель 1  Модель 2  Модель 3 
# (1)       (2)       (3)   
# -------------------------------------------------
#   CLASS               -1.31***  -0.91***  -1.07*** 
#   (0.28)    (0.28)    (0.16)  
# 
# EXPN                           2.54***   2.05*** 
#   (0.57)    (0.33)  
# 
# INCOME                                   1.05*** 
#   (0.05)  
# 
# Constant            83.66***  60.16***  29.18*** 
#   (6.28)    (7.97)    (4.85)  
# 
# -------------------------------------------------
#   Observations           200       200       200   
# R2                    0.10      0.18      0.73   
# Adjusted R2           0.09      0.17      0.73   
# Residual Std. Error   20.34     19.42     11.19  
# F Statistic         21.71***  21.91***  176.60***
#   =================================================
#   Note:                 *p<0.1; **p<0.05; ***p<0.01



#Сводная табличка (со степенями свободы)
stargazer(model_1, model_2, model_3,
          title="Simple cross-section regressions", type="text", 
          column.labels=c("Модель 1", "Модель 2", "Модель 3"), 
          df=TRUE, digits=2)

# Simple cross-section regressions
# =========================================================================================
#   Dependent variable:                         
#   ---------------------------------------------------------------------
#   TEST                                 
# Модель 1               Модель 2               Модель 3        
# (1)                    (2)                     (3)          
# -----------------------------------------------------------------------------------------
#   CLASS                      -1.31***               -0.91***               -1.07***        
#   (0.28)                 (0.28)                 (0.16)         
# 
# EXPN                                              2.54***                 2.05***        
#   (0.57)                 (0.33)         
# 
# INCOME                                                                    1.05***        
#   (0.05)         
# 
# Constant                   83.66***               60.16***               29.18***        
#   (6.28)                 (7.97)                 (4.85)         
# 
# -----------------------------------------------------------------------------------------
#   Observations                 200                    200                     200          
# R2                           0.10                   0.18                   0.73          
# Adjusted R2                  0.09                   0.17                   0.73          
# Residual Std. Error    20.34 (df = 198)       19.42 (df = 197)       11.19 (df = 196)    
# F Statistic         21.71*** (df = 1; 198) 21.91*** (df = 2; 197) 176.60*** (df = 3; 196)
# =========================================================================================
#   Note:                                                         *p<0.1; **p<0.05; ***p<0.01


#Сравнение двух моделей при помощи теста на линейное ограничение
waldtest(model_1,model_3)
# Wald test
# 
# Model 1: TEST ~ CLASS
# Model 2: TEST ~ CLASS + EXPN + INCOME
# Res.Df Df      F    Pr(>F)    
# 1    198                        
# 2    196  2 229.04 ((( < 2.2e-16 *** )))
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

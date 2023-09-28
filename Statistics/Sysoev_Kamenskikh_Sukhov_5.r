install.packages("readxl")
library(readxl)
Ural <- read_xlsx("Урал.xlsx", sheet=1, skip=0)

#Предварительно уберем в файле пустую строку
#Будем считать, что общий доход является суммой валового дохода от земледелия и полеводства и дохода от скотоводства и птицы

ValDox <- Ural$`валовый доход от земледелия и полевдоства`
Doxod <- Ural$`доход от  скотоводства и птицы`

#Обозначим вектор общих доходов, как Income и посчитаем его среднее значение и дисперсию
#В столбце валового дохода заменим пропуски на нули

ValDox[is.na(ValDox)] <- 0

Income <- ValDox + Doxod
mean(Income) #586.59 
var(Income) #313737.4

#Построим гистограмму значений дохода

hist(Income)

install.packages("moments")
library(moments)

#Расчитаем эксцесс и асимметрию

skewness(Income) #2.123
kurtosis(Income) #7.85

#Получим выборку из нормального распределения для сравнения

Test <- rnorm(length(Income), mean(Income), sqrt(var(Income)))
hist(Test)
skewness(Test) #0.123
kurtosis(Test) #2.68

#Проверим гипотезу о нормальности распределения с помощью двух тестов

shapiro.test(Income) #тест Шапиро-Уилка на нормальность, W = 0.763, p-value = 1.161e-09

#p-value < 0,05 ==> отвергаем гипотезу о нормальном распределении

ks.test(Income, "pnorm", mean = mean(Income), sd = sqrt(var(Income))) # Тест Колмогорова-Смирнова на нормальность, D = 0.226, p-value = 0.00075

#p-value < 0,05 ==> отвергаем гипотезу о нормальном распределении

#Будем считать, что площадь посевных полей в распоряжении является суммой площадей долевых и арендованных полей
#Обозначим наш вектор суммарных площадей, как S

Dol <-Ural$`площадь земли долевой`
Ar <-Ural$`площадь арендованной`
S <- Dol+Ar

#Посчитаем корреляцию между доходом и площадью посевных полей
#Доход не распределен нормально, поэтому не будем использовать критерий Пирсона

cor.test(S,Income, method = "kendall")
#data:  S and Income
#z = 8.069, p-value = 7.089e-16
#alternative hypothesis: true tau is not equal to 0
#sample estimates:
#  tau 
#0.6463205 

cor.test(S,Income, method = "spearman")
#data:  S and Income
#S = 12197, p-value < 2.2e-16
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.81184

#В обоих случаях p-value достаточно маленькое, поэтому мы отвергаем нулевую гипотезу о равенстве корреляции нулю

#Проверим гипотезу о равенстве дисперсий дохода и расхода
#Будем считать, что суммарные расходы складываются из расходов на личные и хозяйственные потребности
#Обозначим суммарные доходы, как Rasx

Rasx_1 <-Ural$`израсходовано на хозяйственные потребности`
Rasx_2 <-Ural$`израсходовано на личные потребности`
Rasx <- Rasx_1 + Rasx_2

var.test(Rasx, Income)
#data:  Rasx and Income
#F = 0.46538, num df = 74, denom df = 74, p-value = 0.001194
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  0.2940167 0.7366108
#sample estimates:
#  ratio of variances 
#0.4653771 

#p-value < 0.05 ==> отвергаем гипотезу о равенстве дисперсий

#Построим доверительный интервал для среднего стоимости скота в предположении, что стоимость распределена показательно
#Удалим из вектора стоимости скота ячейку, где нет значения
#Назовем вектор стоимостей скота Skot

Skot <- Ural$`стоимость скота`
Skot <- Skot[!is.na (Skot)]

#Воспользуемся тем фактом, что 2nλX, где Х - среднее значение выборки, распределено так же, как и хи-квадрат с 2n степенями свободы
#Мат. ожидание показательного распределения равняется 1/λ

mean(Skot) #382
RKrit <- qchisq(p = 0.025, df = 2*length(Skot), lower.tail = FALSE) #183.573
LKrit <- qchisq(p = 0.025, df = 2*length(Skot), lower.tail = TRUE)  #116.212

Left <- 2 * length(Skot) * mean(Skot) / RKrit #307.98
Right <- 2 * length(Skot) * mean(Skot) / LKrit #486.49

#Доверительный интервал для средней стоимости скота с уровнем значимости 95% (307.98, 486.49)


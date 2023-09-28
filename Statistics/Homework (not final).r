# install.packages("readxl")
# install.packages("moments")
# install.packages("corrplot")
library(readxl)
library(moments)
library(corrplot)
library(EnvStats)



############## TASK 2
### preprocessing data
Data2 <- read_excel("task2.xlsx", sheet=1, skip=0)
Data2

# доход, посевная площадь, расход, ср. стоимость скота
u <- Data2[c(32, 31, 12:15)]
u[ is.na(u) ] <- 0
j <- u$`посевная площадь (надельная)` + u$`посевная площадь (купчая)` + u$`посевная площадь (арендованная, надельная)` + u$`посевная площадь (арендованная, купчая)`

rel_data2 <- cbind(u[1:2], j)
colnames(rel_data2) <- c('income', 'expenditure', 'cult_area')
rel_data2

### mean and sample variance of income
mean_income_2 <- mean(rel_data2$income) # 635.888
var_income_2 <- var(rel_data2$income) # 104660.1


### histogram of income
hist(rel_data2$income)

### skewness and kurtosis of income
skewness_income_2 <- skewness(rel_data2$income) # 1.569469
kurtosis_income_2 <- kurtosis(rel_data2$income) # 6.215781


  # g <- rnorm(n=92, mean=mean_income_2, sd=sqrt(var_income_2))
  # g
  # hist(g)
  # skewness(g) # 0.09766422
  # kurtosis(g) # 2.372802


### hypothesis testing: income -- normal dist
shapiro.test(rel_data2$income)
  # W = 0.87805, p-value = 3.778e-07
  # p-value < 0.05 => reject the null hypothesis 
  # => income does not have normal distribution

ks.test(rel_data2$income, "pnorm")
  # D = 1, p-value = 3.331e-16; alternative hypothesis: two-sided
  # p-value < 0.05 => reject the null hypothesis 
  # => income does not have normal distribution


### correlation between income and cultivated area
cor_income_cult_area2_1 <- cor(rel_data2$income, rel_data2$cult_area, method="kendall") # 0.5402764
cor_income_cult_area2_2 <-cor(rel_data2$income, rel_data2$cult_area, method="spearman") # 0.7356443

cor.test(rel_data2$income, rel_data2$cult_area, method="pearson")
  # t = 10.465, df = 90, p-value < 2.2e-16; alternative hypothesis: true correlation is not equal to 0
  # 95 percent confidence interval:
  #  0.6319462 0.8210924
  # sample estimates:
  # cor 
  #  0.7408701 

cor.test(rel_data2$income, rel_data2$cult_area, method="kendall")
  # z = 7.6132, p-value = 2.674e-14; alternative hypothesis: true tau is not equal to 0
  # sample estimates:
  # tau 
  #  0.5402764 

cor.test(rel_data2$income, rel_data2$cult_area, method="spearman")
  # S = 34304, p-value < 2.2e-16; alternative hypothesis: true rho is not equal to 0
  # sample estimates:
  # rho 
  #  0.7356443


### hypothesis testing. H_0: income variance = expenditure variance
var.test(rel_data2$income, rel_data2$expenditure)
# F = 1.1264, num df = 91, denom df = 91, p-value = 0.5713; alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval (for ratio of variances):
#  0.7449449 1.7032974
# sample estimates:
# ratio of variances 
#  1.126438

# p-value > 0.05 => НЕ отвергаем нулевую гипотезу => предпологаем, что истинные дисперсии равны


### CI for average livestock cost (not really, 'cause no data)
livestock_cost <- Data2$`общая стоимость (без земли)`
livestock_cost
hist(livestock_cost)
mean(livestock_cost) # 1042.944

# 1st method
eexp(livestock_cost, ci=TRUE)
#          LCL          UCL 
# 0.0007729476 0.0011644277 

UCL_exp_2_1 <- 1 / 0.0007729476
LCL_exp_2_1 <- 1 / 0.0011644277
conf_int_2_1 <- c(LCL_exp_2_1, UCL_exp_2_1) # (859, 1294)
conf_int_2_1 # (858.791, 1293.749)

# 2nd method
Rquantile_exp_2 <- qchisq(0.025, lower.tail=FALSE, df= 2 * length(livestock_cost))
Lquantile_exp_2 <- qchisq(0.025, lower.tail=TRUE, df= 2 * length(livestock_cost))

LCL_exp_2_2 <- (2 * length(livestock_cost) * mean(livestock_cost) ) / Rquantile_exp_2
UCL_exp_2_2 <- (2 * length(livestock_cost) * mean(livestock_cost) ) / Lquantile_exp_2

conf_int_2_2 <- c(LCL_exp_2_2, UCL_exp_2_2)
conf_int_2_2 # ~ (859, 1294)



############## TASK 1
### preprocessing data
Data1 <- read_excel("task1.xlsx", sheet=1, skip=0)
Data1

# доход, посевная площадь, расход, стоимость скота
rel_data1 <- Data1[c(18, 10, 20, 15)]
rel_data1[ is.na(rel_data1) ] <- 0

colnames(rel_data1) <- c('income', 'cult_area', 'expenditure', 'livestock_cost')
# rel_data2


### mean and sample variance of income
mean_income_1 <- mean(rel_data1$income) # 1912.269
var_income_1 <- var(rel_data1$income) # 1171135


### histogram of income
hist(rel_data1$income)


### skewness and kurtosis of income
skewness_income_1 <- skewness(rel_data1$income) # 1.422833
kurtosis_income_1 <- kurtosis(rel_data1$income) # 3.250623


  # g <- rnorm(n=92, mean=mean_income_1, sd=sqrt(var_income_1))
  # g
  # hist(g)
  # skewness(g) # 0.3399809
  # kurtosis(g) # 0.9187746


### hypothesis testing: income -- normal dist
shapiro.test(rel_data1$income)
# W = 0.90255, p-value = 1.955e-05
# p-value < 0.05 => reject the null hypothesis => income is not normally distributed

ks.test(rel_data1$income, "pnorm")
# D = 1, p-value = 3.331e-16
# alternative hypothesis: two-sided
# p-value < 0.05 => reject the null hypothesis => income is not normally distributed


### correlation between income and cultivated area
  ## for normal data only!
cor.test(rel_data1$income, rel_data1$cult_area, method="pearson")
# t = 8.5046, df = 76, p-value = 1.188e-12
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.5633018 0.7969843
# sample estimates:
# cor 
#   0.6982993 -- sufficiently large value of est. corr

# p-value < 0.05 => reject the null hypothesis (corr = 0) => corr значимо отделена от 0


cor.test(rel_data1$income, rel_data1$cult_area, method="kendall")
# z = 6.2572, p-value = 3.919e-10
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
# tau 
#   0.484223

# p-value < 0.05 => reject the null hypothesis (corr = 0) => corr значимо отделена от 0


cor.test(rel_data1$income, rel_data1$cult_area, method="spearman")
# S = 28360, p-value = 2.51e-10
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
#   0.641372 -- sufficiently large value of est. corr

# p-value < 0.05 => reject the null hypothesis (corr = 0) => corr значимо отделена от 0



### hypothesis testing. H_0: income variance = expenditure variance
var.test(rel_data1$income, rel_data1$expenditure)
# F = 1.0926, num df = 77, denom df = 77, p-value = 0.6986
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.6966418 1.7136044
# sample estimates:
#   ratio of variances 
# 1.092597 

# p-value > 0.05 => NOT reject the null hypothesis => we think, that true variances are equal


### CI for average livestock cost
eexp(rel_data1$livestock_cost, ci=TRUE)
# LCL          UCL 
# 0.0008186549 0.0012778159 

UCL_exp_1 <- 1 / 0.0008186549
LCL_exp_1 <- 1 / 0.0012778159
conf_int_1 <- c(LCL_exp_1, UCL_exp_1) # ~ (783, 1222)
conf_int_1 # (782.5853, 1221.5159)

    
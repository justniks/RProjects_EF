install.packages("readxl")
install.packages("moments")
install.packages("corrplot")
install.packages("EnvStats")
library(readxl)
library(moments)
library(corrplot)
library(EnvStats)

### mean and sample variance of income
D4 <- read_excel("SrednayaAsia.xlsx", sheet=1, skip=0)
Mean_income_4=mean(D4$`общий доход`) #758.927
Var_income_4=var(D4$`общий доход`) #430690.803

### histogram of income
Доход_4 <- D4$`общий доход`
hist(Доход_4, breaks=30, main = paste("Гистограмма дохода"))

### skewness and kurtosis of income
skewness_income_4 <- skewness(D4$`общий доход`) # 3.685
kurtosis_income_4 <- kurtosis(D4$`общий доход`) # 22.578

### hypothesis testing: income -- normal dist
shapiro.test(D4$`общий доход`)
#W = 0.6582, p-value = 2.323e-14
#p-value < 0.05 => reject the null hypothesis 
# => income does not have normal distribution

ks.test(D4$`общий доход`, "pnorm", mean=mean(D4$`общий доход`), sd=sd(D4$`общий доход`))
#D = 0.19914, p-value = 0.0004466; alternative hypothesis: two-sided
#p-value < 0.05 => reject the null hypothesis 
# => income does not have normal distribution

### correlation between income and cultivated area
a <- D4[c(11, 14)]
a[ is.na(a) ] <- 0
b <- a$`площадь под озимую пшеницу`+a$`площадь под яровую пшеницу`
cor.test(D4$`общий доход`, b, method="spearman")
#S = 77849, p-value = 4.886e-12; alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.6077856  

cor.test(D4$`общий доход`, b, method ="kendall")
#z = 6.4709, p-value = 9.74e-11; alternative hypothesis: true tau is not equal to 0
#sample estimates:
#  tau 
#0.4290236

### hypothesis testing. H_0: income variance = expenditure variance
var.test(D4$`общий доход`, D4$`общие расходы`)
#F = 5.8213, num df = 105, denom df = 105, p-value < 2.2e-16
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  3.962547 8.551855
#sample estimates:
#  ratio of variances 
#5.821265
# p-value < 0.05 => отвергаем нулевую гипотезу => предпологаем, что истинные дисперсии не равны

### CI for average livestock cost
y <- D4$`стоимость скота`

#1 способ
eexp(y, ci=TRUE)
#LCL         UCL 
#0.002219491 0.003250792  

lower_confidence_limit_4_1 <- 1 / 0.003250792
upper_confidence_limit_4_1 <- 1 / 0.002219491
CI_4_1 <- c(lower_confidence_limit_4_1, upper_confidence_limit_4_1)
CI_4_1 # ~ (307, 451)

#2 способ
lower_confidence_limit_4_2 <- 2*length(y)*mean(y)/qchisq(0.025, df = 2*length(y), lower.tail = FALSE)
upper_confidence_limit_4_2 <- 2*length(y)*mean(y)/qchisq(0.025, df = 2*length(y), lower.tail = TRUE)
CI_4_2 <- c(lower_confidence_limit_4_2, upper_confidence_limit_4_2)
CI_4_2 # ~ (307, 451)

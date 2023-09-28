install.packages("readxl")
install.packages("moments")
install.packages("corrplot")
install.packages("EnvStats")
library(readxl)
library(moments)
library(corrplot)
library(EnvStats)

### mean and sample variance of income
D3 <- read_excel("SimbirskayaGuberniya.xlsx", sheet=1, skip=0)
Mean_income_3=mean(D3$`всего доходов`) #464.24
Var_income_3=var(D3$`всего доходов`) #90615.32

### histogram of income
Доход_3 <- D3$`всего доходов`
hist(Доход_3, breaks=30, main = paste("Гистограмма дохода"))

### skewness and kurtosis of income
skewness_income_3 <- skewness(D3$`всего доходов`) # 2.289
kurtosis_income_3 <- kurtosis(D3$`всего доходов`) # 11.09997

### hypothesis testing: income -- normal dist
shapiro.test(D3$`всего доходов`)
#W = 0.79904, p-value = 2.562e-16
#p-value < 0.05 => reject the null hypothesis 
# => income does not have normal distribution

ks.test(D3$`всего доходов`, "pnorm", mean=mean(D3$`всего доходов`), sd=sd(D3$`всего доходов`))
#D = 0.15961, p-value = 2.1e-05; alternative hypothesis: two-sided
#p-value < 0.05 => reject the null hypothesis 
# => income does not have normal distribution

### correlation between income and cultivated area
cor.test(D3$`всего доходов`, D3$`посевная пл`, method="spearman")
#S = 471401, p-value < 2.2e-16; alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho
#  0.7516853 

cor.test(D3$`всего доходов`, D3$`посевная пл`, method ="kendall")
#z = 12.266, p-value < 2.2e-16; alternative hypothesis: true tau is not equal to 0
#sample estimates:
#  tau 
#0.5514624

### hypothesis testing. H_0: income variance = expenditure variance
var.test(D3$`всего доходов`, D3$`всего год расходы`)
#F = 1.5306, num df = 224, denom df = 224, p-value = 0.001524
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  1.177143 1.990226
#sample estimates:
#  ratio of variances 
#1.530614 
# p-value < 0.05 => отвергаем нулевую гипотезу => предпологаем, что истинные дисперсии не равны

### CI for average livestock cost
x <- D3$`ст-ть скота`

#1 способ
eexp(x, ci=TRUE)
#LCL         UCL 
#0.002169324 0.002818006  

lower_confidence_limit_3_1 <- 1 / 0.002818006
upper_confidence_limit_3_1 <- 1 / 0.002169324
CI_3_1 <- c(lower_confidence_limit_3_1, upper_confidence_limit_3_1)
CI_3_1 # ~ (354, 461)

#2 способ
lower_confidence_limit_3_2 <- 2*length(x)*mean(x)/qchisq(0.025, df = 2*length(x), lower.tail = FALSE)
upper_confidence_limit_3_2 <- 2*length(x)*mean(x)/qchisq(0.025, df = 2*length(x), lower.tail = TRUE)
CI_3_2 <- c(lower_confidence_limit_3_2, upper_confidence_limit_3_2)
CI_3_2 # ~ (354, 461)

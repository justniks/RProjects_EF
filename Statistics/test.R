############# LIBRARIES
library(readxl)
library(moments)
library(corrplot)

############# FUNCTIONS
get_moda <- function(col){ 
  dens <- density(col) 
  dens_dataframe <- as.data.frame(cbind(dens$x,dens$y))
  colnames(dens_dataframe) <- c('value', 'density')
  dens_dataframe$value[which.max(dens_dataframe$density)]}


############# DATAFRAME LOAD
Data <- read_excel('EurovisionDZ.xlsx')
Data

############# SELECTING RELEVANT DATA
Data_2011 <- Data[Data$year == 2011, ]
Data_2011
cor.test(Data_2011$number, Data_2011$points)


############# 
# age, points, semifinal, lang, win, dev 
# moda, median, kurtosis, skewness, histogram, emprical distribution func

# hist(Data_2011$age)
# D <- density(Data_2011$age)
# D
# plot(D$x,D$y)

# Dens <- as.data.frame(cbind(D$x,D$y))
# moda <- Dens$V1[which.max(Dens$V2)]
# moda

############# AGE
age_moda <- get_moda(Data_2011$age)
age_moda # 22.22905

summary(Data_2011$age)

age_mean <- mean(Data_2011$age)
age_mean # 25.56

age_median <- median(Data_2011$age)
age_median # 24

age_kurtosis <- kurtosis(Data_2011$age)
age_kurtosis # 6.612469

age_skewness <- skewness(Data_2011$age)
age_skewness # 1.860073

hist(Data_2011$age)


############# POINTS
points_moda <- get_moda(Data_2011$points)
points_moda # 78.64098

summary(Data_2011$points)

points_mean <- mean(Data_2011$points)
points_mean # 99.76

points_median <- median(Data_2011$points)
points_median # 96

points_kurtosis <- kurtosis(Data_2011$points)
points_kurtosis # 3.114705

points_skewness <- skewness(Data_2011$points)
points_skewness # 0.7880587

hist(Data_2011$points, breaks=15, freq=FALSE)
plot(density(Data_2011$points)$x, density(Data_2011$points)$y)


############# CORRELATION
correlation_dataframe <- Data_2011[c(8, 4, 15, 7, 10, 12)]
head(correlation_dataframe)
correlation_matrix_ken <- cor(correlation_dataframe, method="kendall")
correlation_matrix_ken
corrplot(correlation_matrix_ken)

correlation_matrix_sp <- cor(correlation_dataframe, method="spearman")
correlation_matrix_sp
corrplot(correlation_matrix_sp)


############# CONFIDENCE INTERVALS

# conf intr = (mean - t(alpha/2, n-1) * s / sqrt(n), mean + ...)
# age - normal
n <- length(Data_2011$age) # 25
gam <- 0.9
alpha_div_2 <- (1 - gam) / 2 # 0.05
alpha_div_2

s <- sqrt( var(Data_2011$age) ) # 6.880407
t_border <- qt(alpha_div_2, lower.tail=FALSE, df=n-1) # 1.710882

confidence_interval_age_mean <- c(age_mean - t_border * s / sqrt(n), 
                                  age_mean + t_border * s / sqrt(n))
confidence_interval_age_mean # (23.20569, 27.91431)


############# HYPOTHESIS TESTING
### 8
observed_age <- ..
expected_age <- ..



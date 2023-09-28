#Установка R. https://cran.r-project.org/
#https://www.rstudio.com/products/rstudio/download/

#Установите рабочую папку
setwd("/Users/nikitasysoev/Documents/R Projects")

# function declaration
get_moda <- function(col){ 
  dens <- density(col) 
  dens_dataframe <- as.data.frame(cbind(dens$x,dens$y))
  colnames(dens_dataframe) <- c('value', 'density')
  dens_dataframe$value[which.max(dens_dataframe$density)]}

#Простые вычисления
10+5
10*5
10^2+cos(0)
10^2+cos(pi)
abs(-5)
sqrt(9)
log(exp(1))

10/5
10%/%3
10%%3

2+2*2
(2+2)*2

#Можно создавать объекты и выполнять с ними операции
a <- 1
a <- sqrt(9)
a <- a + 1
a <- log(exp(a+1))
b <- a^0.5
c <- a/b


#Создадим вектора
x <- 1:10
x
x <- (log(exp(1))+1):(5*2)
a <- 10
x <- 1:a

x <- c(0,0,1,1,1)
y <- c(5,3,5,6,7,1:10,2:5)
const <- c(1,1,1,1,1)

x <- rep(1, 10)

#С векторами можно проводить стандартные операции
x+1
2*x

x <- c(0,0,1,1,1)
y <- c(5,3,5,6,7)

x+y
x*y
#Обратите внимание, что последняя команда -- это не привычное произведение матриц (о нем ниже),
#а поэлементное провизведение: первый элемент вектора x умножается на первый элемент вектора y b и т.д.

#Можно обращаться к элементам вектора
length(x)
length(a)
x[1]
x[6]
x[1:3]
z <- x[c(1,3,5)]
x[1]+y[3]


#Можно склеить два вектора в один очень длинный вектор
x <- c(0,0,1,1,1)
q <- c(const,x)
#Из такого вектора можно собрать матрицу
X <- matrix(q, nrow=5, ncol=2)
X

#Можно посмотреть на конкретный элемент этой матрицы, например, на правый нижний:
X[5,2]

#Можно посмотреть на целую строчку. Например, на первую
x <- X[1,]
X*X
#Пример матричных вычислений. Вычислим (X'X)
t(X) %*% X

#Конечно, вам не нужно помнить названия всех команд наизусть
#Можно посмотреть, например, здесь: http://www.statmethods.net/advstats/matrix.html
#Или здесь: http://aakinshin.net/ru/blog/r/functions/

#Разные типы данных
xNum <- c(1, 3.14159, 5, 7)
xLog <- c(TRUE, FALSE, TRUE, TRUE)
xChar <- c("один", "два", "три", "четыре")
xMix <- c(1, TRUE, 3, "Hello, world!")

#Узнать больше об объекте
summary(xNum)
summary(xChar)

#Операции с разными типами данных

xNum[1] + 1
xMix[1] + 1
as.numeric(xMix)
as.numeric(xMix[1])+1

xNum <- as.character(xNum)
xNum[1]+xNum[2]
paste(xNum[1],xNum[2],sep = "")


#Логические операторы
5>4
4<3

4>4
4>=4

3==3
3==4
3!=4
5>4|4<3
5>4&4<3

x <- c(5,3,5,6,7)
x[x>5]
x[x!=5]
x[x!=5&x<7]

install.packages("readxl")
library(readxl)

D <- read_excel("males.xlsx", sheet=1, skip=0)
# D <- read.csv('males.csv', sep=';')
D

#Простой способ посмотреть на описательные статистики ваших данных
summary(D)

#Немного преобразований данных
D_1982 <- D[D$YEAR==1982,]

#Распределения
rnorm(n, mean=0, sd=1) #нормальное распределение
rexp(n, rate=1) #экспоненциальное распределение
rgamma(n, shape, scale=1) #гамма-распределение
rpois(n, lambda) #распределение Пуассона
rweibull(n, shape, scale=1) #распределение Вейбулла
rcauchy(n, location=0, scale=1) #распределение Коши
rbeta(n, shape1, shape2) #бета-распределение
rt(n, df) #распределение Стьюдента
rf(n, df1, df2) #распределение Фишера
rchisq(n, df) #распределение Пирсона
rbinom(n, size, prob) #биномиальное распределение
rgeom(n, prob) #геометрическое распределение
rhyper(nn, m, n, k) #гипергеометрическое распределение
rlogis(n, location=0, scale=1) #логистическое распределение
rlnorm(n, meanlog=0, sdlog=1) #логнормальное распределение
rnbinom(n, size, prob) #отрицательное биномиальное распределение
runif(n, min=0, max=1) #равномерное распределение

x <- rnorm(20)
x <- rnorm(20, sd=0.1)

dt(1.5, df=2) #плотность Стьюдента(2) в точке x=1.5
pt(1.5, df=2) #ФР Стьюдента(2) в точке x=1.5
qt(0.14, lower.tail = TRUE, df=2) #квантиль Стьюдента(2) (вероятность быть меньше чего равна 0.14)
qt(0.14, lower.tail = FALSE, df=2) #квантиль Стьюдента(2) (вероятность быть больше чего равна 0.14)
y<-rt(200,df=2)
hist(y, breaks=30,ylim=c(0,0.5), freq =FALSE)
x=seq(-5,5,by=0.01)
curve(dt(x,df=2),add=TRUE)
curve(dt(x,df=4),add=TRUE)
curve(dt(x,df=10),add=TRUE)
curve(dt(x,df=Inf),col="red", add=TRUE)
pt(5, df=2)-pt(-5, df=2) #вероятность попасть от -5 до 5
a=seq(0,1,by=0.1)
qt(a,1)

#Выборочные х-ки
x <- rnorm(50,mean = 4,sd = 1)
x
mean(x)
var(x)
sd(x)
max(x)
min(x)
median(x)
install.packages("moments")
library(moments)

skewness(x)
kurtosis(x)

quantile(x)
quantile(x, c(0.05,0.95,0.1,0.2,0.7))

hist(x)
D <- density(x)
D
plot(D$x,D$y)
Dens <- as.data.frame(cbind(D$x,D$y))
moda <- Dens$V1[which.max(Dens$V2)]
moda
moda1 <- get_moda(x)
moda1


cor.test(D$x,D$y)

install.packages("corrplot")
library(corrplot)
corrplot(cor(Data))




binom.test()
t.test()
prop.test()
var.test(x, y)

#Критерии согласия
chisq.test(x)
ks.test()
shapiro.test()


###### Задание 0 ######
# используется метод ветвей и границ:
# http://lpsolve.sourceforge.net/5.5/integer.htm 
library(lpSolve)
library(gMOIP)
#options(rgl.useNULL = TRUE)
#library(rgl)

### Постановка задачи ###
# z = 21 * x_1 + 11 * x_1 \to max
# 7 * x_1 + 4 * x_2 <= 13
# x_j \in \mathbb{Z}, x_j >= 0


#зададим матрицу коэффициентов
A <- matrix(c(7,4), ncol = 2, byrow = TRUE)

#вектор свободных членов
b <- c(13)

#коэффициенты целевой функции
obj <- c(21, 11)

#визуализация решения задачи с учетом целочисленности (type = "i" = integer)
p1 <- plotPolytope(
  A,
  b,
  obj,
  type = rep("i", ncol(A)), # _i_ for integer
  crit = "max",
  plotFaces = TRUE,
  plotFeasible = TRUE,
  plotOptimum = TRUE,
  labels = "coord"
)
p1
#type - тип переменной
#i - integer, c - continuous

#визуализация решения задачи без ограничения на целочисленность
p2 <- plotPolytope(
  A,
  b,
  obj,
  type = rep("c", ncol(A)),
  crit = "max",
  plotFaces = TRUE,
  plotFeasible = TRUE,
  plotOptimum = TRUE,
  labels = "coord"
) 
p2



#для решения задачи в общем виде добавим ограничения
constraints <- c("<=")
mod <- lp("max", objective.in = obj, const.mat = A, 
          const.dir = constraints, 
          const.rhs = b, all.int = TRUE)
#all.int = TRUE - указание на то, что все переменные целые

#оптимальное решение
mod$solution

#значение целевой функции
mod$objval


###### Задание 1 ######
data <-  read.csv(file  =  "SpeciesOfInt.csv",  header  =  T, sep = ";")
data

## Формализация задачи ##
# x_i = 1, if охраняем 1-ю зону; 0, иначе
# c_i -- издержки охраны i-й зоны
# z = \sum_i^n c_i * x_i \to min
# \sum_i^n x_i * a_{ij}  \forall j = 1, ..., 14

obj <- data$Cost
A <- t(data[,2:15]) # транспонируем
b <- rep(1,  times  =  14)
constraints <- rep(">=",  times  =  14)
mod <- lp("min", objective.in = obj, const.mat = A, 
          const.dir = constraints, 
          const.rhs = b, all.bin = TRUE)
mod$solution
mod$objval


###### Задание 2 ######


###### Задание 3 ######

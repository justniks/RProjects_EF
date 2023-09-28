#install.packages('lpSolve')
#install.packages('readxl')

library(readxl)
library(lpSolve)
data <-  read.csv(file  =  "MOR HW.csv",  header  =  T)
A <- data[1:72,2:73]
b <- data[1:72,74]
obj <- as.vector(data[73, 2:73])
constraints <- c(rep("=", times = 12), rep("<=", times = 12), rep("=", times = 36), rep("<=", times = 12))
mod <- lp("min", objective.in = obj, const.mat = A, 
          const.dir = constraints, 
          const.rhs = b, int.vec=1:36)
mod$solution
mod$objval

k <- c(rep((1), times = 36), rep((0), times = 35), 0)

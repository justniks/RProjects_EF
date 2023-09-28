library(ggplot2)
library(gMOIP)
library(lpSolve)


C <- c(3.4,4.3,2.4,2.2,3.7)
A <- matrix(c(1.1,1.2,1.8,1.1,1.3,
              0.9,1.1,0.7,1,1.1,
              50,60,40,30,60,
              24,45,18,12,37,
              210,340,150,260,300), byrow = TRUE, ncol = 5, nrow = 5)
b <- c(250,128,7000,3700,32000)

constraints_direction <- c('>=', '>=', '>=', '>=', '>=')

mod <- lp(direction = "min", objective.in = C,
          const.mat = A, const.dir = constraints_direction, const.rhs = b,
          compute.sens = TRUE)

mod$status # 0 -- success
mod$solution # 0.000000  0.000000 97.040566  9.874405 49.588566
mod$objval # 438.0987

sum(mod$solution)

#                    c_1          c_2          c_3          c_4          c_5
mod$sens.coef.from # 2.590905     ((4.208532)) 2.020599     1.576991     2.568317
mod$sens.coef.to #   1.000000e+30 1.000000e+30 3.521116e+00 3.193913e+00 3.766228e+00


###
C1 <- c(3.4,4.208531,2.4,2.2,3.7)
mod1 <- lp(direction = "min", objective.in = C1,
           const.mat = A, const.dir = constraints_direction, const.rhs = b,
           compute.sens = TRUE)

mod1$status # 0 -- success
mod1$solution # 0.00000  17.36782 (>0 => it's good) 101.22770  12.42429  25.60157
mod1$objval # 438.0987





# ######
# s <- 4.3
# step <- 0.0001
# 
# mod1 <- mod
# mod1
# 
# while (mod1$solution[2] == 0){
#   s <- s - step
#   
#   C1 <- c(3.4,s,2.4,2.2,3.7)
#   mod1 <- lp(direction = "min", objective.in = C1,
#              const.mat = A, const.dir = constraints_direction, const.rhs = b,
#              compute.sens = TRUE)
# }
# 
# s # ~ 4.2085



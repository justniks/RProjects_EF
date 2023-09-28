library(ggplot2)
library(gMOIP)
library(lpSolve)


C <- c(1.5,3,3,5)
A <- matrix(c(1,1,0,0,
              0,0,1,1,
              5,0,8,0,
              0.8,0,1.2,0,
              0.5,0,0.5,0), byrow = TRUE, ncol = 4, nrow = 5)
b <- c(300,300,2600,400,200)

constraints_direction <- c("=", "=", '<=', '<=', '<=')

mod <- lp(direction = "min", objective.in = C,
          const.mat = A, const.dir = constraints_direction, const.rhs = b,
          compute.sens = TRUE)

mod$status # 0 -- success
mod$solution # 200 100 200 100
mod$objval # 1700


######
C1 <- c(1.5,3,3,5.5)
mod1 <- lp(direction = "min", objective.in = C1,
           const.mat = A, const.dir = constraints_direction, const.rhs = b,
           compute.sens = TRUE)

mod1$status # 0 -- success
mod1$solution # 40 260 300   0
mod1$objval # 1740


######
mod$duals # 3.00  5.00  0.00 -1.25 -1.00  0.00  0.00  0.00  0.00
mod$duals.from # 2.0e+02  2.0e+02 -1.0e+30  3.6e+02  2.0e+02 -1.0e+30 -1.0e+30 -1.0e+30 -1.0e+30
mod$duals.to
# [1] 1.000000e+30 1.000000e+30 1.000000e+30 4.000000e+02 2.166667e+02 1.000000e+30 1.000000e+30 1.000000e+30
# [9] 1.000000e+30

#                    c_1      c_2      c_3      c_4
mod$sens.coef.from # 1.000000 2.833333 2.750000 4.500000
mod$sens.coef.to #   1.666667 3.500000 3.500000 5.250000



######
C2 <- c(1.5,3,3,5.2)
mod2 <- lp(direction = "min", objective.in = C2,
           const.mat = A, const.dir = constraints_direction, const.rhs = b,
           compute.sens = TRUE)

mod2$status # 0 -- success
mod2$solution # 200 100 200 100
mod2$objval # 1720
mod2$duals # 3.00  5.20  0.00 -1.75 -0.20  0.00  0.00  0.00  0.00
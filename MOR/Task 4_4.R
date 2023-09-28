library(ggplot2)
library(gMOIP)
library(lpSolve)


C <- c(-5,10,25,-10,5,20)
A <- matrix(c(0.88,-0.12,-0.12,0,0,0,
              -0.18,0.82,-0.18,0,0,0,
              0,0,0,0.75,-0.25,-0.25,
              1,0,0,1,0,0,
              0,1,0,0,1,0,
              0,0,1,0,0,1), byrow = TRUE, ncol = 6, nrow = 6)
b <- c(0,0,0,33,80,60)

constraints_direction <- c(">=", "<=", '>=', '<=', '<=', '<=')

mod <- lp(direction = "max", objective.in = C,
          const.mat = A, const.dir = constraints_direction, const.rhs = b,
          compute.sens = TRUE)

mod$status # 0 -- success
mod$solution # 10.28571 15.42857 60.00000 21.52381 64.57143  0.00000
mod$objval # 1710.476

mod$solution[4:6]
sum(mod$solution[4:6])

sum(mod$solution)




######
b1 <- c(0,0,0,33+5,80,60)
b1

mod1 <- lp(direction = "max", objective.in = C,
           const.mat = A, const.dir = constraints_direction, const.rhs = b1,
           compute.sens = TRUE)

mod1$status # 0 -- success
mod1$solution # 10.28571 15.42857 60.00000 21.52381 64.57143  0.00000
mod1$objval # 1710.476


mod$duals
# [1]  -3.714286   9.619048 -13.333333   ((0.000000))   1.666667  26.285714   0.000000
# [8]   0.000000   0.000000   0.000000   0.000000  -9.619048

# \Delta z = y*_4 \cdot \Delta b_4 = 0 => opt \equiv


mod$duals.from
# [1] -8.780488e+00 -4.807692e+00 -1.614286e+01 ((-1.000000e+30))  1.542857e+01
# [6]  7.105427e-15 -1.000000e+30 -1.000000e+30 -1.000000e+30 -1.000000e+30
# [11] -1.000000e+30 -5.136364e+01

mod$duals.to
# [1] 1.096491e+00 5.136364e+01 8.928571e-01 ((1.000000e+30)) 8.357143e+01 7.388889e+01
# [7] 1.000000e+30 1.000000e+30 1.000000e+30 1.000000e+30 1.000000e+30 4.807692e+00

# почти при любом изменении b_4 y* \equiv => 3rd Th: \Delta z = \Delta b_4 \cdot y*_4
# = 5 * 0 = 0

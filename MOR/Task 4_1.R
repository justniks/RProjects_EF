library(lpSolve)

C <- c(0.3,0.9)
A <- matrix(c(-0.21,0.3,
              -0.03, 0.01,
              1,1), ncol=2, nrow=3, byrow=TRUE)
b <- c(0,0,800)
# 800 = b_3
constraints_direction <- c('>=','<=','>=')

mod <- lp(direction = "min", objective.in = C,
          const.mat = A, const.dir = constraints_direction, const.rhs = b,
          compute.sens = TRUE)

mod$status # 0 -- success
mod$solution # 470.5882 329.4118
mod$objval # 437.6471

mod$duals
# y_1       y_2       y_3           ограничения двойственной задачи
# 1.1764706 0.0000000 ((0.5470588)) 0.0000000 0.0000000

# x* \cdot (c - Ay*) -> скобки -- это ограничение двойственной задачи

mod$duals.from
# -1.680000e+02 -1.000000e+30  ((2.273737e-13)) -1.000000e+30 -1.000000e+30

mod$duals.to
# 1.38e+02 1.00e+30 ((1.00e+30)) 1.00e+30 1.00e+30

# => b_3 \in (2.273737e-13, 1.00e+30) при таких b_3 y* \equiv => 3-я Th двойственности вып-ся
# если b_3 увел-ся на 100, то \Delta z = \Delta b_3 \cdot y*_3 = 100 * 0.5470588 \approx 54.7


###### 
b1 <- c(0,0,800+100)
mod1 <- lp(direction = "min", objective.in = C,
           const.mat = A, const.dir = constraints_direction, const.rhs = b1,
           compute.sens = TRUE)

mod1$status # 0 -- success
mod1$solution # 529.4118 370.5882
mod1$objval # 492.3529 = z*_0 + \Delta z, \Delta z = \Delta b_3 \cdot y_3* =
# = 0.547 \cdot \Delta b_3 = 0.547 * 100 \approx 54.7


mod1$duals # 1.1764706 0.0000000 0.5470588 0.0000000 0.0000000
sum(mod$duals != mod1$duals) # 0 => y_3* \equiv => 3rd Theorem is applicable

mod$sens.coef.from # -0.63  0.30
mod$sens.coef.to # 9e-01 1e+30 = 0.9 дофига
# => в этих интервалах могут меняться c_1 и с_2, чтобы x* \equiv



#######
b_3 <- 800:1000
z <- NULL # вектор оптимальных значений

for (i in 1:length(b_3)){
  b2 <- c(0,0,b_3[i])
  mod2 <- lp(direction = "min", objective.in = C,
             const.mat = A, const.dir = constraints_direction, const.rhs = b2,
             compute.sens = TRUE)
  z[i] <- mod2$objval
}

plot(b_3, z, type='l')

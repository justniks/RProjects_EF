
###############
##### ЗЛП #####
###############

#подгрузим библиотеки
# options(rgl.useNULL = TRUE) #оказалось не обязательно запускать эту строку
# library(rgl)
library(ggplot2)
library(gMOIP) # рисует графическое решение
library(ggplot2)
library(lpSolve) # решает ЗЛП


#решение через симплексный метод
C <- c(1,8,9,9)
A <- matrix(c(1,2,1,-1,
              -1,1,3,1), byrow = TRUE, ncol = 4, nrow = 2)
b <- c(24, -16)

#вектор знаков линейных ограничений
constraints_direction <- c(">=", "=")

#поиск оптимального решения
mod <- lp(direction = "min", objective.in = C,
          const.mat = A, const.dir = constraints_direction, const.rhs = b,
          compute.sens = TRUE)
#compute.sens = TRUE - проводить анализ чувствительности

#совместна ли система
mod$status # 0 -- success

#значение переменных в оптимуме
mod$solution

#значение функции
mod$objval

#двоственные переменные
mod$duals
#первые значения относятся к двойственным переменным, а вторые -- дополняющая
#нежесткость ограничений двойственной задачи (c-ay)

#интервалы допустимости: границы, при которых не меняется оптимальный х
mod$duals.from
mod$duals.to

#интервалы оптимальности
mod$sens.coef.from
mod$sens.coef.to
#относятся к коэффициентам C: границы, при которых не меняется оптимальный х



############################################
################ задача ###################
############################################

C1 <- c(-2,-1,5,-11)
A1 <- matrix(c(3,7,-11,-2,
              1,2,-3,-1), byrow = TRUE, ncol = 4, nrow = 2)
b1 <- c(21, 6)

constraints_direction1 <- c("<=", "=")

#поиск оптимального решения
mod1 <- lp(direction = "min", objective.in = C1,
          const.mat = A1, const.dir = constraints_direction1, const.rhs = b1,
          compute.sens = TRUE)
mod1$status # 3 -- неограниченная задача: z^* \to +\infty
mod1$solution
mod1$objval



############################################
################ задача 3.6'' ##############
############################################

C2 <- c(11,12,4,3)
A2 <- matrix(c(1,1,-1,-1,
               1,2,1,1), byrow = TRUE, ncol = 4, nrow = 2)
b2 <- c(2, 1)

constraints_direction2 <- c("=", "<=")

#поиск оптимального решения
mod2 <- lp(direction = "max", objective.in = C2,
           const.mat = A2, const.dir = constraints_direction2, const.rhs = b2,
           compute.sens = TRUE)
mod2$status # 2 -- No feasible solution == задача недопустимая (ДМ = \emptyset)
mod2$solution
mod2$objval



############################################
################ задача ####################
############################################

C3 <- c(11,-12,4,3)
A3 <- matrix(c(1,-1,-1,1,
               1,-2,2,-1), byrow = TRUE, ncol = 4, nrow = 2)
b3 <- c(6.5, 12.3)

constraints_direction3 <- c(">=", ">=")

#поиск оптимального решения
mod3 <- lp(direction = "min", objective.in = C3,
           const.mat = A3, const.dir = constraints_direction3, const.rhs = b3,
           compute.sens = TRUE)
mod3$status # 0
mod3$solution
mod3$objval

### целочисленная задача
C4 <- c(11,-12,4,3)
A4 <- matrix(c(1,-1,-1,1,
               1,-2,2,-1), byrow = TRUE, ncol = 4, nrow = 2)
b4 <- c(6.5, 12.3)

constraints_direction4 <- c(">=", ">=")

#поиск оптимального решения
mod4 <- lp(direction = "min", objective.in = C4,
           const.mat = A4, const.dir = constraints_direction4, const.rhs = b4,
           compute.sens = TRUE, int.vec = c(1))
mod4$status # 0
mod4$solution
mod4$objval



############################################
##### задача #####
############################################


C <- c(30, 35)
A <- matrix(c(3,5,
              14,12), byrow = TRUE, ncol = 2, nrow = 2)
b <- c(120, 400)
constraints_direction <- c("<=", "<=")

mod <- lp(direction = "max", objective.in = C,
          const.mat = A, const.dir = constraints_direction, const.rhs = b,
          compute.sens = TRUE)



##############################
## Спрос на ресурсы в цикле ##
##############################

a <- 1
for(i in 1:11) {
  a[i+1] <- a[i] + 1
}
#вместо i подставляем по очереди все целые числа от 1 до 11 и запускаем код внутри фигурных скобок

#как будет меняться значение целевой функции, если менять b1? 
b1 <- 50:200

#создадим пустой вектор z
z <- NULL

#запустим цикл
for(i in 1:length(b1)) {
  b <- c(b1[i], 400)
  mod <- lp(direction = "max", objective.in = C,
            const.mat = A, const.dir = constraints_direction, const.rhs = b,
            compute.sens = TRUE)
  z[i] <- mod$objval #целевое значение функции подставляй в вектор z под номером i
}
#for(i in 1:length(b1)) - подставляй вместо i по очереди все целые числа от 1 до числа
#равного размеру вектора b1

#построим график
plot(b1, z, type = "l")


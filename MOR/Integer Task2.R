library(readxl)

## Формализация задачи ##
# s - запас работников на самое начало мес. до найма/увольнения
# s_1 >= 30
# s_1 = 20 + h_1 - f_1
# s_2 >= 60
# s_2 = s_1 + h_2 - f_2
# s_3 >- 55
# s_3 = s_2 + h_2 - f_2
# s_4 >= 40
# s_4 = s_3 + h_4 - f_4
# s_5 >= 45
# s_5 = s_4 + h_5 - f_5
# s_6 >= 50
# s_6 = s_5 + h_6 - f_6
# \forall i = 1, ..., 6,  s_i, h_i, f_i \in \mathbb{Z}, >= 0
#
# z = 8*(s_1 + ... + s_6) + 5*(h_1 + ... + h_6) + 10*(f_1 + ... + f_6) \to min

data <- read_excel("Integer Tables.xlsx")

obj <- as.vector(data[13, 2:19])
A <- data[1:12, 2:19]
b <- data[1:12, 20]
constraints <- c( rep('>=', 6), rep('=', 6) )
mod <- lp("min", objective.in = obj, const.mat = A, 
          const.dir = constraints, 
          const.rhs = b, all.int = TRUE)
mod$solution
mod$objval

# x*_4 = 45 > 40, т.е. выгодно держать больше пилотов, чем требуется, потому что 
# держать сотрудника выгоднее, чем уволить его, а потом нанять еще одного

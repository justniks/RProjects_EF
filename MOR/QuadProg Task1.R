library(quadprog)
# данный метод решает только те задачи, где целевая функция может быть представлена как
# квадратичная форма (или просто квадраты и какие-то линейные добавки)
# только для положительно определенных квадратичных форм
# а ограничения должны быть линейными

Dmat <- matrix(c(2,0,0,
                 0,2,0,
                 0,0,2), nrow = 3)
# x_1^2 + x_2^2 + x_3^2 \tp extr
# => D = 
# (2 0 0)
# (0 2 0)
# (0 0 2)

dvec <- c(0,0,0)
# d = (0, 0, 0), т.к. нет линейных добавок в целевой функции

Amat <- matrix(c(1,1,1,
                 -2,1,-1), nrow = 3, byrow = TRUE)
# - 2 * x_1 + x_2 - x_3 >= -5
# x_1 + x_2 + x_3 = 3
# ( 1 1  1) (x_1)    ( 3)
# (-2 1 -1) (x_2) >= (-5)
#           (x_3)
# почему-то r считает данную матрицу A.T

bvec <- c(3,-5)


qp <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq=1)
# meq -- 1-е ограничение - равенство
# the first meq constraints are treated as equality constraints, all further 
# as inequality constraints (defaults to 0).

qp
# solves min (  1/2 * b.T * D * b - d.T * b )  s.t. A.T * b >= b_0

# $solution
# [1] 1 1 1
# 
# $value
# [1] 3
# 
# $unconstrained.solution
# [1] 0 0 0
# 
# $iterations
# [1] 2 0
# 
# $Lagrangian
# [1] 2 0
# 
# $iact
# [1] 1
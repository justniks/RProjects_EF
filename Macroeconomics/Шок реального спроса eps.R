### Экзогенные параметры ###
Y_n = 100
pi_target = 2
alpha = 1
rho = 2
phi = 0.25
theta_pi = 0.5
theta_y = 0.5

### Эндогенные переменные ###
Y = rep(NA, times = 20)
pi = rep(NA, times = 20)
pi_exp = rep(NA, times = 20)
r = rep(NA, times = 20)
i = rep(NA, times = 20)

### Начальные значения ###

Y[1] = Y_n
pi[1] = pi_target
pi_exp[1] = pi_target
r[1] = rho
i[1] = rho + pi_target

### Шоки ###
epsilon = rep(NA, times = 20) # шок спроса
v = rep(NA, times = 20) # шок издержек (AS)
eta = rep(NA, times = 20) # шок ожиданий (ож. инф.)
u = rep(NA, times = 20)

### Последствия случайных шоков ###
epsilon [1] = 0
epsilon [2] = 1
epsilon [3:20] = 0

v[1:20] = 0
eta[1:20] = 0
u[1:20] = 0

left_mat = matrix(data = c(1,       0,            alpha,0,
                           -phi,    1,            0,    0,
                           0,       1,            1,    -1,
                           -theta_y,-(1+theta_pi),0,    1),
                  nrow = 4, byrow = TRUE)

for (j in 2:20) {
  pi_exp[j] = pi[j-1] + eta[j-1]
  right_mat = matrix(data = c((Y_n+alpha*rho+epsilon[j]),
                              (pi_exp[j]-phi*Y_n+v[j]),
                              -eta[j],
                              (rho-theta_pi*pi_target-theta_y*Y_n+u[j])),
                     nrow = 4)
  sr_eq = solve(left_mat, right_mat)
  Y[j] = as.numeric(sr_eq[1,])
  pi[j] = sr_eq[2,]
  r[j] = sr_eq[3,]
  i[j] = sr_eq[4,]
}

Y=ts(data=Y, start = -1, end = 18, frequency = 1)
Y_eq = ts(data=rep(Y_n, times = 20), start = -1, end = 18, frequency = 1)
plot(Y)
lines(Y_eq, col = "red")

pi = ts(data=pi, start = -1, end = 18, frequency = 1)
pi_eq = ts(data=rep(pi_target, times = 20), start = -1, end = 18, frequency = 1)
plot(pi)
lines(pi_eq, col = "red")

r = ts(data=r, start = -1, end = 18, frequency = 1)
r_eq = ts(data=rep(rho, times = 20), start = -1, end = 18, frequency = 1)
plot(r)
lines(r_eq, col = "red")

i = ts(data=i, start = -1, end = 18, frequency = 1)
i_eq = ts(data=rep((rho+pi_target), times = 20), start = -1, end = 18, frequency = 1)
plot(i)
lines(i_eq, col = "red")
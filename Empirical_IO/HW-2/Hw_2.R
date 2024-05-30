# Libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(stargazer)
library(MASS)


# Data collection
df <- read_xlsx('HW-2/BR91-DENTS.xlsx')
head(df)


# 1 Graphs
a <- cut(df$TPOP, c(seq(0, 8, by=1), Inf))
df$TPOP_cut <- a

a <- df %>%
  group_by(TPOP_cut, DENTS) %>%
  summarise(n = n())
a 

ggplot( a, aes(x=TPOP_cut, y=n, fill=DENTS)) + 
  geom_bar( position="stack", stat="identity" )

hist(df$DENTS, breaks=0:17, xlim=c(0,17))
table(df$DENTS)
median( df$DENTS ) # 2

plot(df$TPOP)
plot(df$DENTS)
plot(df$OPOP)
plot(df$NGRW)
plot(df$PGRW)
plot(df$OCTY)
plot(df$BIRTHS)
plot(df$ELD)
plot(df$PINC)
plot(df$LNHDD)
plot(df$LANDV)


df_noout <- subset(
  df, df$BIRTHS < 5000
)
plot(df_noout$BIRTHS)

df_noout <- subset(
  df_noout, df_noout$OCTY < 8
)
plot(df_noout$OCTY)

df_noout <- subset(
  df_noout, df_noout$TPOP < 30
)
plot(df_noout$TPOP)

df_noout <- subset(
  df_noout, df_noout$DENTS <= 15
)
plot(df_noout$DENTS)

df_noout <- subset(
  df_noout, df_noout$OPOP <= 3
)
plot(df_noout$OPOP)

dim(df_noout)
dim(df)

###
a <- cut(df_noout$TPOP, c(seq(0, 8, by=1), Inf))
df_noout$TPOP_cut <- a

a <- df_noout %>%
  group_by(TPOP_cut, DENTS) %>%
  summarise(n = n())
a 

ggplot( a, aes(x=TPOP_cut, y=n, fill=DENTS)) + 
  geom_bar( position="stack", stat="identity" )

hist(df_noout$DENTS, breaks=0:15, xlim=c(0,15))
table(df_noout$DENTS)
median( df_noout$DENTS ) # 1


# 2 Corr
cor(df$DENTS, df$TPOP) # 0.8598786
cor(df$DENTS, df$BIRTHS) # 0.1563089
cor(df$DENTS, df$ELD) # -0.2604731

cor(df_noout$DENTS, df_noout$TPOP) # 0.8646404
cor(df_noout$DENTS, df_noout$BIRTHS) # 0.5330391
cor(df_noout$DENTS, df_noout$ELD) # -0.2751395


# 3 LinReg
res <- lm(
  DENTS ~ TPOP + OPOP + NGRW + PGRW + OCTY + BIRTHS + ELD + PINC + LNHDD + LANDV, 
data=df)
summary(res)
stargazer(res)

res1 <- lm(
  DENTS ~ TPOP + OPOP + NGRW + PGRW + OCTY + BIRTHS + ELD + PINC + exp(LNHDD) + LANDV, 
  data=df)
summary(res1)
stargazer(res1)

res2 <- lm(
  DENTS ~ TPOP + OPOP + NGRW + PGRW + OCTY + BIRTHS + ELD + PINC + LNHDD + LANDV, 
  data=df_noout)
summary(res2)
stargazer(res2)


# 4 Ordered logit / probit
# install.packages('MASS')

res_logit <- polr(
  formula = as.factor(DENTS_F) ~ TPOP + OPOP + NGRW + PGRW + OCTY + BIRTHS + ELD + PINC + LNHDD + LANDV,
  data=df, 
  method=c('logistic'),
  Hess=T
)
summary(res_logit)
stargazer(res_logit)


res1_logit <- polr(
  formula = as.factor(DENTS_F) ~ TPOP + OPOP + NGRW + PGRW + OCTY + BIRTHS + ELD + PINC + LNHDD + LANDV,
  data=df_noout, 
  method=c('logistic'),
  Hess=T
)
summary(res1_logit)
stargazer(res1_logit)


# res_probit <- polr(
#   formula = as.factor(DENTS_F) ~ TPOP + OPOP + NGRW + PGRW + OCTY + BIRTHS + ELD + PINC + LNHDD + LANDV,
#   data=df, 
#   method=c('probit'),
#   Hess=T
# )
# summary(res_probit)
# stargazer(res_probit)

res1_probit <- polr(
  formula = as.factor(DENTS_F) ~ TPOP + OPOP + NGRW + PGRW + OCTY + BIRTHS + ELD + PINC + LNHDD + LANDV,
  data=df_noout, 
  method=c('probit'),
  Hess=T
)
summary(res1_probit)
stargazer(res1_probit)


install.packages("seasonal")
install.packages ("mFilter")
install.packages("writexl")


library(seasonal)
library(readxl)
library(mFilter)
library(writexl)

df <- read_xlsx("proc_data.xlsx")

iip_ts <- ts(df$IIP_abs, start = c(2015, 1), end = c(2023, 3), frequency = 12)
plot(iip_ts)

m <- seas(iip_ts)

plot(m)
summary(m)
monthplot(m)

plot(m$series$s11) # seasonally adjusted data
plot(m$series$s12)
plot(m$series$s10)
plot(m$series$s13)
plot(m$series$s16)
plot(m$series$s18)


# lets use the filter to find the trend (btw there is a trend variable in seasonal package m$series$s12)
hpn <- hpfilter(m$series$s11)

plot(hpn)

# save the data: seasonally adjusted data and trend
write_xlsx(data.frame(m$series$s11), "./proc/seasonadj_iip.xlsx")
write_xlsx(data.frame(hpn$trend), "./proc/trend_iip.xlsx")
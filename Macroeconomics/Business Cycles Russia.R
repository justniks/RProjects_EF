library(BCDating)
library(readxl)

dataset <- read_excel('BCDating Dataset Russia.xlsx')
df = subset(dataset, select = 2)
ts_df <- ts(df, frequency=4, start=c(1995, 1))
bcd_obj = BBQ(ts_df)
show(bcd_obj)
summary(bcd_obj)
plot(bcd_obj, ts_df, ylab="Реальный ВВП на душу населения (руб)", main="ВВП на душу населения Россия, 1995:1-2021:4")


# Peaks Troughs Duration
# 1   <NA>  1996Q2     <NA>
# 2 1997Q4  1998Q3      3
# 3 2008Q3  2009Q1      2
# 4 2012Q4  2016Q3      15
# 5 2018Q4  2020Q2      6

# Phase ]Start  ;End] Duration LevStart LevEnd Amplitude
# 1  Recession   <NA> 1996Q2       NA       NA   1764        NA
# 2  Expansion 1996Q2 1997Q4        6     1764   1882     117.8
# 3  Recession 1997Q4 1998Q3        3     1882   1250     632.4
# 4  Expansion 1998Q3 2008Q3       40     1250   4553    3303.3
# 5  Recession 2008Q3 2009Q1        2     4553   3518    1035.4
# 6  Expansion 2009Q1 2012Q4       15     3518   5223    1705.5
# 7  Recession 2012Q4 2016Q3       15     5223   4471     751.7
# 8  Expansion 2016Q3 2018Q4        9     4471   5189     717.3
# 9  Recession 2018Q4 2020Q2        6     5189   4479     709.4
# 10 Expansion 2020Q2   <NA>       NA     4479     NA        NA
# 
# Amplitude Duration
# Exp=]T;P]    1461.0     17.5
# Rec=]P;T]     782.2      6.5
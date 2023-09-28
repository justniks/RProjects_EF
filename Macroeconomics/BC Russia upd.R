library(BCDating)
library(readxl)

df <- read_excel('BCDating Dataset Russia upd.xlsx')
ts_df <- ts(df, frequency=4, start=c(2003, 1))
bcd_obj = BBQ(ts_df)
show(bcd_obj)
summary(bcd_obj)
plot(bcd_obj, ts_df, ylab="Реальный ВВП на душу населения (руб)", main="ВВП на душу населения Россия, 2003:1-2021:3")


# Peaks Troughs Duration
# 1 2008Q2  2009Q2        4
# 2 2013Q4  2015Q4        8
# 3 2019Q2  2020Q2        4

# Phase ]Start  ;End] Duration LevStart LevEnd Amplitude
# 1 Expansion   <NA> 2008Q2       NA       NA 144569        NA
# 2 Recession 2008Q2 2009Q2        4   144569 130956   13612.6
# 3 Expansion 2009Q2 2013Q4       18   130956 151209   20252.6
# 4 Recession 2013Q4 2015Q4        8   151209 145530    5678.8
# 5 Expansion 2015Q4 2019Q2       14   145530 156993   11463.3
# 6 Recession 2019Q2 2020Q2        4   156993 147761    9232.0
# 7 Expansion 2020Q2   <NA>       NA   147761     NA        NA

# Amplitude Duration
# Exp=]T;P]   15857.9     16.0
# Rec=]P;T]    9507.8      5.3
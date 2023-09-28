install.packages('BCDating')
library(BCDating)
library(readxl)

dataset <- read_excel('BCDating Dataset.xlsx')
df = subset(dataset, select = 2)
ts_df <- ts(df, frequency=4, start=c(1996, 1))
# plot(ts_df)
# show(ts_df)
bcd_obj = BBQ(ts_df)
show(bcd_obj)
summary(bcd_obj)
# plot(bcd_obj)
plot(bcd_obj, ts_df, ylab="Real GDP per capita (chained 2010 €)", main="The Netherlands GDP per capita, 1996:1-2022:3")


#### Results
# Peaks Troughs Duration
# 1 2001Q4  2003Q3        7
# 2 2008Q2  2009Q2        4
# 3 2011Q1  2012Q4        7
# 4 2019Q4  2020Q2        2

# Phase ]Start  ;End] Duration LevStart LevEnd Amplitude
# 1 Expansion   <NA> 2001Q4       NA       NA   8915        NA
# 2 Recession 2001Q4 2003Q3        7     8915   8821      94.0
# 3 Expansion 2003Q3 2008Q2       19     8821   9996    1174.5
# 4 Recession 2008Q2 2009Q2        4     9996   9513     482.9
# 5 Expansion 2009Q2 2011Q1        7     9513   9752     239.2
# 6 Recession 2011Q1 2012Q4        7     9752   9496     256.5
# 7 Expansion 2012Q4 2019Q4       28     9496  10522    1026.0
# 8 Recession 2019Q4 2020Q2        2    10522   9531     990.9
# 9 Expansion 2020Q2   <NA>       NA     9531     NA        NA

# Amplitude Duration
# Exp=]T;P]     813.2       18
# Rec=]P;T]     456.1        5


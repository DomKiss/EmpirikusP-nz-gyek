library(quantmod)
library(dplyr)
library(ggplot2)
#adat letoltes yahoo-rol, es adj close kimentese
tesla_df <- getSymbols("TSLA", env=globalenv(), source="yahoo", auto.assign = FALSE, from='2016-09-06', to='2020-09-04')[,6]


#adat lementese (csak hogy reprodukalhato legyen)
write.csv(tesla_df, "C:/Users/Domonkos/Documents/EmpirikusP-nz-gyek/tesla_adj_close.csv")

#loghozamok kiszamitasa
tesla_df$log_returns <- diff(log(tesla_df$TSLA.Adjusted))
tesla_df <- na.omit(cbind(index(tesla_df), tesla_df %>% as_tibble))


names(tesla_df[1]) <- "Date"
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#elso momentum kiszamolasa
tesla_df$elso_momentum <- apply.fromstart(tesla_df$log_returns, "mean")

#masodik momentum kiszamolasa
tesla_df$masodik_momentum <- apply.fromstart(tesla_df$log_returns, "StdDev")

#harmadik momentum kiszamolasa
tesla_df$harmadik_momentum <- apply.fromstart(tesla_df$log_returns, "skewness")

#harmadik momentum kiszamolasa
tesla_df$negyedik_momentum <- apply.fromstart(tesla_df$log_returns, "kurtosis")





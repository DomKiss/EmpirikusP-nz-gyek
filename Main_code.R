library(quantmod)
library(dplyr)
#adat letoltes yahoo-rol, es adj close kimentese
tesla_df <- getSymbols("TSLA", env=globalenv(), source="yahoo", auto.assign = FALSE, from='2016-09-06', to='2020-09-04')[,6]

#adat lementese (csak hogy reprodukalhato legyen)
write.csv(tesla_df, "C:/Users/Domonkos/Documents/EmpirikusP-nz-gyek/tesla_adj_close.csv")
tesla_df$log_returns <- diff(log(tesla_df$TSLA.Adjusted))

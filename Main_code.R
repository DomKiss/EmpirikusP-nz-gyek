library(quantmod)
library(dplyr)
#adat letoltes yahoo-rol, es adj close kimentese
tesla_adj_price <- getSymbols("TSLA", env=globalenv(), source="yahoo", auto.assign = FALSE, from='2016-09-06', to='2020-09-04')[,6]

#adat lementese
write.csv(tesla_adj_price, "C:/Users/Domonkos/Documents/EmpirikusP-nz-gyek/tesla_adj_close.csv")


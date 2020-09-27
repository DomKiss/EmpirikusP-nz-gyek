library(quantmod)
library(dplyr)
library(ggplot2)
library(ggpubr)
#adat letoltes yahoo-rol, es adj close kimentese
tesla_df <-
  getSymbols(
    "TSLA",
    env = globalenv(),
    source = "yahoo",
    auto.assign = FALSE,
    from = '2016-09-06',
    to = '2020-09-04'
  )[, 6]

#adat lementese (csak hogy reprodukalhato legyen)
write.csv(tesla_df,
          "~/EmpirikusP-nz-gyek/tesla_adj_close.csv")

#loghozamok kiszamitasa
tesla_df$log_returns <- diff(log(tesla_df$TSLA.Adjusted))
tesla_df <- na.omit(cbind(index(tesla_df), tesla_df %>% as_tibble))
tesla_df$loss<-tesla_df$log_returns*(-1)

names(tesla_df)[1] <- "Date"
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

#elso momentum kiszamolasa
tesla_df$expected_value <-
  apply.fromstart(tesla_df$log_returns, "mean")

#masodik momentum kiszamolasa
tesla_df$standard_dev <-
  apply.fromstart(tesla_df$log_returns, "StdDev")

#harmadik momentum kiszamolasa
tesla_df$skewness <-
  apply.fromstart(tesla_df$log_returns, "skewness")

#negyedik momentum kiszamolasa
tesla_df$kurtosis <-
  apply.fromstart(tesla_df$log_returns, "kurtosis")


#normalitas vizsgalata
  #1. shapiro test szerint normalis
  shapiro.test(tesla_df$log_returns)
  
  #2. Anderson-Darling szerint is normalis
  #install.packages("nortest")
  nortest::ad.test(tesla_df$log_returns)
  
  #3. Jarque-Bera teszt szerint is normalis
  #install.packages("tsoutliers")
  tsoutliers::JarqueBera.test(tesla_df$log_returns)
 
#hozamok eloszlásának abrazolasa
ggplot(tesla_df, aes(x = log_returns)) + geom_histogram() + 
  labs(title ="Tesla hozamainak eloszlása", x = "Y", y = "Gyakoriság")

#hozamok ábrázolása időben
ggplot(tesla_df, aes(y = log_returns, x = Date))+
  geom_line()+labs(title="Tesla hozamai az időben", y="Y", x="T")

#varhato ertek abrazolasa
ggplot(tesla_df, aes(y = expected_value, x = Date))+
  geom_line()+labs(title="Tesla hozamainak várható értéke az időben", y="Mu", x="T")

#szoras abrazolasa
ggplot(tesla_df, aes(y = standard_dev, x = Date))+
  geom_line()+labs(title="Tesla hozamainak szórása az időben", y="StD", x="T")

#skewness abrazolasa
ggplot(tesla_df, aes(y = skewness, x = Date))+
  geom_line()+labs(title="Tesla hozamainak ferdesége az időben", y="S", x="T")


#kurtosis abrazolasa
ggplot(tesla_df, aes(y = kurtosis, x = Date))+
  geom_line()+labs(title="Tesla hozamainak csúcsossága az időben", y="K", x="T")

#normál q-q ábra
ggplot(mapping = aes(sample=tesla_df$log_returns))+
  geom_qq() + geom_qq_line(color=2)+labs(title="Normal Q-Q Plot")

#hatványkitevő becslése regresszíóval
N<-nrow(tesla_df)
n<-0
xi<-seq(from = 0.15,to=0.055,by=-0.005)

for(i in 1:nrow(reg_df)){
  for(j in 1:nrow(tesla_df)){
    if(tesla_df$loss[j]>xi[i]){
      n = n+1
      }
    }
  yi[i]=n/N
  n=0
  }

yi<-yi

reg_df<-data.frame(xi = xi, yi = yi, lnxi = log(xi), lnyi = log(yi))
atlagx<-mean(reg_df$lnxi)
atlagy<-mean(reg_df$lnyi)
reg_df$dlnxi2<-(reg_df$lnxi-atlagx)^2
reg_df$dlnxiyi<-(reg_df$lnxi-atlagx)*(reg_df$lnyi-atlagy)
dlnx2sum<-sum(reg_df$dlnxi2)
dlnxysum<-sum(reg_df$dlnxiyi)

Reg_alpha<--dlnxysum/dlnx2sum

#hatványkitevő becslése Hill módszerrel
NHill<-50
Hill_df<-data.frame(Sequence=index(tesla_df$loss[tesla_df$loss>0]),PosLoss=sort(tesla_df$loss[tesla_df$loss>0], decreasing=TRUE))
A<-Hill_df[NHill,2]
Hill_df<-cbind(Hill_df, lnxiA=log(Hill_df$PosLoss/A))

Hill_alpha<-NHill/sum(Hill_df$lnxiA[Hill_df$lnxiA>0])

#hatványkitevő ábrázolása Hill plottal
lnXi<-log(Hill_df$PosLoss)
lnXiN<-cumsum(lnXi)/Hill_df$Sequence
alphas<-1/(lnXiN-lnXi)
p=Hill_df$Sequence/nrow(tesla_df)
Hill_df<-cbind(Hill_df, lnXi, lnXiN, alphas, p)

ggplot(Hill_df[-1,], aes(y = alphas, x = p))+
  geom_line()+labs(title="Hill-plot", x="P", y="Alfa")


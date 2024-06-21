library(graphics)
library(RColorBrewer)
library(ggplot2)
library(tseries)
library(dplyr)
library(aod)
library(stargazer)
library(xtable)
library(stats)
library(zoo)
library(forecast)
library(rugarch)
library(xts)
library(highfrequency)






load("Data/stocks.RData")

#ADF test,Ljung-Box test 

stocks$ADFp_val = rep(NA, times = nrow(stocks))
stocks$LBp_val = rep(NA, times = nrow(stocks))

for(stockn in stocks$stockname){ 
  stocks[[which(stocks$stockname==stockn),"ADFp_val"]]= adf.test(allstocks[[stockn]]$ret, k = 4)$p.value 
  stocks[[which(stocks$stockname==stockn),"LBp_val"]] = Box.test(allstocks[[stockn]]$ret, type = 'Ljung-Box',lag = 5)$p.value 
}

plot(stocks$LBp_val)





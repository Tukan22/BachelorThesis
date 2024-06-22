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

for(filename in setdiff(list.files("Data"), list.dirs("Data", recursive = FALSE, full.names = FALSE))){
  load(paste("Data/", filename, sep = ""))
}

rm(list = c("filename"))

#ADF test,Ljung-Box test 
stocks$ADFp_val = rep(NA, times = nrow(stocks))
stocks$LBp_val = rep(NA, times = nrow(stocks))

for(stockn in stocks$stockname){ 
  stocks[[which(stocks$stockname==stockn),"ADFp_val"]]= adf.test(allstocks[[stockn]]$ret, k = 4)$p.value 
  stocks[[which(stocks$stockname==stockn),"LBp_val"]] = Box.test(allstocks[[stockn]]$ret, type = 'Ljung-Box',lag = 5)$p.value 
}

AR1_RV_fit = list() 
HAR_fit = list() 
HARAS_fit = list() 
HARS_fit = list() 
HARSK_fit = list() 
RGARCH_fit = list()
ARMAGARCH_fit = list() 

stocks$JBpval_AR1_RV_fit = rep(NA, times = nrow(stocks))
stocks$JBpval_HAR_fit = rep(NA, times = nrow(stocks))
stocks$JBpval_HARAS_fit = rep(NA, times = nrow(stocks))
stocks$JBpval_HARS_fit = rep(NA, times = nrow(stocks))
stocks$JBpval_HARSK_fit = rep(NA, times = nrow(stocks))
stocks$JBpval_RGARCH_fit =rep(NA, times = nrow(stocks))
stocks$JBpval_ARMAGARCH_fit =rep(NA, times = nrow(stocks))  

# All model fittings - runs approximately 15 minutes. 
start_time = Sys.time()
counter = 1 
for(stockn in stocks$stockname){
  print(paste(counter,": ", stockn, sep = ""))
  
  print("   AR-RV:")
  AR1_RV_fit[[stockn]] <- arima(allstocks[[stockn]]$RV, order = c(1, 0, 0))
  stocks[which(stocks$stockname ==stockn),"JBpval_AR1_RV_fit"]= jarque.bera.test(residuals(AR1_RV_fit[[stockn]]))$p.val
  
  print("   HAR:")
  HAR_fit[[stockn]] <- lm(HARmeasures[[stockn]]$RV_0 ~ HARmeasures[[stockn]]$RV_1 + HARmeasures[[stockn]]$RV_5+ HARmeasures[[stockn]]$RV_22)
  stocks[which(stocks$stockname ==stockn),"JBpval_HAR_fit"]= jarque.bera.test(residuals(HAR_fit[[stockn]]))$p.val
  
  print("   HAS_AS:")
  HARAS_fit[[stockn]]<-lm(HARmeasures[[stockn]]$RV_0 ~ HARmeasures[[stockn]]$RV_p + HARmeasures[[stockn]]$RV_n + HARmeasures[[stockn]]$RV_5+ HARmeasures[[stockn]]$RV_22)
  stocks[which(stocks$stockname ==stockn),"JBpval_HARAS_fit"]= jarque.bera.test(residuals(HARAS_fit[[stockn]]))$p.val
  
  print("   HARS:")
  HARS_fit[[stockn]] <-lm(HARmeasures[[stockn]]$RV_0 ~ HARmeasures[[stockn]]$RS + HARmeasures[[stockn]]$RV_1 + HARmeasures[[stockn]]$RV_5 + HARmeasures[[stockn]]$RV_22)
  stocks[which(stocks$stockname ==stockn),"JBpval_HARS_fit"]= jarque.bera.test(residuals(HARS_fit[[stockn]]))$p.val
  
  print("   HARSK:")
  HARSK_fit[[stockn]] <-lm(HARmeasures[[stockn]]$RV_0 ~ HARmeasures[[stockn]]$RS + HARmeasures[[stockn]]$RK + HARmeasures[[stockn]]$RV_1 + HARmeasures[[stockn]]$RV_5 + HARmeasures[[stockn]]$RV_22)
  stocks[which(stocks$stockname ==stockn),"JBpval_HARSK_fit"]= jarque.bera.test(residuals(HARSK_fit[[stockn]]))$p.val
  
  print("   ARMAGARCH:")
  ARMAGARCH <- ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean=TRUE), # TODO ARMA order 
                          variance.model = list(garchOrder = c(1, 1)), distribution.model = "std") # TODO GARCH order  
  ARMAGARCH_fit[[stockn]] <- ugarchfit(ARMAGARCH, allstocks[[stockn]]$ret, realizedVol = allstocks[[stockn]]$RV, solver ='hybrid')
  stocks[which(stocks$stockname ==stockn),"JBpval_ARMAGARCH_fit"] = jarque.bera.test(residuals(ARMAGARCH_fit[[stockn]]))$p.val 
  
  print("   RGARCH:")
  RGARCH<- ugarchspec(variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)), # TODO ARMA order, # TODO GARCH order 
                      mean.model = list(armaOrder=c(0, 0)),distribution.model = "std") # TODO distribution model 
  RGARCH_fit[[stockn]] <- ugarchfit(spec = RGARCH, data = allstocks[[stockn]]$ret, realizedVol= allstocks[[stockn]]$RV, solver ='hybrid')
  stocks[which(stocks$stockname ==stockn),"JBpval_RGARCH_fit"] = jarque.bera.test(residuals(RGARCH_fit[[stockn]]))$p.val 

  counter = counter + 1   
}

end_time = Sys.time()
print(end_time-start_time)

save(allstocks, file = "Data/allstocks.Rdata") 
save(AR1_RV_fit, file = "Data/AR1_RV_fit.Rdata") 
save(ARMAGARCH_fit, file = "Data/ARMAGARCH_fit.Rdata") 
save(HAR_fit, file = "Data/HAR_fit.Rdata") 
save(HARAS_fit, file = "Data/HARAS_fit.Rdata") 
save(HARmeasures, file = "Data/HARmeasures.Rdata") 
save(HARS_fit, file = "Data/HARS_fit.Rdata") 
save(HARS_fit, file = "Data/HARSK_fit.Rdata") 
save(RGARCH_fit, file = "Data/RGARCH_fit.Rdata") 
save(stocks, file = "Data/stocks.Rdata")
save(varstokeep, file = "Data/varstokeep.Rdata")  


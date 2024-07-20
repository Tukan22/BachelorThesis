AR1_RV_fit = list() 
HAR_fit = list() 
HARAS_fit = list() 
HARS_fit = list() 
HARSK_fit = list() 
RGARCH_fit = list()
ARMAGARCH_fit = list() 

ARMA_fit = list() 

RGARCH = list() 

HARmeasures = list() 

# Compute values for realized HAR models 
for(stockn in stocks$stockname){
  TT <- length(allstocks[[stockn]]$RV)
  RV_0 <- as.numeric(allstocks[[stockn]]$RV[23:TT])
  RV_1 <- as.numeric(allstocks[[stockn]]$RV[22:(TT - 1)])
  RV_5 <- unlist(lapply(lapply(1:(TT - 4), function (t) {return(RV_0[t:(t + 4)])}), mean))
  T_5 <- length(RV_5)
  RV_5 <- RV_5[18:(T_5 - 1)]
  RV_22 <- unlist(lapply(lapply(1:(TT - 21), function (t) {return(RV_0[t:(t + 21)])}), mean))
  T_22 <- length(RV_22)
  RV_22 <- RV_22[1:(T_22 - 1)]
  RV_n <- as.numeric(allstocks[[stockn]]$RSm[22:(TT - 1)])
  RV_p <- as.numeric(allstocks[[stockn]]$RSp[22:(TT - 1)])
  RK <- as.numeric(allstocks[[stockn]]$RKu[22:(TT - 1)])     # TODO: What is this?!? 
  RS <- as.numeric(allstocks[[stockn]]$RSk[22:(TT - 1)])     # TODO: What is this?!? 
  
  output = data.frame(RV_0, RV_1, RV_5, RV_22, RV_n, RV_p, RK, RS)  
  HARmeasures[[stockn]] = output 
}


# All model fittings - runs approximately 15 minutes. 
start_time = Sys.time()
counter = 1 
for(stockn in stocks$stockname){
  print(paste(counter,": ", stockn, sep = ""))
  
  AR1_RV_fit[[stockn]] <- arima(allstocks[[stockn]]$RV, order = c(1, 0, 0))
  HAR_fit[[stockn]] <- lm(HARmeasures[[stockn]]$RV_0 ~ HARmeasures[[stockn]]$RV_1 + HARmeasures[[stockn]]$RV_5+ HARmeasures[[stockn]]$RV_22)
  HARAS_fit[[stockn]]<-lm(HARmeasures[[stockn]]$RV_0 ~ HARmeasures[[stockn]]$RV_p + HARmeasures[[stockn]]$RV_n + HARmeasures[[stockn]]$RV_5+ HARmeasures[[stockn]]$RV_22)
  HARS_fit[[stockn]] <-lm(HARmeasures[[stockn]]$RV_0 ~ HARmeasures[[stockn]]$RS + HARmeasures[[stockn]]$RV_1 + HARmeasures[[stockn]]$RV_5 + HARmeasures[[stockn]]$RV_22)
  HARSK_fit[[stockn]] <-lm(HARmeasures[[stockn]]$RV_0 ~ HARmeasures[[stockn]]$RS + HARmeasures[[stockn]]$RK + HARmeasures[[stockn]]$RV_1 + HARmeasures[[stockn]]$RV_5 + HARmeasures[[stockn]]$RV_22)
  ARMAGARCH <- ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean=TRUE), # TODO ARMA order 
                          variance.model = list(garchOrder = c(1, 1)), distribution.model = "std") # TODO GARCH order  
  ARMAGARCH_fit[[stockn]] <- ugarchfit(ARMAGARCH, allstocks[[stockn]]$ret, realizedVol = allstocks[[stockn]]$RV, solver ='hybrid')
  RGARCH<- ugarchspec(variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)), # TODO ARMA order, # TODO GARCH order 
                      mean.model = list(armaOrder=c(1, 1)),distribution.model = "std") # TODO distribution model 
  RGARCH_fit[[stockn]] <- ugarchfit(spec = RGARCH, data = allstocks[[stockn]]$ret, realizedVol= allstocks[[stockn]]$RV, solver ='hybrid')
  
  counter = counter + 1   
  
  ARMA_fit[[stockn]] = arima(allstocks[[stockn]]$ret, order = c(1, 0, 1))
}
end_time = Sys.time()
print(end_time-start_time) 


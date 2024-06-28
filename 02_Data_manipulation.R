### Run Jarque-Bera test for both returns and model residuals and save p-values 

stocks$JBpval_AR1_RV_resid = rep(NA, times = nrow(stocks))
stocks$JBpval_HAR_resid = rep(NA, times = nrow(stocks))
stocks$JBpval_HAR_AS_resid = rep(NA, times = nrow(stocks))
stocks$JBpval_HAR_RS_resid = rep(NA, times = nrow(stocks))
stocks$JBpval_HAR_RSRK_resid = rep(NA, times = nrow(stocks))
stocks$JBpval_RGARCH_resid =rep(NA, times = nrow(stocks))
stocks$JBpval_ARMAGARCH_resid =rep(NA, times = nrow(stocks))  

stocks$JBpval_rets = rep(NA, times = nrow(stocks))


for(stockn in stocks$stockname){
  stocks[which(stocks$stockname ==stockn),"JBpval_AR1_RV_resid"]= jarque.bera.test(AR1_RV_fit[[stockn]]$residuals)$p.val
  stocks[which(stocks$stockname ==stockn),"JBpval_HAR_resid"]= jarque.bera.test(HAR_fit[[stockn]]$residuals)$p.val
  stocks[which(stocks$stockname ==stockn),"JBpval_HAR_AS_resid"]= jarque.bera.test(HARAS_fit[[stockn]]$residuals)$p.val
  stocks[which(stocks$stockname ==stockn),"JBpval_HAR_RS_resid"]= jarque.bera.test(HARS_fit[[stockn]]$residuals)$p.val
  stocks[which(stocks$stockname ==stockn),"JBpval_HAR_RSRK_resid"]= jarque.bera.test(HARSK_fit[[stockn]]$residuals)$p.val
  stocks[which(stocks$stockname ==stockn),"JBpval_ARMAGARCH_resid"] = jarque.bera.test(ARMAGARCH_fit[[stockn]]@fit$residuals)$p.val 
  stocks[which(stocks$stockname ==stockn),"JBpval_RGARCH_resid"] = jarque.bera.test(RGARCH_fit[[stockn]]@fit$residuals)$p.val 
  
  stocks[which(stocks$stockname ==stockn),"JBpval_rets"] = jarque.bera.test(allstocks[[stockn]]$ret)$p.val 
}

stocks[which(stocks$stockname ==stockn),"JBpval_HAR_RS_resid"]= jarque.bera.test(HARS_fit[[stockn]]$residuals)$p.val


### Run ADF test, Ljung-Box test on returns 

stocks$ADFp_val = rep(NA, times = nrow(stocks))
stocks$LBp_val = rep(NA, times = nrow(stocks))

for(stockn in stocks$stockname){ 
  stocks[[which(stocks$stockname==stockn),"ADFp_val"]]= adf.test(allstocks[[stockn]]$ret, k = 4)$p.value 
  stocks[[which(stocks$stockname==stockn),"LBp_val"]] = Box.test(allstocks[[stockn]]$ret, type = 'Ljung-Box',lag = 5)$p.value 
}


### Save start date, end date and number of observations for each stock 

stocks$start_date = rep(NA, times = nrow(stocks))
stocks$end_date = rep(NA, times = nrow(stocks))
stocks$obs = rep(NA, times = nrow(stocks))

for(stockn in stocks$stockname){
#  stocks$start_date = allstocks[[stock]][1,0]
#  stocks$end_date = allstocks[[stock]][1,0] 
#  print(stocks[which(stocks$stockname==stockn),] )  
#  print(stocks[[which(stocks$stockname==stockn),"start_date"]]) 
#  print(index(allstocks[[stockn]][1,0]))
  stocks[[which(stocks$stockname==stockn),"start_date"]] = index(allstocks[[stockn]][1,0])
  stocks[[which(stocks$stockname==stockn),"end_date"]] = index(allstocks[[stockn]][nrow(allstocks[[stockn]]),0]) 
  stocks[[which(stocks$stockname==stockn),"obs"]] = nrow(allstocks[[stockn]]) 
}

stocks$start_date = as.Date(stocks$start_date) 
stocks$end_date = as.Date(stocks$end_date) 

# Eliminate stocks which are starting only after the start of covid date 

stocks$before_covid = stocks$start_date <= pre_covid_end_date 
stocks_to_remove = c(which(stocks$before_covid == FALSE)) 
if(length(stocks_to_remove)==0 ) {stocks_to_remove = -seq(from = 1, to = nrow(stocks))}
allstocks = allstocks[-stocks_to_remove]
stocks = stocks[-stocks_to_remove,]


# Eliminate stocks which do not have enough observations before covid 

stocks$enough_pre_covid_obs = rep(NA, times = nrow(stocks))
stocks$pre_covid_obs = rep(NA, times = nrow(stocks)) 

for(stockn in stocks$stockname){
  stocks[which(stocks$stockname == stockn),]$pre_covid_obs = (sum(index(allstocks[[stockn]])<pre_covid_end_date)) 
  stocks[which(stocks$stockname == stockn),]$enough_pre_covid_obs = (sum(index(allstocks[[stockn]])<pre_covid_end_date)>minimum_length+n_for) 
}

stocks_to_remove = c(which(stocks$enough_pre_covid_obs == FALSE)) 
if(length(stocks_to_remove)==0 ) {stocks_to_remove = -seq(from = 1, to = nrow(stocks))}
allstocks = allstocks[-stocks_to_remove]
stocks = stocks[-stocks_to_remove,]


# Eliminate stocks which have a long break in the middle 

stocks$max_date_diff = rep(NA, times = nrow(stocks))

for(stockn in stocks$stockname){
  stocks$max_date_diff[which(stocks$stockname == stockn)] = max(index(allstocks[[stockn]])[2:length(index(allstocks[[stockn]]))] - index(allstocks[[stockn]])[1:length(index(allstocks[[stockn]]))-1] ) 
}

stocks$no_breaks = rep(NA, times = nrow(stocks)) 
stocks$no_breaks = stocks$max_date_diff < max_possible_date_diff

stocks_to_remove = c(which(stocks$no_breaks == FALSE)) 
if(length(stocks_to_remove)==0 ) {stocks_to_remove = -seq(from = 1, to = nrow(stocks))}
allstocks = allstocks[-stocks_to_remove]
stocks = stocks[-stocks_to_remove,]


# Compute width of forecasting window - from the beginning to start of covid 

stocks$w_l = rep(NA, times = nrow(stocks))
stocks$n_for = rep(NA, times = nrow(stocks)) 

for(stockn in stocks$stockname){
  stocks$w_l[which(stocks$stockname == stockn)] = which(index(allstocks[[stockn]]) == pre_covid_end_date) 
  stocks$n_for[which(stocks$stockname == stockn)] = 66 
}


# Calculate values to be used in HAR forecasts 

data_aux = list() 
TT = list() 
RV_5 = list() 
RV_22 = list() 
T_5 = list() 
T_22 = list() 
HAR_DATA = list() 

counter = 1 
for(stockn in stocks$stockname){
  print(counter)
  data_aux[[stockn]] <- allstocks[[stockn]][1:(stocks[which(stocks$stockname == stockn),"w_l"]+stocks[which(stocks$stockname == stockn),"n_for"]),]
  TT[[stockn]] <- nrow(data_aux[[stockn]])
  RV_5[[stockn]] <- unlist(lapply(lapply(1:(TT[[stockn]] - 4), function (x) {return(data_aux[[stockn]]$RV[x:(x + 4)])}), mean))
  RV_22[[stockn]] <- unlist(lapply(lapply(1:(TT[[stockn]] - 21), function (x) {return(data_aux[[stockn]]$RV[x:(x + 21)])}), mean))
  T_5[[stockn]] <- length(RV_5[[stockn]])
  T_22[[stockn]] <- length(RV_22[[stockn]])
  HAR_DATA[[stockn]] <- data.frame(data_aux[[stockn]]$RV[23:TT[[stockn]]],
                                   data_aux[[stockn]]$RSp[22:(TT[[stockn]] - 1)],
                                   data_aux[[stockn]]$RSm[22:(TT[[stockn]] - 1)],
                                   RV_5[[stockn]][18:(T_5[[stockn]] - 1)],
                                   RV_22[[stockn]][1:(T_22[[stockn]] - 1)],
                                   data_aux[[stockn]]$RKu[22:(TT[[stockn]] - 1)],
                                   data_aux[[stockn]]$RSk[22:(TT[[stockn]] - 1)])
  colnames(HAR_DATA[[stockn]]) <- c("RV","RV_p","RV_n","RV_5","RV_22", "RK", "RS") 
  
  counter = counter + 1   
}


# Remove LIN stockwhich  throws error in some procedures, do not know why. 

stocks_to_remove = c(which(stocks$stockname == "LIN"))  
if(length(stocks_to_remove)==0 ) {stocks_to_remove = -seq(from = 1, to = nrow(stocks))}

allstocks = allstocks[-stocks_to_remove]
stocks = stocks[-stocks_to_remove,]

t_df_names = c("AAL","AAPL","ABBV","ACN","ADBE","AMAT","AMD","AMGN","AMZN","ANET","AVGO","BA","BAC","BKNG","C","CAT","CMCSA","CMG","COP","COST","CRM","CSCO","CVS","CVX","DIS","EMR","FCX","FTNT","GE","GME","GOOG","GS","HD","HES","IBM","INTC","JNJ","JPM","KO","LRCX","MA","MCD","MCHP","MELI","META","MRK","MSFT","MSTR","MU","NEE","NFLX","NKE","NOW","NVDA","NXPI","ORCL","PANW","PEP","PFE","PG","PYPL","QCOM","SBUX","SHOP","SMCI","SO","SPGI","SYK","TJX","TMO","TMUS","TSN","TXN","UNH","V","VRTX","WDAY","WFC","WMT","XOM")
t_df_numbers = c(0.1,0.1,0.1,1.5,0.1,0.1,0.1,0.1,0.1,0.1,1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,1,0.1,1.5,0.1,0.1,0.1,3,0.1,0.1,0.1,0.1,0.1,5,0.1,0.1,0.1,1,0.1,1,1,0.1,0.1,0.1,1,1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,1,2.5,1.5,0.1,1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)

stocks$t_df_start = rep(NA, times = nrow(stocks))


for(stockn in stocks$stockname) {
  stocks$t_df_start[which(stocks$stockname == stockn)] = t_df_numbers[which(stocks$stockname == stockn)] 
}


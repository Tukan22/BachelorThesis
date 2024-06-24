# install.packages("arrow")
# install.packages("xts")
library(arrow)
library(xts)


#  varstokeep = c("allstocks", "HARmeasures","stocks", "varstokeep") 


#read all filenames 
stocks = as.data.frame(list.files("Data/parquet"))

#create stock names from file names 
names(stocks) = "filename"
stocks$stockname = substr(stocks$filename, 4, nchar(stocks$filename))
# head(stocks)

allstocks = list() 

for(i in seq(1,nrow(stocks))){
  # read file 
  allstocks[[stocks[i,"stockname"]]] =as.xts(read_parquet(paste("Data/parquet/", stocks[i,"filename"], sep=""))) 
  # print(sum(is.na(allstocks[[stocks[i,"stockname"]]])))
  # check if No Na's are present
  stocks$NoNas[i] = sum(is.na(allstocks[[stocks[i,"stockname"]]])) == 0
  # compute returns 
  allstocks[[stocks[i,"stockname"]]]$ret = diff(allstocks[[stocks[i,"stockname"]]][,"close_price"])/lag(allstocks[[stocks[i,"stockname"]]][,"close_price"]) 
  # reorganize  columns 
  allstocks[[stocks[i,"stockname"]]] = allstocks[[stocks[i,"stockname"]]][,c(7,2,3,4,5,6,1)] 
  allstocks[[stocks[i,"stockname"]]] = allstocks[[stocks[i,"stockname"]]][-1,]
}

head(allstocks[["AAL"]])
tail(allstocks[["AAL"]])

# individual data reading - not relevant anymore 
# AAL = read_parquet("Data/parquet/RM_AAL")
# AAL = as.xts(AAL)
# sum(is.na(AAL))
# AAL$ret = diff(AAL[,"close_price"])/lag(AAL[,"close_price"]) 
# 
# AAL = AAL[,c(7,2,3,4,5,6,1)]
# AAL = AAL[-1,]
# 
# head(AAL)
# tail(AAL)
# 
# the same for AAPL 
# AAPL = read_parquet("Data/parquet/RM_AAPL")
# AAPL = as.xts(AAPL)
# sum(is.na(AAPL))
# AAPL$ret = diff(AAPL[,"close_price"])/lag(AAPL[,"close_price"]) 
# 
# AAPL = AAPL[,c(7,2,3,4,5,6,1)]
# AAPL = AAPL[-1,]
# 
# head(AAPL)
# tail(AAPL)



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

minimum_length = 1000 # TODO check how many necessary 

pre_covid_end_date = as.Date("2019-11-29")




stocks$before_covid = stocks$start_date <= pre_covid_end_date 

stocks_to_remove = c(which(stocks$before_covid == FALSE)) 

allstocks = allstocks[-stocks_to_remove]
stocks = stocks[-stocks_to_remove,]

stocks$enough_pre_covid_obs = rep(NA, times = nrow(stocks))


for(stockn in stocks$stockname){
  stocks[which(stocks$stockname == stockn),]$enough_pre_covid_obs = (sum(index(allstocks[[stockn]])<pre_covid_end_date)>minimum_length+n_for) 
}

stocks_to_remove = c(which(stocks$enough_pre_covid_obs == FALSE)) 

allstocks = allstocks[-stocks_to_remove]
stocks = stocks[-stocks_to_remove,]

stocks$max_date_diff = rep(NA, times = nrow(stocks))

for(stockn in stocks$stockname){
  stocks$max_date_diff[which(stocks$stockname == stockn)] = max(index(allstocks[[stockn]])[2:length(index(allstocks[[stockn]]))] - index(allstocks[[stockn]])[1:length(index(allstocks[[stockn]]))-1] ) 
}

stocks$no_breaks = rep(NA, times = nrow(stocks)) 
stocks$no_breaks = stocks$max_date_diff < 21

stocks_to_remove = c(which(stocks$no_breaks == FALSE)) 

allstocks = allstocks[-stocks_to_remove]
stocks = stocks[-stocks_to_remove,]



HARmeasures = list() 

# Compute measures for realized HAR models 

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


for(stockn in stocks$stockname){
  print(stockn)
  #  stocks$w_l[which(stocks$stockname == stockn)] = round(stocks[which(stocks$stockname == stockn),"obs"]*1/2) 
  stocks$w_l[which(stocks$stockname == stockn)] = which(index(allstocks[[stockn]]) == pre_covid_end_date) 
  #  stocks$n_for[which(stocks$stockname == stockn)] = round(stocks[which(stocks$stockname == stockn),"obs"]*1/6)
  stocks$n_for[which(stocks$stockname == stockn)] = 66 
}


# rm(list=setdiff(ls(), varstokeep))

save(stocks, file = "Data/stocks.Rdata")  
save(allstocks, file = "Data/allstocks.Rdata")  
save(HARmeasures, file = "Data/HARmeasures.Rdata")





data_aux = list() 
TT = list() 
RV_5 = list() 
RV_22 = list() 
T_5 = list() 
T_22 = list() 
HAR_DATA = list() 


# Preparation for other HAR models 

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


save(data_aux, file = "Data/data_aux.Rdata") 
save(TT, file = "Data/TT.Rdata") 
save(RV_5, file = "Data/RV_5.Rdata") 
save(RV_22, file = "Data/RV_22.Rdata") 
save(T_5, file = "Data/T_5.Rdata") 
save(T_22, file = "Data/T_22.Rdata") 
save(HAR_DATA, file = "Data/HAR_DATA.Rdata") 




# Remove data where models did not produce reasonable results 

stocks$all_models_good = rep(TRUE, times = nrow(stocks))

for(stockn in stocks$stockname){
  if(
    (length(nrow(AR1_RV_fc_r[[stockn]]) == n_for) != 1) ||
    (length(nrow(AR1_RV_fc_e[[stockn]]) == n_for) != 1) ||
    (length(nrow(HAR_fc_r[[stockn]]) == n_for) != 1) ||
    (length(nrow(HAR_fc_e[[stockn]]) == n_for) != 1) ||
    (length(nrow(HAR_AS_fc_r[[stockn]]) == n_for) != 1) ||
    (length(nrow(HAR_AS_fc_e[[stockn]]) == n_for) != 1) ||
    (length(nrow(HAR_RS_fc_r[[stockn]]) == n_for) != 1) ||
    (length(nrow(HAR_RS_fc_e[[stockn]]) == n_for) != 1) ||
    (length(nrow(HAR_RSRK_fc_r[[stockn]]) == n_for) != 1) ||
    (length(nrow(HAR_RSRK_fc_e[[stockn]]) == n_for) != 1) ||
    (length(nrow(RGARCH_fc_r[[stockn]]) == n_for) != 1) ||
    (length(nrow(RGARCH_fc_e[[stockn]]) == n_for) != 1) ||
    (length(nrow(ARMAGARCH_fc_r[[stockn]]) == n_for) != 1) ||
    (length(nrow(ARMAGARCH_fc_e[[stockn]]) == n_for) != 1)
    ) {
    stocks[which(stocks$stockname == stockn),"all_models_good"] = FALSE 
  }
}

stocks_to_remove = which(stocks$all_models_good == FALSE)

allstocks = allstocks[-stocks_to_remove]
stocks = stocks[-stocks_to_remove,]




allstocks = allstocks[-which(stocks$stockname == "LIN")]
stocks = stocks[-which(stocks$stockname == "LIN"),]

nrow(stocks)

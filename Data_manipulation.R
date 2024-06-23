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

save.image("Data/stocks.RData")

save(stocks, file = "Data/stocks.Rdata")  
save(allstocks, file = "Data/allstocks.Rdata")  
save(HARmeasures, file = "Data/HARmeasures.Rdata")
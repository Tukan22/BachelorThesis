
##########################
######### AR1_RV #########
##########################

AR1_RV_fc_r = list() 
AR1_RV_fc_e = list() 

# Runs approximately 11 minutes 
start_time = Sys.time()
print(start_time)
counter = 1 

for(stockn in stocks$stockname){
# for(stockn in c("ANET")){
  w_l = stocks$w_l[which(stocks$stockname == stockn)] 
  n_for = stocks$n_for[which(stocks$stockname == stockn)] 
  
  print(paste(counter, ": ", stockn, sep = ""))
  print("   Rolling")

  # AR1-RV
  # rolling
  AR1_RV_fc_r[[stockn]]<-lapply(1:n_for, function(x) arima(allstocks[[stockn]]$RV[x:(w_l+x),],order=c(1,0,0))) 
  AR1_RV_fc_r[[stockn]]<-sapply(1:n_for, function(x) predict(AR1_RV_fc_r[[stockn]][[x]],n.ahead = 1)$pred)
  AR1_RV_fc_r[[stockn]]<-xts(AR1_RV_fc_r[[stockn]],order.by=index(allstocks[[stockn]][(w_l+2):(w_l+1+n_for),]))
  
  nrow(allstocks[[stockn]]) 
  
  print("   Expanding")
  # expanding
  AR1_RV_fc_e[[stockn]] <- lapply(1:n_for, function(x) arima(allstocks[[stockn]]$RV[1:(w_l+x),],order=c(1,0,0))) 
  AR1_RV_fc_e[[stockn]] <- sapply(1:n_for, function(x) predict(AR1_RV_fc_e[[stockn]][[x]],n.ahead = 1)$pred)
  AR1_RV_fc_e[[stockn]] <- xts(AR1_RV_fc_e[[stockn]],order.by=index(allstocks[[stockn]][(w_l+2):(w_l+1+n_for),]))
  
  counter = counter + 1 
}

AR1_RV_fc_r = lapply(AR1_RV_fc_r, sqrt)   # TODO TBC if correct 
AR1_RV_fc_e = lapply(AR1_RV_fc_e, sqrt)   # TODO TBC if correct 

AR1_RV_fc_r = lapply(AR1_RV_fc_r, function(x) {ifelse(x<0,0,x)})
AR1_RV_fc_e = lapply(AR1_RV_fc_e, function(x) {ifelse(x<0,0,x)})

save(AR1_RV_fc_r, file = "Data/AR1_RV_fc_r.Rdata")  
save(AR1_RV_fc_e, file = "Data/AR1_RV_fc_e.Rdata")

end_time = Sys.time()
print(end_time-start_time)



#######################
######### HAR #########
#######################

HAR_fc_r = list()
HAR_fc_e = list() 

# Runs approximately 12 minutes  
start_time = Sys.time()
print(start_time)  
counter = 1 

for(stockn in stocks$stockname){
  w_l = stocks$w_l[which(stocks$stockname == stockn)]    # TODO set better numbers 
  n_for = stocks$n_for[which(stocks$stockname == stockn)] # TODO set better numbers  
  
  print(paste(counter, ": ", stockn, sep = ""))
  print("   Rolling")
  
  # HAR 
  # rolling
  HAR_fc_r[[stockn]]<-lapply(1:n_for, function(x) HARmodel(data = allstocks[[stockn]]$RV[x:(w_l+x),] , periods = c(1,5,22),
                                                           type = "HAR", h = 1, transform = NULL, inputType = "RM"))
  HAR_fc_r[[stockn]]<-sapply(1:n_for, function (x) predict(HAR_fc_r[[stockn]][[x]]))
  HAR_fc_r[[stockn]]<-xts(HAR_fc_r[[stockn]],order.by = index(allstocks[[stockn]]$ret[(w_l+2):(w_l+1+n_for),]))
  
  print("   Expanding")
  # expanding
  HAR_fc_e[[stockn]] <- lapply(1:n_for, function(x) HARmodel(data = allstocks[[stockn]]$RV[1:(w_l+x),] , 
                                                             periods = c(1,5,22), RVest = c("rCov"),
                                                             type = "HAR", h = 1, transform = NULL,
                                                             inputType = "RM"))
  HAR_fc_e[[stockn]] <- sapply(1:n_for, function (x) predict(HAR_fc_e[[stockn]][[x]]))
  HAR_fc_e[[stockn]] <- xts(HAR_fc_e[[stockn]],order.by = index(allstocks[[stockn]][(w_l+2):(w_l+1+n_for),]))
  
  counter = counter + 1 
}
 
HAR_fc_r = lapply(HAR_fc_r, function(x) {ifelse(x<0,0,x)})
HAR_fc_e = lapply(HAR_fc_e, function(x) {ifelse(x<0,0,x)})

HAR_fc_r = lapply(HAR_fc_r, sqrt)
HAR_fc_e = lapply(HAR_fc_e, sqrt)

save(HAR_fc_r, file = "Data/HAR_fc_r.Rdata")  
save(HAR_fc_e, file = "Data/HAR_fc_e.Rdata")

end_time = Sys.time()
print(end_time-start_time)



##########################
######### HAR-AS #########
##########################

start_time = Sys.time()

HAR_AS_fc_r = list() 
HAR_AS_fc_e = list() 

# HAR-AS 

counter = 1 

for(stockn in stocks$stockname){
  print(paste(counter, ": ", stockn, sep = ""))
  print("   Rolling")
  
  n_for = stocks[which(stocks$stockname == stockn),"n_for"] 
  w_l = stocks[which(stocks$stockname == stockn),"w_l"]
  
  HAR_AS_fc_r[[stockn]] <- rep(NA, n_for)
  for (i in 0:(n_for - 1)) {
#  for (i in 1:(n_for)) {
    temp <- HAR_DATA[[stockn]][1+ i:(w_l + i - 22), ] %>% ts()
    fc_data <- HAR_DATA[[stockn]][w_l + i + 1 - 22, -1]
    model <- tslm(RV ~ RV_n + RV_p + RV_5 + RV_22, data = temp)
    HAR_AS_fc_r[[stockn]][i + 1] <- predict(model, newdata=fc_data)
  }
  
  HAR_AS_fc_r[[stockn]]<-xts(HAR_AS_fc_r[[stockn]],order.by = index(allstocks[[stockn]]$ret[(w_l+1):(w_l+n_for)]))

  print("   Expanding")
  
  HAR_AS_fc_e[[stockn]] <- rep(NA, n_for)
  for (i in 0:(n_for - 1)) {
#  for (i in 1:(n_for)) {
    temp <- HAR_DATA[[stockn]][1:(w_l + i - 22), ] %>% ts()
    fc_data <- HAR_DATA[[stockn]][w_l + i + 1 - 22, -1]
    model <- tslm(RV ~ RV_n + RV_p + RV_5 + RV_22, data = temp)
    HAR_AS_fc_e[[stockn]][i + 1] <- predict(model, newdata=fc_data)
  }
  
  HAR_AS_fc_e[[stockn]]<-xts(HAR_AS_fc_e[[stockn]],order.by = index(allstocks[[stockn]]$ret[(w_l+1):(w_l+n_for)]))
  
  counter = counter + 1 
}

HAR_AS_fc_r = lapply(HAR_AS_fc_r, function(x) {ifelse(x<0,0,x)})
HAR_AS_fc_e = lapply(HAR_AS_fc_e, function(x) {ifelse(x<0,0,x)})

HAR_AS_fc_r = lapply(HAR_AS_fc_r, sqrt)
HAR_AS_fc_e = lapply(HAR_AS_fc_e, sqrt)

save(HAR_AS_fc_r, file = "Data/HAR_AS_fc_r.Rdata")  
save(HAR_AS_fc_e, file = "Data/HAR_AS_fc_e.Rdata")  

end_time = Sys.time()
print(end_time-start_time)

##########################
######### HAR-RS #########
##########################

start_time = Sys.time()

HAR_RS_fc_r = list() 
HAR_RS_fc_e = list() 

#HAR-RS 

counter = 1 

for(stockn in stocks$stockname){
  print(paste(counter, ": ", stockn, sep = ""))
  print("   Rolling")
  
  n_for = stocks[which(stocks$stockname == stockn),"n_for"] 
  w_l = stocks[which(stocks$stockname == stockn),"w_l"]
  
  HAR_RS_fc_r[[stockn]] <- rep(NA, n_for)
  for (i in 0:(n_for - 1)) {
    temp <- HAR_DATA[[stockn]][1+ i:(w_l + i - 22), ] %>% ts()
    fc_data <- HAR_DATA[[stockn]][w_l + i + 1 - 22, -1]
    model <- tslm(RV ~ RS + RV_5 + RV_22, data = temp)
    HAR_RS_fc_r[[stockn]][i + 1] <- predict(model, newdata=fc_data)
  }
  
  HAR_RS_fc_r[[stockn]]<-xts(HAR_RS_fc_r[[stockn]],order.by = index(allstocks[[stockn]]$ret[(w_l+1):(w_l+n_for)]))

  print("   Expanding")
  
  HAR_RS_fc_e[[stockn]] <- rep(NA, n_for)
  for (i in 0:(n_for - 1)) {
    temp <- HAR_DATA[[stockn]][1:(w_l + i - 22), ] %>% ts()
    fc_data <- HAR_DATA[[stockn]][w_l + i + 1 - 22, -1]
    model <- tslm(RV ~ RS + RV_5 + RV_22, data = temp)
    HAR_RS_fc_e[[stockn]][i + 1] <- predict(model, newdata=fc_data)
  }
  
  HAR_RS_fc_e[[stockn]]<-xts(HAR_RS_fc_e[[stockn]],order.by = index(allstocks[[stockn]]$ret[(w_l+1):(w_l+n_for)]))
  counter = counter + 1 
}

HAR_RS_fc_r = lapply(HAR_RS_fc_r, function(x) {ifelse(x<0,0,x)})
HAR_RS_fc_e = lapply(HAR_RS_fc_e, function(x) {ifelse(x<0,0,x)})

HAR_RS_fc_r = lapply(HAR_RS_fc_r, sqrt)
HAR_RS_fc_e = lapply(HAR_RS_fc_e, sqrt)

save(HAR_RS_fc_r, file = "Data/HAR_RS_fc_r.Rdata")  
save(HAR_RS_fc_e, file = "Data/HAR_RS_fc_e.Rdata")  

end_time = Sys.time()
print(end_time-start_time)



############################
######### HAR-RSRK #########
############################

HAR_RSRK_fc_r = list() 
HAR_RSRK_fc_e  = list() 

# HAR-RS-RK 

counter = 1 

for(stockn in stocks$stockname){
  print(paste(counter, ": ", stockn, sep = ""))
  print("   Rolling")
  
  n_for = stocks[which(stocks$stockname == stockn),"n_for"] 
  w_l = stocks[which(stocks$stockname == stockn),"w_l"]
  
  HAR_RSRK_fc_r[[stockn]] <- rep(NA, n_for)
  for (i in 0:(n_for - 1)) {
    temp <- HAR_DATA[[stockn]][(1 + i):(w_l + i - 22), ] %>% ts()
    fc_data <- HAR_DATA[[stockn]][w_l + i + 1 - 22, -1]
    model <- tslm(RV ~ RS + RK + RV_5 + RV_22, data = temp)
    HAR_RSRK_fc_r[[stockn]][i + 1] <- predict(model, newdata=fc_data)
  }
  
  HAR_RSRK_fc_r[[stockn]] <- xts(HAR_RSRK_fc_r[[stockn]],order.by = index(allstocks[[stockn]]$ret[(w_l+1):(w_l+n_for)]))
  
  print("   Expanding")

    HAR_RSRK_fc_e[[stockn]] <- rep(NA, n_for)
  for (i in 0:(n_for - 1)) {
    temp <- HAR_DATA[[stockn]][1:(w_l + i - 22), ] %>% ts()
    fc_data <- HAR_DATA[[stockn]][w_l + i + 1 - 22, -1]
    model <- tslm(RV ~ RS + RK + RV_5 + RV_22, data = temp)
    HAR_RSRK_fc_e[[stockn]][i + 1] <- predict(model, newdata=fc_data)
  }
  
  HAR_RSRK_fc_e[[stockn]]<-xts(HAR_RSRK_fc_e[[stockn]], order.by = index(allstocks[[stockn]]$ret[(w_l+1):(w_l+n_for)]))
  
  counter = counter + 1 
}

HAR_RSRK_fc_r = lapply(HAR_RSRK_fc_r, function(x) {ifelse(x<0,0,x)})
HAR_RSRK_fc_e = lapply(HAR_RSRK_fc_r, function(x) {ifelse(x<0,0,x)})

HAR_RSRK_fc_r = lapply(HAR_RSRK_fc_r, sqrt)
HAR_RSRK_fc_e = lapply(HAR_RSRK_fc_e, sqrt)

save(HAR_RSRK_fc_r, file = "Data/HAR_RSRK_fc_r.Rdata")  
save(HAR_RSRK_fc_e, file = "Data/HAR_RSRK_fc_e.Rdata") 



##############################
######### ARMA-GARCH #########
##############################

ARMAGARCH_fc_r_model = list()  
ARMAGARCH_fc_e_model = list()  

ARMAGARCH_fc_r = list()  
ARMAGARCH_fc_e = list()  

# Runs approximately 44 minutes

start_time = Sys.time()
print(start_time)

counter = 1 

for(stockn in stocks$stockname){
  w_l = stocks$w_l[which(stocks$stockname == stockn)]    # TODO set better numbers 
  n_for = stocks$n_for[which(stocks$stockname == stockn)] # TODO set better numbers  
  
  print(paste("ARMA-GARCH ",counter, ": ", stockn, sep = ""))
  print("   Rolling")
  
  # ARMA-GARCH  
  # rolling
  
  ARMAGARCH_fc_r_model[[stockn]] = ugarchroll(ARMAGARCH, 100*allstocks[[stockn]]$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                                             n.start = NULL, refit.every = 21, refit.window = c("moving"), 
                                             window.size = w_l, solver = "hybrid", fit.control = list(), 
                                             solver.control = list(), calculate.VaR = FALSE, 
                                             keep.coef = TRUE) 
  
  ARMAGARCH_fc_r[[stockn]] <- xts(ARMAGARCH_fc_r_model[[stockn]]@forecast[["density"]]$Sigma,
                                  order.by = as.Date(rownames(ARMAGARCH_fc_r_model[[stockn]]@forecast[["density"]])))/100
  
  # expanding
  print("   Expanding") 

  ARMAGARCH_fc_e_model[[stockn]] = ugarchroll(ARMAGARCH, 100*allstocks[[stockn]]$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                                              n.start = NULL, refit.every = 21, refit.window = c("recursive"), 
                                              window.size = w_l, solver = "hybrid", fit.control = list(), 
                                              solver.control = list(), calculate.VaR = FALSE, 
                                              keep.coef = TRUE) 
  
    ARMAGARCH_fc_e[[stockn]]<- xts(ARMAGARCH_fc_e_model[[stockn]]@forecast[["density"]]$Sigma,
                      order.by = as.Date(rownames(ARMAGARCH_fc_e_model[[stockn]]@forecast[["density"]])))/100

  print(paste("Estimated termination: ",start_time + (Sys.time()  -  start_time)/counter*nrow(stocks))) 
  counter = counter + 1
}


save(ARMAGARCH_fc_r, file = "Data/ARMAGARCH_fc_r.Rdata")  
save(ARMAGARCH_fc_e, file = "Data/ARMAGARCH_fc_e.Rdata")

end_time = Sys.time()
print(end_time-start_time)



###########################
######### R-GARCH #########  
###########################

RGARCH_fc_r = list()  
RGARCH_fc_e = list()  

# Runs approximately 7,5 hours  
start_time2 = Sys.time()
print(start_time2) 
counter = 1 

# for(stockn in c("AAL")){
for(stockn in stocks$stockname){
  w_l = stocks$w_l[which(stocks$stockname == stockn)]    # TODO set better numbers 
  n_for = stocks$n_for[which(stocks$stockname == stockn)] # TODO set better numbers  
  
  print(paste("RGARCH ",counter, ": ", stockn, sep = ""))
  print("   Rolling")
  
  # RGARCH  
  # rolling
  RGARCH_fc_r[[stockn]] <- ugarchroll(RGARCH, 100*allstocks[[stockn]]$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                                      n.start = NULL, refit.every = 21, refit.window = c("moving"), window.size = w_l,
                                      solver = "hybrid", calculate.VaR = FALSE,keep.coef = TRUE,realizedVol = (100*sqrt(allstocks[[stockn]]$RV[1:(w_l+n_for+1),])))
  RGARCH_fc_r[[stockn]] <- xts(RGARCH_fc_r[[stockn]]@forecast[["density"]]$Sigma,
                               order.by = as.Date(rownames(RGARCH_fc_r[[stockn]]@forecast[["density"]])))/100
  
  # expanding
  print("   Expanding")
  RGARCH_fc_e[[stockn]] <- ugarchroll(RGARCH, 100*allstocks[[stockn]]$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                                      n.start = NULL, refit.every = 21, refit.window = c("recursive"), 
                                      window.size = w_l, solver = "hybrid", calculate.VaR = FALSE, 
                                      keep.coef = TRUE,realizedVol = 100*sqrt((allstocks[[stockn]]$RV[1:(w_l+n_for+1),])))
  RGARCH_fc_e[[stockn]]<- xts(RGARCH_fc_e[[stockn]]@forecast[["density"]]$Sigma,
                              order.by = as.Date(rownames(RGARCH_fc_e[[stockn]]@forecast[["density"]])))/100

  print(paste("Estimated termination: ",start_time2 + (Sys.time()  -  start_time2)/counter*nrow(stocks))) 
  counter = counter + 1 
}
    
save(RGARCH_fc_r, file = "Data/RGARCH_fc_r.Rdata") 
save(RGARCH_fc_e, file = "Data/RGARCH_fc_e.Rdata") 

end_time2 = Sys.time()
print(end_time2-start_time2)



# Check where all forecasts produced reasonable results
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

# Remove data where models could not be estimated 
stocks_to_remove = which(stocks$all_models_good == FALSE)
if(length(stocks_to_remove)==0 ) {stocks_to_remove = -seq(from = 1, to = nrow(stocks))}
allstocks = allstocks[-stocks_to_remove]
stocks = stocks[-stocks_to_remove,]

# Correct data for identical dates 
for(stockn in stocks$stockname){
  print(stockn)
  n_for = stocks$n_for[which(stocks$stockname == stockn)]  
  print(n_for)  

  AR1_RV_fc_e[[stockn]] = AR1_RV_fc_e[[stockn]][-n_for]
  AR1_RV_fc_r[[stockn]] = AR1_RV_fc_r[[stockn]][-n_for]
  HAR_fc_e[[stockn]] = HAR_fc_e[[stockn]][-n_for]
  HAR_fc_r[[stockn]] = HAR_fc_r[[stockn]][-n_for]
  HAR_AS_fc_e[[stockn]] = HAR_AS_fc_e[[stockn]][-1]
  HAR_AS_fc_r[[stockn]] = HAR_AS_fc_r[[stockn]][-1]
  HAR_RS_fc_e[[stockn]] = HAR_RS_fc_e[[stockn]][-1]
  HAR_RS_fc_r[[stockn]] = HAR_RS_fc_r[[stockn]][-1]
  HAR_RSRK_fc_e[[stockn]] = HAR_RSRK_fc_e[[stockn]][-1]
  HAR_RSRK_fc_r[[stockn]] = HAR_RSRK_fc_r[[stockn]][-1]
  RGARCH_fc_e[[stockn]] = RGARCH_fc_e[[stockn]][-n_for]
  RGARCH_fc_r[[stockn]] = RGARCH_fc_r[[stockn]][-n_for]
  ARMAGARCH_fc_e[[stockn]] = ARMAGARCH_fc_e[[stockn]][-n_for]
  ARMAGARCH_fc_r[[stockn]] = ARMAGARCH_fc_r[[stockn]][-n_for]
}

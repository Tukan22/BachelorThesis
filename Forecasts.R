for(filename in setdiff(list.files("Data"), list.dirs("Data", recursive = FALSE, full.names = FALSE))){
  load(paste("Data/", filename, sep = ""))
}


stocks$w_l = rep(NA, times = nrow(stocks))
stocks$n_for = rep(NA, times = nrow(stocks)) 

AR1_RV_fc_r = list()
AR1_RV_fc_e = list() 
HAR_fc_r = list()
HAR_fc_e = list() 
ARMAGARCH_fc_r = list()  
ARMAGARCH_fc_e = list()  
RGARCH_fc_r = list()  
RGARCH_fc_e = list()  

stockn = "XOM" 

for(stockn in stocks$stockname){
  stocks$w_l[which(stocks$stockname == stockn)] = round(stocks[which(stocks$stockname == stockn),"obs"]*1/2) 
  stocks$n_for[which(stocks$stockname == stockn)] = round(stocks[which(stocks$stockname == stockn),"obs"]*1/6)
}

# Runs approximately 45 minutes 
start_time = Sys.time()
counter = 1 

for(stockn in stocks$stockname){
  w_l = stocks$w_l[which(stocks$stockname == stockn)]    # TODO set better numbers 
  n_for = stocks$n_for[which(stocks$stockname == stockn)] # TODO set better numbers  
  
  print(paste(counter, ": ", stockn, sep = ""))
  print("   Rolling")

  # AR1-RV
  # rolling
  AR1_RV_fc_r[[stockn]]<-lapply(1:n_for, function(x) arima(allstocks[[stockn]]$RV[x:(w_l+x),],order=c(1,0,0)))   # TODO arima order 
  AR1_RV_fc_r[[stockn]]<-sapply(1:n_for, function(x) predict(AR1_RV_fc_r[[stockn]][[x]],n.ahead = 1)$pred)
  AR1_RV_fc_r[[stockn]]<-xts(AR1_RV_fc_r[[stockn]],order.by=index(allstocks[[stockn]][(w_l+2):(w_l+1+n_for),]))
  
  nrow(allstocks[[stockn]]) 
  
  print("   Expanding")
  # expanding
  AR1_RV_fc_e[[stockn]] <- lapply(1:n_for, function(x) arima(allstocks[[stockn]]$RV[1:(w_l+x),],order=c(1,0,0))) # TODO arima order 
  AR1_RV_fc_e[[stockn]] <- sapply(1:n_for, function(x) predict(AR1_RV_fc_e[[stockn]][[x]],n.ahead = 1)$pred)
  AR1_RV_fc_e[[stockn]] <- xts(AR1_RV_fc_e[[stockn]],order.by=index(allstocks[[stockn]][(w_l+2):(w_l+1+n_for),]))
  
  counter = counter + 1 
}

save(AR1_RV_fc_r, file = "Data/AR1_RV_fc_r.Rdata")  
save(AR1_RV_fc_e, file = "Data/AR1_RV_fc_e.Rdata")

end_time = Sys.time()
print(end_time-start_time)





# Runs almost 2 hours 
start_time = Sys.time()
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

save(HAR_fc_r, file = "Data/HAR_fc_r.Rdata")  
save(HAR_fc_e, file = "Data/HAR_fc_e.Rdata")

end_time = Sys.time()
print(end_time-start_time)








# Runs TODO 
start_time = Sys.time()
counter = 1 

for(stockn in stocks$stockname){
  w_l = stocks$w_l[which(stocks$stockname == stockn)]    # TODO set better numbers 
  n_for = stocks$n_for[which(stocks$stockname == stockn)] # TODO set better numbers  
  
  print(paste("ARMA-GARCH ",counter, ": ", stockn, sep = ""))
  print("   Rolling")
  
  # ARMA-GARCH  
  # rolling
  ARMAGARCH_fc_r[[stockn]] <- ugarchroll(ARMAGARCH, 100*allstocks[[stockn]]$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                                         n.start = NULL, refit.every = 1, refit.window = c("moving"), 
                                         window.size = w_l, solver = "hybrid", fit.control = list(), 
                                         solver.control = list(), calculate.VaR = FALSE, 
                                         keep.coef = TRUE,realizedVol = 100*(allstocks[[stockn]]$RV[1:(w_l+n_for+1),]))   # TODO: why is RV here? 
  ARMAGARCH_fc_r[[stockn]] <- xts(ARMAGARCH_fc_r[[stockn]]@forecast[["density"]]$Sigma,
                                  order.by = as.Date(rownames(ARMAGARCH_fc_r[[stockn]]@forecast[["density"]])))/100
  
  # expanding
  ARMAGARCH_fc_e[[stockn]] <- ugarchroll(ARMAGARCH, 100*allstocks[[stockn]]$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                                         n.start = NULL, refit.every = 1, refit.window = c("recursive"), 
                                         window.size = w_l, solver = "hybrid", fit.control = list(), 
                                         solver.control = list(), calculate.VaR = FALSE, 
                                         keep.coef = TRUE,realizedVol = 100*(allstocks[[stockn]]$RV[1:(w_l+n_for+1),]))    # TODO: why is RV here? 
  ARMAGARCH_fc_e<- xts(ARMAGARCH_fc_e[[stockn]]@forecast[["density"]]$Sigma,
                       order.by = as.Date(rownames(ARMAGARCH_fc_e[[stockn]]@forecast[["density"]])))/100
  
  counter = counter + 1 
}

save(ARMAGARCH_fc_r, file = "Data/ARMAGARCH_fc_r.Rdata")  
save(ARMAGARCH_fc_e, file = "Data/ARMAGARCH_fc_e.Rdata")

end_time = Sys.time()
print(end_time-start_time)


# Runs TODO 
start_time2 = Sys.time()
counter = 1 

for(stockn in stocks$stockname){
  w_l = stocks$w_l[which(stocks$stockname == stockn)]    # TODO set better numbers 
  n_for = stocks$n_for[which(stocks$stockname == stockn)] # TODO set better numbers  
  
  print(paste("RGARCH ",counter, ": ", stockn, sep = ""))
  print("   Rolling")
  
  # RGARCH  
  # rolling
  RGARCH_fc_r[[stockn]] <- ugarchroll(RGARCH, 100*allstocks[[stockn]]$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                                      n.start = NULL, refit.every = 1, refit.window = c("moving"), window.size = w_l,
                                      solver = "hybrid", calculate.VaR = FALSE,keep.coef = TRUE,realizedVol = (100*allstocks[[stockn]]$RV[1:(w_l+n_for+1),]))
  RGARCH_fc_r[[stockn]] <- xts(RGARCH_fc_r[[stockn]]@forecast[["density"]]$Sigma,
                               order.by = as.Date(rownames(RGARCH_fc_r[[stockn]]@forecast[["density"]])))/100
  
  # expanding
  RGARCH_fc_e[[stockn]] <- ugarchroll(RGARCH, 100*allstocks[[stockn]]$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                                      n.start = NULL, refit.every = 1, refit.window = c("recursive"), 
                                      window.size = w_l, solver = "hybrid", calculate.VaR = FALSE, 
                                      keep.coef = TRUE,realizedVol = 100*((allstocks[[stockn]]$RV[1:(w_l+n_for+1),])))
  RGARCH_fc_e[[stockn]]<- xts(RGARCH_fc_e[[stockn]]@forecast[["density"]]$Sigma,
                              order.by = as.Date(rownames(RGARCH_fc_e[[stockn]]@forecast[["density"]])))/100

  counter = counter + 1 
}

save(RGARCH_fc_r, file = "Data/RGARCH_fc_r.Rdata")  
save(RGARCH_fc_e, file = "Data/RGARCH_fc_e.Rdata")

end_time2 = Sys.time()
print(end_time2-start_time2)

print("ARMAGARCH running time: ") 
print(start_time - end_time)

print("RGARCH running time: ") 
print(start_time2 - end_time2)
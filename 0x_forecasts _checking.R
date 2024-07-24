stockn = "AAPL"

w_l = stocks$w_l[which(stocks$stockname == stockn)]    # TODO set better numbers 
n_for = stocks$n_for[which(stocks$stockname == stockn)] # TODO set better numbers  

trygarchvar <- ugarchroll(ARMAGARCH, 100*allstocks[[stockn]]$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                                       n.start = NULL, refit.every = 21, refit.window = c("moving"), 
                                       window.size = w_l, solver = "hybrid", fit.control = list(), 
                                       solver.control = list(), calculate.VaR = TRUE, vaR.alpha=c(0.1), 
                                       keep.coef = TRUE,realizedVol = 100*(allstocks[[stockn]]$RV[1:(w_l+n_for+1),]))   # TODO: why is RV here? 

trygarchvarwRV = ugarchroll(ARMAGARCH, 100*allstocks[[stockn]]$ret[1:(w_l+1),], n.ahead = 1, forecast.length = n_for, 
                            n.start = NULL, refit.every = 21, refit.window = c("moving"), 
                            window.size = w_l, solver = "hybrid", fit.control = list(), 
                            solver.control = list(), calculate.VaR = TRUE, vaR.alpha=c(0.1), 
                            keep.coef = TRUE)   # TODO: why is RV here? 

head(trygarchvarwRV@forecast$VaR$realized)
head(trygarchvar@forecast$VaR$realized)

trygarchvar@forecast 

AR1_RV_fc_r_something=list() 

AR1_RV_fc_r_something[[stockn]]<-lapply(1:n_for, function(x) arima(allstocks[[stockn]]$ret[x:(w_l+x),],order=c(1,0,1)))   # TODO arima order 
AR1_RV_fc_r_something[[stockn]]<-sapply(1:n_for, function(x) predict(AR1_RV_fc_r_something[[stockn]][[x]],n.ahead = 1)$pred)
AR1_RV_fc_r_something[[stockn]]<-xts(AR1_RV_fc_r_something[[stockn]],order.by=index(allstocks[[stockn]][(w_l+2):(w_l+1+n_for),]))

trygarchvarshort  = trygarchvar

VaR=trygarchvarwRV@forecast$VaR[,"alpha(5%)"]
return=trygarchvarwRV@forecast$VaR[,"realized"]
sum(VaR>return)/n_for 

tail(trygarchvar@forecast$density, 4)  
tail(trygarchvarshort@forecast$density,  3)  

plot(trygarchvar@forecast$VaR[,2], type = 'l') 

lines(VaR95_AR1_RV_r[[stockn]]) 

allstocks[[stockn]]$ret<trygarchvar@forecast$VaR[,2]




fit.t = fitdistr(
  x = x*1000, 
  densfun = "t", 
  start = list(m=mean(x),s=sd(x), df=stocks$t_df_start[which(stocks$stockname == stockn)]), 
  lower=c(-1, 0.001,0.01))$estimate/c(1000,1000,1) 
fit.df = fit.t[3] 

tquantile = qt(p = 0.1, df = fit.df)

VaR95_AR1_RV_r[[stockn]] =    as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(AR1_RV_fc_r[[stockn]]))  

table(trygarchvar@forecast$VaR["realized"]<trygarchvar@forecast$VaR$`alpha(5%)`)  

plot(allstocks[[stockn]]$ret, type = 'l')

boxplot(trygarchvar@forecast$VaR["realized"])

as.numeric(trygarchvar@forecast$VaR["realized"])

plot(trygarchvar@forecast$VaR$realized, type = 'l')  
lines()

allstocks[[stockn]]$ret[1:(w_l+n_for+1),]


allstocks[["AAPL"]][w_l]

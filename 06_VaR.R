
for(stockn in stocks$stockname){
  print(stockn)
  dist = fitdist(data = as.numeric(AR1_RV_fc_e[[stockn]])*1000, distr = "norm")["estimate"] # TODO here I am assuming normal distribution - jarque bera test! 
  estimates = dist$estimate/1000 
  AR1_RV_fc_e[[stockn]]
}

VaR95_AR1_RV_e = list() 
VaR95_AR1_RV_r = list() 
VaR95_HAR_e = list() 
VaR95_HAR_r = list() 
VaR95_HAR_AS_e = list() 
VaR95_HAR_AS_r = list() 
VaR95_HAR_RS_e = list() 
VaR95_HAR_RS_r = list() 
VaR95_HAR_RSRK_e = list() 
VaR95_HAR_RSRK_r = list() 
VaR95_RGARCH_e = list() 
VaR95_RGARCH_r = list() 
VaR95_ARMAGARCH_e = list() 
VaR95_ARMAGARCH_r = list() 

Backtests_AR1_RV_e = list() 
Backtests_AR1_RV_r = list() 
Backtests_HAR_e = list() 
Backtests_HAR_r = list() 
Backtests_HAR_AS_e = list() 
Backtests_HAR_AS_r = list() 
Backtests_HAR_RS_e = list() 
Backtests_HAR_RS_r = list() 
Backtests_HAR_RSRK_e = list() 
Backtests_HAR_RSRK_r = list() 
Backtests_RGARCH_e = list() 
Backtests_RGARCH_r = list() 
Backtests_ARMAGARCH_e = list() 
Backtests_ARMAGARCH_r = list() 

stockn = "AAL"
errorstocks = c("hi")

Backtests = list() 

# Runs approximately 30 seconds 
start_time = Sys.time()

counter = 1 
for(stockn in stocks$stockname){   
  print(paste(counter, " - ", stockn ,sep="")) 
  
  retstouse = allstocks[[stockn]]$ret[
    min(which(index(allstocks[[stockn]])>=as.Date(forecast_start_date))+1):
      min(which(index(allstocks[[stockn]])>=(as.Date(forecast_start_date)))+n_for)] 
  
  x=as.xts(allstocks[[stockn]]$ret)     
    fit.t = fitdistr(
      x = x*1000, 
      densfun = "t", 
      start = list(m=mean(x),s=sd(x), df=stocks$t_df_start[which(stocks$stockname == stockn)]), 
      lower=c(-1, 0.001,0.01))$estimate/c(1000,1000,1) 
    fit.df = fit.t[3] 

    # Check how the VaR should be calculated      
    VaR95_AR1_RV_e[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (AR1_RV_fc_e[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_AR1_RV_r[[stockn]] =as.xts(mean(allstocks[[stockn]]$ret) + (AR1_RV_fc_r[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_HAR_e[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (HAR_fc_e[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_HAR_r[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (HAR_fc_r[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_HAR_AS_e[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (HAR_AS_fc_e[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_HAR_AS_r[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (HAR_AS_fc_r[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_HAR_RS_e[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (HAR_RS_fc_e[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_HAR_RS_r[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (HAR_RS_fc_r[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_HAR_RSRK_e[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (HAR_RSRK_fc_e[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_HAR_RSRK_r[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (HAR_RSRK_fc_r[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_RGARCH_e[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (RGARCH_fc_e[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_RGARCH_r[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (RGARCH_fc_r[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_ARMAGARCH_e[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (ARMAGARCH_fc_e[[stockn]])*qt(p = 0.05, df = fit.df))  
    VaR95_ARMAGARCH_r[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + (ARMAGARCH_fc_r[[stockn]])*qt(p = 0.05, df = fit.df))   

  Backtests_AR1_RV_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_AR1_RV_e[[stockn]], alpha = 0.05)
  Backtests_AR1_RV_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_AR1_RV_r[[stockn]], alpha = 0.05)
  Backtests_HAR_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_e[[stockn]], alpha = 0.05)
  Backtests_HAR_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_r[[stockn]], alpha = 0.05)
  Backtests_HAR_AS_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_AS_e[[stockn]], alpha = 0.05)
  Backtests_HAR_AS_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_AS_r[[stockn]], alpha = 0.05)
  Backtests_HAR_RS_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_RS_e[[stockn]], alpha = 0.05)
  Backtests_HAR_RS_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_RS_r[[stockn]], alpha = 0.05)
  Backtests_HAR_RSRK_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_RSRK_e[[stockn]], alpha = 0.05)
  Backtests_HAR_RSRK_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_RSRK_r[[stockn]], alpha = 0.05)
  Backtests_RGARCH_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_RGARCH_e[[stockn]], alpha = 0.05)
  Backtests_RGARCH_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_RGARCH_r[[stockn]], alpha = 0.05)
  Backtests_ARMAGARCH_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_ARMAGARCH_e[[stockn]], alpha = 0.05)
  Backtests_ARMAGARCH_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_ARMAGARCH_r[[stockn]], alpha = 0.05)
  
  counter = counter + 1 
}

end_time = Sys.time()
print(end_time-start_time)

unname(Backtests_AR1_RV_e[[stockn]]$LRuc[2]) 
unname(Backtests_AR1_RV_r[[stockn]]$LRuc[2]) 
unname(Backtests_HAR_e[[stockn]]$LRuc[2])  
unname(Backtests_HAR_r[[stockn]]$LRuc[2])  
unname(Backtests_HAR_AS_e[[stockn]]$LRuc[2])  
unname(Backtests_HAR_AS_r[[stockn]]$LRuc[2])   
unname(Backtests_HAR_RS_e[[stockn]]$LRuc[2])   
unname(Backtests_HAR_RS_r[[stockn]]$LRuc[2])   
unname(Backtests_HAR_RSRK_e[[stockn]]$LRuc[2])   
unname(Backtests_HAR_RSRK_r[[stockn]]$LRuc[2])   
unname(Backtests_RGARCH_e[[stockn]]$LRuc[2])   
unname(Backtests_RGARCH_r[[stockn]]$LRuc[2])   
unname(Backtests_ARMAGARCH_e[[stockn]]$LRuc[2])   
unname(Backtests_ARMAGARCH_r[[stockn]]$LRuc[2])   

sum(retstouse < VaR95_AR1_RV_e[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_AR1_RV_r[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_HAR_e[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_HAR_r[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_HAR_AS_e[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_HAR_AS_r[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_HAR_RS_e[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_HAR_RS_r[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_HAR_RSRK_e[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_HAR_RSRK_r[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_RGARCH_e[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_RGARCH_r[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_ARMAGARCH_e[[stockn]])/nrow(retstouse)
sum(retstouse < VaR95_ARMAGARCH_r[[stockn]])/nrow(retstouse)



save(VaR95_AR1_RV_e, file = "Data/VaR95_AR1_RV_e.Rdata")
save(VaR95_AR1_RV_r, file = "Data/VaR95_AR1_RV_r.Rdata")
save(VaR95_HAR_e, file = "Data/VaR95_HAR_e.Rdata")
save(VaR95_HAR_r, file = "Data/VaR95_HAR_r.Rdata")
save(VaR95_HAR_AS_e, file = "Data/VaR95_HAR_AS_e.Rdata")
save(VaR95_HAR_AS_r, file = "Data/VaR95_HAR_AS_r.Rdata")
save(VaR95_HAR_RS_e, file = "Data/VaR95_HAR_RS_e.Rdata")
save(VaR95_HAR_RS_r, file = "Data/VaR95_HAR_RS_r.Rdata")
save(VaR95_HAR_RSRK_e, file = "Data/VaR95_HAR_RSRK_e.Rdata")
save(VaR95_HAR_RSRK_r, file = "Data/VaR95_HAR_RSRK_r.Rdata")
save(VaR95_RGARCH_e, file = "Data/VaR95_RGARCH_e.Rdata")
save(VaR95_RGARCH_r, file = "Data/VaR95_RGARCH_r.Rdata")
save(VaR95_ARMAGARCH_e, file = "Data/VaR95_ARMAGARCH_e.Rdata")
save(VaR95_ARMAGARCH_r, file = "Data/VaR95_ARMAGARCH_r.Rdata")

save(Backtests_AR1_RV_e, file = "Data/Backtests_AR1_RV_e.Rdata")
save(Backtests_AR1_RV_r, file = "Data/Backtests_AR1_RV_r.Rdata")
save(Backtests_HAR_e, file = "Data/Backtests_HAR_e.Rdata")
save(Backtests_HAR_r, file = "Data/Backtests_HAR_r.Rdata")
save(Backtests_HAR_AS_e, file = "Data/Backtests_HAR_AS_e.Rdata")
save(Backtests_HAR_AS_r, file = "Data/Backtests_HAR_AS_r.Rdata")
save(Backtests_HAR_RS_e, file = "Data/Backtests_HAR_RS_e.Rdata")
save(Backtests_HAR_RS_r, file = "Data/Backtests_HAR_RS_r.Rdata")
save(Backtests_HAR_RSRK_e, file = "Data/Backtests_HAR_RSRK_e.Rdata")
save(Backtests_HAR_RSRK_r, file = "Data/Backtests_HAR_RSRK_r.Rdata")
save(Backtests_RGARCH_e, file = "Data/Backtests_RGARCH_e.Rdata")
save(Backtests_RGARCH_r, file = "Data/Backtests_RGARCH_r.Rdata")
save(Backtests_ARMAGARCH_e, file = "Data/Backtests_ARMAGARCH_e.Rdata")
save(Backtests_ARMAGARCH_r, file = "Data/Backtests_ARMAGARCH_r.Rdata")

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 







stockn = "XOM"

sum(retstouse < VaR95_RGARCH_e[[stockn]]) / nrow(VaR95_RGARCH_e[[stockn]])

mean(allstocks[[stockn]]$ret)
mean(VaR95_AR1_RV_e[[stockn]])

BacktestVaR(data = retstouse, VaR = VaR95_AR1_RV_e[[stockn]], alpha = 0.05)

# < 
#    VaR95_AR1_RV_e[[stockn]]
#) / nrow(VaR95_AR1_RV_e[[stockn]])

tail(VaR95_AR1_RV_e)

retstouse1 = allstocks[["AAL"]]$ret[which(
  (index(allstocks[["AAL"]])>=as.Date("2019-12-03"))   # TODO: Generalize this for any date 
  &(index(allstocks[["AAL"]])<=as.Date("2020-03-09")))] # TODO: Generalize this for any date 

retstouse2 = allstocks[["XOM"]]$ret[which(
  (index(allstocks[["XOM"]])>=as.Date("2019-12-03"))   # TODO: Generalize this for any date 
  &(index(allstocks[["XOM"]])<=as.Date("2020-03-09")))] # TODO: Generalize this for any date 

BackTest1 = BacktestVaR(data = retstouse1, VaR = VaR95_AR1_RV_e[["AAL"]], alpha = 0.05)
BackTest2 = BacktestVaR(data = retstouse2, VaR = VaR95_AR1_RV_e[["XOM"]], alpha = 0.05)

head(AR1_RV_fc_e_er[["AAL"]])
head(AR1_RV_fc_e_er[["XOM"]])

head(VaR95_AR1_RV_e[["AAL"]]) 
head(VaR95_AR1_RV_e[["XOM"]]) 


for(i in seq(1:nrow(stocks))){
  print(stocks$stockname[i])
}

mean(AR1_RV_fc_e[["AAL"]])
mean(AR1_RV_fc_e[["XOM"]]) 
mean(AR1_RV_fc_e[["ACN"]])

var95

sum(VaR95_AR1_RV_e[[stockn]] > AR1_RV_fc_e_er[[stockn]])   

AR1_RV_fc_e_er[[stockn]]
AR1_RV_fc_r_er[[stockn]]
HAR_fc_e_er[[stockn]]
HAR_fc_r_er[[stockn]]
HAR_AS_fc_e_er[[stockn]]
HAR_AS_fc_r_er[[stockn]]
HAR_RSV_fc_e_er[[stockn]]
HAR_RSV_fc_r_er[[stockn]]
HAR_RSRK_fc_e_er[[stockn]]
HAR_RSRK_fc_r_er[[stockn]]
RGARCH_fc_e_er[[stockn]]
RGARCH_fc_r_er[[stockn]]
ARMAGARCH_fc_e_er[[stockn]]
ARMAGARCH_fc_r_er[[stockn]]

VaR95_AR1_RV_e[[stockn]]
VaR95_AR1_RV_r[[stockn]]
VaR95_HAR_e[[stockn]]
VaR95_HAR_r[[stockn]]
VaR95_HAR_AS_e[[stockn]]
VaR95_HAR_AS_r[[stockn]]
VaR95_HAR_RSV_e[[stockn]]
VaR95_HAR_RSV_r[[stockn]]
VaR95_HAR_RSRK_e[[stockn]]
VaR95_HAR_RSRK_r[[stockn]]
VaR95_RGARCH_e[[stockn]]
VaR95_RGARCH_r[[stockn]]
VaR95_ARMAGARCH_e[[stockn]]
VaR95_ARMAGARCH_r[[stockn]]
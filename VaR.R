
stockn = "AAL" 

for(stockn in stocks$stockname){
  print(stockn)
  dist = fitdist(data = as.numeric(AR1_RV_fc_e[[stockn]])*1000, distr = "norm")["estimate"] # TODO here I am assuming normal distribution - jarque bera test! 
  estimates = dist$estimate/1000 
  AR1_RV_fc_e[[stockn]]
}



fitdist(data = as.numeric(AR1_RV_fc_e[[stockn]]), distr = "t", list(m=1,s=0.001, df=3))["estimate"]





fitdist(x, "t")


unique(stocks$JBpval_rets) 







VaR95_AR1_RV_e = list() 
VaR95_AR1_RV_r = list() 
VaR95_HAR_e = list() 
VaR95_HAR_r = list() 
VaR95_HAR_AS_e = list() 
VaR95_HAR_AS_r = list() 
VaR95_HAR_RSV_e = list() 
VaR95_HAR_RSV_r = list() 
VaR95_HAR_RSRK_e = list() 
VaR95_HAR_RSRK_r = list() 
VaR95_RGARCH_e = list() 
VaR95_RGARCH_r = list() 
VaR95_ARMAGARCH_e = list() 
VaR95_ARMAGARCH_r = list() 



# TODO add forecasts from other models 
# TODO save backtest results 

stockn = "AAL"
errorstocks = c("hi")


for(stockn in stocks$stockname){   
#  VaR95_AR1_RV_e[[stockn]]  = as.xts(mean(allstocks[[stockn]]$ret)      # Assuming normal distribution  
#                                     + AR1_RV_fc_e_er[[stockn]]*qnorm(0.05, mean = 0, sd = 1))  # TODO assuming normal distribution 

  retstouse = allstocks[[stockn]]$ret[which(
    (index(allstocks[[stockn]])>=as.Date("2019-12-03"))   # TODO: Generalize this for any date 
    &(index(allstocks[[stockn]])<=as.Date("2020-03-09")))] # TODO: Generalize this for any date 
  
  # print(stockn)


  


      result <- tryCatch({
    # x=as.xts(AR1_RV_fc_e[[stockn]])     ################# TODO: Here is the error, the distribution should be fitting the returns!  
        x=as.xts(allstocks[[stockn]]$ret)     
    fit.t = fitdistr(
      x = x*1000, 
      densfun = "t", 
      start = list(m=mean(x),s=sd(x), df=stocks$t_df_start[which(stocks$stockname == stockn)]), 
      lower=c(-1, 0.001,0.01))$estimate/c(1000,1000,1) 
    fit.df = fit.t[3] 

    # Check how the VaR should be calculate     
    VaR95_AR1_RV_e[[stockn]]  = as.xts(mean(allstocks[[stockn]]$ret)        # Assuming t distribution 
                                       + AR1_RV_fc_e[[stockn]]*qt(p = 0.05, df = fit.df))  # TODO assuming normal distribution 

#     VaR95_AR1_RV_e[[stockn]]  = as.xts(mean(retstouse)        # Assuming t distribution 
#                                        + AR1_RV_fc_e[[stockn]]*qt(p = 0.05, df = fit.df))  # TODO assuming normal distribution 
    
    BackTest = BacktestVaR(data = retstouse, VaR = VaR95_AR1_RV_e[[stockn]], alpha = 0.05)  # Does not make sense, here should be the returns 
#    print("OK")
  }, error = function(e) {
    print(paste(stockn, "Error"))
  }) 
  print(paste(stockn,":  ",unname(BackTest$LRuc["Pvalue"]) ,sep=""))
}



# TODO: Change backtesting, it should compare the VaR with returns in the given period 
# For that, I will need to store the date of a beginning of forecast 

stockn = "XOM"

sum(retstouse < VaR95_AR1_RV_e[[stockn]]) / nrow(VaR95_AR1_RV_e[[stockn]])

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







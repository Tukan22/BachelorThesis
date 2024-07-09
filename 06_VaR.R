
# for(stockn in stocks$stockname){
#  dist = fitdist(data = as.numeric(AR1_RV_fc_e[[stockn]])*1000, distr = "norm")["estimate"] # TODO here I am assuming normal distribution - jarque bera test! 
#  estimates = dist$estimate/1000 
#  AR1_RV_fc_e[[stockn]]
#}

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




VaR <- function(VaRalpha)
{
  
  # Runs approximately 90 seconds 
  start_time = Sys.time()
  
  counter = 1 
  
  # VaRalpha = 0.01
  
  for(stockn in stocks$stockname){   
    VaRresults[[stockn]] = as.data.frame(matrix(rep(NA,  14), ncol = 1))
    
    #  for(VaRalpha in c (0.1, 0.05, 0.01)){
    
    print(paste(counter, " - ", stockn ,sep="")) 
    
    retstouse = allstocks[[stockn]]$ret[
      min(which(index(allstocks[[stockn]])>=as.Date(forecast_start_date))+1):
        min(which(index(allstocks[[stockn]])>=(as.Date(forecast_start_date)))+n_for)] 
    
    x=as.xts(allstocks[[stockn]]$ret)     
    
    # Fit the returns to t-distribution, i. e. find the right number of degrees of freedom.  
    fit.t = fitdistr(
      x = x*1000, 
      densfun = "t", 
      start = list(m=mean(x),s=sd(x), df=stocks$t_df_start[which(stocks$stockname == stockn)]), 
      lower=c(-1, 0.001,0.01))$estimate/c(1000,1000,1) 
    fit.df = fit.t[3] 
    
    tquantile = qt(p = VaRalpha, df = fit.df)
    
    # Compute VaR for each model 
    VaR95_AR1_RV_e[[stockn]] =    as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(AR1_RV_fc_e[[stockn]]))  
    VaR95_AR1_RV_r[[stockn]] =    as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(AR1_RV_fc_r[[stockn]]))  
    VaR95_HAR_e[[stockn]] =       as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(HAR_fc_e[[stockn]]))  
    VaR95_HAR_r[[stockn]] =       as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(HAR_fc_r[[stockn]]))  
    VaR95_HAR_AS_e[[stockn]] =    as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(HAR_AS_fc_e[[stockn]]))  
    VaR95_HAR_AS_r[[stockn]] =    as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(HAR_AS_fc_r[[stockn]]))  
    VaR95_HAR_RS_e[[stockn]] =    as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(HAR_RS_fc_e[[stockn]]))  
    VaR95_HAR_RS_r[[stockn]] =    as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(HAR_RS_fc_r[[stockn]]))  
    VaR95_HAR_RSRK_e[[stockn]] =  as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(HAR_RSRK_fc_e[[stockn]]))  
    VaR95_HAR_RSRK_r[[stockn]] =  as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(HAR_RSRK_fc_r[[stockn]]))  
    VaR95_RGARCH_e[[stockn]] =    as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(RGARCH_fc_e[[stockn]]))  
    VaR95_RGARCH_r[[stockn]] =    as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(RGARCH_fc_r[[stockn]]))  
    VaR95_ARMAGARCH_e[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(ARMAGARCH_fc_e[[stockn]]))  
    VaR95_ARMAGARCH_r[[stockn]] = as.xts(mean(allstocks[[stockn]]$ret) + tquantile*(ARMAGARCH_fc_r[[stockn]]))   
    
    # Backtest each model 
    Backtests_AR1_RV_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_AR1_RV_e[[stockn]], alpha = VaRalpha)
    Backtests_AR1_RV_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_AR1_RV_r[[stockn]], alpha = VaRalpha)
    Backtests_HAR_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_e[[stockn]], alpha = VaRalpha)
    Backtests_HAR_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_r[[stockn]], alpha = VaRalpha)
    Backtests_HAR_AS_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_AS_e[[stockn]], alpha = VaRalpha)
    Backtests_HAR_AS_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_AS_r[[stockn]], alpha = VaRalpha)
    Backtests_HAR_RS_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_RS_e[[stockn]], alpha = VaRalpha)
    Backtests_HAR_RS_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_RS_r[[stockn]], alpha = VaRalpha)
    Backtests_HAR_RSRK_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_RSRK_e[[stockn]], alpha = VaRalpha)
    Backtests_HAR_RSRK_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_HAR_RSRK_r[[stockn]], alpha = VaRalpha)
    Backtests_RGARCH_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_RGARCH_e[[stockn]], alpha = VaRalpha)
    Backtests_RGARCH_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_RGARCH_r[[stockn]], alpha = VaRalpha)
    Backtests_ARMAGARCH_e[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_ARMAGARCH_e[[stockn]], alpha = VaRalpha)
    Backtests_ARMAGARCH_r[[stockn]] = BacktestVaR(data = retstouse, VaR = VaR95_ARMAGARCH_r[[stockn]], alpha = VaRalpha)
    
    
    #  }
    
    # Kupiec test p-values
    
    Kupieccolname = paste((1-VaRalpha)*100,"% Kupiec p-val", sep = "") 
    
    VaRresults[[stockn]][Kupieccolname] =  c(
      unname(Backtests_AR1_RV_e[[stockn]]$LRuc[2]), 
      unname(Backtests_AR1_RV_r[[stockn]]$LRuc[2]), 
      unname(Backtests_HAR_e[[stockn]]$LRuc[2]),  
      unname(Backtests_HAR_r[[stockn]]$LRuc[2]),  
      unname(Backtests_HAR_AS_e[[stockn]]$LRuc[2]),  
      unname(Backtests_HAR_AS_r[[stockn]]$LRuc[2]),   
      unname(Backtests_HAR_RS_e[[stockn]]$LRuc[2]),   
      unname(Backtests_HAR_RS_r[[stockn]]$LRuc[2]),   
      unname(Backtests_HAR_RSRK_e[[stockn]]$LRuc[2]),   
      unname(Backtests_HAR_RSRK_r[[stockn]]$LRuc[2]),   
      unname(Backtests_RGARCH_e[[stockn]]$LRuc[2]),   
      unname(Backtests_RGARCH_r[[stockn]]$LRuc[2]),   
      unname(Backtests_ARMAGARCH_e[[stockn]]$LRuc[2]),   
      unname(Backtests_ARMAGARCH_r[[stockn]]$LRuc[2])   
    )
    
    
    Perccolname = paste((1-VaRalpha)*100,"% percentage below VaR", sep = "") 
    
    # Percentage returns outside of the alpha-% VaR 
    VaRresults[[stockn]][Perccolname] = c(
      sum(retstouse < VaR95_AR1_RV_e[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_AR1_RV_r[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_HAR_e[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_HAR_r[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_HAR_AS_e[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_HAR_AS_r[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_HAR_RS_e[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_HAR_RS_r[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_HAR_RSRK_e[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_HAR_RSRK_r[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_RGARCH_e[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_RGARCH_r[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_ARMAGARCH_e[[stockn]])/nrow(retstouse),
      sum(retstouse < VaR95_ARMAGARCH_r[[stockn]])/nrow(retstouse) 
    )   
    counter = counter + 1 
  }
  
  VaRmeans = apply(VaRresults_output, MARGIN = 2, FUN = mean)
  VaRsds = apply(VaRresults_output, MARGIN = 2, FUN = sd)
  VaRouts = apply(VaRresults_output, MARGIN = 2, FUN = function(x){sum(x<0.05)})
  
  Kupiec_summary = cbind(VaRmeans, VaRsds, VaRouts) 
  rownames(Kupiec_summary) = paste(rownames(Kupiec_summary), rep(c("expanding","rolling"),times = 7)) 
  colnames(Kupiec_summary) = c("Mean","SD","p-val < 0.05")
  
  end_time = Sys.time()
  print(end_time-start_time)
  
}


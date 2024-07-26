pdf(file = "VaRcomparison.pdf", width = 16, height = 12) 

for(stockn in c("AAPL","ABBV","C","XOM")){
  w_l = stocks$w_l[which(stocks$stockname == stockn)]    # TODO set better numbers 
  n_for = stocks$n_for[which(stocks$stockname == stockn)] # TODO set better numbers  
  
  x=as.xts(allstocks[[stockn]]$ret)    
  
  GARCHmodel = ugarchroll(ARMAGARCH, x[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                          n.start = NULL, refit.every = 21, refit.window = c("moving"), 
                          window.size = w_l, solver = "hybrid", fit.control = list(), 
                          solver.control = list(), calculate.VaR = TRUE, 
                          keep.coef = TRUE) 
  
  GARCHforecast = xts(GARCHmodel@forecast[["density"]]$Sigma,
                      order.by = as.Date(rownames(GARCHmodel@forecast[["density"]])))
  
  VaRalpha = 0.05 
  
  fit.t = fitdistr(x = x*1000, densfun = "t", start = list(m=mean(x),s=sd(x), df=stocks$t_df_start[which(stocks$stockname == stockn)]), lower=c(-1, 0.001,0.01))$estimate/c(1000,1000,1) 
  fit.df = fit.t[3] 
  tquantile = qt(p = VaRalpha, df = fit.df)
  nquantile = qnorm(p = VaRalpha)
  
  ugarchvar = GARCHmodel@forecast$VaR$`alpha(5%)`
  VaR95_ARMAGARCH_r_normal = as.xts(mean(x) + nquantile*(GARCHforecast))   
  VaR95_ARMAGARCH_r_normal_demean = as.xts(nquantile*(GARCHforecast)) 
  VaR95_ARMAGARCH_r_t_dist = as.xts(mean(x) + tquantile*(GARCHforecast))   
  
  plot(GARCHmodel@forecast$VaR$realized, type = 'l', ylab = '', lwd = 1, main = stockn)
  lines(ugarchvar, type = 'l', col = "red", lwd = 1)
  lines(as.numeric(VaR95_ARMAGARCH_r_normal), type = 'l', col = "blue", lwd = 1)
  lines(as.numeric(VaR95_ARMAGARCH_r_normal_demean), type = 'l', col = "orange", lwd = 1)
  lines(as.numeric(VaR95_ARMAGARCH_r_t_dist), type = 'l', col = "green", lwd = 1)
  legend("topleft", legend = c("Returns", "Ugarchroll VaR", "VaR N(0,1) with mean", "VaR N(0,1) without mean", "VaR t-dist"), col = c("black", "red", "blue", "orange", "green"), lty = c(1,1,1,1,1))
  

  
  # Should be 0.05 
  print("Hit rates:")
  print(paste("ugarchroll:", sum(x[seq(from = w_l+2, to = w_l+n_for+1)]<ugarchvar)/length(ugarchvar))) 
  print(paste("N(0,1):", sum(x[seq(from = w_l+2, to = w_l+n_for+1)]<VaR95_ARMAGARCH_r_normal)/nrow(VaR95_ARMAGARCH_r_normal)))
  print(paste("N(0,1) demeaned:", sum(x[seq(from = w_l+2, to = w_l+n_for+1)]<VaR95_ARMAGARCH_r_normal_demean)/nrow(VaR95_ARMAGARCH_r_normal_demean))) 
  print(paste("t-dist:", sum(x[seq(from = w_l+2, to = w_l+n_for+1)]<VaR95_ARMAGARCH_r_t_dist)/nrow(VaR95_ARMAGARCH_r_t_dist))) 
}

dev.off() 

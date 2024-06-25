
stockn = "AAL" 

for(stockn in stocks$stockname){
  print(stockn)
  dist = fitdist(data = as.numeric(AR1_RV_fc_e[[stockn]])*1000, distr = "norm")["estimate"] # TODO here I am assuming normal distribution - jarque bera test! 
  estimates = dist$estimate/1000 
  AR1_RV_fc_e[[stockn]]
}

# TODO run JBtest results in model_fitting 





class() 

fitdist(distr = 'norm' , x = rets)$pars



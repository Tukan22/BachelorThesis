
true_vals = list() 

AR1_RV_fc_e_er = list() 
AR1_RV_fc_r_er = list() 
HAR_fc_e_er = list() 
HAR_fc_r_er = list() 
HAR_AS_fc_e_er = list() 
HAR_AS_fc_r_er = list() 
HAR_RSV_fc_e_er = list() 
HAR_RSV_fc_r_er = list() 
HAR_RSRK_fc_e_er = list() 
HAR_RSRK_fc_r_er = list() 
RGARCH_fc_e_er = list() 
RGARCH_fc_r_er = list() 
ARMAGARCH_fc_e_er = list() 
ARMAGARCH_fc_r_er = list() 

MSE_e = list() 
MSE_r = list() 
MSEs = list() 
MAE_e = list() 
MAE_r = list() 
MAEs = list() 

for(stockn in stocks$stockname){
  n_for = stocks[which(stocks$stockname == stockn),"n_for"] 
  w_l = stocks[which(stocks$stockname == stockn),"w_l"]
  
  true_vals[[stockn]] <- as.vector(allstocks[[stockn]]$RV[(w_l+2):(w_l+1+n_for),])
  
  AR1_RV_fc_e_er[[stockn]]  <- true_vals[[stockn]] - AR1_RV_fc_e[[stockn]]
  AR1_RV_fc_r_er[[stockn]]  <- true_vals[[stockn]] - AR1_RV_fc_r[[stockn]]
  HAR_fc_e_er[[stockn]] <- true_vals[[stockn]] - HAR_fc_e[[stockn]]
  HAR_fc_r_er[[stockn]] <- true_vals[[stockn]] - HAR_fc_r[[stockn]]
  HAR_AS_fc_e_er[[stockn]] <- true_vals[[stockn]] - HAR_AS_fc_e[[stockn]]
  HAR_AS_fc_r_er[[stockn]] <- true_vals[[stockn]] - HAR_AS_fc_r[[stockn]]
  HAR_RSV_fc_e_er[[stockn]] <- true_vals[[stockn]] - HAR_RS_fc_e[[stockn]]
  HAR_RSV_fc_r_er[[stockn]] <- true_vals[[stockn]] - HAR_RS_fc_r[[stockn]]
  HAR_RSRK_fc_e_er[[stockn]] <- true_vals[[stockn]] - HAR_RSRK_fc_e[[stockn]]
  HAR_RSRK_fc_r_er[[stockn]] <- true_vals[[stockn]] - HAR_RSRK_fc_r[[stockn]]
  RGARCH_fc_e_er[[stockn]] <- true_vals[[stockn]] - RGARCH_fc_e[[stockn]]
  RGARCH_fc_r_er[[stockn]] <- true_vals[[stockn]] - RGARCH_fc_r[[stockn]]
  ARMAGARCH_fc_e_er[[stockn]] <- true_vals[[stockn]] - ARMAGARCH_fc_e[[stockn]]
  ARMAGARCH_fc_r_er[[stockn]] <- true_vals[[stockn]] - ARMAGARCH_fc_r[[stockn]]
}



save(AR1_RV_fc_e_er, file = "Data/AR1_RV_fc_e_er.Rdata")
save(AR1_RV_fc_r_er, file = "Data/AR1_RV_fc_r_er.Rdata")
save(HAR_fc_e_er, file = "Data/HAR_fc_e_er.Rdata")
save(HAR_fc_r_er, file = "Data/HAR_fc_r_er.Rdata")  
save(HAR_AS_fc_e_er, file = "Data/HAR_AS_fc_e_er.Rdata")
save(HAR_AS_fc_r_er, file = "Data/HAR_AS_fc_r_er.Rdata")
save(HAR_RSV_fc_e_er, file = "Data/HAR_RSV_fc_e_er.Rdata")
save(HAR_RSV_fc_r_er, file = "Data/HAR_RSV_fc_r_er.Rdata")  
save(HAR_RSRK_fc_e_er, file = "Data/HAR_RSRK_fc_e_er.Rdata")
save(HAR_RSRK_fc_r_er, file = "Data/HAR_RSRK_fc_r_er.Rdata")
save(RGARCH_fc_e_er, file = "Data/RGARCH_fc_e_er.Rdata")
save(RGARCH_fc_r_er, file = "Data/RGARCH_fc_r_er.Rdata")  
save(ARMAGARCH_fc_e_er, file = "Data/ARMAGARCH_fc_e_er.Rdata")
save(ARMAGARCH_fc_r_er, file = "Data/ARMAGARCH_fc_r_er.Rdata")


for(stockn in stocks$stockname){
  errs[[stockn]] <- list(AR1_RV_fc_e_er[[stockn]], AR1_RV_fc_r_er[[stockn]], HAR_fc_e_er[[stockn]], 
               HAR_fc_r_er[[stockn]], HAR_AS_fc_e_er[[stockn]], HAR_AS_fc_r_er[[stockn]],
               HAR_RSV_fc_e_er[[stockn]], HAR_RSV_fc_r_er[[stockn]], HAR_RSRK_fc_e_er[[stockn]], 
               HAR_RSRK_fc_r_er[[stockn]], RGARCH_fc_e_er[[stockn]], RGARCH_fc_r_er[[stockn]], 
               ARMAGARCH_fc_e_er[[stockn]], ARMAGARCH_fc_r_er[[stockn]])
}

save(errs, file = "Data/errs.Rdata")

for(stockn in stocks$stockname){
  #MSE
  MSE_e[[stockn]] <- sapply(errs[[stockn]][c(1,3,5,7,9,11,13)], function(x) mean(x^2, na.rm = TRUE))
  MSE_r[[stockn]] <- sapply(errs[[stockn]][c(2,4,6,8,10,12,14)], function(x) mean(x^2, na.rm = TRUE))
  
  MSEs[[stockn]]  <- matrix(c(MSE_e[[stockn]], MSE_r[[stockn]], ifelse(MSE_r[[stockn]] > MSE_e[[stockn]], "Exanding", "Rolling")), ncol = 3, byrow = FALSE)
  colnames(MSEs[[stockn]]) <- c("Expanding window error", "Rolling window error", "Better forecast scheme")
  rownames(MSEs[[stockn]])  <- c("AR(1)-RV", "HAR","HAR_AS", "HAR-RSV", "HAR-RSRK", "Realized GARCH", "ARMA-GARCH")
  
  # print("MSE metrics for the models:")
  # MSEs
  # xtable(MSEs[[stockn]],digits = 8)
  
  
  
  #MAE 
  MAE_e[[stockn]] <- sapply(errs[[stockn]][c(1,3,5,7,9,11,13)], function(x) mean(abs(x), na.rm = TRUE))
  MAE_r[[stockn]] <- sapply(errs[[stockn]][c(2,4,6,8,10,12,14)], function(x) mean(abs(x), na.rm = TRUE))
  
  MAEs[[stockn]]  <- matrix(c(MAE_e[[stockn]], MAE_r[[stockn]], ifelse(MAE_r[[stockn]] > MAE_e[[stockn]],"Expanding","Rolling")), ncol = 3, byrow = FALSE)
  colnames(MAEs[[stockn]]) <- c("Expanding window error", "Rolling window error", "Expanding better")
  rownames(MAEs[[stockn]])  <- c("AR(1)-RV", "HAR","HAR_AS", "HAR-RSV", "HAR-RSRK", "Realized GARCH", "ARMA-GARCH")
  
  # print("MAE metrics for the models:")               
  # MAEs
  # xtable(MAEs[[stockn]],digits = 8)
}

save(MSE_e, file = "Data/MSE_e.Rdata")
save(MSE_r, file = "Data/MSE_r.Rdata")
save(MSEs, file = "Data/MSEs.Rdata")
save(MAE_e, file = "Data/MAE_e.Rdata")
save(MAE_r, file = "Data/MAE_r.Rdata")
save(MAEs, file = "Data/MAEs.Rdata")


#Exp  <- c("AR1_RV_fc_e", "HAR_fc_e", "HAR_RS_fc_e", "HAR_RSRK_fc_e", "RGARCH_fc_e", "ARMAGARCH_fc_e")
Exp  <- c("AR1_RV_fc_e", "HAR_fc_e", "HAR_AS_fc_e", "HAR_RS_fc_e", "HAR_RSRK_fc_e", "RGARCH_fc_e", "ARMAGARCH_fc_e")
#Roll  <- c("AR1_RV_fc_r", "HAR_fc_r", "HAR_RS_fc_r", "HAR_RSRK_fc_r", "RGARCH_fc_r", "ARMAGARCH_fc_r")
Roll <- c("AR1_RV_fc_r", "HAR_fc_r", "HAR_AS_fc_r", "HAR_RS_fc_r", "HAR_RSRK_fc_r", "RGARCH_fc_r", "ARMAGARCH_fc_r")


Exp_comb = list() 
Roll_comb = list() 

Exp_comb[[stockn]] <- t(combn(Exp, 2))
Roll_comb[[stockn]] <- t(combn(Roll, 2))

Diebold_e = list() 
Diebold_r = list() 


row.names(Exp_comb[[stockn]])<-apply(Exp_comb[[stockn]],1,function(x) paste(x[1],'|',x[2]))
Diebold_e[[stockn]] <- data.frame(apply(Exp_comb[[stockn]],1,function(x) 
  dm.test(true_vals[[stockn]] - get(x[1])[[stockn]], true_vals[[stockn]] - get(x[2])[[stockn]], alternative = c("two.sided"))$p.value))
colnames(Diebold_e[[stockn]])<-"P-Value"

row.names(Roll_comb[[stockn]])<-apply(Roll_comb[[stockn]],1,function(x) paste(x[1],'|',x[2]))
Diebold_r[[stockn]] <- data.frame(apply(Roll_comb[[stockn]],1,function(x) 
  dm.test(true_vals[[stockn]] - get(x[1])[[stockn]], true_vals[[stockn]] - get(x[2])[[stockn]], alternative = c("two.sided"))$p.value))
colnames(Diebold_r[[stockn]])<-"P-Value"

#DB-tests
print("Expanding DM-test")
print(round(Diebold_e[[stockn]],3))
xtable(round(Diebold_e[[stockn]],3))

print("Rolling DM-test")
print(round(Diebold_r[[stockn]],3))
xtable(round(Diebold_r[[stockn]],3))



save(Exp_comb , file = "Data/Exp_comb.Rdata")
save(Roll_comb, file = "Data/Roll_comb.Rdata")
save(Diebold_e, file = "Data/Diebold_e.Rdata")
save(Diebold_r, file = "Data/Diebold_r.Rdata")




# M-Z regression 

regressions = list() 

Minc_AR1_RV_e = list() 
Minc_AR1_RV_r = list() 
Minc_HAR_e = list()  
Minc_HAR_r = list() 
Minc_HAR_AS_e = list() 
Minc_HAR_AS_r = list() 
Minc_HAR_RS_e = list() 
Minc_HAR_RS_r = list() 
Minc_HAR_RSRK_e = list() 
Minc_HAR_RSRK_r = list() 
Minc_RGARCH_e = list() 
Minc_RGARCH_r = list() 
Minc_ARMAGARCH_e = list() 
Minc_ARMAGARCH_r = list() 

mincer = list() 

# Mincer - Zarnowitz regression 


wald_test <- function(lm) {
  wald <- wald.test(b = coef(lm), Sigma = vcov(lm), H0 = c(0, 1), Terms = 1:2) 
  coefs <- coef(lm)
  return(c(coefs, wald$result$chi2[1], round(wald$result$chi2[3], 2)))
}

counter = 1 

for(stockn in stocks$stockname){
  print(counter)
  
  regressions[[stockn]] <- list(
    Minc_AR1_RV_e[[stockn]] <- lm(true_vals[[stockn]] ~ AR1_RV_fc_e[[stockn]]),
    Minc_AR1_RV_r[[stockn]] <- lm(true_vals[[stockn]] ~ AR1_RV_fc_r[[stockn]]),
    Minc_HAR_e[[stockn]] <- lm(true_vals[[stockn]] ~ HAR_fc_e[[stockn]]),
    Minc_HAR_r[[stockn]] <- lm(true_vals[[stockn]] ~ HAR_fc_r[[stockn]]),
    Minc_HAR_AS_e[[stockn]] <- lm(true_vals[[stockn]] ~ HAR_AS_fc_e[[stockn]]),
    Minc_HAR_AS_r[[stockn]] <- lm(true_vals[[stockn]] ~ HAR_AS_fc_r[[stockn]]),
    Minc_HAR_RS_e[[stockn]] <- lm(true_vals[[stockn]] ~ HAR_RS_fc_e[[stockn]]),
    Minc_HAR_RS_r[[stockn]] <- lm(true_vals[[stockn]] ~ HAR_RS_fc_r[[stockn]]),
    Minc_HAR_RSRK_e[[stockn]] <- lm(true_vals[[stockn]] ~ HAR_RSRK_fc_e[[stockn]]),
    Minc_HAR_RSRK_r[[stockn]] <- lm(true_vals[[stockn]] ~ HAR_RSRK_fc_r[[stockn]]),
    Minc_RGARCH_e[[stockn]] <- lm(true_vals[[stockn]] ~  RGARCH_fc_e[[stockn]]),
    Minc_RGARCH_r[[stockn]] <- lm(true_vals[[stockn]] ~ RGARCH_fc_r[[stockn]]),
    Minc_ARMAGARCH_e[[stockn]] <- lm(true_vals[[stockn]] ~ ARMAGARCH_fc_e[[stockn]]),
    Minc_ARMAGARCH_r[[stockn]] <- lm(true_vals[[stockn]] ~ ARMAGARCH_fc_r[[stockn]])
  )
  

  
  mincer[[stockn]] <- as.data.frame(matrix(rep(NA, 5 * 14), ncol = 5))
  rownames(mincer[[stockn]]) <- c("AR(1)-RV Expanding", "AR(1)-RV Rolling", "HAR Expanding", "HAR Rolling",
                                  "HAR-AS Expanding" ,"HAR-AS Rolling","HAR-RSV Expanding", "HAR-RSV Rolling", 
                                  "HAR-RSRK Expanding", "HAR-RSRK Rolling", "Realized GARCH Expanding", "Realized GARCH Rolling", 
                                  "ARMA-GARCH Expanding", "ARMA-GARCH Rolling")
  colnames(mincer[[stockn]]) <- c("Intercept", "Slope", "Statistic", "p-value", "R Squared")
  
  for (i in 1:length(regressions[[stockn]])) {
    mincer[[stockn]][i, ] <- c(wald_test(regressions[[stockn]][[i]]), summary(regressions[[stockn]][[i]])$r.squared)
  }
  
  counter = counter + 1 
}



save(regressions , file = "Data/regressions.Rdata")
save(Minc_AR1_RV_e , file = "Data/Minc_AR1_RV_e.Rdata")
save(Minc_AR1_RV_r , file = "Data/Minc_AR1_RV_r .Rdata")
save(Minc_HAR_e , file = "Data/Minc_HAR_e.Rdata")
save(Minc_HAR_r , file = "Data/Minc_HAR_r.Rdata")
save(Minc_HAR_AS_e , file = "Data/Minc_HAR_AS_e.Rdata")
save(Minc_HAR_AS_r , file = "Data/Minc_HAR_AS_r.Rdata")
save(Minc_HAR_RS_e , file = "Data/Minc_HAR_RS_e.Rdata")
save(Minc_HAR_RS_r , file = "Data/Minc_HAR_RS_r.Rdata")
save(Minc_HAR_RSRK_e , file = "Data/Minc_HAR_RSRK_e.Rdata")
save(Minc_HAR_RSRK_r , file = "Data/Minc_HAR_RSRK_r.Rdata")
save(Minc_RGARCH_e , file = "Data/Minc_RGARCH_e.Rdata")
save(Minc_RGARCH_r , file = "Data/Minc_RGARCH_r.Rdata")
save(Minc_ARMAGARCH_e , file = "Data/Minc_ARMAGARCH_e.Rdata")
save(Minc_ARMAGARCH_r , file = "Data/Minc_ARMAGARCH_r.Rdata")
save(mincer , file = "Data/mincer.Rdata")
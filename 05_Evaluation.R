


###################################
######### Computer errors #########
###################################

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
  
  true_vals[[stockn]] <- as.vector(sqrt(allstocks[[stockn]]$RV[(w_l+2):(w_l+1+n_for),]))
  
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

for(stockn in stocks$stockname){
  errs[[stockn]] <- list(AR1_RV_fc_e_er[[stockn]], AR1_RV_fc_r_er[[stockn]], HAR_fc_e_er[[stockn]], 
               HAR_fc_r_er[[stockn]], HAR_AS_fc_e_er[[stockn]], HAR_AS_fc_r_er[[stockn]],
               HAR_RSV_fc_e_er[[stockn]], HAR_RSV_fc_r_er[[stockn]], HAR_RSRK_fc_e_er[[stockn]], 
               HAR_RSRK_fc_r_er[[stockn]], RGARCH_fc_e_er[[stockn]], RGARCH_fc_r_er[[stockn]], 
               ARMAGARCH_fc_e_er[[stockn]], ARMAGARCH_fc_r_er[[stockn]])
}



####################################
######### Compute MSE, MAE #########
####################################

for(stockn in stocks$stockname){
  #MSE
  MSE_e[[stockn]] <- sapply(errs[[stockn]][c(1,3,5,7,9,11,13)], function(x) mean(x^2, na.rm = TRUE))
  MSE_r[[stockn]] <- sapply(errs[[stockn]][c(2,4,6,8,10,12,14)], function(x) mean(x^2, na.rm = TRUE))
  
  MSEs[[stockn]]  <- matrix(c(MSE_e[[stockn]], MSE_r[[stockn]], ifelse(MSE_r[[stockn]] > MSE_e[[stockn]], "Expanding", "Rolling")), ncol = 3, byrow = FALSE)
  colnames(MSEs[[stockn]]) <- c("Expanding window error", "Rolling window error", "Better forecast scheme")
  rownames(MSEs[[stockn]])  <- c("AR(1)-RV", "HAR","HAR_AS", "HAR-RSV", "HAR-RSRK", "Realized GARCH", "ARMA-GARCH")
  
  #MAE 
  MAE_e[[stockn]] <- sapply(errs[[stockn]][c(1,3,5,7,9,11,13)], function(x) mean(abs(x), na.rm = TRUE))
  MAE_r[[stockn]] <- sapply(errs[[stockn]][c(2,4,6,8,10,12,14)], function(x) mean(abs(x), na.rm = TRUE))
  
  MAEs[[stockn]]  <- matrix(c(MAE_e[[stockn]], MAE_r[[stockn]], ifelse(MAE_r[[stockn]] > MAE_e[[stockn]],"Expanding","Rolling")), ncol = 3, byrow = FALSE)
  colnames(MAEs[[stockn]]) <- c("Expanding window error", "Rolling window error", "Better forecast scheme")
  rownames(MAEs[[stockn]])  <- c("AR(1)-RV", "HAR","HAR_AS", "HAR-RSV", "HAR-RSRK", "Realized GARCH", "ARMA-GARCH")
}



########################################
######### Diebold-Mariano test #########
########################################

Exp  <- c("AR1_RV_fc_e", "HAR_fc_e", "HAR_AS_fc_e", "HAR_RS_fc_e", "HAR_RSRK_fc_e", "RGARCH_fc_e", "ARMAGARCH_fc_e")
Roll <- c("AR1_RV_fc_r", "HAR_fc_r", "HAR_AS_fc_r", "HAR_RS_fc_r", "HAR_RSRK_fc_r", "RGARCH_fc_r", "ARMAGARCH_fc_r")

Exp_comb = list() 
Roll_comb = list() 


Diebold_e = list() 
Diebold_r = list() 

for(stockn in stocks$stockname){
  Exp_comb[[stockn]] <- t(combn(Exp, 2))
  Roll_comb[[stockn]] <- t(combn(Roll, 2))
  
  row.names(Exp_comb[[stockn]])<-apply(Exp_comb[[stockn]],1,function(x) paste(x[1],'|',x[2]))
  Diebold_e[[stockn]] <- data.frame(apply(Exp_comb[[stockn]],1,function(x) 
    dm.test(true_vals[[stockn]] - get(x[1])[[stockn]], true_vals[[stockn]] - get(x[2])[[stockn]], alternative = c("two.sided"))$p.value))
  colnames(Diebold_e[[stockn]])<-"P-Value"
  
  row.names(Roll_comb[[stockn]])<-apply(Roll_comb[[stockn]],1,function(x) paste(x[1],'|',x[2]))
  Diebold_r[[stockn]] <- data.frame(apply(Roll_comb[[stockn]],1,function(x) 
    dm.test(true_vals[[stockn]] - get(x[1])[[stockn]], true_vals[[stockn]] - get(x[2])[[stockn]], alternative = c("two.sided"))$p.value))
  colnames(Diebold_r[[stockn]])<-"P-Value"
}



# DM-tests
# print("Expanding DM-test")
# print(round(Diebold_e[[stockn]],3))
# xtable(round(Diebold_e[[stockn]],3))

# print("Rolling DM-test")
# print(round(Diebold_r[[stockn]],3))
# xtable(round(Diebold_r[[stockn]],3))



###############################################
######### Mincer-Zarnowitz regression #########
###############################################

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

# Function for a better working with wald test 
wald_test <- function(lm) {
  wald <- wald.test(b = coef(lm), Sigma = vcov(lm), H0 = c(0, 1), Terms = 1:2) 
  coefs <- coef(lm)
  return(c(coefs, wald$result$chi2[1], round(wald$result$chi2[3], 2)))
}


# Compute MZ-regressions for all all forecasts 

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








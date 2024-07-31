
###############################
### Plot of mean vs. stddev ###
###############################

pdf(file = "Plots/Meanstddev.pdf", width = 16, height = 12) 

plot(
  x = unlist(lapply(allstocks, FUN = function(x){mean(x$ret)})), 
  y = unlist(lapply(allstocks, FUN = function(x){sd(x$ret)})), 
  xlab = "Mean", ylab = "Standard deviation", 
  main = "Standard deviation vs. mean of selected stocks", 
  cex.main = 3, 
  cex.lab = 1.6, 
  cex.axis = 2, 
  cex = 2 
)
model = lm(unlist(lapply(allstocks, FUN = function(x){sd(x$ret)}))~unlist(lapply(allstocks, FUN = function(x){mean(x$ret)})), )

dev.off() 



###################################
### Ljung-Box test on residuals ### 
################################### 

pdf(file = "Plots/LBresidpval.pdf", width = 16, height = 12) 

LBresid =   unlist(
  lapply(
    ARMA_fit, 
    FUN = function(x){
      Box.test(x$residuals, type = 'Ljung-Box',lag = log(length(x$residuals)) )$p.value
    }
  )
)

boxplot(LBresid, type = 'o', main = "Ljung-Box test on residuals of ARMA(1,1) model", yaxt = 'n', cex.main = 3, xlab = "p-value", cex.lab = 3)
abline(h = 0.05, col = "red", lty = 2, lwd = 2)
axis(2, at=seq(from = 0, to = 1, by = 0.05), labels=TRUE, las = 2, cex.axis = 1.8)

dev.off() 



#####################################
### Basic overview of used stocks ###
#####################################

sd = as.data.frame(as.character(stocks$start_date),row.names = stocks$stockname)
ed = as.data.frame(as.character(stocks$end_date),row.names = stocks$stockname) 
rest = as.data.frame(
  stocks[,c(
    which(names(stocks)=="obs"), 
    which(names(stocks)=="pre_covid_obs")
  )],
  row.names = stocks$stockname
)

overview = cbind(sd, ed, rest)
colnames(overview) = c("Start date","End date", "Observations", paste("Observations before",pre_covid_end_date))



overview_side = cbind(
  rownames(  overview[seq(from = 1, to = nrow(overview)/2),]),
  overview[seq(from = 1, to = nrow(overview)/2),], 
  rownames(overview[seq(from = nrow(overview)/2+1, to = nrow(overview)),]),
  overview[seq(from = nrow(overview)/2+1, to = nrow(overview)),]
  ) 

colnames(overview_side) = rep(c("Stock","Start","End date", "Obs", paste("Obs bef.",pre_covid_end_date)), times = 2)  

 print(
   xtable(overview_side,
         caption = c(paste("Overview of start date, end date, number of observations available and number of observations 
                     in the training set", sep =""), 
                     "Overview"), 
         label = "Table:Overview", 
         auto = TRUE
  ),
  file = "Outputs/Overview.tex", 
)



##############################################
### Mean square error, mean absolute error ###
############################################## 

MSE_e_output = t(as.data.frame(MSE_e)) 
MSE_r_output = t(as.data.frame(MSE_r)) 
MAE_e_output = t(as.data.frame(MAE_e)) 
MAE_r_output = t(as.data.frame(MAE_r)) 

colnames(MSE_e_output) = c("AR(1)-RV", "HAR", "HAR-AS", "HAR-RSV", "HAR-RSRK", "RGARCH", "GARCH")
colnames(MSE_r_output) = c("AR(1)-RV", "HAR", "HAR-AS", "HAR-RSV", "HAR-RSRK", "RGARCH", "GARCH")  
colnames(MAE_e_output) = c("AR(1)-RV", "HAR", "HAR-AS", "HAR-RSV", "HAR-RSRK", "RGARCH", "GARCH")  
colnames(MAE_r_output) = c("AR(1)-RV", "HAR", "HAR-AS", "HAR-RSV", "HAR-RSRK", "RGARCH", "GARCH")  

erroroutputs = list(MSE_e_output, MSE_r_output, MAE_e_output, MAE_r_output)

titles = c("MSE expanding window", "MSE rolling window", "MAE expanding window", "MAE rolling window")

pdf(file = "Plots/Errors.pdf", width = 16, height = 12) 

par(mfrow = c(2,2))
for(varno in seq(from = 1, to = 4, by = 1)){
  #  pdf(file =   paste("Plots/",str_replace_all(titles[varno], " ","_"),".pdf", sep =""), width = 16, height = 12)  
  plotvar = erroroutputs[[varno]]

  boxplot(plotvar, main=titles[varno], outline = FALSE, cex.main = 2) 

  #  dev.off() 
}

par(mfrow = c(1, 1))

dev.off() 


MSE_r_output = t(as.data.frame(MSE_r)) 
colnames(MSE_r_output)  <- c("AR(1)-RV", "HAR","HAR-AS", "HAR-RS", "HAR-RSRK", "RGARCH", "GARCH")

MSE_e_output = t(as.data.frame(MSE_e)) 
colnames(MSE_e_output)  <- c("AR(1)-RV", "HAR","HAR-AS", "HAR-RS", "HAR-RSRK", "RGARCH", "GARCH")

MAE_r_output = t(as.data.frame(MAE_r)) 
colnames(MAE_r_output)  <- c("AR(1)-RV", "HAR","HAR-AS", "HAR-RS", "HAR-RSRK", "RGARCH", "GARCH")

MAE_e_output = t(as.data.frame(MAE_e)) 
colnames(MAE_e_output)  <- c("AR(1)-RV", "HAR","HAR-AS", "HAR-RS", "HAR-RSRK", "RGARCH", "GARCH")

better_MSE = ifelse(MSE_r_output > MSE_e_output, "Expanding", "Rolling")
better_MAE = ifelse(MAE_r_output > MAE_e_output, "Expanding", "Rolling")

print(
 xtable(
as.data.frame(
rbind(
colSums(better_MSE == "Rolling"),
colSums(better_MSE == "Expanding")
),
row.names = c("Rolling","Expanding")
), 
caption = c("Better performing forecasting scheme for each model according to MSE",
"Better scheme MSE summary"), 
label = "Table:Better_MSE_summary"
 ),
 file = "Outputs/Better_MSE_summary.tex"
)

print(
 xtable(
as.data.frame(
rbind(
colSums(better_MAE == "Rolling"),
colSums(better_MAE == "Expanding")
),
row.names = c("Rolling","Expanding")
), 
caption = c("Better performing forecasting scheme for each model according to MAE",
               "Better scheme MAE summary"), 
   label = "Table:Better_MAE_summary"
 ), 
 file = "Outputs/Better_MAE_summary.tex"
)



###########################################
### Mincer-Zarnowitz regression results ###
###########################################

mincer_p_vals = 
  t(
    as.data.frame(
      lapply(mincer, FUN = function(x) {#paste(
        #        round(x[,1], digits = 2), 
        #        round(x[,2], digits = 2), 
        #        round(x[,3], digits = 2), 
        x[,4]
        #        )
      })))


outputcolnames = c("AR(1)-RV", "AR(1)-RV", "HAR", "HAR", "HAR-AS", "HAR-AS", "HAR-RSV", "HAR-RSV", 
                   "HAR-RSRK", "HAR-RSRK", "RGARCH", "RGARCH", "GARCH", "GARCH")

colnames(mincer_p_vals) = rownames(mincer[[1]])
mincer_p_vals_e = mincer_p_vals[, seq(from = 1, to = 14, by= 2)]
mincer_p_vals_r = mincer_p_vals[, seq(from = 2, to = 14, by= 2)]

mincer_R_squared = 
  t(
    as.data.frame(
      lapply(mincer, FUN = function(x) {#paste(
        #        round(x[,1], digits = 2), 
        #        round(x[,2], digits = 2), 
        #        round(x[,3], digits = 2), 
        x[,5]
        #        )
      })))

colnames(mincer_R_squared) = rownames(mincer[[1]])
mincer_R_squared_e = mincer_R_squared[, seq(from = 1, to = 14, by= 2)]
mincer_R_squared_r = mincer_R_squared[, seq(from = 2, to = 14, by= 2)]

pdf(file = "Plots/MZreg.pdf", width = 16, height = 12)

par(mfrow = c(2,2))

boxplot(mincer_p_vals_e, outline = FALSE, xaxt = 'n', main = "MZ regression p-value of expanding forecasts", cex.main = 1.6)
abline(h = 0.05, col = "red", lwd = 2, lty = 2) 
axis(1, at=1:7, labels=FALSE, xaxt ='n')
text(x=1:7, y=par("usr")[3] , labels=outputcolnames[seq(from = 1, to = 14, by = 2)], srt=90, adj=1, xpd=TRUE, cex=1.1)

boxplot(mincer_p_vals_r, outline = FALSE, xaxt = 'n', main = "MZ regression p-value of rolling forecasts", cex.main = 1.6)
abline(h = 0.05, col = "red", lwd = 2, lty = 2) 
axis(1, at=1:7, labels=FALSE, xaxt ='n')
text(x=1:7, y=par("usr")[3] , labels=outputcolnames[seq(from = 2, to = 14, by = 2)], srt=90, adj=1, xpd=TRUE, cex=1.1)

boxplot(mincer_R_squared_e, outline = FALSE, xaxt = 'n', main = "MZ regression R-squared of expanding forecasts", cex.main = 1.6)
#abline(h = 0.05, col = "red", lwd = 2, lty = 2) 
axis(1, at=1:7, labels=FALSE, xaxt ='n')
text(x=1:7, y=par("usr")[3] , labels=outputcolnames[seq(from = 1, to = 14, by = 2)], srt=90, adj=1, xpd=TRUE, cex=1.1)

boxplot(mincer_R_squared_r, outline = FALSE, xaxt = 'n', main = "MZ regression R-squared of rolling forecasts", cex.main = 1.6)
#abline(h = 0.05, col = "red", lwd = 2, lty = 2) 
axis(1, at=1:7, labels=FALSE, xaxt ='n')
text(x=1:7, y=par("usr")[3] , labels=outputcolnames[seq(from = 2, to = 14, by = 2)], srt=90, adj=1, xpd=TRUE, cex=1.1)

dev.off()

par(mfrow = c(1,1))




####################################
### Diebold-Mariano test results ### 
####################################

modelnames = c("AR(1)-RV","HAR","HAR-AS","HAR-RSV","HAR-RSRK","RGARCH","GARCH")

pdf(file = "Plots/DMpval.pdf", width = 8.27, height = 11.69) 

par(mfrow = c(2,4))

for(i1 in seq(from = 1, to =7)){
  modelselection = seq(from = 1, to = 7)[-i1] 
  DM_box_data_e = data.frame(matrix(rep(NA, times = 7*nrow(stocks)),
                                  ncol = nrow(stocks))) 
  for(i2 in modelselection){
    DM_box_data_e[i2,] = unlist(DMresults_e[[i1]][[i2]])
  }
  rownames(DM_box_data_e) = modelnames 
  par(bty = 'n')
  boxplot(t(DM_box_data_e)[,modelselection], ylim = c(0,1), 
          main = modelnames[i1], xaxt = 'n', cex.main = 2, yaxt = 'n', bty = 'n') 
  abline(h = 0.05, col = "red", lwd = 4, lty = 2) 
  abline(h = 0.95, col = "red", lwd = 4, lty = 2) 
  axis(1, at=1:6, labels=FALSE, xaxt ='n')
  axis(2, at=seq(from = 0, to = 1, by = 0.05), labels=seq(from = 0, to = 1, by = 0.05), xaxt ='n')
  text(x=1:6, y=par("usr")[3]+0.025 , labels=modelnames[modelselection], srt=90, adj=1, xpd=TRUE, cex=1.3)
}

dev.off() 



############################################
### VaR hit rate and backtesting results ###
############################################

VaRresults = list() 
Backtests = list() 

VaRresults_output_1 = VaR(0.1)
VaRresults_output_2 = VaR(0.05)
VaRresults_output_3 = VaR(0.01)

VarResults = list(VaRresults_output_1, VaRresults_output_2, VaRresults_output_3)

printbacktesting <- function(VaRresults, scheme, filename){
  siglevels = c(0.1, 0.05, 0.01)
  
  outputcolnames = c("AR(1)-RV", "AR(1)-RV", "HAR", "HAR", "HAR-AS", "HAR-AS", "HAR-RSV", "HAR-RSV", 
                     "HAR-RSRK", "HAR-RSRK", "RGARCH", "RGARCH", "GARCH", "GARCH")
  
  
  pdf(file = filename, width = 8.27, height = 11.29)
  par(mfrow =c(4,3))
  
  for(measure in seq(from = 2, to = 5, by = 1)){
    counter = 1 
    for(VaRresults_output in VarResults){
    # VaRresults_output = VaR(VaRalpha)
    
      output_e = data.frame(matrix(rep(NA, times =  7*nrow(stocks)), ncol = 7)) 
      output_r = data.frame(matrix(rep(NA, times =  7*nrow(stocks)), ncol = 7)) 
      
      colnames(output_e) = outputcolnames[seq(from = 1, to = 14, by =2)]
      colnames(output_r) = outputcolnames[seq(from = 2, to = 14, by =2)]
      rownames(output_e) = stocks$stockname
      rownames(output_r) = stocks$stockname
      
      for(i in seq(from = 1, to = 14, by =2)){ 
        output_e[,(i+1)/2]  = 
          unlist(
            lapply(
              VaRresults_output, FUN = function(x){x[i, measure]}
            )
          )
      }
      
      for(i in seq(from = 2, to = 14, by =2)){ 
        output_r[,i/2]= 
          unlist(
            lapply(
              VaRresults_output, FUN = function(x){x[i, measure]}
            )
          )
      }
      
      if(scheme == "expanding"){
        plotoutput = output_e 
      } else if (scheme == "rolling"){
        plotoutput = output_r 
      }
      
      if(measure == 5){
        boxplot(plotoutput, outline  = FALSE, main = colnames(VaRresults_output[[1]])[measure], cex.main = 2, xaxt = 'n', ylim = c(0, 0.12))
      }
      else{
        boxplot(plotoutput, outline  = FALSE, main = colnames(VaRresults_output[[1]])[measure], cex.main = 2, xaxt = 'n', ylim = c(0, 1))
      }
      
      axis(1, at=1:7, labels=FALSE, xaxt ='n')
      text(x=1:7, y=par("usr")[3] , labels=colnames(plotoutput), srt=90, adj=1, xpd=TRUE, cex=1.2)
      
      if(measure == 5){
        abline(h = siglevels[counter], col = "green", lty = 2, lwd = 2)
      }
      else{
        abline(h = 0.05, col = "red", lty = 2, lwd = 2)
      }
      counter = counter + 1
    }
  }
  par(mfrow =c(1,1))
  dev.off() 
}

printbacktesting(VaRresults = VaRresults, scheme = "rolling", filename = "Plots/Backtesting_r.pdf")
printbacktesting(VaRresults = VaRresults, scheme = "expanding", filename = "Plots/Backtesting_e.pdf")

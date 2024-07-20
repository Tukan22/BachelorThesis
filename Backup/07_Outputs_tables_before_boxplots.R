
###############################
### Plot of mean vs. stddev ###
###############################

plot(
  x = unlist(lapply(allstocks, FUN = function(x){mean(x$ret)})), 
  y = unlist(lapply(allstocks, FUN = function(x){sd(x$ret)})), 
  xlab = "Mean", ylab = "Standard deviation"
)
model = lm(unlist(lapply(allstocks, FUN = function(x){sd(x$ret)}))~unlist(lapply(allstocks, FUN = function(x){mean(x$ret)})), )




##############################################
### Jarque-Bera test p-values on residuals ###
##############################################

JBpvals_out = 
  as.data.frame(
    stocks[,c(
      which(names(stocks)=="JBpval_AR1_RV_resid"),
      which(names(stocks)=="JBpval_HAR_resid"),
      which(names(stocks)=="JBpval_HAR_AS_resid"),
      which(names(stocks)=="JBpval_HAR_RS_resid"),
      which(names(stocks)=="JBpval_HAR_RSRK_resid"),
      which(names(stocks)=="JBpval_RGARCH_resid"),
      which(names(stocks)=="JBpval_ARMAGARCH_resid")
    )],
    row.names = stocks$stockname
  )

colnames(JBpvals_out) = c("AR1-RV", "HAR", "HAR-AS", "HAR-RS", "HAR-RSRK", "RGARCH", "GARCH") 

print(
  xtable(cbind(
    JBpvals_out[seq(from = 1, to = nrow(JBpvals_out)/2),] 
  ),
  caption = c("This table shows the p-values of the Jarque-Bera test of normality for residuals 
              of all 7 models for the first half of stocks.", 
              "p-values of Jarque-Bera test on model residuals (1)"), 
  label = "Table:JBresid_p_vals_1"
  ),
  file = "Outputs/JBresid_p_vals_1.tex"
)

print(
  xtable(cbind(
    JBpvals_out[seq(from = nrow(JBpvals_out)/2+1, to = nrow(JBpvals_out)),]
  ),
  caption = c("This table shows the p-values of the Jarque-Bera test of normality for residuals 
              of all 7 models for the second half of stocks.", 
              "p-values of Jarque-Bera test on model residuals (2)"), 
  label = "Table:JBresid_p_vals_2"
  ),
  file = "Outputs/JBresid_p_vals_2.tex"
)



################################
### Ljung-Box test on returns ##
################################

pdf(file = "Plots/LBpstat.pdf", width = 16, height = 12) 

plot(stocks$LBp_val, type="o", col="blue", xaxt="n", xlab="Stock", ylab="p-value", main="Ljung-Box test p-statistic", 
     ylim = c(0 ,max(stocks$LBp_val)*1.1))
abline(h = 0.05, col = "red", lwd = 2, lty = 2)

# Add the custom x-axis labels, rotate them 90 degrees, and make them smaller
axis(1, at=1:length(stocks$LBp_val), labels=FALSE, xaxt ='n')
# indent = 10^(round(log(mean(plotvar), base = 10))-2)*5  
text(x=1:nrow(stocks), y=par("usr")[3] , labels=rownames(MSE_e_output), srt=90, adj=1, xpd=TRUE, cex=1)

# Add a grid for better readability
grid(nx = nrow(stocks) + 5)
legend("topleft", legend = c("Ljung-Box p-stat","0.05"), col = c("blue","red"), lty = c(1,2), lwd = 1)

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
         caption = c(paste("This table shows the overview of start date, end date, number of observations and number of observations 
                     for the before covid training set, i. e. observations prior to ",pre_covid_end_date, sep =""), 
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


colors = c("black","red","green","blue","orange","darkgreen", "cyan")
ltys = c("solid", "solid", "dashed","dotdash","longdash","twodash")
titles = c("MSE of expanding forecast", "MSE of rolling forecast", "MAE of expanding forecast", "MAE of rolling forecast")
ylabs = c("MSE","MSE","MAE","MAE")
erroroutputs = list(MSE_e_output, MSE_r_output, MAE_e_output, MAE_r_output)

for(varno in seq(from = 1, to = 4, by = 1)){
#  pdf(file =   paste("Plots/",str_replace_all(titles[varno], " ","_"),".pdf", sep =""), width = 16, height = 12)  
  
  plotvar = erroroutputs[[varno]]
  # Create a plot without the x-axis
  plot(plotvar[,1], type="l", col="black", xaxt="n", xlab="Custom Labels", ylab=ylabs[varno], main=titles[varno], ylim = c(0 ,max(plotvar)*1.1))
  
  for(i in seq(from = 2, to = ncol(plotvar), by = 1)){
    lines(plotvar[,i], type="l", col=colors[i]) 
  }
  
  # Add the custom x-axis labels, rotate them 90 degrees, and make them smaller
  axis(1, at=1:nrow(stocks), labels=FALSE, xaxt ='n')
  indent = 10^(round(log(mean(plotvar), base = 10))-2)*5  
  text(x=1:nrow(stocks), y=par("usr")[3] , labels=rownames(MSE_e_output), srt=90, adj=1, xpd=TRUE, cex=1)
  
  # Add a grid for better readability
  grid(nx = nrow(stocks) + 5)
  legend(x = 0, 
         y = max(plotvar)*1.1, 
         legend = colnames(plotvar), 
         col = colors, 
         lty = ltys, 
         cex = 1
  ),
#  dev.off() 
}



par(mfrow = c(2, 2))

for(varno in seq(from = 1, to = 4, by = 1)){
  #  pdf(file =   paste("Plots/",str_replace_all(titles[varno], " ","_"),".pdf", sep =""), width = 16, height = 12)  
  plotvar = erroroutputs[[varno]]

  boxplot(plotvar, main=titles[varno], outline = FALSE)  

  #  dev.off() 
}

par(mfrow = c(1, 1))






errmeans = rbind(
  apply(MSE_e_output, MARGIN = 2, FUN = mean), 
  apply(MSE_r_output, MARGIN = 2, FUN = mean), 
  apply(MAE_e_output, MARGIN = 2, FUN = mean), 
  apply(MAE_r_output, MARGIN = 2, FUN = mean) 
)

rownames(errmeans) = c("MSE expanding", "MSE rolling", "MAE expanding", "MAE rolling")
errmeans_text = apply(errmeans, MARGIN = 2, FUN = function(x){sprintf("%.5f", x)} )

errsds = rbind(
  apply(MSE_e_output, MARGIN = 2, FUN = sd), 
  apply(MSE_r_output, MARGIN = 2, FUN = sd), 
  apply(MAE_e_output, MARGIN = 2, FUN = sd), 
  apply(MAE_r_output, MARGIN = 2, FUN = sd) 
)
rownames(errsds) = c("MSE expanding", "MSE rolling", "MAE expanding", "MAE rolling")

errorders = t(apply(errmeans, MARGIN = 1, FUN = rank)) 
errorders_text = t(apply(errorders, MARGIN = 1, FUN = as.character)) 

err_means_order_text = matrix(paste(matrix(errmeans_text), " (", errorders_text, ")", sep = ""), nrow = nrow(errmeans_text)) 
rownames(err_means_order_text) = rownames(errmeans)
colnames(err_means_order_text) = colnames(errmeans) 

print(
  xtable(
    err_means_order_text, 
    caption = c("This table shows the means of error measures for each model with its rank (smallest to largest) for each respective error measure and forecasting window in parentheses. ", 
                "Error measures means"), 
    label = "Table:Error_means", 
    digits = 5 
  ), 
  file = "Outputs/Error_means.tex" 
)

print(
  xtable(
    errsds, 
    caption = c("This table shows the standard deviations of error measures for each model. ", 
                "Error measures stddevs"), 
    label = "Table:Error_stddevs", 
    digits = 5 
  ), 
  file = "Outputs/Error_stddevs.tex" 
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

mincer_p_vals_e = mincer_p_vals[,c(1,3,5,7,9,11,13)]
mincer_p_vals_r = mincer_p_vals[,c(2,4,6,8,10,12,14)]
colnames(mincer_p_vals_e) = c("AR(1)-RV", "HAR", "HAR-AS", "HAR-RSV", "HAR-RSRK", "RGARCH", "GARCH")  
colnames(mincer_p_vals_r) = c("AR(1)-RV", "HAR", "HAR-AS", "HAR-RSV", "HAR-RSRK", "RGARCH", "GARCH")

mincer_p_vals_summary = t(
  rbind(
    apply(mincer_p_vals, MARGIN = 2, FUN = mean), 
    apply(mincer_p_vals, MARGIN = 2, FUN = sd), 
    apply(mincer_p_vals, MARGIN = 2, FUN = function(x){sum(x<0.05)})/nrow(mincer_p_vals) 
  )
)
colnames(mincer_p_vals_summary) = c("Mean", "StdDev", "% p-vals <0.05")
rownames(mincer_p_vals_summary) = rownames(mincer[[1]])

print(
  xtable(
    mincer_p_vals_summary, 
    caption = c("This table shows the means and stddevs of Mincer-Zarnowitz p-values, as well as the ratio of cases in which the t-value is lower than 0.05 (i. e. the ratio of biased tests). ", 
                  "Mincer-Zarnowitz p-values"), 
    label = "Table:MZpvals", 
    digits = 5 
  ), 
  file = "Outputs/MZpvals.tex" 
)



####################################
### Diebold-Mariano test results ### 
####################################

print(
  xtable(
    DM_output("mean"), 
    caption = c("This table shows the means of p-values of the Diebold-Mariano test for respective combinations of models.
                The values below the diagonal are for rolling window forecast, the values above the diagonal are for the expanding window forecast.", 
      "DM test means"), 
    label = "Table:DM_test_mean", 
    digits = 2 
  ), 
  file = "Outputs/DM_test_mean.tex" 
)

print(
  xtable(
    DM_output("sd"), 
    caption = c("This table shows the standard deviations of p-values of the Diebold-Mariano test for respective combinations of models. 
                The values below the diagonal are for rolling window forecast, the values above the diagonal are for the expanding window forecast.", 
                "DM test standard deviations"), 
    label = "Table:DM_test_SD", 
    digits = 2 
  ), 
  file = "Outputs/DM_test_SD.tex" 
)

print(
  xtable(
    DM_output("threshold"), 
    caption = c("This table shows the the percentage for how many stocks the p-value of the Diebold-Mariano test was below 0.05 for respective combinations of models.
                The values below the diagonal are for rolling window forecast, the values above the diagonal are for the expanding window forecast.", 
                "DM test below 0.05"), 
    label = "Table:DM_test_threshold", 
    digits = 2 
  ), 
  file = "Outputs/DM_test_threshold.tex" 
)



# colnames(VaRresults_output) = c("AR(1)-RV expanding", "AR(1)-RV rolling", "HAR expanding", "HAR rolling", "HAR-AS expanding", 
#                         "HAR-AS rolling", "HAR-RSV expanding", "HAR-RSV rolling", "HAR-RSRK expanding", "HAR-RSRK rolling", 
#                         "RGARCH expanding", "RGARCH rolling", "GARCH expanding", "GARCH rolling")



#############################################
### VaR hit ratio and backtesting results ###
#############################################

VaRresults = list() 
Backtests = list() 

MeanVarValues_2 = data.frame(matrix(rep(NA, times = 14), ncol = 1))

#  for(VaRalpha in c(0.1, 0.05, 0.01)){

for(VaRalpha in c(0.1)){
  VaRresults_output = VaR(VaRalpha)
  
  meanVaRvalues = data.frame(matrix(rep(NA, times = 4*14), ncol = 4))
  
  for(model in seq(from = 1, to = 14, by = 1)){
    for(col in seq(from = 2, to = 5, step = 1)){
      
      meanVaRvalues[model, col-1] = 
        mean(unlist(lapply(VaRresults_output, FUN = function(x){
          lapply(x[col],  FUN = function(y){y[model]})
        })))
    }
  }
  

  lapply(
    VaRresults_output, FUN = function(x){
    lapply(x[col],  FUN = function(y){y[model]})
  })
  
  lapply(VaRresults_output[["XOM"]][col],  FUN = function(y){y[model]})
  
  
  
  colnames(meanVaRvalues) = colnames(VaRresults_output[[1]])[2:5] 
  rownames(meanVaRvalues) = c("AR(1)-RV expanding", "AR(1)-RV rolling", "HAR expanding", "HAR rolling", 
                              "HAR-AS expanding", "HAR-AS rolling", "HAR-RSV expanding", "HAR-RSV rolling", 
                              "HAR-RSRK expanding", "HAR-RSRK rolling", "RGARCH expanding", "RGARCH rolling", 
                              "GARCH expanding", "GARCH rolling")
    
  MeanVarValues_2 = cbind(MeanVarValues_2, meanVaRvalues)  
}

MeanVarValues_2 = MeanVarValues_2[-1]

varorders = round(apply(MeanVarValues_2, MARGIN = 2, FUN = rank)) 

varmeans_text = apply(MeanVarValues_2, MARGIN = 2, FUN = function(x){sprintf("%.5f", x)} )

varorders_text = t(apply(varorders, MARGIN = 1, FUN = as.character)) 

var_means_order_text = matrix(paste(matrix(varmeans_text), " (", varorders_text, ")", sep = ""), nrow = nrow(varmeans_text)) 

colnames(var_means_order_text) = colnames(varorders)
rownames(var_means_order_text) = rownames(varorders)


print(
  xtable(
    var_means_order_text[,c(1,5,9)], 
    caption = c("This table shows the means of Kupiec's test p-values for each model and forecasting window for each respective VaR level. In parentheses is the rank of the value across all models. ", 
                "Kupiec's test means"), 
    label = "Table:VaR_means_Kupiec", 
    digits = 5 
  ), 
  file = "Outputs/VaR_means_Kupiec.tex" 
)

print(
  xtable(
    var_means_order_text[,c(2,6,10)], 
    caption = c("This table shows the means of Christoffersen's test p-values for each model and forecasting window for each respective VaR level. In parentheses is the rank of the value across all models. ", 
                "Christoffersen's test means"), 
    label = "Table:VaR_means_Christoffersen", 
    digits = 5 
  ), 
  file = "Outputs/VaR_means_Christoffersen.tex" 
)

print(
  xtable(
    var_means_order_text[,c(3,7,11)], 
    caption = c("This table shows the means of Dynamic quantile test p-values for each model and forecasting window for each respective VaR level. In parentheses is the rank of the value across all models. ", 
                "DQ test means"), 
    label = "Table:VaR_means_DQ", 
    digits = 5 
  ), 
  file = "Outputs/VaR_means_DQ.tex" 
)

print(
  xtable(
    var_means_order_text[,c(4,8,12)], 
    caption = c("This table shows the means of hit rate for each model and forecasting window for each respective VaR level. In parentheses is the rank of the value across all models. ", 
                "Hit ratio means"), 
    label = "Table:VaR_means_HitRate", 
    digits = 5 
  ), 
  file = "Outputs/VaR_means_HitRate.tex" 
)








par(mfrow =c(3,4))

for(measure in seq(from = 2, to = 5, step = 1)){
  output = data.frame(matrix(rep(NA, times =  14*nrow(stocks)), ncol = 14)) 
  
  
  colnames(output) = c("AR(1)-RV exp.", "AR(1)-RV roll.", "HAR exp.", "HAR roll.", 
                       "HAR-AS exp.", "HAR-AS roll.", "HAR-RSV exp.", "HAR-RSV roll.", 
                       "HAR-RSRK exp.", "HAR-RSRK roll.", "RGARCH exp.", "RGARCH roll.", 
                       "GARCH exp.", "GARCH roll.")
  
  for(i in seq(from = 1, to = 14, step =2)){ 
    output[,i]  = 
      unlist(
        lapply(
          VaRresults_output, FUN = function(x){x[i, measure]}
        )
      )
  }
  
  boxplot(output, outline  = FALSE, main = colnames(VaRresults_output[[1]])[measure])
}



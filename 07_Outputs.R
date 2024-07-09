# Print a table of Jarque-Bera test p-values on residuals of each model 

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


# Print a table of p-values for Augmented Dickey-Fuller test, Ljung-Box test, Jarque-Bera test on returns 

rets_out = 
  as.data.frame(
    stocks[,c(
      which(names(stocks)=="JBpval_rets"),
      which(names(stocks)=="ADFp_val"),
      which(names(stocks)=="LBp_val")
    )],
    row.names = stocks$stockname
  )
colnames(rets_out) = c("Jarque-bera test","Augmented Dickey-Fuller test", "LJung-Box test") 

print(
  xtable(rets_out[seq(from = 1, to = nrow(rets_out)/2),],
         caption = c("This table shows the p-values of the Jarque-Bera test, Augmented Dickey-Fuller test and Ljung-Box test 
                     on returns for the first half of stocks. For the ADF test, a value of $0.01$ means in fact that the p-value is $<0.01$", 
                     "JB test, ADF test, LB text on returns. (1)"), 
         label = "Table:Rets_p_vals_1"
  ),
  file = "Outputs/Rets_p_vals_1.tex"
)

print(
  xtable(rets_out[seq(from = nrow(rets_out)/2+1, to = nrow(rets_out)),],
         caption = c("This table shows the p-values of the Jarque-Bera test, Augmented Dickey-Fuller test and Ljung-Box test 
                     on returns for the second half of stocks. For the ADF test, a value of $0.01$ means in fact that the p-value is $<0.01$", 
                     "JB test, ADF test, LB text on returns. (2)"), 
         label = "Table:Rets_p_vals_2"
  ),
  file = "Outputs/Rets_p_vals_2.tex"
)

 

# Print basic overview of used stocks 


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


print(
  xtable(overview[seq(from = 1, to = nrow(overview)/2),],
         caption = c(paste("This table shows the overview for the first half of stocks of basic end date, start date, number of observations and number of observations 
                     for the before covid training set, i. e. observations prior to ",pre_covid_end_date, sep =""), 
                     "Overview_1"), 
         label = "Table:Overview_1", 
         auto = TRUE
  ),
  file = "Outputs/Overview_1.tex"
)

print(
  xtable(overview[seq(from = nrow(overview)/2+1, to = nrow(overview)),],
         caption = c(paste("This table shows the overview for the second half of stocks of basic end date, start date, number of observations and number of observations 
                     for the before covid training set, i. e. observations prior to ",pre_covid_end_date, sep =""), 
                     "Overview_2"), 
         label = "Table:Overview_2", 
         auto = TRUE
  ),
  file = "Outputs/Overview_2.tex"
)


# TODO Now: MSE, MAE - probably put into one table a check whether the better scheme (expanding/rolling) is the same for both types of errors 

### Mean square error 
## Expanding scheme 

MSE_e_output = t(as.data.frame(MSE_e)) 
colnames(MSE_e_output)  <- c("AR(1)-RV", "HAR","HAR-AS", "HAR-RS", "HAR-RSRK", "RGARCH", "GARCH")

print(
  xtable(
    1000*MSE_e_output[seq(from = 1, to = nrow(MSE_e_output)/2),],
         caption = c("This table shows the mean square errors of expanding forecast scheme for each type of model for the first half of stocks. 
                     The values are multiplied by 1000", 
                     "MSE expanding forecast (1)"), 
         label = "Table:MSE_e_1", 
         digits = 3 
  ),
  file = "Outputs/MSE_e_1.tex"
)

print(
  xtable(
    1000*MSE_e_output[seq(from = nrow(MSE_e_output)/2+1, to = nrow(MSE_e_output)),],
    caption = c("This table shows the mean square errors of expanding forecast scheme for each type of model for the second half of stocks. 
                     The values are multiplied by 1000", 
                "MSE expanding forecast (2)"), 
    label = "Table:MSE_e_2", 
    digits = 3 
  ),
  file = "Outputs/MSE_e_2.tex"
)

## Rolling scheme 

MSE_r_output = t(as.data.frame(MSE_r)) 
colnames(MSE_r_output)  <- c("AR(1)-RV", "HAR","HAR-AS", "HAR-RS", "HAR-RSRK", "RGARCH", "GARCH")

print(
  xtable(
    1000*MSE_r_output[seq(from = 1, to = nrow(MSE_r_output)/2),],
    caption = c("This table shows the mean square errors of rolling forecast scheme for each type of model for the first half of stocks. 
                     The values are multiplied by 1000", 
                "MSE rolling forecast (1)"), 
    label = "Table:MSE_r_1", 
    digits = 3 
  ),
  file = "Outputs/MSE_r_1.tex"
)

print(
  xtable(
    1000*MSE_e_output[seq(from = nrow(MSE_r_output)/2+1, to = nrow(MSE_r_output)),],
    caption = c("This table shows the mean square errors of rolling forecast scheme for each type of model for the second half of stocks. 
                     The values are multiplied by 1000", 
                "MSE rolling forecast (2)"), 
    label = "Table:MSE_r_2", 
    digits = 3 
  ),
  file = "Outputs/MSE_r_2.tex"
)


### Mean absolute error 
## Expanding scheme 

MAE_e_output = t(as.data.frame(MAE_e)) 
colnames(MAE_e_output)  <- c("AR(1)-RV", "HAR","HAR-AS", "HAR-RS", "HAR-RSRK", "RGARCH", "GARCH")

print(
  xtable(
    1000*MAE_e_output[seq(from = 1, to = nrow(MAE_e_output)/2),],
    caption = c("This table shows the mean absolute errors of expanding forecast scheme for each type of model for the first half of stocks. 
                     The values are multiplied by 1000", 
                "MAE expanding forecast (1)"), 
    label = "Table:MAE_e_1", 
    digits = 3 
  ),
  file = "Outputs/MAE_e_1.tex"
)

print(
  xtable(
    1000*MAE_e_output[seq(from = nrow(MAE_e_output)/2+1, to = nrow(MAE_e_output)),],
    caption = c("This table shows the mean absolute errors of expanding forecast scheme for each type of model for the second half of stocks. 
                     The values are multiplied by 1000", 
                "MAE expanding forecast (2)"), 
    label = "Table:MAE_e_2", 
    digits = 3 
  ),
  file = "Outputs/MAE_e_2.tex"
)

## Rolling scheme 

MAE_r_output = t(as.data.frame(MAE_r)) 
colnames(MAE_r_output)  <- c("AR(1)-RV", "HAR","HAR-AS", "HAR-RS", "HAR-RSRK", "RGARCH", "GARCH")

print(
  xtable(
    1000*MAE_r_output[seq(from = 1, to = nrow(MAE_r_output)/2),],
    caption = c("This table shows the mean absolute errors of rolling forecast scheme for each type of model for the first half of stocks. 
                     The values are multiplied by 1000", 
                "MAE rolling forecast (1)"), 
    label = "Table:MAE_r_1", 
    digits = 3 
  ),
  file = "Outputs/MAE_r_1.tex"
)

print(
  xtable(
    1000*MAE_e_output[seq(from = nrow(MAE_r_output)/2+1, to = nrow(MAE_r_output)),],
    caption = c("This table shows the mean absolute errors of rolling forecast scheme for each type of model for the second half of stocks. 
                     The values are multiplied by 1000", 
                "MAE rolling forecast (2)"), 
    label = "Table:MAE_r_2", 
    digits = 3 
  ),
  file = "Outputs/MAE_r_2.tex"
)



### Better forecasting scheme according to MSE 

better_MSE = ifelse(MSE_r_output > MSE_e_output, "Expanding", "Rolling")

print(
  xtable(
    better_MSE[seq(from = 1, to = nrow(better_MSE)/2),], 
    caption = c("This table shows which forecasting scheme performs better on the training set, according to mean square error 
              for each model and the first half of stocks.",
                "Better scheme MSE (1)"),
    label = "Table:Better_MSE_1"
  ), 
  file = "Outputs/Better_MSE_1.tex"
)


print(
  xtable(
    better_MSE[seq(from = nrow(better_MSE)/2+1, to = nrow(better_MSE)),], 
    caption = c("This table shows which forecasting scheme performs better on the training set, according to mean square error
              for each model and the second half of stocks.",
                "Better scheme MSE (2)"),
    label = "Table:Better_MSE_2"
  ), 
  file = "Outputs/Better_MSE_2.tex"
)


### Better forecasting scheme according to MAE 

better_MAE = ifelse(MAE_r_output > MAE_e_output, "Expanding", "Rolling")

print(
  xtable(
    better_MAE[seq(from = 1, to = nrow(better_MAE)/2),], 
    caption = c("This table shows which forecasting scheme performs better on the training set, according to mean absolute error 
              for each model and the first half of stocks.",
                "Better scheme MAE (1)"),
    label = "Table:Better_MAE_1"
  ), 
  file = "Outputs/Better_MAE_1.tex"
)


print(
  xtable(
    better_MAE[seq(from = nrow(better_MAE)/2+1, to = nrow(better_MAE)),], 
    caption = c("This table shows which forecasting scheme performs better on the training set, according to mean absolute error
              for each model and the second half of stocks.",
                "Better scheme MAE (2)"),
    label = "Table:Better_MAE_2"
  ), 
  file = "Outputs/Better_MAE_2.tex"
)




print(
  xtable(
    as.data.frame(
      rbind(
        colSums(better_MSE == "Rolling"),
        colSums(better_MSE == "Expanding")
      ),
      row.names = c("Rolling","Expanding")
    ), 
    caption = c("This table shows a summary of how many stocks for each model perform better with expanding or rolling forecasting scheme according to mean square error. ",
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
    caption = c("This table shows a summary of how many stocks for each model perform better with expanding or rolling forecasting scheme according to mean absolute error. ",
                "Better scheme MAE summary"), 
    label = "Table:Better_MAE_summary"
  ), 
  file = "Outputs/Better_MAE_summary.tex"
)




### Mincer-Zarnowitz regression results 

# TODO: Not printed! 

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
    apply(mincer_p_vals, MARGIN = 2, FUN = sd)
  )
)
colnames(mincer_p_vals_summary) = c("Mean", "StdDev")
rownames(mincer_p_vals_summary) = rownames(mincer[[1]]) 





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



#





VaRresults_output = data.frame(
  matrix(
    unlist(lapply(VaRresults, FUN = function(x){x[2]})), 
    ncol = 14 
  )
) 

# colnames(VaRresults_output) = c("AR(1)-RV expanding", "AR(1)-RV rolling", "HAR expanding", "HAR rolling", "HAR-AS expanding", 
#                         "HAR-AS rolling", "HAR-RSV expanding", "HAR-RSV rolling", "HAR-RSRK expanding", "HAR-RSRK rolling", 
#                         "RGARCH expanding", "RGARCH rolling", "GARCH expanding", "GARCH rolling")

colnames(VaRresults_output) = c("AR(1)-RV", "AR(1)-RV", "HAR", "HAR", "HAR-AS", 
                                                         "HAR-AS", "HAR-RSV", "HAR-RSV", "HAR-RSRK", "HAR-RSRK", 
                                                         "RGARCH", "RGARCH", "GARCH", "GARCH")
                                
rownames(VaRresults_output) = stocks$stockname  




# TODO: This needs to be run 3 times for each level of alpha 

VaRresults = list() 
Backtests = list() 

for(VaRalpha in c(0.1, 0.05, 0.01)){
  VaR(VaRalpha)
  
  
  
  print(
    xtable(
      VaRresults_output[seq(from = 1, to = nrow(VaRresults_output)/2),
                        seq(from = 1, to = ncol(VaRresults_output), by = 2)], 
      caption = c(
        paste("This table shows the p-values of the Kuppiec test on ", (1-VaRalpha), " VaR computed using expanding forecast values of all 7 models for the first half of stocks.", sep = ""), 
        paste("Kupiec test p-values, alpha =", (1-VaRalpha), " (1)", sep = "")), 
      label = paste("Table:Kupiec_test_expanding_",(1-VaRalpha),"_1", sep = "")
    ), 
    file = paste("Outputs/Kupiec_p_vals_e_", (1-VaRalpha), "_1.tex", sep = "") 
  )
  
  print(
    xtable(
      VaRresults_output[seq(from = nrow(VaRresults_output)/2, to = nrow(VaRresults_output)),
                        seq(from = 1, to = ncol(VaRresults_output), by = 2)], 
      caption = c(
        paste("This table shows the p-values of the Kuppiec test on ", (1-VaRalpha), " VaR computed using expanding forecast values of all 7 models for the second half of stocks.", sep = ""), 
        paste("Kupiec test p-values, alpha =", (1-VaRalpha), " (2)", sep = "")), 
      label = paste("Table:Kupiec_test_expanding_",(1-VaRalpha),"_2", sep = "")
    ), 
    file = paste("Outputs/Kupiec_p_vals_e_", (1-VaRalpha), "_2.tex", sep = "") 
  )
  
  
  
  print(
    xtable(
      VaRresults_output[seq(from = 1, to = nrow(VaRresults_output)/2),
                        seq(from = 1, to = ncol(VaRresults_output), by = 2)], 
      caption = c(
        paste("This table shows the p-values of the Kuppiec test on ", (1-VaRalpha), " VaR computed using rolling forecast values of all 7 models for the first half of stocks.", sep = ""), 
        paste("Kupiec test p-values, alpha =", (1-VaRalpha), " (1)", sep = "")), 
      label = paste("Table:Kupiec_test_rolling_",(1-VaRalpha),"_1", sep = "")
    ), 
    file = paste("Outputs/Kupiec_p_vals_r_", (1-VaRalpha), "_1.tex", sep = "") 
  )
  
  print(
    xtable(
      VaRresults_output[seq(from = nrow(VaRresults_output)/2, to = nrow(VaRresults_output)),
                        seq(from = 1, to = ncol(VaRresults_output), by = 2)], 
      caption = c(
        paste("This table shows the p-values of the Kuppiec test on ", (1-VaRalpha), " VaR computed using rolling forecast values of all 7 models for the second half of stocks.", sep = ""), 
        paste("Kupiec test p-values, alpha =", (1-VaRalpha), " (2)", sep = "")), 
      label = paste("Table:Kupiec_test_rolling_",(1-VaRalpha),"_2", sep = "")
    ), 
    file = paste("Outputs/Kupiec_p_vals_r_", (1-VaRalpha), "_2.tex", sep = "") 
  )
  
  print(
    xtable(
      Kupiec_summary, 
      caption = c(
        paste("This table shows the summary statistics of the p-values of the Kuppiec test on ", (1-VaRalpha), " VaR. 
            The first column shows the mean of p-values, the second column the standard deviation 
            and the third column shows in how many cases the p-value was lower than 0.05, i. e. in how many cases the VaR computation was unsuccesful.",
              sep = ""), 
        paste("Kupiec test p-values summary, alpha =", (1-VaRalpha), sep = "")), 
      label = paste("Table:Kupiec_test_summary_",(1-VaRalpha), sep = ""), 
      digits = 0 
    ), 
    file = paste("Outputs/Kupiec_p_vals_summary_", (1-VaRalpha), ".tex", sep = "") 
  )
  
}






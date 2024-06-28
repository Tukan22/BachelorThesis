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




# Až to budu vizualizovat, tak DM testy vizualizovat ve 2D tabulce, ne jako to je v tom projektu 
# Třeba v horní části mít expanding a ve spodní rolling 
#  Update: DM testy pravděpodobně nějak zprůměrovat ? 


Diebold_e_all = t(as.data.frame(lapply(X = Diebold_e, FUN = function(x){x})))
rownames(Diebold_e_all) = stocks$stockname
Diebold_e_all_means = as.data.frame(apply(X = Diebold_e_all, MARGIN = 2, FUN = mean)) 

Diebold_r_all = t(as.data.frame(lapply(X = Diebold_r, FUN = function(x){x})))
rownames(Diebold_r_all) = stocks$stockname
Diebold_r_all_means = as.data.frame(apply(X = Diebold_r_all, MARGIN = 2, FUN = mean)) 

Diebold_e_output = as.data.frame(
  matrix(rep(NA, times = 49), 
         ncol = 7)
)
rownames(Diebold_e_output) = c("AR(1)-RV", "HAR", "HAR-AS", "HAR-RSV", "HAR-RSRK", "RGARCH", "GARCH")
colnames(Diebold_e_output) = c("AR(1)-RV", "HAR", "HAR-AS", "HAR-RSV", "HAR-RSRK", "RGARCH", "GARCH")

Diebold_e_output[1,] = c(rep("",times = 1), Diebold_e_all_means[1:6,])
Diebold_e_output[2,] = c(rep("",times = 2), Diebold_e_all_means[7:11,])
Diebold_e_output[3,] = c(rep("",times = 3), Diebold_e_all_means[12:15,])
Diebold_e_output[4,] = c(rep("",times = 4), Diebold_e_all_means[16:18,])
Diebold_e_output[5,] = c(rep("",times = 5), Diebold_e_all_means[19:20,])
Diebold_e_output[6,] = c(rep("",times = 6), Diebold_e_all_means[21,])
Diebold_e_output[7,] = rep("",times = 7)

Diebold_r_output = as.data.frame(
  matrix(rep(NA, times = 49), 
         ncol = 7)
)
rownames(Diebold_r_output) = c("AR(1)-RV", "HAR", "HAR-AS", "HAR-RSV", "HAR-RSRK", "RGARCH", "GARCH")
colnames(Diebold_r_output) = c("AR(1)-RV", "HAR", "HAR-AS", "HAR-RSV", "HAR-RSRK", "RGARCH", "GARCH")

Diebold_r_output[1,] = c(rep("", times = 1), Diebold_r_all_means[1:6,])
Diebold_r_output[2,] = c(rep("",times = 2), Diebold_r_all_means[7:11,])
Diebold_r_output[3,] = c(rep("",times = 3), Diebold_r_all_means[12:15,])
Diebold_r_output[4,] = c(rep("",times = 4), Diebold_r_all_means[16:18,])
Diebold_r_output[5,] = c(rep("",times = 5), Diebold_r_all_means[19:20,])
Diebold_r_output[6,] = c(rep("",times = 6), Diebold_r_all_means[21,])
Diebold_r_output[7,] = rep("",times = 7)

Diebold_r_output = t(Diebold_r_output)

# Diebold_output = Diebold_e_output + Diebold_r_output 

# Diebold_output[1,1] = "-"
# Diebold_output[2,2] = "-" 
# Diebold_output[3,3] = "-" 
# Diebold_output[4,4] = "-" 
# Diebold_output[5,5] = "-" 
# Diebold_output[6,6] = "-" 
# Diebold_output[7,7] = "-" 


# TODO print 
# TODO the same for SDs (data prepared) - make a function out of it with the function as a parameter, do not repeat it whole. 
# TODO: Both MZ and DM new when RGARCH data recalculated 
# TODO: The mean probably does not make much sese - maybe rather show in what percentage of cases the p-val is <0.05 for the specific configuration. 
# TODO !!! : The above/over diagonal scheme does not really make sense since the unidirectional relationship is inversed! keep it two tables 
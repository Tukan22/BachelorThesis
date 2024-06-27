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
colnames(JBpvals_out) = c("AR1-RV", "HAR", "HAR-AS", "HAR-RS", "HAR-RSRK", "RGARCH", "ARMAGARCH") 

print(
  xtable(JBpvals_out,
         caption = c("This table shows the p-values of the Jarque-Bera test of normality for residuals of all 7 models.", 
                     "p-values of Jarque-Bera test on model residuals"), 
         label = "Table:JBresid_p_vals"
  ),
  file = "Outputs/JBresid_p_vals.tex"
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
  xtable(rets_out,
         caption = c("This table shows the p-values of the Jarque-Bera test, Augmented Dickey-Fuller test and Ljung-Box test 
                     on returns for each stock. For the ADF test, a value of $0.01$ means in fact that the p-value is $<0.01$", 
                     "JB test, ADF test, LB text on returns."), 
         label = "Table:Rets_p_vals"
  ),
  file = "Outputs/Rets_p_vals.tex"
)



# Print basic overview of used stocks 

overview = 
  as.data.frame(
    stocks[,c(
      which(names(stocks)=="start_date"),
      which(names(stocks)=="end_date"), 
      which(names(stocks)=="obs"), 
      which(names(stocks)=="pre_covid_obs")
    )],
    row.names = stocks$stockname
  )

colnames(overview) = c("Start date","End date", "Observations", "Observations before covid") 

print(
  xtable(overview,
         caption = c(paste("This table shows the overview of basic end date, start date, number of observations and number of observations 
                     for the before covid training set, i. e. observations prior to ",pre_covid_end_date, sep =""), 
                     "Overview"), 
         label = "Table:Overview" 
  ),
  file = "Outputs/Overview.tex"
)



# Až to budu vizualitovat, tak DM testy vizualizovat ve 2D tabulce, ne jako to je v tom projektu 
# Třeba v horní části mít expanding a ve spodní rolling 
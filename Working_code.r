install.packages("aod")
install.packages("ggplot2")


install.packages("tseries")



install.packages("dplyr")
install.packages("aod")
install.packages("stargazer")
install.packages("xtable")
install.packages("stats")
install.packages("zoo")
install.packages("forecast")
install.packages("rugarch")
install.packages("xts")
install.packages("highfrequency")



library(graphics)
library(RColorBrewer)
library(ggplot2)
library(tseries)
library(dplyr)
library(aod)
library(stargazer)
library(xtable)
library(stats)
library(zoo)
library(forecast)
library(rugarch)
library(xts)
library(highfrequency)



# loading data, checking summaries 
load("Data/5.rData")
load("Data/stocks.rData")

sum(is.na(abc))
summary(abc)
dim(abc)
xtable(as.table(summary(abc)))

head(abc)


### Problem 1 ###

# histogram of distribution
for(name in names(abc)){
  hist(abc[,name], 50, prob = T, main = paste("Density of ", name, "vs Normal distribution"))
  x = seq(from = min(abc[,name]), to = max(abc[,name]), by = 0.001)
  lines(x, dnorm(x, mean = mean(abc[,name]), sd = sd(abc[,name])), lwd = 2)
}

# graph of results
for(name in names(abc)){
  plot.zoo(abc[,name], main = name)
  abline(h = mean(abc[,name]), col = "red", lwd = 2)
}

# ACF,PACF
for(name in names(abc)){
  acf(abc[,name], main = name)
  pacf(abc[,name], main = name)
}

#ADF test
for(name in names(abc)){
  print(adf.test(abc[,name], k = 4))
}

# Box test
for(name in names(abc)){
  print(Box.test(abc[,name], type = 'Ljung-Box',lag = 5))
}


### Problem 2 ###

TT <- length(abc$RV)
RV_0 <- as.numeric(abc$RV[23:TT])
RV_1 <- as.numeric(abc$RV[22:(TT - 1)])
RV_5 <- unlist(lapply(lapply(1:(TT - 4), function (t) {return(RV_0[t:(t + 4)])}), mean))
T_5 <- length(RV_5)
RV_5 <- RV_5[18:(T_5 - 1)]
RV_22 <- unlist(lapply(lapply(1:(TT - 21), function (t) {return(RV_0[t:(t + 21)])}), mean))
T_22 <- length(RV_22)
RV_22 <- RV_22[1:(T_22 - 1)]
RV_n <- as.numeric(abc$RV_n[22:(TT - 1)])
RV_p <- as.numeric(abc$RV_p[22:(TT - 1)])
RK <- as.numeric(abc$RK[22:(TT - 1)])
RS <- as.numeric(abc$RS[22:(TT - 1)])

# Models and their graph modeling

# ARMA(1,0) - RV 
AR1_RV_fit <- arima(abc$RV, order = c(1, 0, 0))
plot(index(abc$RV),abc$RV,col="black", type="l",main="AR-RV vs RV comparison", ylab="RV",xlab="period")
lines(index(abc$RV),fitted(AR1_RV_fit),col="cyan")
# HAR
HAR_fit <- lm(RV_0 ~ RV_1 + RV_5+ RV_22)
summary(HAR_fit)
plot(index(RV_0),RV_0,col="black", type="l",main="HAR vs RV comparison", ylab="RV",xlab="period")
lines(index(HAR_fit$fitted.values),HAR_fit$fitted.values,col="red")
# HAR-asymm 
HARAS_fit<-lm(RV_0 ~ RV_p + RV_n + RV_5+ RV_22)
summary(HARAS_fit)
plot(index(RV_0),RV_0,col="black", type="l",main="HAR-AS vs RV comparison", ylab="RV",xlab="period")
lines(index(HARAS_fit$fitted.values),HARAS_fit$fitted.values,col="orange")
# HAR RS
HARS_fit <-lm(RV_0 ~ RS + RV_1 + RV_5 + RV_22)
summary(HARS_fit)
plot(index(RV_0),RV_0,col="black", type="l",main="HAR-RS vs RV comparison", ylab="RV",xlab="period")
lines(index(HARS_fit$fitted.values),HARS_fit$fitted.values,col="green")
# HAR RS-RK 
HARSK_fit <-lm(RV_0 ~ RS + RK + RV_1 + RV_5 + RV_22)
summary(HARSK_fit)
plot(index(RV_0),RV_0,col="black", type="l",main="HAR-RS-RK vs RV comparison", ylab="RV",xlab="period")
lines(index(HARSK_fit$fitted.values),HARSK_fit$fitted.values,col="blue")
# RGARCH 
RGARCH<- ugarchspec(variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)),
                    mean.model = list(armaOrder=c(0, 0)),distribution.model = "std")
RGARCH_fit <- ugarchfit(spec = RGARCH, data = abc$ret, realizedVol= abc$RV)
RGARCH_fit
plot(index(abc$RV),abc$RV ,col="black", type="l",main="RGARCH vs RV comparison", ylab="RV",xlab="period")
lines(index(abc$RV),RGARCH_fit@fit$sigma,col="gold")

qqnorm(residuals(RGARCH_fit))
qqline(residuals(RGARCH_fit))
jarque.bera.test(residuals(RGARCH_fit)) # we reject hence data are not normally distibuted

# ARMA-GARCH 
ARMAGARCH <- ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean=TRUE), 
                        variance.model = list(garchOrder = c(1, 1)), distribution.model = "std")
ARMAGARCH_fit <- ugarchfit(ARMAGARCH, abc$ret, realizedVol = abc$RV)
ARMAGARCH_fit
plot(index(abc$RV),abc$RV,col="black", type="l",main="ARMA-GARCH vs RV comparison", ylab="RV",xlab="period")
lines(index(abc$RV),ARMAGARCH_fit@fit$sigma,col="purple")

qqnorm(residuals(ARMAGARCH_fit))
qqline(residuals(ARMAGARCH_fit))
jarque.bera.test(residuals(ARMAGARCH_fit)) # we reject hence data are not normally distributed

### Problem 3 ### 
w_l = 750
n_for = 250

# AR1-RV
# rolling
AR1_RV_fc_r<-lapply(1:n_for, function(x) arima(abc$RV[x:(w_l+x),],order=c(1,0,0)))
AR1_RV_fc_r<-sapply(1:n_for, function(x) predict(AR1_RV_fc_r[[x]],n.ahead = 1)$pred)
AR1_RV_fc_r<-xts(AR1_RV_fc_r,order.by=index(abc[(w_l+2):(w_l+1+n_for),]))
# expanding
AR1_RV_fc_e <- lapply(1:n_for, function(x) arima(abc$RV[1:(w_l+x),],order=c(1,0,0)))
AR1_RV_fc_e <- sapply(1:n_for, function(x) predict(AR1_RV_fc_e[[x]],n.ahead = 1)$pred)
AR1_RV_fc_e <- xts(AR1_RV_fc_e,order.by=index(abc[(w_l+2):(w_l+1+n_for),]))

plot(index(abc$RV)[751:1000],abc$RV[751:1000],lwd=2,col="grey", type="l", main = "AR(1)-RV Forecasts", xlab = "Time", ylab = "RV")
lines(index(abc$RV)[751:1000],AR1_RV_fc_e,lwd= 2,col="darkgreen")
lines(index(abc$RV)[751:1000],AR1_RV_fc_r,lwd=2,col="pink")
legend("top",ncol=3,legend = c("RV","AR(1)-RV_expanding","AR(1)-RV_rolling"),col=c("grey","darkgreen","pink"),lwd=2,bty = "n",cex=0.8)

# HAR 
# rolling
HAR_fc_r<-lapply(1:n_for, function(x) HARmodel(data = abc$RV[x:(w_l+x),] , periods = c(1,5,22),
                                               type = "HAR", h = 1, transform = NULL, inputType = "RM"))
HAR_fc_r<-sapply(1:n_for, function (x) predict(HAR_fc_r[[x]]))
HAR_fc_r<-xts(HAR_fc_r,order.by = index(abc$ret[(w_l+2):(w_l+1+n_for),]))
# expanding
HAR_fc_e <- lapply(1:n_for, function(x) HARmodel(data = abc$RV[1:(w_l+x),] , 
                                                 periods = c(1,5,22), RVest = c("rCov"),
                                                 type = "HAR", h = 1, transform = NULL,
                                                 inputType = "RM"))
HAR_fc_e <- sapply(1:n_for, function (x) predict(HAR_fc_e[[x]]))
HAR_fc_e <- xts(HAR_fc_e,order.by = index(abc[(w_l+2):(w_l+1+n_for),]))

plot(index(abc$RV)[751:1000],abc$RV[751:1000],lwd=2,col="grey", type="l", main = "HAR Forecasts", xlab = "Time", ylab = "RV")
lines(index(abc$RV)[751:1000],HAR_fc_e,lwd=2,col="darkgreen")
lines(index(abc$RV)[751:1000],HAR_fc_r,lwd=2,col="pink")
legend("top",ncol=3,legend = c("RV","HAR_expanding","HAR_rolling"),col=c("grey","darkgreen","pink"),lwd=2,bty = "n",cex=0.8)

# Preparation for other HAR models 
data_aux <- abc[1:(w_l+n_for),]
TT <- nrow(data_aux)
RV_5 <- unlist(lapply(lapply(1:(TT - 4), function (x) {return(data_aux$RV[x:(x + 4)])}), mean))
RV_22 <- unlist(lapply(lapply(1:(TT - 21), function (x) {return(data_aux$RV[x:(x + 21)])}), mean))
T_5 <- length(RV_5)
T_22 <- length(RV_22)
HAR_DATA <- data.frame(data_aux$RV[23:TT],
                       data_aux$RV_p[22:(TT - 1)],
                       data_aux$RV_n[22:(TT - 1)],
                       RV_5[18:(T_5 - 1)],
                       RV_22[1:(T_22 - 1)],
                       data_aux$RK[22:(TT - 1)],
                       data_aux$RS[22:(TT - 1)])
colnames(HAR_DATA) <- c("RV","RV_p","RV_n","RV_5","RV_22", "RK", "RS")
 
# HAR-AS 

HAR_AS_fc_r <- rep(NA, n_for)
for (i in 0:(n_for - 1)) {
  temp <- HAR_DATA[1+ i:(w_l + i - 22), ] %>% ts()
  fc_data <- HAR_DATA[w_l + i + 1 - 22, -1]
  model <- tslm(RV ~ RV_n + RV_p + RV_5 + RV_22, data = temp)
  HAR_AS_fc_r[i + 1] <- predict(model, newdata=fc_data)
}

HAR_AS_fc_r<-xts(HAR_AS_fc_r,order.by = index(abc$ret[(w_l+1):(w_l+n_for)]))

HAR_AS_fc_e <- rep(NA, n_for)
for (i in 0:(n_for - 1)) {
  temp <- HAR_DATA[1:(w_l + i - 22), ] %>% ts()
  fc_data <- HAR_DATA[w_l + i + 1 - 22, -1]
  model <- tslm(RV ~ RV_n + RV_p + RV_5 + RV_22, data = temp)
  HAR_AS_fc_e[i + 1] <- predict(model, newdata=fc_data)
}

HAR_AS_fc_e<-xts(HAR_AS_fc_e,order.by = index(abc$ret[(w_l+1):(w_l+n_for)]))

plot(index(abc$RV[751:1000]),abc$RV[751:1000],lwd=2,col="grey", type="l", main = "HAR-AS Forecasts", xlab = "Time", ylab = "RV")
lines(index(abc$RV[751:1000]),HAR_AS_fc_e,lwd=2,col="darkgreen")
lines(index(abc$RV[751:1000]),HAR_AS_fc_r,lwd=2,col="pink")
legend("top",ncol=3,legend = c("RV","HAR_AS_expanding","HAR_AS_rolling"),col=c("grey","darkgreen","pink"),lwd=2,bty = "n",cex=0.8)

#HAR-RS 
HAR_RS_fc_r <- rep(NA, n_for)
for (i in 0:(n_for - 1)) {
  temp <- HAR_DATA[1+ i:(w_l + i - 22), ] %>% ts()
  fc_data <- HAR_DATA[w_l + i + 1 - 22, -1]
  model <- tslm(RV ~ RS + RV_5 + RV_22, data = temp)
  HAR_RS_fc_r[i + 1] <- predict(model, newdata=fc_data)
}

HAR_RS_fc_r<-xts(HAR_RS_fc_r,order.by = index(abc$ret[(w_l+1):(w_l+n_for)]))

HAR_RS_fc_e <- rep(NA, n_for)
for (i in 0:(n_for - 1)) {
  temp <- HAR_DATA[1:(w_l + i - 22), ] %>% ts()
  fc_data <- HAR_DATA[w_l + i + 1 - 22, -1]
  model <- tslm(RV ~ RS + RV_5 + RV_22, data = temp)
  HAR_RS_fc_e[i + 1] <- predict(model, newdata=fc_data)
}

HAR_RS_fc_e<-xts(HAR_RS_fc_e,order.by = index(abc$ret[(w_l+1):(w_l+n_for)]))

plot(index(abc$RV[751:1000]),abc$RV[751:1000],lwd=2,col="grey", type="l", main = "HAR-RS Forecasts", xlab = "Time", ylab = "RV")
lines(index(abc$RV[751:1000]),HAR_RS_fc_e,lwd=2,col="darkgreen")
lines(index(abc$RV[751:1000]),HAR_RS_fc_r,lwd=2,col="pink")
legend("top",ncol=3,legend = c("RV","HAR_RS_expanding","HAR_RS_rolling"),col=c("grey","darkgreen","pink"),lwd=2,bty = "n",cex=0.8)

# HAR-RS-RK 
HAR_RSRK_fc_r <- rep(NA, n_for)
for (i in 0:(n_for - 1)) {
  temp <- HAR_DATA[(1 + i):(w_l + i - 22), ] %>% ts()
  fc_data <- HAR_DATA[w_l + i + 1 - 22, -1]
  model <- tslm(RV ~ RS + RK + RV_5 + RV_22, data = temp)
  HAR_RSRK_fc_r[i + 1] <- predict(model, newdata=fc_data)
}

HAR_RSRK_fc_r <- xts(HAR_RSRK_fc_r,order.by = index(abc$ret[(w_l+1):(w_l+n_for)]))

HAR_RSRK_fc_e <- rep(NA, n_for)
for (i in 0:(n_for - 1)) {
  temp <- HAR_DATA[1:(w_l + i - 22), ] %>% ts()
  fc_data <- HAR_DATA[w_l + i + 1 - 22, -1]
  model <- tslm(RV ~ RS + RK + RV_5 + RV_22, data = temp)
  HAR_RSRK_fc_e[i + 1] <- predict(model, newdata=fc_data)
}

HAR_RSRK_fc_e<-xts(HAR_RSRK_fc_e,order.by = index(abc$ret[(w_l+1):(w_l+n_for)]))

plot(index(abc$RV[751:1000]),abc$RV[751:1000],lwd=2,col="grey", type="l", main = "HAR-RS-RK Forecasts", xlab = "Time", ylab = "RV")
lines(index(abc$RV[751:1000]),lwd=2,HAR_RSRK_fc_e,col="darkgreen")
lines(index(abc$RV[751:1000]),lwd=2,HAR_RSRK_fc_r,col="pink")
legend("top",ncol=3,legend = c("RV","HAR_RS_RK_expanding","HAR_RS_RK_rolling"),col=c("grey","darkgreen","pink"),lwd=2,bty = "n",cex=0.8)

RGARCH_fc_r<-to_save$RGARCH_fc_r
RGARCH_fc_e<-to_save$RGARCH_fc_e
ARMAGARCH_fc_r<-to_save$ARMAGARCH_fc_r
ARMAGARCH_fc_e<-to_save$ARMAGARCH_fc_e

# Realized GARCH 
RGARCH_fc_r <- ugarchroll(RGARCH, 100*abc$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                          n.start = NULL, refit.every = 1, refit.window = c("moving"), window.size = w_l,
                          solver = "solnp", calculate.VaR = FALSE,keep.coef = TRUE,realizedVol = (100*abc$RV[1:(w_l+n_for+1),]))
RGARCH_fc_r <- xts(RGARCH_fc_r@forecast[["density"]]$Sigma,
                   order.by = as.Date(rownames(RGARCH_fc_r@forecast[["density"]])))/100

RGARCH_fc_e <- ugarchroll(RGARCH, 100*abc$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                          n.start = NULL, refit.every = 1, refit.window = c("recursive"), 
                          window.size = w_l, solver = "solnp", calculate.VaR = FALSE, 
                          keep.coef = TRUE,realizedVol = 100*((abc$RV[1:(w_l+n_for+1),])))
RGARCH_fc_e<- xts(RGARCH_fc_e@forecast[["density"]]$Sigma,
                  order.by = as.Date(rownames(RGARCH_fc_e@forecast[["density"]])))/100

plot(index(abc$RV)[751:1000],abc$RV[751:1000],lwd=2,col="grey", type="l", main = "RGARCH Forecasts", xlab = "Time", ylab = "RV")
lines(index(abc$RV)[751:1000],RGARCH_fc_e,lwd=2,col="darkgreen")
lines(index(abc$RV)[751:1000],RGARCH_fc_r,lwd=2,col="pink")
legend("top",ncol=3,legend = c("RV","RGARCH_expanding","RGARCH_rolling"),col=c("grey","darkgreen","pink"),lwd=2,bty = "n",cex=0.8)

# ARMA-GARCH 
ARMAGARCH_fc_r <- ugarchroll(ARMAGARCH, 100*abc$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                             n.start = NULL, refit.every = 1, refit.window = c("moving"), 
                             window.size = w_l, solver = "solnp", fit.control = list(), 
                             solver.control = list(), calculate.VaR = FALSE, 
                             keep.coef = TRUE,realizedVol = 100*(abc$RV[1:(w_l+n_for+1),]))
ARMAGARCH_fc_r <- xts(ARMAGARCH_fc_r@forecast[["density"]]$Sigma,
                      order.by = as.Date(rownames(ARMAGARCH_fc_r@forecast[["density"]])))/100

ARMAGARCH_fc_e <- ugarchroll(ARMAGARCH, 100*abc$ret[1:(w_l+n_for+1),], n.ahead = 1, forecast.length = n_for, 
                             n.start = NULL, refit.every = 1, refit.window = c("recursive"), 
                             window.size = w_l, solver = "solnp", fit.control = list(), 
                             solver.control = list(), calculate.VaR = FALSE, 
                             keep.coef = TRUE,realizedVol = 100*(data$RV[1:(w_l+n_for+1),]))
ARMAGARCH_fc_e<- xts(ARMAGARCH_fc_e@forecast[["density"]]$Sigma,
                     order.by = as.Date(rownames(ARMAGARCH_fc_e@forecast[["density"]])))/100

plot(index(abc$RV)[751:1000],abc$RV[751:1000],lwd=2,col="grey", type="l", main = "ARIMA-GARCH Forecasts", xlab = "Time", ylab = "RV")
lines(index(abc$RV)[751:1000],ARMAGARCH_fc_e,lwd=2,col="darkgreen")
lines(index(abc$RV)[751:1000],ARMAGARCH_fc_r,lwd=2,col="pink")
legend("top",ncol=3,legend = c("RV","ARMA_GARCH_expanding","ARMA_GARCH_rolling"),col=c("grey","darkgreen","pink"),lwd=2,bty = "n",cex=0.8)

true_vals <- as.vector(abc$RV[(w_l+2):(w_l+1+n_for),])

AR1_RV_fc_e_er  <- true_vals - AR1_RV_fc_e
AR1_RV_fc_r_er  <- true_vals - AR1_RV_fc_r
HAR_fc_e_er <- true_vals - HAR_fc_e
HAR_fc_r_er <- true_vals - HAR_fc_r
HAR_AS_fc_e_er<- true_vals- HAR_AS_fc_e
HAR_AS_fc_r_er<- true_vals - HAR_AS_fc_r
HAR_RSV_fc_e_er <- true_vals - HAR_RS_fc_e
HAR_RSV_fc_r_er <- true_vals - HAR_RS_fc_r
HAR_RSRK_fc_e_er <- true_vals - HAR_RSRK_fc_e
HAR_RSRK_fc_r_er <- true_vals - HAR_RSRK_fc_r
RGARCH_fc_e_er <- true_vals - RGARCH_fc_e
RGARCH_fc_r_er <- true_vals - RGARCH_fc_r
ARMAGARCH_fc_e_er <- true_vals - ARMAGARCH_fc_e
ARMAGARCH_fc_r_er <- true_vals - ARMAGARCH_fc_r


#loss function

errs <- list(AR1_RV_fc_e_er, AR1_RV_fc_r_er, HAR_fc_e_er, HAR_fc_r_er, HAR_AS_fc_e_er,HAR_AS_fc_r_er,
             HAR_RSV_fc_e_er, HAR_RSV_fc_r_er, HAR_RSRK_fc_e_er, HAR_RSRK_fc_r_er,RGARCH_fc_e_er,RGARCH_fc_r_er, 
             ARMAGARCH_fc_e_er, ARMAGARCH_fc_r_er)

#MSE
MSE_e <- sapply(errs[c(1,3,5,7,9,11,13)], function(x) mean(x^2, na.rm = TRUE))
MSE_r <- sapply(errs[c(2,4,6,8,10,12,14)], function(x) mean(x^2, na.rm = TRUE))

MSEs  <- matrix(c(MSE_e, MSE_r, MSE_r > MSE_e), ncol = 3, byrow = FALSE)
colnames(MSEs) <- c("Expanding window error", "Rolling window error", "Expanding better")
rownames(MSEs)  <- c("AR(1)-RV", "HAR","HAR_AS", "HAR-RSV", "HAR-RSRK", "Realized GARCH", "ARMA-GARCH")

print("MSE metrics for the models:")
MSEs
xtable(MSEs,digits = 8)


#MAE 
MAE_e <- sapply(errs[c(1,3,5,7,9,11,13)], function(x) mean(abs(x), na.rm = TRUE))
MAE_r <- sapply(errs[c(2,4,6,8,10,12,14)], function(x) mean(abs(x), na.rm = TRUE))

MAEs  <- matrix(c(MAE_e, MAE_r, MAE_r > MAE_e), ncol = 3, byrow = FALSE)
colnames(MAEs) <- c("Expanding window error", "Rolling window error", "Expanding better")
rownames(MAEs)  <- c("AR(1)-RV", "HAR","HAR_AS", "HAR-RSV", "HAR-RSRK", "Realized GARCH", "ARMA-GARCH")

print("MAE metrics for the models:")               
MAEs
xtable(MAEs,digits = 8)


allmodels<- c("AR1_RV_fc_r", "AR1_RV_fc_e", "HAR_fc_r", "HAR_fc_e", 
              "HAR_AS_fc_r", "HAR_AS_fc_e", "HAR_RC_fc_r", "HAR_RS_fc_e", 
              "HAR_RSRK_fc_r","HAR_RSRK_fc_e", "RGARCH_fc_r", "RGARCH_fc_e", 
              "ARMAGARCH_fc_r", "ARMAGARCH_fc_e")


#Exp  <- c("AR1_RV_fc_e", "HAR_fc_e", "HAR_RS_fc_e", "HAR_RSRK_fc_e", "RGARCH_fc_e", "ARMAGARCH_fc_e")
Exp  <- c("AR1_RV_fc_e", "HAR_fc_e", "HAR_AS_fc_e", "HAR_RS_fc_e", "HAR_RSRK_fc_e", "RGARCH_fc_e", "ARMAGARCH_fc_e")
#Roll  <- c("AR1_RV_fc_r", "HAR_fc_r", "HAR_RS_fc_r", "HAR_RSRK_fc_r", "RGARCH_fc_r", "ARMAGARCH_fc_r")
Roll  <- c("AR1_RV_fc_r", "HAR_fc_r", "HAR_AS_fc_r", "HAR_RS_fc_r", "HAR_RSRK_fc_r","RGARCH_fc_r", "ARMAGARCH_fc_r")

Exp_comb <- t(combn(Exp, 2))
Roll_comb <- t(combn(Roll, 2))

row.names(Exp_comb)<-apply(Exp_comb,1,function(x) paste(x[1],'|',x[2]))
Diebold_e <- data.frame(apply(Exp_comb,1,function(x) dm.test(true_vals - get(x[1]),true_vals - get(x[2]),alternative = c("two.sided"))$p.value))
colnames(Diebold_e)<-"P-Value"

row.names(Roll_comb)<-apply(Roll_comb,1,function(x) paste(x[1],'|',x[2]))
Diebold_r <- data.frame(apply(Roll_comb,1,function(x) dm.test(true_vals - get(x[1]),true_vals - get(x[2]),alternative = c("two.sided"))$p.value))
colnames(Diebold_r)<-"P-Value"

#DB-tests
print("Expanding DM-test")
print(round(Diebold_e,3))
xtable(round(Diebold_e,3))


print("Rolling DM-test")
print(round(Diebold_r,3))
xtable(round(Diebold_r,3))

  

# Mincer - Zarnowitz regression 

regressions <- list(
  Minc_AR1_RV_e <- lm(true_vals ~ AR1_RV_fc_e),
  Minc_AR1_RV_r <- lm(true_vals ~ AR1_RV_fc_r),
  Minc_HAR_e <- lm(true_vals ~ HAR_fc_e),
  Minc_HAR_r <- lm(true_vals ~ HAR_fc_r),
  Minc_HAR_AS_e <- lm(true_vals ~ HAR_AS_fc_e),
  Minc_HAR_AS_r <- lm(true_vals ~ HAR_AS_fc_r),
  Minc_HAR_RS_e <- lm(true_vals ~ HAR_RS_fc_e),
  Minc_HAR_RS_r <- lm(true_vals ~ HAR_RS_fc_r),
  Minc_HAR_RSRK_e <- lm(true_vals ~ HAR_RSRK_fc_e),
  Minc_HAR_RSRK_r <- lm(true_vals ~ HAR_RSRK_fc_r),
  Minc_RGARCH_e <- lm(true_vals ~  RGARCH_fc_e),
  Minc_RGARCH_r <- lm(true_vals ~ RGARCH_fc_r),
  Minc_ARMAGARCH_e <- lm(true_vals ~ ARMAGARCH_fc_e),
  Minc_ARMAGARCH_r <- lm(true_vals ~ ARMAGARCH_fc_r)
)

wald_test <- function(lm) {
  wald <- wald.test(b = coef(lm), Sigma = vcov(lm), H0 = c(0, 1), Terms = 1:2) 
  coefs <- coef(lm)
  return(c(coefs, wald$result$chi2[1], round(wald$result$chi2[3], 2)))
}

mincer <- as.data.frame(matrix(rep(NA, 5 * 14), ncol = 5))
rownames(mincer) <- c("AR(1)-RV Expanding", "AR(1)-RV Rolling", "HAR Expanding", "HAR Rolling","HAR-AS Expanding" ,"HAR-AS Rolling","HAR-RSV Expanding", "HAR-RSV Rolling", "HAR-RSRK Expanding", "HAR-RSRK Rolling", "Realized GARCH Expanding", "Realized GARCH Rolling", "ARMA-GARCH Expanding", "ARMA-GARCH Rolling")
colnames(mincer) <- c("Intercept", "Slope", "Statistic", "p-value", "R Squared")

for (i in 1:length(regressions)) {
  mincer[i, ] <- c(wald_test(regressions[[i]]), summary(regressions[[i]])$r.squared)
}

mincer

xtable(mincer, digits=8)



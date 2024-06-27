
library(arrow)
library(xts)
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
library(fitdistrplus)
library(MASS)
library(mosaic)
library(segMGarch)
library(GAS)



##############################
### READING FROM PARQUET   ###
###    Not necessary if    ###
### .RData files available ### 
##############################

# #read all filenames 
# stocks = as.data.frame(list.files("Data/parquet"))

# #create stock names from file names 
# names(stocks) = "filename"
# stocks$stockname = substr(stocks$filename, 4, nchar(stocks$filename))
# # head(stocks)

# allstocks = list() 

# for(i in seq(1,nrow(stocks))){
#  # read file 
#  allstocks[[stocks[i,"stockname"]]] =as.xts(read_parquet(paste("Data/parquet/", stocks[i,"filename"], sep=""))) 
#  # print(sum(is.na(allstocks[[stocks[i,"stockname"]]])))
#  # check if No Na's are present
#  stocks$NoNas[i] = sum(is.na(allstocks[[stocks[i,"stockname"]]])) == 0
#  # compute returns 
#  allstocks[[stocks[i,"stockname"]]]$ret = diff(allstocks[[stocks[i,"stockname"]]][,"close_price"])/lag(allstocks[[stocks[i,"stockname"]]][,"close_price"]) 
#  # reorganize  columns 
#  allstocks[[stocks[i,"stockname"]]] = allstocks[[stocks[i,"stockname"]]][,c(7,2,3,4,5,6,1)] 
#  allstocks[[stocks[i,"stockname"]]] = allstocks[[stocks[i,"stockname"]]][-1,]
#}

for(filename in setdiff(list.files("Data"), list.dirs("Data", recursive = FALSE, full.names = FALSE))){
  load(paste("Data/", filename, sep = ""))
}

# Model parameters - loaded from data 

# minimum_length = 1000 
# pre_covid_end_date = as.Date("2019-11-29")
# n_for = 66 
# max_possible_date_diff = 21 
# forecast_start_date = as.Date("2019-11-29") 

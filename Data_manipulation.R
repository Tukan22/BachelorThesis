install.packages("arrow")
install.packages("xts")
library(arrow)
library(xts)


#read all filenames 
stocks = as.data.frame(list.files("Data/parquet"))

#create stock names from file names 
names(stocks) = "filename"
stocks$stockname = substr(stocks$filename, 4, nchar(stocks$filename))
# head(stocks)

allstocks = list() 

for(i in seq(1,nrow(stocks))){
  # read file 
  allstocks[[stocks[i,"stockname"]]] =as.xts(read_parquet(paste("Data/parquet/", stocks[i,"filename"], sep=""))) 
  # print(sum(is.na(allstocks[[stocks[i,"stockname"]]])))
  # check if No Na's are present
  stocks$NoNas[i] = sum(is.na(allstocks[[stocks[i,"stockname"]]])) == 0
  # compute returns 
  allstocks[[stocks[i,"stockname"]]]$ret = diff(allstocks[[stocks[i,"stockname"]]][,"close_price"])/lag(allstocks[[stocks[i,"stockname"]]][,"close_price"]) 
  # reorganize  columns 
  allstocks[[stocks[i,"stockname"]]] = allstocks[[stocks[i,"stockname"]]][,c(7,2,3,4,5,6,1)] 
  allstocks[[stocks[i,"stockname"]]] = allstocks[[stocks[i,"stockname"]]][-1,]
}

head(allstocks[["AAL"]])
tail(allstocks[["AAL"]])

# individual data reading - not relevant anymore 
# AAL = read_parquet("Data/parquet/RM_AAL")
# AAL = as.xts(AAL)
# sum(is.na(AAL))
# AAL$ret = diff(AAL[,"close_price"])/lag(AAL[,"close_price"]) 
# 
# AAL = AAL[,c(7,2,3,4,5,6,1)]
# AAL = AAL[-1,]
# 
# head(AAL)
# tail(AAL)
# 
# the same for AAPL 
# AAPL = read_parquet("Data/parquet/RM_AAPL")
# AAPL = as.xts(AAPL)
# sum(is.na(AAPL))
# AAPL$ret = diff(AAPL[,"close_price"])/lag(AAPL[,"close_price"]) 
# 
# AAPL = AAPL[,c(7,2,3,4,5,6,1)]
# AAPL = AAPL[-1,]
# 
# head(AAPL)
# tail(AAPL)

rm(i)
save.image("Data/stocks.RData")

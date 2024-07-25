library(xts)

setwd("F:/IES/BachelorThesis/Folder/BachelorThesis/ToSend")

load("allstocks.RData")
load("stocks.RData")
load("ARMAGARCH_fc_e.RData")
load("RGARCH_fc_e.RData")

pdf(file = "VolPlots.pdf", width = 16, height = 12) 

for(stockn in stocks$stockname){
# for(stockn in c("AAPL","ABBV", "XOM")){
  
  myplot = plot(ARMAGARCH_fc_e[[stockn]], ylim = c(-0.1, 0.1), col = "green", main = stockn)  

  lines(RGARCH_fc_e[[stockn]],col = "blue")
  lines(sqrt(allstocks[[stockn]]$RV), col = "red") 
  lines(allstocks[[stockn]]$ret, col = "grey")  
  addLegend("topleft", on=1, 
          legend.names = c("Returns", "GARCH(1,1) forecast","Realized volatility","RGARCH forecast"), 
          lty=c(1, 1, 1, 1), lwd=c(2, 2, 2,2),
          col=c("grey", "green","red", "blue"))
  
  print(myplot)
}

dev.off() 



tail(ARMAGARCH_fc_e[["AAPL"]]) 

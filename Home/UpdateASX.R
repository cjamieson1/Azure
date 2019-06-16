

## Script designed to retrieve and identify changes in the stock market

# fetch all data\\




asxlist <-  readRDS("ASXCurrent.Rds")

UpdatedASX <- BatchGetSymbols::BatchGetSymbols(asxlist)


tickers <- unique(UpdatedASX$df.tickers$ticker)
moversDF <- data.frame("ticker" = "","lastprice"=0,"day"=0,"dayp"=0,"week" = 0,"weekp"=0,"month"=0,"monthp"=0)
moversDF2 <- data.frame("ticker" = "","lastprice"=0,"day"=0,"dayp"=0,"week" = 0,"weekp"=0,"month"=0,"monthp"=0)

for(i in 1:length(tickers)){
  tick = tickers[i]
  selected <- dplyr::filter(UpdatedASX$df.tickers,ticker==tick)
  prices <- selected$price.adjusted
  moversDF$ticker <- tick
  moversDF$lastprice <- prices[length(prices)]
  moversDF$day <- prices[length(prices)]-prices[length(prices)-1]
  moversDF$dayp <- (prices[length(prices)]-prices[length(prices)-1])/prices[length(prices)-1]
  
  moversDF$week <- prices[length(prices)]-prices[length(prices)-5]
  moversDF$weekp <- (prices[length(prices)]-prices[length(prices)-5])/prices[length(prices)-5]
  
  moversDF$month <-prices[length(prices)]-prices[1]
  moversDF$monthp <-(prices[length(prices)]-prices[1])/prices[1]
  
  moversDF2 <- rbind.data.frame(moversDF2,moversDF)
  
}

moversDF2 <-  moversDF2[-1,]
stockmovers <-moversDF2[order(moversDF2$monthp,decreasing = T),]
stockmovers


Sys.time({
  tickers <- unique(UpdatedASX$df.tickers$ticker)
  moversDF <- data.frame("ticker" = "","lastprice"=0,"day"=0,"dayp"=0,"week" = 0,"weekp"=0,"month"=0,"monthp"=0)
  moversDF2 <- data.frame("ticker" = "","lastprice"=0,"day"=0,"dayp"=0,"week" = 0,"weekp"=0,"month"=0,"monthp"=0)
  
  moversDF3 <- foreach(i=1:length(tickers), .combine = rbind) %dopar% {
    tick = tickers[i]
    tick
    selected <- dplyr::filter(UpdatedASX$df.tickers,ticker==tick)
    prices <- selected$price.adjusted
    moversDF$ticker <- tick
    moversDF$lastprice <- prices[length(prices)]
    moversDF$day <- prices[length(prices)]-prices[length(prices)-1]
    moversDF$dayp <- (prices[length(prices)]-prices[length(prices)-1])/prices[length(prices)-1]
    
    moversDF$week <- prices[length(prices)]-prices[length(prices)-5]
    moversDF$weekp <- (prices[length(prices)]-prices[length(prices)-5])/prices[length(prices)-5]
    
    moversDF$month <-prices[length(prices)]-prices[1]
    moversDF$monthp <-(prices[length(prices)]-prices[1])/prices[1]
    
    moversDF
  } 
  
  
  moversDF2 <-  moversDF2[-1,]
  stockmovers <-moversDF3[order(moversDF2$monthp,decreasing = T),]
  stockmovers 
})

write.csv(stockmovers,"Movers.csv",row.names = F)

googlesheets::gs_title("Movers")
googlesheets::gs_upload(file = "Movers.csv","Movers",overwrite = T)


# parallel running 


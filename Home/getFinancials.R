getFin <- function(stock){
  if ("rvest" %in% installed.packages()) {
    library(rvest)
  }else{
    install.packages("rvest")
    library(rvest)
  }
  for (i in 1:length(stock)) {
    tryCatch(
      {
        url <- "https://finance.yahoo.com/quote/"
        url <- paste0(url,stock[i],"/financials?p=",stock[i])
        wahis.session <- html_session(url)                                
        p <-    wahis.session %>%
          html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
          html_table(fill = TRUE)
        IS <- p[[1]]
        colnames(IS) <- paste(IS[1,])
        IS <- IS[-c(1,5,12,20,25),]
        names_row <- paste(IS[,1])
        IS <- IS[,-1]
        IS <- apply(IS,2,function(x){gsub(",","",x)})
        IS <- as.data.frame(apply(IS,2,as.numeric))
        rownames(IS) <- paste(names_row)
        temp1 <- IS
        url <- "https://finance.yahoo.com/quote/"
        url <- paste0(url,stock[i],"/balance-sheet?p=",stock[i])
        wahis.session <- html_session(url)
        p <-    wahis.session %>%
          html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
          html_table(fill = TRUE)
        BS <- p[[1]]
        colnames(BS) <- BS[1,]
        BS <- BS[-c(1,2,17,28),]
        names_row <- BS[,1]
        BS <- BS[,-1] 
        BS <- apply(BS,2,function(x){gsub(",","",x)})
        BS <- as.data.frame(apply(BS,2,as.numeric))
        rownames(BS) <- paste(names_row)
        temp2 <- BS
        url <- "https://finance.yahoo.com/quote/"
        url <- paste0(url,stock[i],"/cash-flow?p=",stock[i])
        wahis.session <- html_session(url)
        p <-    wahis.session %>%
          html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
          html_table(fill = TRUE)
        CF <- p[[1]]
        colnames(CF) <- CF[1,]
        CF <- CF[-c(1,3,11,16),]
        names_row <- CF[,1]
        CF <- CF[,-1] 
        CF <- apply(CF,2,function(x){gsub(",","",x)})
        CF <- as.data.frame(apply(CF,2,as.numeric))
        rownames(CF) <- paste(names_row)
        temp3 <- CF
        assign(paste0(stock[i],'.f'),value = list(IS = temp1,BS = temp2,CF = temp3),envir = parent.frame())
        stockdata <- list(IS = temp1,BS = temp2,CF = temp3)
        return(stockdata)
      },
      error = function(cond){
        message(stock[i], "Give error ",cond)
      }
    )
  }
}


HashtagStocks <- function(text,ASXTickers){
  options(stringsAsFactors = FALSE)
  hashtags <- str_extract_all(text, "#\\S+")
  hash <- as.data.frame(hashtags,stringsAsFactors = F)
  ticks <- toupper(substr(hash[,1],2,4))
  stocks <- ASXTickers[match(ticks,ASXTickers[,2]),1]
  stocknames <- paste(ticks,stocks,sep = " - ")
}

getShorts <- function(ndays){
  dates <- c(20190101,
             20190128,
             20190419,
             20190420,
             20190421,
             20190422,
             20190425,
             20190610,
             20190806,
             20191007,
             20191225,
             20191226)
  hols <- as.Date.character(dates,"%Y%m%d")
  bizdays::create.calendar(name = "mycal",holidays = hols,weekdays = c("saturday","sunday"))
  nbiz1 = bizdays::bizdiff(c(today()-days(ndays),today()),cal = "mycal")
  while(nbiz1<6) {
    ndays <- ndays + 1 
    nbiz1 = bizdays::bizdiff(c(today()-days(ndays),today()),cal = "mycal")
  }
  print(bizdays::is.bizday(today()-ndays,"mycal"))
  print(nbiz1)
  print(weekdays(Sys.Date()-ndays))
  year <- year(today()-days(ndays))
  month <- month((today()-days(ndays)))
  if(nchar(month)==1){
    month = paste0(0,month)
  }
  day <- day((today()-days(ndays)))
  if(nchar(day)==1){
    day = paste0(0,day)
  }
  
  tryCatch(expr = {url <- paste0("https://asic.gov.au/Reports/Daily/",year,"/",month,"/RR",year,month,day,"-001-SSDailyAggShortPos.csv")
  download.file(url,"Data/ShortPositions.csv")},error = function(e){
    url <- paste0("https://asic.gov.au/Reports/Daily/",year,"/",month,"/RR",year,month,day-1,"-001-SSDailyAggShortPos.csv")
    download.file(url,"Data/ShortPositions.csv")
  })
  
  ndaysmonth = 30
  nbiz = bizdays::bizdiff(c(today()-days(ndaysmonth),today()),cal = "mycal")
  while (nbiz<25&!bizdays::is.bizday(today()-ndaysmonth,"mycal")) {
    ndaysmonth <- ndaysmonth + 1 
    nbiz = bizdays::bizdiff(c(today()-days(ndaysmonth),today()),cal = "mycal")
  }
  print(weekdays(Sys.Date()-ndaysmonth))
  year <- year(today()-days(ndaysmonth))
  month <- month((today()-days(ndaysmonth)))
  if(nchar(month)==1){
    month = paste0(0,month)
  }
  day <- day((today()-days(ndaysmonth)))
  if(nchar(day)==1){
    day = paste0(0,day)
  }
  
  tryCatch(expr = {url <- paste0("https://asic.gov.au/Reports/Daily/",year,"/",month,"/RR",year,month,day,"-001-SSDailyAggShortPos.csv")
  download.file(url,"Data/ShortPositionsMonth.csv")},error = function(e){
    url <- paste0("https://asic.gov.au/Reports/Daily/",year,"/",month,"/RR",year,month,day-1,"-001-SSDailyAggShortPos.csv")
    download.file(url,"Data/ShortPositionsMonth.csv")
  })
  
  shorts <- read.csv("Data/ShortPositions.csv",header = T,sep = "\t",fileEncoding = "UTF-16")
  shortsMonth <- read.csv("Data/ShortPositionsMonth.csv",header = T,sep = "\t",fileEncoding = "UTF-16")
  names(shorts)<- c("Company","Ticker","Reported Short Positions","Total Shares","Percent")
  validshorts <- na.exclude(match(shortsMonth$Product.Code,shorts$Ticker))
  validshorts2 <- na.exclude(match(shorts$Ticker,shortsMonth$Product.Code))
  shortsMonth <- shortsMonth[validshorts2,]
  shorts<- shorts[validshorts,]
  #change1 <- shorts$`Reported Short Positions`-shortsMonth$Reported.Short.Positions 
  change2 <- shorts$Percent-shortsMonth$X..of.Total.Product.in.Issue.Reported.as.Short.Positions
  shortdata <- cbind(shorts,change2/100)
  #shortdata[,c(3,4,5,6)] <- round(shortdata[,c(3,4,5,6)],2)
  names(shortdata)<- c("Company","Ticker","Reported Short Positions","Total Shares on Issue","% Short Positions","Change")
  ordered <- shortdata[order(shortdata[,5],decreasing = T),]
}

getASXdata <- function(ASXTickers){
  df_all_stocks <- data.frame()
  for (ax_ticker in ASXTickers) {
    tryCatch({
      df_get <- data.frame(na.omit(getSymbols(
        ax_ticker
        ,src = 'yahoo'
        ,from=Sys.Date()-365 # feel free to change this date range
        ,to=Sys.Date()       # this one too
        ,auto.assign = FALSE)))
      
      df_get$Date <- row.names(df_get)
      row.names(df_get) <- NULL
      df_stock <- data.frame(ax_ticker=ax_ticker,df_get)
      names(df_stock) <- c("ax_ticker","OPEN","HIGH","LOW","CLOSE","VOLUME","ADJUSTED","Date")
      df_all_stocks <- rbind(df_all_stocks,df_stock)  
    },
    error=function(e){})
  }
}

getWrap <- function(Ticker){
  url <- paste0("http://www2.aspecthuntley.com.au/pdf/cba/advancedprofile/today/",Ticker,"_advancedprofile.pdf")
  download.file(url = url,destfile = paste0("./Company Wraps/",Ticker,"-Company-Wrap.pdf"),mode = "wb")
}

getHours <- function(Person,Date1, Date2){
  
}

getStockdata <- function(ticker){
  
  symb = paste0(substr(ticker,1,3),".AX")
  print("getstockdata")
  data <- na.omit(quantmod::getSymbols(symb,src = "yahoo",auto.assign = F,warnings = F))
  print(tail(data))
  print("check")
  daychange <- as.numeric(data[nrow(data),6])-as.numeric(data[nrow(data)-1,6])
  day <- c(daychange,100*daychange/data[nrow(data)-1,6])
  weekchange <- as.numeric(data[nrow(data),6])-as.numeric(data[nrow(data)-5,6])
  week <- c(weekchange,100*weekchange/data[nrow(data)-5,6])
  monthchange <- as.numeric(data[nrow(data),6])-as.numeric(data[1,6])
  month <- c(monthchange,100*monthchange/data[1,6])
  price <- as.numeric(data[nrow(data),6])
  #price <- c(day,0)
  data.frame(day,week,month,price)
}

getStockname <- function(ticker,ASXTickers){
  stocks <- ASXTickers[match(ticker,ASXTickers[,2]),1]
  stocknames <- paste(ticker,stocks,sep = " - ")
}

getStockChart <- function(ticker){
  symb = paste0(substr(ticker,1,3),".AX")
  plotdata <- na.omit(getSymbols(symb,from = today()-months(1), to = today(),auto.assign = F))
  plotdata <- as.data.frame(plotdata)
  p <- plot_ly(data = plotdata,
               y = plotdata[,6],
               #color = "maroon",
               name = paste(ticker,"-","Chart"),
               type = "bar"
  ) %>% layout(title = paste(ticker,"-","Chart"))
  print(p)
}


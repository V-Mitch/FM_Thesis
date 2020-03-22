#rm(list=ls())
require(lubridate)
require(sandwich)
require(lmtest)

USDCAD <- "~/R tests/finance related projects/USDCAD_M1_200801020901_202002241546.csv"
AUDUSD <- "~/R tests/finance related projects/AUDUSD_M1_200001030001_202001081538.csv"
NZDUSD <- "~/R tests/finance related projects/NZDUSD_M1_200801020000_202001081600.csv"
GBPUSD <- "~/R tests/finance related projects/GBPUSD_M1_200001030001_202001081459.csv"
EURUSD <- "~/R tests/finance related projects/EURUSD_M1_200801020901_202002241559.csv"
USDCHF <- "~/R tests/finance related projects/USDCHF_M1_200801020900_202001081800.csv"
EURCAD <- "~/R tests/finance related projects/EURCAD_M1_200801020901_202002241652.csv"

retrieve_data <- function(news_event) {

if(news_event == 1 || news_event == "audcpi_q_q.txt"){
  # # Australian CPI (quarterly announcements Metatrader Time)
  file_macro <- "~/R tests/finance related projects/audcpi_q_q.txt"
  file_prices <- AUDUSD
}
else if(news_event == 2 || news_event == "audret_m_m.txt"){
  # AUD retail sales 
  file_macro <- "~/R tests/finance related projects/audret_m_m.txt"
  file_prices <- AUDUSD
}
else if(news_event == 3 || news_event == "audemc_m_m.txt"){
  # AUD employment change 
  file_macro <- "~/R tests/finance related projects/audemc_m_m.txt"
  file_prices <- AUDUSD
}
else if(news_event == 4 || news_event == "audemr_m_m.txt"){
  # AUD employment rate
  file_macro <- "~/R tests/finance related projects/audemr_m_m.txt"
  file_prices <- AUDUSD
}
else if(news_event == 5 || news_event == "nzdcpi_q_q.txt"){
  # NZD CPI (quarterly announcements Metatrader Time)
  file_macro <- "~/R tests/finance related projects/nzdcpi_q_q.txt"
  file_prices <- NZDUSD
}
else if(news_event == 6 || news_event == "cadcrs_m_m.txt"){
  # CAD core retail sales 
  file_macro <- "~/R tests/finance related projects/cadcrs_m_m.txt"
  file_prices <- USDCAD
}
else if(news_event == 7 || news_event == "cademc_m_m.txt"){
  # CAD employment change in thousands 
  file_macro <- "~/R tests/finance related projects/cademc_m_m.txt"
  file_prices <- EURCAD 
}
else if(news_event == 8 || news_event == "cademr_m_m.txt"){
  # CAD employment rate in percentage
  file_macro <- "~/R tests/finance related projects/cademr_m_m.txt"
  file_prices <- EURCAD  
}
else if(news_event == 9 || news_event == "cademc_m_m.txt"){
  # CAD employment rate in percentage
  file_macro <- "~/R tests/finance related projects/cademc_m_m.txt"
  file_prices <- USDCAD  
}
else if(news_event == 10 || news_event == "gbpret_m_m.txt"){
  # GBP retail sales percentage
  file_macro <- "~/R tests/finance related projects/gbpret_m_m.txt"
  file_prices <- GBPUSD  
}
else if(news_event == 11 || news_event == "eurzew_m_m.txt"){
  # EUR German ZEW
  file_macro <- "~/R tests/finance related projects/eurzew_m_m.txt"
  file_prices <- EURUSD
}
else if(news_event == 12 || news_event == "usdahe_m_m.txt"){
  # USD average hourly earnings (coincides with the NFP)
  file_macro <- "~/R tests/finance related projects/usdahe_m_m.txt"
  file_prices <- USDCHF
}
else if(news_event == 13 || news_event == "usdnfp_m_m.txt"){
  # USD NFP (coincides with the AHE)
  file_macro <- "~/R tests/finance related projects/usdnfp_m_m.txt"
  file_prices <- USDCHF
}
else if(news_event == 14 || news_event == "usdpce_m_m.txt"){
  # USD 
  file_macro <- "~/R tests/finance related projects/usdpce_m_m.txt"
  file_prices <- USDCHF
}
else if(news_event == 15 || news_event == "usdemr_m_m.txt"){
  # USD employment rate 
  file_macro <- "~/R tests/finance related projects/usdemr_m_m.txt"
  file_prices <- USDCHF
}
else if(news_event == 16 || news_event == "usdcrs_m_m.txt"){
  # USD core retail sales 
  file_macro <- "~/R tests/finance related projects/usdcrs_m_m.txt"
  file_prices <- USDCHF
}
else {
print("incorrect input, please use a number or the file name including the .txt")
}
  
return(c(file_macro,file_prices))
}
#event <- "usdemr_m_m.txt"

file_macro <- retrieve_data(event)[1]
file_prices <- retrieve_data(event)[2]

## Data Cleaning and Sorting Forex Factory csvS
event_file <- read.delim(file_macro,header = TRUE)
event_file$Date <- as.Date(event_file$Date, format ="%b %d %Y")
event_file$Month <- as.character(event_file$Date, format = "%b", levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
event_file$Actual <- as.numeric(gsub("%","",event_file$Actual))
event_file$Forecast <- as.numeric(gsub("%","",event_file$Forecast))
event_file$Previous <- as.numeric(gsub("%","",event_file$Previous))
event_file$Difference <- event_file$Actual - event_file$Forecast
event_file$std_Difference <- event_file$Difference / sqrt(var(event_file$Difference))

event_file$Date_and_Time <- paste(as.character(event_file$Date), as.character(event_file$Time), sep = " ")
event_file$Date_and_Time <- as.POSIXct(event_file$Date_and_Time, format = "%Y-%m-%d %H:%M:%S")


## Data Cleaning and Sorting price_file MT5 csv file
price_file <- read.csv(file_prices, header = TRUE, sep = "\t")
price_file$X.DATE. <- as.Date(gsub("[.]", "-", price_file$X.DATE.))
price_file$X.DATE.TIME. <- 0
price_file$X.DATE.TIME. <- paste(as.character(price_file$X.DATE.), as.character(price_file$X.TIME.), sep = " ")
price_file$X.DATE.TIME. <- as.POSIXct(price_file$X.DATE.TIME., format = "%Y-%m-%d %H:%M:%S")

## Adding pips moved column
event_file$m5 <- 0
event_file$m30 <- 0
event_file$h1 <- 0
event_file$hlm5 <- 0
event_file$hlm30 <- 0
event_file$hlh1 <- 0
unique.X.DATE. <- sort(unique(price_file$X.DATE.), decreasing = TRUE)
endtime <- c(0,0,0)
enddate <- c(0,0,0)

## Cycle through every event date and define the variables so they are reset at every iteration
## Choose the most recent time of release and modifications will go from there; No DST.
for (i in event_file$Date[1:length(event_file$Date)]){
  
  starttime <- as.character(event_file$Time[which(event_file$Date == i)])
  end_date_time1 <- event_file$Date_and_Time[which(event_file$Date == i)] + 300
  end_date_time2 <- event_file$Date_and_Time[which(event_file$Date == i)] + 1800
  end_date_time3 <- event_file$Date_and_Time[which(event_file$Date == i)] + 3600
  endtime[1] <- strftime(end_date_time1, format = "%H:%M:%S")
  endtime[2] <- strftime(end_date_time2, format = "%H:%M:%S")
  endtime[3] <- strftime(end_date_time3, format = "%H:%M:%S")
  enddate[1] <- strftime(end_date_time1, format = "%Y-%m-%d")
  enddate[2] <- strftime(end_date_time2, format = "%Y-%m-%d")
  enddate[3] <- strftime(end_date_time3, format = "%Y-%m-%d")
  #zi <- 0
  
  ## Cycle through every date in the price data and match with the events days
  for(j in unique.X.DATE.[1:length(unique.X.DATE.)]){
    if(i == j){
      ## Finding the closest available price at or after the given starting/ending time
      ## While loop allows us to ensure a data point is actually selected
      startpips <- price_file[ which(price_file$X.DATE. == j & price_file$X.TIME. == starttime),3]
      cycle.minute.start <- as.numeric(substr(starttime,4,5))
      while(length(startpips)==0){
        cycle.minute.start <- cycle.minute.start - 1
        starttime <- sub("*:\\d+:*", paste(":",cycle.minute.start,":", sep = ""), starttime)
        startpips <- price_file[ which(price_file$X.DATE. == j & price_file$X.TIME. == starttime), 3]
        zi <- cycle.minute.start - as.numeric(substr(starttime,4,5))
      }
      #m5
      endpips.m5 <- price_file[ which(price_file$X.DATE. == enddate[1] & price_file$X.TIME. == endtime[1]),6]
      maxhi.m5 <- max(price_file[ which(price_file$X.DATE. == enddate[1] & price_file$X.TIME. == starttime[1])+0:+5+zi,4])
      minlo.m5 <- min(price_file[ which(price_file$X.DATE. == enddate[1] & price_file$X.TIME. == starttime[1])+0:+5+zi,5])
      cycle.minute.end <- as.numeric(substr(endtime[1],4,5))
      while(length(endpips.m5)==0){
        cycle.minute.end <- cycle.minute.end + 1
        endtime[1] <- sub("*:\\d+:*", paste(":",cycle.minute.end,":", sep = ""), endtime[1])
        endpips.m5 <- price_file[ which(price_file$X.DATE. == enddate[1] & price_file$X.TIME. == endtime[1]), 6]
      }
      ## Calculation of the difference between the start and ending times in pips 
      pips.m5 <- (endpips.m5 - startpips) * 10000
      hl.m5 <- (maxhi.m5 - minlo.m5) * 10000
      event_file$m5[which(event_file$Date == i)] <- pips.m5
      event_file$hlm5[which(event_file$Date == i)] <- hl.m5
      #m30
      endpips.m30 <- price_file[ which(price_file$X.DATE. == enddate[2] & price_file$X.TIME. == endtime[2]),6]
      cycle.minute.end <- as.numeric(substr(endtime[2],4,5))
      while(length(endpips.m30)==0){
        cycle.minute.end <- cycle.minute.end + 1
        endtime[2] <- sub("*:\\d+:*", paste(":",cycle.minute.end,":", sep = ""), endtime[2])
        endpips.m30 <- price_file[ which(price_file$X.DATE. == enddate[2] & price_file$X.TIME. == endtime[2]), 6]
      }
      ## Calculation of the difference between the start and ending times in pips 
      pips.m30 <- (endpips.m30 - startpips) * 10000
      event_file$m30[which(event_file$Date == i)] <- pips.m30
      
      #h1
      endpips.h1 <- price_file[ which(price_file$X.DATE. == enddate[3] & price_file$X.TIME. == endtime[3]),6]
      cycle.minute.end <- as.numeric(substr(endtime[3],4,5))
      while(length(endpips.h1)==0){
        cycle.minute.end <- cycle.minute.end + 1
        endtime[3] <- sub("*:\\d+:*", paste(":",cycle.minute.end,":", sep = ""), endtime[3])
        endpips.h1 <- price_file[ which(price_file$X.DATE. == enddate[3] & price_file$X.TIME. == endtime[3]), 6]
      }
      ## Calculation of the difference between the start and ending times in pips 
      pips.h1 <- (endpips.h1 - startpips) * 10000
      event_file$h1[which(event_file$Date == i)] <- pips.h1
    }
  }
}


#write.csv(event_file, file = "~/R tests/finance related projects/audret_dataframe.csv")

#______m_m <- event_file
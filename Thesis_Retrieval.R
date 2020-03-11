rm(list=ls())
require(lubridate)

# # Canadian CPI (Time is ET, GMT -5 and uses DST)
# file_macro <- "~/R tests/finance related projects/cadcpi_m_m.txt"
# file_prices <- "~/R tests/finance related projects/USDCAD_M1_200801020901_202002241546.csv"
# schedule_change_1 <- as.numeric(as.Date("2014-11-20"))
# schedule_change_2 <- as.numeric(as.Date("2012-03-23"))
# event_time1 <- "15:30:00" ; event_time2 <- "14:30:00" ; event_time3 <- "13:00:00"
# event_end1 <- c("15:35:00","16:00:00","16:30:00") ; event_end2 <- c("14:35:00","15:00:00","15:30:00") ; event_end3 <- c("13:05:00","13:30:00","14:00:00")

# #UK CPI (GMT 0 with DST)
# file_macro <- "~/R tests/finance related projects/gbpcpi_y_y.txt"
# file_prices <- "~/R tests/finance related projects/GBPUSD_M1_200001030001_202001081459.csv"
# schedule_change_1 <- as.numeric(as.Date("2014-11-20"))
# schedule_change_2 <- 0
# event_time1 <- "11:30:00" ; event_time2 <- "10:30:00"
# event_end1 <- c("11:35:00","12:00:00","12:30:00") ; event_end2 <- c("10:35:00","11:00:00","11:30:00")

# # US CPI (Metatrader 5 time in file)
file_macro <- "~/R tests/finance related projects/usdcpi_m_m.txt"
file_prices <- "~/R tests/finance related projects/USDCHF_M1_200801020900_202001081800.csv"
schedule_change_1 <- as.numeric(as.Date("2014-11-20"))
schedule_change_2 <- 0
event_time1 <- "15:30:00" ; event_time2 <- "14:30:00"
event_end1 <- c("15:35:00","16:00:00","16:30:00") ; event_end2 <- c("14:35:00","15:00:00","15:30:00")

# Australian CPI (quarterly announcements)

## Data Cleaning and Sorting Forex Factory csvS
event_file <- read.delim(file_macro,header = TRUE)
event_file$Date <- as.Date(event_file$Date, format ="%b %d %Y")
event_file$Month <- as.character(event_file$Date, format = "%b", levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
event_file$Year <- year(event_file$Date)
event_file$Actual <- as.numeric(gsub("%","",event_file$Actual))
event_file$Forecast <- as.numeric(gsub("%","",event_file$Forecast))
event_file$Previous <- as.numeric(gsub("%","",event_file$Previous))
event_file$Difference <- event_file$Actual - event_file$Forecast
event_file$std_Difference <- event_file$Difference / sqrt(var(event_file$Difference))


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
unique.X.DATE. <- sort(unique(price_file$X.DATE.), decreasing = TRUE)

## Cycle through every event date and define the variables so they are reset at every iteration
## Choose the most recent time of release and modifications will go from there; No DST.
for (i in event_file$Date[1:length(event_file$Date)]){
  starttime <- event_time1
  endtime <- event_end1
  ## Account for a schedule change in the release
  if(i <= schedule_change_1){
    starttime <- event_time2
    endtime <- event_end2
  }
  ## Account for a schedule change in the release
  if(i <= schedule_change_2){
    starttime <- event_time3
    endtime <- event_end3
  }
  ## Cycle through every date in the price data and match with the events days
  for(j in unique.X.DATE.[1:length(unique.X.DATE.)]){
    if(i == j){
      ## Finding the closest available price at or after the given starting/ending time
      ## While loop allows us to ensure a data point is actually selected
      startpips <- price_file[ which(price_file$X.DATE. == j & price_file$X.TIME. == starttime),3]
      cycle.minute.start <- as.numeric(substr(starttime,4,5))
      while(length(startpips)==0){
        cycle.minute.start <- cycle.minute.start + 1
        starttime <- sub("*:\\d+:*", paste(":",cycle.minute.start,":", sep = ""), starttime)
        startpips <- price_file[ which(price_file$X.DATE. == j & price_file$X.TIME. == starttime), 3]
      }
      #m5
      endpips.m5 <- price_file[ which(price_file$X.DATE. == j & price_file$X.TIME. == endtime[1]),6]
      cycle.minute.end <- as.numeric(substr(endtime[1],4,5))
      while(length(endpips.m5)==0){
        cycle.minute.end <- cycle.minute.end + 1
        endtime[1] <- sub("*:\\d+:*", paste(":",cycle.minute.end,":", sep = ""), endtime[1])
        endpips.m5 <- price_file[ which(price_file$X.DATE. == j & price_file$X.TIME. == endtime[1]), 6]
      }
      ## Calculation of the difference between the start and ending times in pips 
      pips.m5 <- (endpips.m5 - startpips) * 10000
      event_file$m5[which(event_file$Date == i)] <- pips.m5
      #m30
      endpips.m30 <- price_file[ which(price_file$X.DATE. == j & price_file$X.TIME. == endtime[2]),6]
      cycle.minute.end <- as.numeric(substr(endtime[2],4,5))
      while(length(endpips.m30)==0){
        cycle.minute.end <- cycle.minute.end + 1
        endtime[2] <- sub("*:\\d+:*", paste(":",cycle.minute.end,":", sep = ""), endtime[2])
        endpips.m30 <- price_file[ which(price_file$X.DATE. == j & price_file$X.TIME. == endtime[2]), 6]
      }
      ## Calculation of the difference between the start and ending times in pips 
      pips.m30 <- (endpips.m30 - startpips) * 10000
      event_file$m30[which(event_file$Date == i)] <- pips.m30
      
      #h1
      endpips.h1 <- price_file[ which(price_file$X.DATE. == j & price_file$X.TIME. == endtime[3]),6]
      cycle.minute.end <- as.numeric(substr(endtime[3],4,5))
      while(length(endpips.h1)==0){
        cycle.minute.end <- cycle.minute.end + 1
        endtime[3] <- sub("*:\\d+:*", paste(":",cycle.minute.end,":", sep = ""), endtime[3])
        endpips.h1 <- price_file[ which(price_file$X.DATE. == j & price_file$X.TIME. == endtime[3]), 6]
      }
      ## Calculation of the difference between the start and ending times in pips 
      pips.h1 <- (endpips.h1 - startpips) * 10000
      event_file$h1[which(event_file$Date == i)] <- pips.h1
    }
  }
}

# write.csv(event_file, file = "~/R tests/finance related projects/audret_dataframe.csv")
# write.csv(event_file, file = "~/R tests/finance related projects/gbpcpi_dataframe.csv")

mod1 <- lm(m5 ~ std_Difference, data = event_file)


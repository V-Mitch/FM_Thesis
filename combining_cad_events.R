#requires stable model to be run with right events
#combine two events cadcpi cadcrs

data_1_o <- read.csv("~/R tests/finance related projects/cadcpi_dataframe.csv")
data_1 <- data_1_o[nrow(data_1_o):1,]

data_2_o <- read.csv("~/R tests/finance related projects/cadcrs_dataframe.csv")
data_2 <- data_2_o[nrow(data_2_o):1,]

cpivec <- rep("cadcpi", nrow(data_1))
crsvec <- rep("cadcrs", nrow(data_2))
colnames(data_1)[10] <- "dif_cpi"
colnames(data_2)[9] <- "dif_crs"
data_1$dif_crs <- 0
data_2$dif_cpi <- 0

cadtot <- cbind(c(cpivec,crsvec),rbind(data_1[,c(-8,-9)], data_2[,c(1:7,9,11:13,17)]))
cadtot <- cadtot[order(as.Date(cadtot$Date)),]

duplicates <- cadtot[which(duplicated(cadtot$Date)|duplicated(cadtot$Date, fromLast = TRUE)),] 
            #  && 
            # duplicated(cadtot$m5)|duplicated(cadtot$m5, fromLast = TRUE)
            # ),]
            # 

duplicates <- duplicates[17:nrow(duplicates),]


firstcad <- cadtot[1:77,]
secondcad <- cadtot[78:nrow(cadtot),]


secondcad2 <- secondcad

for(i in 1:nrow(secondcad)){
  if(is.na(secondcad2$`c(cpivec, crsvec)`[i])){
    break
  }
  if(secondcad2$`c(cpivec, crsvec)`[i] == "cadcpi"){
    if(secondcad2[i,10] == secondcad2[i+1,10]){
      if(secondcad2[i,3] == secondcad2[i,3])
      secondcad2[i,13] <- secondcad2[i+1,13]
      secondcad2 <- secondcad2[-(i+1),]
    }
  }
}

cadfinal <- rbind(firstcad,secondcad2)
mod1 <- lm(m5 ~ dif_cpi + dif_crs, data = cadfinal)
X_t <- rbind(cadfinal$dif_cpi, cadfinal$dif_crs)

data_1_o <- read.csv("~/R tests/finance related projects/cadcpi_dataframe.csv")
data_1 <- data_1_o[nrow(data_1_o):1,]

removals <- match(as.Date(duplicates$Date), as.Date(data_1$Date))[c(TRUE,FALSE)]

dim(data_1[-removals,])
data_1 <- data_1[-removals,]

mod1 <- lm(m5 ~ std_Difference, data = data_1)
X_t <- t(as.matrix(data_1$std_Difference))
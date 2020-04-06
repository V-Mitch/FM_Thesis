# Requires Thesis_Retrieval File

# List of datasets
# cadcrs_m_m ; usdahe_m_m ; 

data_1_o <- read.csv("~/R tests/finance related projects/cadcrs_dataframe.csv")
data_1 <- data_1_o[nrow(data_1_o):1,]

data_2_o <- read.csv("~/R tests/finance related projects/cademr_dataframe.csv")
data_2 <- data_2_o[nrow(data_1_o):1,]

data_3_o <- read.csv("~/R tests/finance related projects/usdemr_m_m_dataframe.csv")
data_3 <- data_3_o[nrow(data_1_o):1,]

# data_3$std_Difference <- data_3$std_Difference * -1 (avoid singular matrices)

# 1 parameter case
# mod1 <- lm(hlm5 ~ abs(std_Difference), data = data_1)
# mod1 <- lm(m30 ~ std_Difference, data = data_1)
mod1 <- lm(m5 ~ std_Difference, data = data_1)
X_t <- t(as.matrix(data_1$std_Difference))

# 2 parameters case
mod1 <- lm(data_1$m5 ~ data_1$std_Difference + data_2$std_Difference)
X_t <- rbind(data_1$std_Difference, data_2$std_Difference)

# 3 parameters case
mod1 <- lm(data_1$m5 ~ data_1$std_Difference + data_2$std_Difference + data_3$std_Difference)
X_t <- rbind(data_1$std_Difference, data_2$std_Difference, data_3$std_Difference)


#### Asymmetry case

library(dplyr)


data_1$std_Neg <- 0 ; data_1$std_Pos <- 0
for(i in 1:nrow(data_1)){
  if(data_1$std_Difference[i] <= 0){
    data_1$std_Neg[i] <- data_1$std_Difference[i]
  }
  else{
    data_1$std_Pos[i] <- data_1$std_Difference[i]
  }
}


mod1 <- lm(m5 ~ std_Difference, data = data_1)

mod2 <- lm(m5 ~ std_Neg + std_Pos , data = data_1)

mod1 <- lm(m5 ~ I(std_Neg + std_Pos), data = data_1)




X_t <- rbind(data_1$std_Pos, data_1$std_Neg)
X_t <- t(as.matrix(data_1$std_Pos))
X_t <- t(as.matrix(data_1$std_Neg))
#mod1 <- lm(m5 ~ std_Neg + std_Pos + I(std_Neg^2) + I(std_Pos^2), data = data_1)
  


head(data_1)

library(sandwich)
library(lmtest)
# HAC standard errors
# When our error terms are serially correlated. We don't have all of the independent variables.
# New estimator for the variance of Betas. Higher variance.
data_1 <- read.csv("~/R tests/finance related projects/cadcpi_dataframe.csv")
mod1 <- lm(m5 ~ std_Difference, data = data_1)

m <- 0.75 * (length(data_1$std_Difference))^(1/3)

NW_VCOV <- NeweyWest(lm(m5 ~ std_Difference, data = data_1),
                     lag = m - 1, prewhite = F,
                     adjust = T)

# NW_VCOV <- NeweyWest(lm(data_1$m5 ~ data_1$std_Difference + data_2$std_Difference + data_3$std_Difference),
#                      lag = m - 1, prewhite = F,
#                      adjust = T)
sqrt(diag(NW_VCOV))[2]

coeftest(mod1, vcov = NW_VCOV)






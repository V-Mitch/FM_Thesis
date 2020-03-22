# Requires Thesis_Retrieval File

# List of datasets
# cadcrs_m_m ; usdahe_m_m ; 

data_1 <- read.csv("~/R tests/finance related projects/cadcrs_dataframe.csv")

data_2 <- read.csv("~/R tests/finance related projects/audemr_dataframe.csv")

data_3 <- read.csv("~/R tests/finance related projects/usdemr_m_m_dataframe.csv")

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


#### Asymmetry
library(dplyr)
library(magrittr)

data_1$std_Difference_neg <- 0
data_1$std_Difference_neg <- data_1$std_Difference[which(data_1$std_Difference <= 0)]
  


head(data_1)


# HAC standard errors
# When our error terms are serially correlated. We don't have all of the independent variables.
# New estimator for the variance of Betas. Higher variance.


m <- 0.75 * (length(data_1$std_Difference))^(1/3)

NW_VCOV <- NeweyWest(lm(m5 ~ std_Difference, data = event_file),
                     lag = m - 1, prewhite = F,
                     adjust = T)
sqrt(diag(NW_VCOV))[2]

m <- coeftest(mod1, vcov = NW_VCOV)

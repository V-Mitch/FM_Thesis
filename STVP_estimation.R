# Requires Stable Model File

# Standard Recursive Time Variable Parameter

# 1 Param
#S_t <- t(as.matrix(data_1$std_Difference))
# 2 Param
S_t <- rbind(data_1$std_Difference, data_2$std_Difference)
#3 Param
#S_t <- rbind(data_1$std_Difference, data_2$std_Difference, data_3$std_Difference)

Y_t <- t(as.matrix(data_1$m5))
icept <- rep(1,T)
S_t <- rbind(icept, S_t) 

k <- dim(S_t)[1]
T <- nrow(data_1)
beta_hat <- matrix(0,nrow = k, ncol = T)
upper <- matrix(0,nrow = k, ncol = T)
lower <- matrix(0,nrow = k, ncol = T)
epsilon <- matrix(0,nrow = 1, ncol = T)
epsilon_nd <- matrix(0,nrow = 1, ncol = T)
sigma_ML_secondpart <- rep(1,T)
beta_OLS <- as.vector(mod1$coefficients)
beta_hat[,1] <- c(0,0)
g_t <- matrix(0,nrow = k, ncol = T)
residual_error <- matrix(0,nrow = k, ncol = T)
p_t <- matrix(seq(0,0),nrow = k, ncol = k) # p_t matrix will continuously be replaced
p_0 <- matrix(seq(0,0),nrow = k, ncol = k)
diag(p_0) <- rep(10^6, dim(S_t)[1])

sigma <- sqrt(1/(T-dim(S_t)[1] -1) * sum(mod1$residuals^2))
#sigma <- sqrt(sigma_ML_sqr)

R_t <- data_1$m5

# Assuming Random Walk 4.21 is used to describe parameter variations
I <- diag(k) # identity matrix instead of A
I[1,1] <- 0 # assume the intercept is 0.
#I[2,2] <- 0
# Assuming constant parameter variation, the variations are not correlated to one another
#Qa <-  diag(summary(mod1)$coefficients[,2])^2 
Qa <-  diag((1.25*beta_OLS - beta_OLS)^2, nrow = k)
Qa[1,1] <- 0
#Qa[2,2] <- 0
Qnvr <- Qa / sigma^2
p_ttrack <- rep(0,T)
p_tpriors <- rep(0,T)

# Initialization
sigma_temp <- 999
g_t[,1] <- p_0 %*% S_t[,1] %*% (sigma_temp^2 + t(S_t[,1]) %*% p_0 %*% S_t[,1]) ^ -1
p_t <- p_0 - g_t[,1] %*% t(S_t[,1]) %*% p_0


# nrow(data_1
for(t in 2:nrow(data_1)){
  
  #empirical sigma - use full sample until time t
  if(t>k){
  residual_error[2,t] <- R_t[t] - S_t[t] * beta_hat[2,t-1]
  sigma_temp <- sqrt(1/ (t - k) * sum(residual_error[2,1:t]^2))
  #sigma_temp <- sqrt(1/(t-dim(S_t)[1] -1) * sum( lm(m5 ~ std_Difference, data = data_1[1:t,])$residuals^2))
  }
  
  #nvr <- Qa / sigma_temp^2
  
  #priori
  beta_hat[,t] <- I %*% beta_hat[,t-1]
  p_t <- I %*% p_t %*% t(I) + I %*% Qa %*% t(I)
  p_tpriors[t] <- p_t[2,2]
  
  # correction
  g_t[,t] <- p_t %*% S_t[,t] %*% (sigma_temp^2 + t(S_t[,t]) %*% p_t %*% S_t[,t]) ^ -1
  beta_hat[,t] <- beta_hat[,t-1] + g_t[,t] %*% (R_t[t] - S_t[,t] %*% beta_hat[,t-1])
  
  p_t <-  p_t - g_t[,t] %*% t(S_t[,t]) %*% p_t
  
  epsilon[t] <- R_t[t] - S_t[2,t] * beta_hat[2,t-1]
  #epsilon[t] <- t(S_t[,t]) %*% beta_hat[,t]
  
  # upper[,t] <- beta_hat[,t] + 1.96 * sqrt(diag(p_t)*sigma^2/sqrt(t))
  # lower[,t] <- beta_hat[,t] - 1.96 * sqrt(diag(p_t)*sigma^2/sqrt(t))
  upper[,t] <- beta_hat[,t] + 1.96 * sqrt(diag(p_t))#/sqrt(t))
  lower[,t] <- beta_hat[,t] - 1.96 * sqrt(diag(p_t))#/sqrt(t))
  
  # sigma ML
  #sigma_ML_secondpart[t] <- epsilon[t]^2 / (1 + t(S_t[,t]) %*% p_t %*% S_t[,t])
  p_ttrack[t] <- p_t[2,2]
}

sigma_ML_sqr <- 1 / (T-k) * sum(sigma_ML_secondpart[(k+1):T])

par(mfrow=c(1,2))
plot(beta_hat[1,] ~ as.Date(data_1$Date), type = "l")
lines(rep(mod1$coefficients[1],T) ~ as.Date(data_1$Date), lty = "dotdash")
lines(upper[1,]~ as.Date(data_1$Date), type = "l", lty = 3)
lines(lower[1,]~ as.Date(data_1$Date), type = "l", lty = 3)

plot(beta_hat[2,] ~ as.Date(data_1$Date), type = "l", ylim=range(-100,100))
lines(rep(mod1$coefficients[2],T) ~ as.Date(data_1$Date), lty = "dotdash")
lines(upper[2,]~ as.Date(data_1$Date), type = "l", lty = 3)
lines(lower[2,]~ as.Date(data_1$Date), type = "l", lty = 3)

# plot(beta_hat[3,] ~ as.Date(data_1$Date), type = "l")
# lines(rep(mod1$coefficients[3],T) ~ as.Date(data_1$Date), lty = "dotdash")
# lines(upper[3,]~ as.Date(data_1$Date), type = "l", lty = 3)
# lines(lower[3,]~ as.Date(data_1$Date), type = "l", lty = 3)


stvp_df <- data.frame(x = as.vector(beta_hat[2,]), as.vector(upper[2,]), as.vector(lower[2,]), 
                      rep(mod1$coefficients[2],T), as.Date(data_1$Date))
colnames(stvp_df) <- c("beta_hat", "upper","lower", "beta_ols","date")

# stvp_df <- data.frame(x = as.vector(beta_hat[3,]), as.vector(upper[3,]), as.vector(lower[3,]),
#                       rep(mod1$coefficients[3],T), as.Date(data_1$Date))
# colnames(stvp_df) <- c("beta_hat", "upper","lower", "beta_ols","date")

##write.csv(stvp_df, file = "~/R tests/finance related projects/stvp_df.csv")
##write.csv(stvp_df, file = "~/R tests/finance related projects/usdnfp_path_stvp.csv")

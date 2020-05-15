# Requires Stable Model File

# Standard Recursive Time Variable Parameter

# 1 Param
S_t <- t(as.matrix(data_1$std_Difference))
# 2 Param
#S_t <- rbind(data_1$std_Difference, data_2$std_Difference)
#3 Param
#S_t <- rbind(data_1$std_Difference, data_2$std_Difference, data_3$std_Difference)

Y_t <- t(as.matrix(data_1$m5))
icept <- rep(1,T)
S_t <- rbind(icept, rep(0,T), S_t, rep(0,T)) 

k <- dim(S_t)[1]
T <- nrow(data_1)
beta_hat <- matrix(0,nrow = k, ncol = T)
upper <- matrix(0,nrow = k, ncol = T)
lower <- matrix(0,nrow = k, ncol = T)
epsilon <- matrix(0,nrow = 1, ncol = T)
epsilon_nd <- matrix(0,nrow = 1, ncol = T)
beta_hat[,1] <- c(0,0,0,0)
g_t <- matrix(0,nrow = k, ncol = T)
p_t <- matrix(seq(0,0),nrow = k, ncol = k) # p_t matrix will continuously be replaced
p_0 <- matrix(seq(0,0),nrow = k, ncol = k)
diag(p_0) <- rep(10^6, dim(S_t)[1])

sigma <- sqrt(1/(T-dim(S_t)[1] -1) * sum(mod1$residuals^2))

R_t <- data_1$m5

# 
A1 <- matrix(c(1,0,1,1), nrow = 2)
D1 <- matrix(c(0,0,0,1), nrow = 2)
A2 <- matrix(c(1,0,1,1), nrow = 2)
D2 <- matrix(c(0,0,0,1), nrow = 2)
A <- as.matrix(bdiag(A1,A2))
D <- as.matrix(bdiag(D1,D2))
# Assuming constant parameter variation, the variations are not correlated to one another
Qa <-  diag(summary(mod1)$coefficients[,2]) 
Qeta <- diag(c(0, 0, Qa[2,2], 0))
Qnvr <- Qeta/sigma

# Initialization
g_t[,1] <- p_0 %*% S_t[,1] %*% (1 + t(S_t[,1]) %*% p_0 %*% S_t[,1]) ^ -1
p_t <- A %*% p_0 %*% t(A) + D %*% Qnvr %*% t(D)

# nrow(data_1
for(t in 2:nrow(data_1)){
  
  #priori
  beta_hat[,t] <- A %*% beta_hat[,t-1]
  p_t <- A %*% p_t %*% t(A) + D %*% Qnvr %*% t(D)
  # correction
  g_t[,t] <- p_t %*% S_t[,t] %*% (1 + t(S_t[,t]) %*% p_t %*% S_t[,t]) ^ -1
  beta_hat[,t] <- beta_hat[,t-1] + g_t[,t] %*% (R_t[t] - S_t[,t] %*% beta_hat[,t-1])
  
  p_t <-  p_t - g_t[,t] %*% t(S_t[,t]) %*% p_t
  
  upper[,t] <- beta_hat[,t] + 1.96 * sqrt(diag(p_t)/sqrt(t))
  lower[,t] <- beta_hat[,t] - 1.96 * sqrt(diag(p_t)/sqrt(t))
  
}

par(mfrow=c(1,1))
#par(mfrow=c(1,2))
plot(beta_hat[3,] ~ as.Date(data_1$Date), type = "l")
lines(rep(mod1$coefficients[2],T) ~ as.Date(data_1$Date), lty = "dotdash")
lines(upper[3,]~ as.Date(data_1$Date), type = "l", lty = 3)
lines(lower[3,]~ as.Date(data_1$Date), type = "l", lty = 3)


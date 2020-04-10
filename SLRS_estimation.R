# Requires Stable Model File

# RLS method for a stable mean

# 1 Param
S_t <- t(as.matrix(data_1$std_Difference))
# 2 Param
#S_t <- rbind(data_1$std_Difference, data_2$std_Difference)
# 3 Param
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
beta_hat[,1] <- c(-0.4458, 12.6814)
g_t <- matrix(0,nrow = k, ncol = T)
p_t <- matrix(seq(0,0),nrow = k, ncol = k) # p_t matrix will continuously be replaced
p_0 <- matrix(seq(0,0),nrow = k, ncol = k)
diag(p_0) <- c(1.7294,1.7288)

sigma <- sqrt(1/(T-dim(S_t)[1] -1) * sum(mod1$residuals^2))


R_t <- data_1$m5

# Initialization
g_t[,1] <- p_0 %*% S_t[,1] %*% (sigma^2 + t(S_t[,1]) %*% p_0 %*% S_t[,1]) ^ -1
p_t <- p_0 - g_t[,1] %*% t(S_t[,1]) %*% p_0

# nrow(data_1
for(t in 2:nrow(data_1)){
  g_t[,t] <- p_t %*% S_t[,t] %*% ( sigma^2 + t(S_t[,t]) %*% p_t %*% S_t[,t]) ^ -1
  p_t <- p_t - g_t[,t] %*% t(S_t[,t]) %*% p_t
  beta_hat[,t] <- beta_hat[,t-1] + g_t[,t] %*% (R_t[t] - S_t[,t] %*% beta_hat[,t-1])
  #upper[,t] <- sqrt(1/T*(dim(X_t[1])-1)*mod1$residuals^2)
  upper[,t] <- beta_hat[,t] + 1.96 * sqrt(diag(p_t)/sqrt(t))
  lower[,t] <- beta_hat[,t] - 1.96 * sqrt(diag(p_t)/sqrt(t))
  epsilon[t] <- t(S_t[,t]) %*% beta_hat[,t]
}



#par(mfrow=c(1,2))
plot(beta_hat[2,] ~ as.Date(data_1$Date), type = "l")
lines(rep(mod1$coefficients[2],T) ~ as.Date(data_1$Date), lty = "dotdash")
lines(upper[2,]~ as.Date(data_1$Date), type = "l", lty = 3)
lines(lower[2,]~ as.Date(data_1$Date), type = "l", lty = 3)


# rls_mod <- RLS(data_1$m5, data_1$std_Difference, ist = 30)
# plot(rls_mod$beta, type = "l")
# lines(rep(mod1$coefficients[2],T), lty = "dotdash")
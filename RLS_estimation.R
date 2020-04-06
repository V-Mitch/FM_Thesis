# Requires Stable Model File

# RLS method for a stable mean

# 1 Param
S_t <- t(as.matrix(data_1$std_Difference))
# 2 Param
S_t <- rbind(data_1$std_Difference, data_2$std_Difference)
# 3 Param
S_t <- rbind(data_1$std_Difference, data_2$std_Difference, data_3$std_Difference)




k <- dim(S_t)[1]
T <- nrow(data_1)
beta_hat <- matrix(0,nrow = k, ncol = T)
beta_hat[1] <- c(-25)
g_t <- matrix(0,nrow = k, ncol = T)
p_t <- matrix(seq(0,0),nrow = k, ncol = k) # p_t matrix will continuously be replaced
p_0 <- matrix(seq(0,0),nrow = k, ncol = k)
diag(p_0) <- 0.23

R_t <- data_1$m5

# Initialization
g_t[,1] <- p_0 %*% S_t[,1] %*% (1 + t(S_t[,1]) %*% p_0 %*% S_t[,1]) ^ -1
p_t <- p_0 - g_t[,1] %*% t(S_t[,1]) %*% p_0

# nrow(data_1
for(t in 2:nrow(data_1)){
  g_t[,t] <- p_t %*% S_t[,t] %*% ( 1 + t(S_t[,t]) %*% p_t %*% S_t[,t]) ^ -1
  p_t <- p_t - g_t[,t] %*% t(S_t[,t]) %*% p_t
  beta_hat[,t] <- beta_hat[,t-1] + g_t[,t] %*% (R_t[t] - S_t[,t] %*% beta_hat[,t-1])
}

#par(mfrow=c(1,2))
plot(beta_hat[1,] ~ as.Date(data_1$Date), type = "l")
lines(rep(mod1$coefficients[2],T), lty = "dotdash")


rls_mod <- RLS(data_1$m5, data_1$std_Difference, ist = 30)
plot(rls_mod$beta, type = "l")
lines(rep(mod1$coefficients[2],T), lty = "dotdash")
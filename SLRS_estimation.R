# Requires Stable Model File

# RLS method for a stable mean

# 1 Param
S_t <- t(as.matrix(data_1$std_Difference))
# 2 Param
#S_t <- rbind(data_1$std_Difference, data_2$std_Difference)
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
beta_hat[,1] <- c(0,0)
#beta_hat[,1] <- as.vector(mod1$coefficients)
g_t <- matrix(0,nrow = k, ncol = T)
p_t <- matrix(seq(0,0),nrow = k, ncol = k) # p_t matrix will continuously be replaced
p_0 <- matrix(seq(0,0),nrow = k, ncol = k)
diag(p_0) <- rep(10^6, dim(S_t)[1])

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
  epsilon[t] <- R_t[t] - S_t[2,t] * beta_hat[2,t-1]
  #epsilon[t] <- t(S_t[,t]) %*% beta_hat[,t]
  epsilon_nd[t] <- epsilon[t] / (1 + t(S_t[,t]) %*% (p_t/sigma^2) %*% S_t[,t] ) ^ 0.5
}


par(mfrow=c(1,1))
#par(mfrow=c(1,2))
plot(beta_hat[2,] ~ as.Date(data_1$Date), type = "l")
lines(rep(mod1$coefficients[2],T) ~ as.Date(data_1$Date), lty = "dotdash")
lines(upper[2,]~ as.Date(data_1$Date), type = "l", lty = 3)
lines(lower[2,]~ as.Date(data_1$Date), type = "l", lty = 3)

slrs_df <- data.frame(x = as.vector(beta_hat[2,]), as.vector(upper[2,]), as.vector(lower[2,]), 
                      rep(mod1$coefficients[2],T), as.Date(data_1$Date))
colnames(slrs_df) <- c("beta_hat", "upper","lower", "beta_ols","date")

#write.csv(slrs_df, file = "~/R tests/finance related projects/slrs_df.csv")

# rls_mod <- RLS(data_1$m5, data_1$std_Difference, ist = 30)
# plot(rls_mod$beta, type = "l")
# lines(rep(mod1$coefficients[2],T), lty = "dotdash")

# CUSUM test

par(mfrow=c(1,2))

# setup
w_t <- matrix(0,nrow = 1, ncol = T)

h <-  dim(S_t)[1] - 1
p1 <- 1.143 * (T - h) ^ 0.5
p2 <- 3 * 1.143 * (T - h) ^ 0.5
p3 <- 0.948 * (T - h) ^ 0.5
p4 <- 3 * 0.948 * (T - h) ^ 0.5
p5 <- 0.850 * (T - h) ^ 0.5
p6 <- 3 * 0.850 * (T - h) ^ 0.5

sigma_cusum <- 1 / (T - h) * sum(epsilon_nd^2)

for(t in 1:T){
  w_t[t] <- 1/sqrt(sigma_cusum) * sum(epsilon_nd[1:t])
}



plot(as.vector(w_t), type = "l", ylim = c(-p2,p2))
segments(h,p1,T,p2, col = "goldenrod1")
segments(h,-p1,T,-p2, col = "goldenrod1")
segments(h,p3,T,p4, col = "goldenrod2")
segments(h,-p3,T,-p4, col = "goldenrod2")
segments(h,p5,T,p6, col = "goldenrod3")
segments(h,-p5,T,-p6, col = "goldenrod3")

cusum_df <- data.frame(x = as.vector(w_t), as.Date(data_1$Date))
points_df <- data.frame(x = as.Date(data_1$Date[h]), as.Date(data_1$Date[T]), p1, p2 ,p3 ,p4 ,p5 ,p6)
colnames(cusum_df) <- c("w_t", "date")
colnames(points_df) <- c("startdate", "enddate","p1","p2","p3","p4","p5","p6")
#write.csv(cusum_df, file = "~/R tests/finance related projects/cusum_df.csv")
#write.csv(points_df, file = "~/R tests/finance related projects/points_df.csv")

# CUSUM-squared test
# setup
w2_t <- matrix(0,nrow = 1, ncol = T)
beta_dist_line <- matrix(0,nrow = 1, ncol = T)
upper995 <- matrix(0,nrow = 1, ncol = T)

for(t in 2:T){
  w2_t[t] <- 1/ sum(epsilon_nd^2) * sum(epsilon_nd[1:t]^2)
  beta_dist_line[t] <- (t-h)/(T-h)
}

plot(as.vector(w2_t), type = "l")
lines(as.vector(beta_dist_line), col = "blue")
lines(as.vector(beta_dist_line) + 0.144, col = "purple", lty=4)
lines(as.vector(beta_dist_line) - 0.144, col = "purple", lty=4)
lines(as.vector(beta_dist_line) + 0.114, col = "purple", lty=5)
lines(as.vector(beta_dist_line) - 0.114, col = "purple", lty=5)
lines(as.vector(beta_dist_line) + 0.100, col = "purple", lty=5)
lines(as.vector(beta_dist_line) - 0.100, col = "purple", lty=5)

cusum_sq_df <- data.frame(x = as.vector(w2_t), as.Date(data_1$Date),
                          as.vector(beta_dist_line))
colnames(cusum_sq_df) <- c("w2_t", "date", "beta_dist_line")
#write.csv(cusum_sq_df, file = "~/R tests/finance related projects/cusum_sq_df.csv")





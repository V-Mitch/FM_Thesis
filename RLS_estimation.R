# Requires Stable Model File

# RLS method for a stable mean


T <- nrow(data_1)
beta_hat <- rep(0,T)
g_t <- rep(0,T)
p_t <- rep(0,T)
p_0 <- 0.1
S_t <- data_1$std_Difference 
R_t <- data_1$m5

g_t[1] <- p_0 * S_t[1] * (1 + S_t[1] * p_0 * S_t[1]) ^ -1
p_t[1] <- p_0 - g_t[1] * S_t[1] * p_0


for(t in 2:nrow(data_1)){
g_t[t] <- p_t[t-1] * S_t[t] * ( 1 + S_t[1] * p_t[t-1] * S_t[1])^-1
p_t[t] <- p_t[t-1] - g_t[t] * S_t[t] * p_t[t-1]
beta_hat[t] <- beta_hat[t-1] + g_t[t] * (R_t[t] - S_t[t] * beta_hat[t-1])
}

plot(beta_hat, type = "l")

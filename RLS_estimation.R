# Requires Stable Model File

# RLS method for a stable mean


T <- nrow(data_1)
beta_hat <- rep(0,T)
g_t <- rep(0,T)
p_t <- rep(0,T)
p_0 <- 0.23
S_t <- data_1$std_Difference 
R_t <- data_1$m5

g_t[1] <- p_0 * S_t[1] * (1 + S_t[1] * p_0 * S_t[1]) ^ -1
p_t[1] <- p_0 - g_t[1] * S_t[1] * p_0


for(t in 2:nrow(data_1)){
g_t[t] <- p_t[t-1] * S_t[t] * ( 1 + S_t[1] * p_t[t-1] * S_t[1])^-1
p_t[t] <- p_t[t-1] - g_t[t] * S_t[t] * p_t[t-1]
beta_hat[t] <- beta_hat[t-1] + g_t[t] * (R_t[t] - S_t[t] * beta_hat[t-1])
}

par(mfrow=c(1,2))
plot(beta_hat, type = "l", ylim = c(-13,2))
lines(rep(mod1$coefficients[2],T), lty = "dotdash")

rls_mod <- RLS(data_1$m5, data_1$std_Difference, ist = 30)
plot(rls_mod$beta, type = "l", ylim = c(-13,2))
lines(rep(mod1$coefficients[2],T), lty = "dotdash")
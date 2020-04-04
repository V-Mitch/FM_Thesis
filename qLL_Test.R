# Requires Stable_Model_Null File

# qLL Test for k = 1 parameter. Following Elliot and M?ller (2006) methodology
# H0: Stability of the parameter. HA: Instability of the parameter.

# Step 1
Eps_t <- t(as.matrix(mod1$residuals))

# Step 2
X_t <- X_t
k <- nrow(X_t)
n = length(mod1$residuals)
residuals_test <- lm(Eps_t[-n] ~ Eps_t[-1]) # no autocorrelation -> use heteroscedasticity robust estimator
ans <- 0

for (t in 1:n){
  a <- (X_t[,t] %*% t(X_t[,t])) * Eps_t[,t]^2
  ans <- a + ans
}
V_x <- 1/n * ans

# Old univariate calculation
# V_x <- n^-1 * sum(X_t * t(X_t) * Eps_t^2)

# Step 3
V_x_ns <- sqrt(solve(V_x))
U_t <- matrix(ncol = n, nrow = k)
for (t in 1:n){
  U_t[,t] <- V_x_ns %*% X_t[,t] * Eps_t[,t]
}

# Step 4
r <- 1 - 10/n
ome_t <- matrix(ncol = n, nrow = k)
ome_t[,1] <- U_t[,1]
for (i in 2:n){
  ome_t[,i] <- r * ome_t[,i-1] + (U_t[,i] - U_t[,i-1])
}

# Step 5
r_vec <- rep(r^seq(1:n))
Eps_2_t_sqr <- matrix(ncol = n, nrow = k)
for (i in 1:k){
    mod2 <- lm(ome_t[i,] ~ r_vec)
    Eps_2_t_sqr[i,] <- mod2$residuals^2
}

S_Eps_2_t_sqr <- sum(Eps_2_t_sqr)

# Step 6
r * S_Eps_2_t_sqr - sum(U_t^2)

# Nyblom/Hansen stability test 1989


# for (t in 1:T){
#   temp_frame <- data_1[t,]
#   
#   # Main elements of regression and abbreviation of Sum of Errors for simplification
#   X_t <- temp_frame$std_Difference
#   Y_t <- temp_frame$m5
#   d_t <- (Y_t - B0 - B1 * X_t)
#   
#   # First derivatives and Score
#   dl_B1[t] <- -1/sig_sqr * sum(d_t*X_t) 
#   dl_sig_sqr[t] <- -t + 1/sig_sqr * sum(d_t)^2
# }
# 
# S <- sum(c(dl_B1, dl_sig_sqr))

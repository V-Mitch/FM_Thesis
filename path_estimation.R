# Requires qLL_Test File

# Parameter Path estimation in Unstable regression coefficients. M?ller and Petalas (2010).

# With respect to the vector of parameters: B0, B1, and sigma squared
# S_t is the score function or first derivative of the log likelihood
# H is the negative of the second derivative of the log likelihood (hessian for multivariate)

#### SETUP ####
# Some fixed parameters

B0 <- mod1$coefficients[1]
B1 <- mod1$coefficients[2]
sig_sqr <- var(mod1$residuals) 

T <- nrow(data_1)

# Main elements of regression and abbreviation of Sum of Errors for simplification
X_t <- data_1$std_Difference
Y_t <- data_1$m5
d_t <- (Y_t - B0 - B1 * X_t)

# Partial First derivatives and Score
dl_B1 <- -1/sig_sqr * sum(d_t*X_t) 
dl_sig_sqr <- -T + 1/sig_sqr * sum(d_t)^2
dl_B0 <- -1/sig_sqr * sum(d_t)
s_t <- c(dl_B1, dl_sig_sqr, dl_B0)

# Partial Second derivatives and Hessian
dl_B1B1 <- sum(X_t^2) / sig_sqr
dl_B1sig_sqr <- 1/sig_sqr^2 * sum(d_t*X_t)
dl_B1B0 <- sum(X_t) / sig_sqr
dl_sig_sqrB1 <- -2/sig_sqr * sum(d_t*X_t)
dl_sig_sqrsig_sqr <- 1/sig_sqr^2 * sum(d_t^2)
dl_sig_sqrB0 <- -2/sig_sqr * sum(d_t)
dl_B0B1 <- mean(X_t) / sig_sqr
dl_B0sig_sqr <- 1/sig_sqr^2 * sum(d_t)
dl_B0B0 <- T / sig_sqr
H <- 1/T * matrix(c(dl_B1B1, dl_B1sig_sqr, dl_B1B0,
              dl_sig_sqrB1, dl_sig_sqrsig_sqr, dl_sig_sqrB0,
              dl_B0B1, dl_B0sig_sqr, dl_B0B0),
            nrow = 3, ncol = 3, byrow = TRUE)

V <- T^-1 * s_t %*% t(s_t) 

#### PATH LOOP ####
# For loop for the calculation of the Path

# We take only the parameter that is changing in time
H <- H[1,1]

Hinv <- solve(H)

c <- seq(from = 0, to = 50, by = 5)
qLL <- rep(0,11)
w <- rep(0,11)
B1_t <- matrix(ncol = T, nrow = 11)
Path_Matrix <- matrix(ncol = T, nrow = 11)
a_t <- rep(0,T)
b_t <- rep(0,T)
z_t <- rep(0,T)
r_pow <- rep(0,T)
z_b <- rep(0,T)
r <- 0
dl_B1 <- rep(0,T)
  
# Step 1
for (t in 1:T){
temp_frame <- data_1[t,]

# Main elements of regression and abbreviation of Sum of Errors for simplification
X_t <- temp_frame$std_Difference
Y_t <- temp_frame$m5
d_t <- (Y_t - B0 - B1 * X_t)

# First derivatives and Score
dl_B1[t] <- -1/sig_sqr * sum(d_t*X_t) 
# dl_sig_sqr <- -t + 1/sig_sqr * sum(d_t)^2
# dl_B0 <- -1/sig_sqr * sum(d_t)
# s_t <- c(dl_B1, dl_sig_sqr, dl_B0)
s_t <- dl_B1
}

V <- 1/T * sum(dl_B1^2)
Vinv <- solve(V)

a_t <- (c(Hinv) * s_t)
b_t <- (c(H) * c(Vinv) * s_t)

# Step 2
for (i in 1:length(c)){
  for (t in 2:T){
  # (a)
  r <- 1 - c[i] / T 
  z_t[1] <- a_t[1] ; z_t[t] <- r*z_t[t-1] + a_t[t] - a_t[t-1]
  }
  r_pow <- r^(seq(0,T-1,1))
  # (b)
  z_resid <- lm(z_t ~ r_pow)$residuals
  # (c)
  z_b[T] <- z_resid[T] 
  for (t in (T-1):1){
    z_b[t] <- r * z_b[t+1] + z_resid[t] - z_resid[t+1]
  }
  # (d)
  B1_t[i,] <- B1 + a_t - r*z_b
  # (e)
  if(r < 0){
    r = 0
  }
  qLL[i] <- t(r * z_b - a_t) %*% b_t
  w[i] <- sqrt(T*(1-r^2) * r^(T-1) / (1- r^(2*T))) * exp(-1/2 * qLL[i])
  w[1] <- 1
}


# Step 3
w_new <- rep(0,10)
for (i in 1:11){
w_new[i] <- w[i] / sum(w)
Path_Matrix[i,] <- B1_t[i,] * w_new[i]
}

# Step 4

B1_final <-  apply(Path_Matrix, 2, sum)
plot(B1_final, type = "l", main = paste("coefficient path"))
# write.csv(B1_final, file = "~/R tests/finance related projects/cadcrs_path.csv")

# Step 5 (variance and Bayesian intervals)



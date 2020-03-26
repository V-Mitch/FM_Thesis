### HAC understanding
library(quantmod)


# function that computes rho tilde
acf_c <- function(x, j) {
  return(
    t(x[-c(1:j)]) %*% na.omit(Lag(x, j)) / t(x) %*% x
  )
}

# simulate time series with serially correlated errors
set.seed(1)

N <- 145

eps <- arima.sim(n = N, model = list(ma = 0.3))
X <- runif(N, 1, 10)
Y <- 0.5 * X + eps

# compute OLS residuals
res <- lm(Y ~ X)$res

# compute v
v <- (X - mean(X)) * res

# compute robust estimate of beta_1 variance
var_beta_hat <- 1/N * (1/(N-2) * sum((X - mean(X))^2 * res^2) ) / 
  (1/N * sum((X - mean(X))^2))^2

# rule of thumb truncation parameter
m <- floor(0.75 * N^(1/3))

# compute correction factor
f_hat_T <- 1 + 2 * sum(
  (m - 1:(m-1))/m * sapply(1:(m - 1), function(i) acf_c(x = v, j = i))
) 

# compute Newey-West HAC estimate of the standard error 
sqrt(var_beta_hat * f_hat_T)

## [1] 0.04036208







# Using NeweyWest():
NW_VCOV <- NeweyWest(lm(Y ~ X), 
                     lag = m - 1, prewhite = F, 
                     adjust = T)

# compute standard error
sqrt(diag(NW_VCOV))[2]
##          X 
## 0.04036208
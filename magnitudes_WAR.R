
library(ggplot2)
library(ggthemes)

####
# X_t <- t(as.matrix(data_1$std_Difference))
# 
# 
# plot(B1_t[11,], type = "l")
# for (i in 1:10){
#   lines(B1_t[i,])
# }
# lines(B1_final, col = "red")
# 
# N <- 144 

# grw <- rnorm(N, B1, 5)
# lines(grw, col = "blue")


####
#plot(w_new, type = "l")

# ggplot(data=B1_t, aes(x = X, y = B1_final)) +
#   geom_line() +
#   geom_line(aes(x = X, y = upper), color="dark gray", linetype = "dotdash") +
#   geom_line(aes(x = X, y = lower), color="dark gray", linetype = "dotdash") +
#   geom_hline(yintercept = 0, linetype="dashed", color = "dark gray") +
#   ylab("Beta") +
#   ggtitle(title) +
#   theme_tufte()

#beta_weights <- read.csv("~/R tests/finance related projects/gbpcpi_magn.csv")
#betapath <- read.csv("~/R tests/finance related projects/gbpcpi_path.csv")

plotwar <- function(betapath, beta_weights, title){
  B1_final <- betapath$B1_final
  Time <- seq(0,135)
  dat <- data.frame(t(rbind(Time, beta_weights)))[-1,]
  dat <- data.frame(cbind(B1_final, dat))
  #c("Time","one","two","three","four","five","six","seven","eight","nine","ten","eleven") <- colnames(dat)
  ggplot(data=dat, aes(x = X1)) +
    geom_line(aes(y = X2), color = "steelblue", linetype = "twodash") +
    geom_line(aes(y = X3), color = "darkgray") +
    geom_line(aes(y = X4), color = "darkgray") +
    geom_line(aes(y = X5), color = "darkgray") +
    geom_line(aes(y = X6), color = "darkgray") +
    geom_line(aes(y = X7), color = "darkgray") +
    geom_line(aes(y = X8), color = "darkgray") +
    geom_line(aes(y = X9), color = "darkgray") +
    geom_line(aes(y = X10), color = "darkgray") +
    geom_line(aes(y = X11), color = "darkgray") +
    geom_line(aes(y = X12), color = "darkgray") +
    geom_line(aes(y = B1_final), color = "darkgreen") +
    ggtitle(title) +
    theme_tufte()
}

plotcusum <- function(cusumpath, cusumpoints){
  cusumpath$date <- as.Date(cusumpath$date)
  cusumpoints$startdate <- as.Date(cusumpoints$startdate)
  cusumpoints$enddate <- as.Date(cusumpoints$enddate)
  ggplot() +
    geom_line(data = cusumpath, aes(x = date, y = w_t), color = "black", linetype = a) +
    geom_hline(yintercept = 0, linetype="dashed", color = "steel blue") +
    geom_segment(data = cusumpoints, aes(x = startdate, y = p1, xend = enddate, yend = p2), linetype = "dotted") +
    geom_segment(data = cusumpoints, aes(x = startdate, y = -p1, xend = enddate, yend = -p2), linetype = "dotted") +
    geom_segment(data = cusumpoints, aes(x = startdate, y = p3, xend = enddate, yend = p4), linetype = "dotted") +
    geom_segment(data = cusumpoints, aes(x = startdate, y = -p3, xend = enddate, yend = -p4), linetype = "dotted") +
    geom_segment(data = cusumpoints, aes(x = startdate, y = p5, xend = enddate, yend = p6), linetype = "dotted") +
    geom_segment(data = cusumpoints, aes(x = startdate, y = -p5, xend = enddate, yend = -p6), linetype = "dotted") +
    ggtitle("CUSUM Test") +
    theme_tufte()
}

plotcusumsq <- function(cusumsqdf){
  cusumsqdf$date <- as.Date(cusumsqdf$date)
  # cusumpoints$startdate <- as.Date(cusumpoints$startdate)
  # cusumpoints$enddate <- as.Date(cusumpoints$enddate)
  ggplot() +
    geom_line(data = cusumsqdf, aes(x = date, y = w2_t), color = "black", linetype = a) +
    geom_line(data = cusumsqdf, aes(x = date, y = beta_dist_line), color = "steel blue", linetype ="dashed") +
    geom_line(data = cusumsqdf, aes(x = date, y = beta_dist_line + 0.144), linetype ="dotted") +
    geom_line(data = cusumsqdf, aes(x = date, y = beta_dist_line - 0.144), linetype ="dotted") +
    geom_line(data = cusumsqdf, aes(x = date, y = beta_dist_line + 0.114), linetype ="dotted") +
    geom_line(data = cusumsqdf, aes(x = date, y = beta_dist_line - 0.114), linetype ="dotted") +
    geom_line(data = cusumsqdf, aes(x = date, y = beta_dist_line + 0.100), linetype ="dotted") +
    geom_line(data = cusumsqdf, aes(x = date, y = beta_dist_line - 0.100), linetype ="dotted") +
    ggtitle("CUSUM-squared Test") +
    theme_tufte()
}

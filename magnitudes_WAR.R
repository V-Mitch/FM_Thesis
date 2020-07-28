
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
  Time <- as.Date(read.csv("~/R tests/finance related projects/gbpcpi_path_stvp.csv")$date)
  dat <- data.frame(t(rbind(B1_final, beta_weights)))[-1,]
  dat$Year <- as.Date(Time)
  #c("Time","one","two","three","four","five","six","seven","eight","nine","ten","eleven") <- colnames(dat)
  ggplot(data=dat, aes(x = Year)) +
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
    scale_x_date(date_breaks = "1 year", date_labels="%Y") +
    labs(y = "Beta") +
    ggtitle(title) +
    theme_tufte()
}

plotcusum <- function(cusumpath, cusumpoints){
  cusumpath$Year <- as.Date(cusumpath$date)
  cusumpoints$startdate <- as.Date(cusumpoints$startdate)
  cusumpoints$enddate <- as.Date(cusumpoints$enddate)
  ggplot() +
    geom_line(data = cusumpath, aes(x = Year, y = w_t), color = "black", linetype = "solid") +
    geom_hline(yintercept = 0, linetype="dashed", color = "steel blue") +
    geom_segment(data = cusumpoints, aes(x = startdate, y = p1, xend = enddate, yend = p2), linetype = "dotted") +
    geom_segment(data = cusumpoints, aes(x = startdate, y = -p1, xend = enddate, yend = -p2), linetype = "dotted") +
    geom_segment(data = cusumpoints, aes(x = startdate, y = p3, xend = enddate, yend = p4), linetype = "dotted") +
    geom_segment(data = cusumpoints, aes(x = startdate, y = -p3, xend = enddate, yend = -p4), linetype = "dotted") +
    geom_segment(data = cusumpoints, aes(x = startdate, y = p5, xend = enddate, yend = p6), linetype = "dotted") +
    geom_segment(data = cusumpoints, aes(x = startdate, y = -p5, xend = enddate, yend = -p6), linetype = "dotted") +
    scale_x_date(date_breaks = "1 year", date_labels="%Y") +
    labs(y = "Cumulative Standardized Errors") +
    ggtitle("CUSUM Test applied on UK CPI") +
    theme_tufte()
}

plotcusumsq <- function(cusumsqdf){
  cusumsqdf$Year <- as.Date(cusumsqdf$date)
  # cusumpoints$startdate <- as.Date(cusumpoints$startdate)
  # cusumpoints$enddate <- as.Date(cusumpoints$enddate)
  ggplot() +
    geom_line(data = cusumsqdf, aes(x = Year, y = w2_t), color = "black", linetype = "solid") +
    geom_line(data = cusumsqdf, aes(x = Year, y = beta_dist_line), color = "steel blue", linetype ="dashed") +
    geom_line(data = cusumsqdf, aes(x = Year, y = beta_dist_line + 0.144), linetype ="dotted") +
    geom_line(data = cusumsqdf, aes(x = Year, y = beta_dist_line - 0.144), linetype ="dotted") +
    geom_line(data = cusumsqdf, aes(x = Year, y = beta_dist_line + 0.114), linetype ="dotted") +
    geom_line(data = cusumsqdf, aes(x = Year, y = beta_dist_line - 0.114), linetype ="dotted") +
    geom_line(data = cusumsqdf, aes(x = Year, y = beta_dist_line + 0.100), linetype ="dotted") +
    geom_line(data = cusumsqdf, aes(x = Year, y = beta_dist_line - 0.100), linetype ="dotted") +
    scale_x_date(date_breaks = "1 year", date_labels="%Y") +
    labs(y = "Cumulative Contribution to Total Errors") +
    ggtitle("CUSUM-squared Test on UK CPI") +
    theme_tufte()
}

plotslrs <- function(slrsdata){
  slrsdata$Year <- as.Date(slrsdata$date)
  #c("beta_hat", "beta_ols","upper","lower", "date")
  ggplot()+
    geom_line(data = slrsdata, aes(x = Year, y = beta_hat), color = "black", linetype = "solid") +
    geom_line(data = slrsdata, aes(x = Year, y = beta_ols), color = "steel blue", linetype ="dashed") +
    geom_line(data = slrsdata, aes(x = Year, y = upper ), linetype ="dotted") +
    geom_line(data = slrsdata, aes(x = Year, y = lower), linetype ="dotted") +
    scale_x_date(date_breaks = "1 year", date_labels="%Y") +
    labs(y = "Beta") +
    coord_cartesian(ylim = c(max(slrsdata$upper[10:nrow(slrsdata)]), min(slrsdata$lower[10:nrow(slrsdata)]))) +
    #ylim(min(slrsdata$beta_ols)-3,max(slrsdata$beta_ols)-3) +
    theme_tufte()
}

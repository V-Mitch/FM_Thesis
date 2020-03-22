
####
X_t <- t(as.matrix(data_1$std_Difference))
plot(B1_t[11,], type = "l")
for (i in 1:10){
  lines(B1_t[i,])
}
lines(B1_final, col = "red")

N <- 144 
grw <- rnorm(N, B1, 5)
lines(grw, col = "blue")


####
plot(w_new, type = "l")

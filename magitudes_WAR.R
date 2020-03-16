
####
X_t <- t(as.matrix(data_1$std_Difference))
plot(B1_t[11,], type = "l")
for (i in 1:10){
  lines(B1_t[i,])
}
lines(B1_final, col = "red")
points(X_t, col = "blue")

####
plot(w_new, type = "l")

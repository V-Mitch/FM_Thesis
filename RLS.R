# RLS Method

rls_mod <- RLS(data_1$m5, data_1$std_Difference, ist = 30)
plot(rls_mod$beta, type = "l", ylim = c(-35,20))
points(data_1$m5/5)

plot(data_1$std_Difference, type = "l")
plot(data_1$m5, type = "l")

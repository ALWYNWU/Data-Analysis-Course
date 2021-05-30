# (a)
sim_data = rbind(matrix(rnorm(20*50, mean = 0), nrow = 20),
           matrix(rnorm(20*50, mean=0.7), nrow = 20),
           matrix(rnorm(20*50, mean=1.4), nrow = 20))

# (b)
sim_data.pca = prcomp(X)$x
plot(sim_data.pca[,1:2], col=c(rep(1,20), rep(2,20), rep(3,20)))

# (c)
res = kmeans(sim_data, centers = 3)
true_class = c(rep(1,20), rep(2,20), rep(3,20))
table(res$cluster, true_class)

# (d)
res = kmeans(sim_data, centers = 2)
true_class = c(rep(1,20), rep(2,20), rep(3,20))
table(res$cluster, true_class)

# (e)
res = kmeans(sim_data, centers = 4)
true_class = c(rep(1,20), rep(2,20), rep(3,20))
table(res$cluster, true_class)

# (f)
res = kmeans(sim_data.pca[,1:2], centers = 3)
true = c(rep(1,20), rep(2,20), rep(3,20))
table(res$cluster, true_class)


# (g)
res = kmeans(scale(sim_data), centers = 3)
true = c(rep(1,20), rep(2,20), rep(3,20))
table(res$cluster, true_class)

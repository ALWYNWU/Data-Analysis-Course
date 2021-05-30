# (a)
x = cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
plot(x[,1], x[,2])
n = 6


# (b)
set.seed(7)
clusters = sample(2, nrow(x), replace = T)
clusters
plot(x[, 1], x[, 2], col = (clusters + 1), pch = 20, cex = 2)


# (c)
centroids = aggregate(x, list(Cluster = clusters), mean)
centroids


# (d)
library(class)
clusters = knn(centroids[,2:3], x, factor(centroids[,1]))
clusters

col = rep("red", n)
col[clusters == 2] = "blue"
pch = rep(16, n)
pch[clusters == 2] = 17
plot(x, col = col, pch = pch)


# (e)
centroids = aggregate(x, list(Cluster = clusters), mean)
centroids

plot(x, col = col, pch = pch)
points(centroids[1,2:3], col = "red", pch = 8)
points(centroids[2,2:3], col = "blue", pch = 8)

clusters = knn(centroids[,2:3], x, factor(centroids[,1]))
clusters

centroids = aggregate(x, list(Cluster = clusters), mean)
centroids

# (a)
col = rep("orange", n)
col[clusters == 2] = "green"
pch = rep(16, n)
pch[clusters == 2] = 17
plot(x, col = col, pch = pch)
points(centroids[1,2:3], col = "orange", pch = 8)
points(centroids[2,2:3], col = "green", pch = 8)



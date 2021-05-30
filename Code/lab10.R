############## Lab 10 ####################

rm(list = ls())  # clear the memory
###########################


require(graphics)

### Violent crime rates by US state
### This data set contains statistics, in arrests per 100,000 residents 
### for assault, murder, and rape in each of the 50 US states in 1973.

USArrests


hc1 <- hclust(dist(USArrests), "single")
hc2 <- hclust(dist(USArrests), "complete")
hc3 <- hclust(dist(USArrests), "ward.D2")

plot(hc1)
plot(hc1, hang = -1)

par(mfrow=c(3,1))

plot(hc1, hang = -1,sub = "single linkage")
groups1 <- cutree(hc1, k=5) 
rect.hclust(hc1, k=5, border="red") 
groups1

plot(hc2, sub = "complete linkage")
groups2 <- cutree(hc1, k=5) 
rect.hclust(hc1, k=5, border="blue") 
groups2

plot(hc3, sub = "Ward's linkage")
groups3 <- cutree(hc3, k=5) 
rect.hclust(hc3, k=5, border="green") 
groups3



### Straight-line distances among 10 US cities
data(UScitiesD)

mds2 <- -cmdscale(UScitiesD)
plot(mds2, type="n", axes=FALSE, ann=FALSE)
text(mds2, labels=rownames(mds2), xpd = NA)


hcity1 <- hclust(UScitiesD, "single")
hcity2 <- hclust(UScitiesD, "ave")
hcity3 <- hclust(UScitiesD, "ward.D2")

par(mfrow=c(3,1))

plot(hcity1, hang = -1,sub = "single linkage")
groups1 <- cutree(hcity1, k=3) 
rect.hclust(hcity1, k=3, border="red") 
groups1

plot(hcity2, sub = "complete linkage")
groups2 <- cutree(hcity2, k=3) 
rect.hclust(hcity2, k=3, border="blue") 
groups2

plot(hcity3, sub = "Ward's linkage")
groups3 <- cutree(hcity3, k=3) 
rect.hclust(hcity3, k=3, border="green") 
groups3

par(mfrow=c(1,1))




# nonhierarchical clustering k-means
# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
(cl <- kmeans(x, 2))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)


kmeans(x,3)$withinss 
kmeans(x,3)$betweenss 
kmeans(x,3)$tot.withinss 

## random starts do help here with too many clusters
## (and are often recommended anyway!):
ssratio = vector(length = 10)
for(k in 1:length(ssratio)) {
  fit = kmeans(x, k, nstart = 25)  # Tries numerous random starts
  ssratio[k] = fit$betweenss/fit$totss
}

plot(ssratio) # Levels out after k = 4 or 5

## random starts do help here with too many clusters
## (and are often recommended anyway!):
(cl1 <- kmeans(x, 5, nstart = 25))
plot(x, col = cl1$cluster)
points(cl1$centers, col = 1:5, pch = 8)

cl2 = hclust(dist(x), method = "centroid") # compare with agglomerative clustering

par(mfrow=c(2,1))


groups1 = cl1$cluster
groups2 = cutree(cl2, k = 5) 

groups1
groups2

plot(groups1, sub = "k-means")
plot(groups2, sub = "hierarchical, centroid linkage")



############ Lecture 10 Iris data ##########

data(iris)


pairs(iris[,-5],pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])

#dev.off()

library(MASS)

### LDA 

iris.lda = lda(Species ~ ., data = iris,prior = c(1,1,1)/3)

iris.pred = predict(iris.lda, data= iris[,-5])

table(iris.pred$class,iris[,5])

# draw decision boundary 

LD = iris.pred$x

LD.data = data.frame(cbind(LD,iris.pred$class))
names(LD.data) = c('LD1','LD2','Res')

lda.fit = lda(Res ~.,data=LD.data,prior = c(1,1,1)/3)

# lda.pred = predict(lda.fit, data= LD.data[,-3])

# table(lda.pred$class,iris[,5])


GS <- 250
x1 <- seq(min(LD.data[,1]), max(LD.data[,1]), len=GS)
x2 <- seq(min(LD.data[,2]), max(LD.data[,2]), len=GS)

lda.Ghat <- as.numeric(predict(lda.fit, newdata=list(LD1=x1,LD2=x2))$class)


plot(iris.pred$x, col=c("red","green3","blue")[unclass(iris$Species)])
contour(x1, x2, matrix(lda.Ghat, GS,GS), levels=c(1,2,3), add=TRUE, drawlabels=FALSE, col="pink",lwd = 2)

#dev.off()

# # install.packages("klaR")
# library(klaR)
# partimat(Species ~ .,data=iris,method="lda") 
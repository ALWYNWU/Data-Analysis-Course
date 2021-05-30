########## Lab 9 ##############

set.seed(500)
library(MASS)
data <- Boston


attach(data)
# First do a linear fit:
boston.lm = lm(log(medv) ~ crim+zn+indus)

######## PPR fitting 

shape1 = 100*shape 
area1 = area/400
peri1 = peri/100
c(mean(shape1), mean(area1), mean(peri1))

apply(data,2,mean) #2 indicates columns

boston.ppr = ppr(log(medv) ~ crim+zn+indus, nterms = 1, max.terms = 5)
boston.ppr
boston.ppr1 = ppr(log(medv) ~crim+zn+indus, nterms = 1, max.terms = 3)
boston.ppr4 = ppr(log(medv) ~ crim+zn+indus, nterms = 4, max.terms = 4)

summary(boston.ppr1)

######## Plotting

par(mfrow=c(2,2))
plot(boston.lm$fitted, boston.lm$resid)
plot(boston.ppr1)
plot(boston.ppr1$fitted, boston.ppr1$resid)
plot(boston.ppr4$fitted, boston.ppr4$resid)

summary(boston.ppr4)







#First we need to check that no datapoint is missing, 
#otherwise we need to fix the dataset.
apply(data,2,function(x) sum(is.na(x))) #2 indicates columns

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)


#Preparing to fit the neural network

#normalize your data
#min-max method and scale the data in the interval [0,1]. 
#Usually scaling in the intervals [0,1] or [-1,1] tends to give better results.
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

#install.packages("neuralnet")
#this is a toy example, we are going to use 2 hidden layers 
#swith this configuration: 13:5:3:1. 
#The input layer has 13 inputs, 
#the two hidden layers have 5 and 3 neurons and 
#the output layer has, of course, a single output since we are doing regression.
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)


plot(nn)


#Predicting medv using the neural network

pr.nn <- compute(nn,test_[,1:13])

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
we then compare the two MSEs

print(paste(MSE.lm,MSE.nn))
#Apparently the net is doing a better work than the linear model at predicting 

par(mfrow=c(1,2))

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

par(mfrow=c(1,1))
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))


#A (fast) cross validation
library(boot)
set.seed(200)
lm.fit <- glm(medv~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]

set.seed(450)
cv.error <- NULL
k <- 10

#install.packages("plyr")
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
  
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}

mean(cv.error)
cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)






#deep learning for iris
#install.packages("h2o")
library(h2o) 
h2o.init() 
iris.hex <- as.h2o(iris)
#k=10 fold cross-validations
set.seed(5000)
k=10
train = sample(1:nrow(iris), nrow(iris))
folds = matrix(train,k,nrow(iris)/k) #each row represents 1 fold
cv.error = rep(0,k)
for(i in 1:k){
 iris.train <- iris[as.vector(folds[-i,]),]
 iris.test <-  iris[as.vector(folds[i,]),]
 iris.hex.train <- as.h2o(iris.train)
 iris.hex.test <- as.h2o(iris.test)
 iris.dl.train <- h2o.deeplearning(x = 1:4, y = 5, training_frame = iris.hex.train) 
 predictions <- h2o.predict(iris.dl.train, iris.hex.test)
 cv.error[i] <- sum(as.vector(as.character(predictions$predict))!=as.character(iris.test$Species))/15
}
 cv.error
 mean(cv.error)


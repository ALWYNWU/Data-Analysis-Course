# (a)
set.seed(7)
X1 = rnorm(100,mean = 0,sd=1)
X2 = rnorm(100,mean = 0,sd=1)
Z = rnorm(100,mean = 0,sd=1)
y = sigmoid(3*X1 + 3*X2) + (3*X1 - 3*X2)^2 + 0.3*Z

library(neuralnet)
K = 5
MSE = NULL
data_set = data.frame(X1,X2,y)



for (i in 1:10){
  for (j in 1:K){
    index <- sample(1:100,round(0.9*100))
    train <- data_set[index,]
    test <- data_set[-index,]
    
    nn <- neuralnet(y~X1+X2,data=train,hidden=c(i),linear.output=T)
    
    pred <- compute(nn,test)
    #pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
    
    #test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
    
   
  }
}

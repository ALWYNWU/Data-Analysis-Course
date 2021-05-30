# (a)
Hitters = na.omit(Hitters)
Hitters$Salary = log(Hitters$Salary)

# (b)
index = 1:200
train = Hitters[index, ]
test = Hitters[-index, ]

# (c)
library(gbm)
set.seed(11)
pows = seq(-10, -0.2, by = 0.1)
lambdas = 10^pows
train.err = rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.hitters = gbm(Salary ~ ., data = train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  pred.train = predict(boost.hitters, train, n.trees = 1000)
  train.err[i] = mean((pred.train - train$Salary)^2)
}
plot(lambdas, train.err, type = "b", xlab = "Shrinkage values", ylab = "Training MSE")

# (d)
set.seed(11)
test.err <- rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.hitters <- gbm(Salary ~ ., data = train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  yhat <- predict(boost.hitters, test, n.trees = 1000)
  test.err[i] <- mean((yhat - test$Salary)^2)
}
plot(lambdas, test.err, type = "b", xlab = "Shrinkage values", ylab = "Test MSE")

# (e)
library(glmnet)

x = model.matrix(Salary~.,train)
x.test = model.matrix(Salary ~ . , test)
y = train$Salary
ridge = glmnet(x,y,alpha=0)
ridge_predict = predict(ridge,s=0.01,x.test)
ridge_test_mse = mean((ridge_predict-test$Salary)^2)
ridge_test_mse


lasso = glmnet(x,y,alpha=1)
lasso_predict = predict(lasso,s=0.01,x.test)
lasso_test_mse = mean((lasso_predict-test$Salary)^2)
lasso_test_mse

# (f)
boost = gbm(Salary~.,data=train,distribution = "gaussian",n.trees = 1000,shrinkage=lambdas[which.min(test.err)])

summary(boost)

# (g)
library(randomForest)
set.seed(11)
bagging = randomForest(Salary~.,train,mtry=19,importance=TRUE)
bagg_predict = predict(bagging,test)
bagg_test_mse = mean((bagg_predict-test$Salary)^2)
bagg_test_mse








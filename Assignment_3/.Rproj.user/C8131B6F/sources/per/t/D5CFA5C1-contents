library(ISLR)
library(randomForest)
set.seed(7)

# (a)
subset = sample(nrow(Carseats),nrow(Carseats)*0.8)
train = Carseats[subset,]
test = Carseats[-subset,]

# (b)
library(tree)
reg_tree = tree(Sales ~ ., data = train)
summary(reg_tree)

plot(reg_tree)
text(reg_tree, pretty = 0)

yhat <- predict(reg_tree, newdata = test)
mean((yhat - test$Sales)^2)
# MSE is 3.866483

# (c)
cv_car = cv.tree(reg_tree)
plot(cv_car$size, cv_car$dev, type = "b")

prune.carseats = prune.tree(reg_tree, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

yhat = predict(prune.carseats, newdata = test)
mean((yhat - test$Sales)^2)

# (d)
bagging = randomForest(Sales ~ ., data = train, mtry = 10, ntree = 500, importance = TRUE)
yhat_bag = predict(bagging, newdata = test)
mean((yhat_bag - test$Sales)^2)

importance(bagging)

# (e)
rf = randomForest(Sales ~ ., data = train, mtry = 3, ntree = 500, importance = TRUE)
yhat.rf = predict(rf, newdata = test)
mean((yhat.rf - test$Sales)^2)

importance(rf)







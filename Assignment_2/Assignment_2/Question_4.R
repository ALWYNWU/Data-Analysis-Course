library(ISLR)
require(e1071)
require(caret)
data(OJ)


# Question a
dim(OJ)
train_index = sample(nrow(OJ), 800, replace = FALSE)

train = OJ[train_index,]
dim(train)

test = OJ[-train_index,]
dim(test)

# Question b
OJ.svm = svm(Purchase ~ ., data = train, kernel = 'linear', cost = 0.01)
summary(OJ.svm)

# Question c
postResample(predict(OJ.svm, train), train$Purchase)
postResample(predict(OJ.svm, test), test$Purchase)


# Question d
OJ.tune = tune.svm(Purchase~., data = OJ,cost=seq(0.01,10,by=0.5))
summary(OJ.tune)

# Question e
postResample(predict(OJ.tune$best.model, train), train$Purchase)

postResample(predict(OJ.tune$best.model, test), test$Purchase)


# Question f
OJ.radial = svm(Purchase ~ ., data = train, kernel = 'radial')
summary(OJ.radial)

postResample(predict(OJ.radial, train), train$Purchase)
postResample(predict(OJ.radial, test), test$Purchase)


# Question g
OJ.poly = svm(Purchase ~ ., data = train, kernel = 'polynomial', degree = 2)
summary(OJ.poly)

postResample(predict(OJ.poly, train), train$Purchase)
postResample(predict(OJ.poly, test), test$Purchase)

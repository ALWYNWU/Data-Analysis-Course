#Question a
library(ISLR)
summary(Weekly)
pairs(Weekly,panel = panel.smooth,upper.panel = NULL)


#Question b
attach(Weekly)
Weekly.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly,family=binomial)
summary(Weekly.fit)


#Question c
Weekly.prob = predict(Weekly.fit, Weekly, type = "response")
Weekly.pred = ifelse(Weekly.prob > 0.5, "Up","Down")
Weekly.confusion = table(Weekly.pred, Direction)
Weekly.confusion

Weekly.accuracy = round(mean(Weekly.pred == Direction)*100,2)

Weekly.recall = round(length(which(Weekly.pred == "Down" & Direction 
                          == "Down"))/length(which(Direction == "Down"))*100,2)

Weekly.precision = round(length(which(Weekly.pred == "Up" & Direction 
                            == "Up"))/length(which(Direction == "Up"))*100,2)

Weekly.evaluation = data.frame(Accuracy = Weekly.accuracy, Precision = 
                      Weekly.precision, Recall = Weekly.recall)

Weekly.evaluation


#Question d
dim(Weekly)

train = Weekly[Weekly$Year <= 2008,]
dim(train)

test = Weekly[Weekly$Year > 2008,]
dim(test)

Weekly.fit2 = glm(Direction ~ Lag2, data=train, family=binomial)
summary(Weekly.fit2)

Weekly.prob2 = predict(Weekly.fit2, test, type="response")

Weekly.pred2 = ifelse(Weekly.prob2 > 0.5, "Up","Down")

Weekly.confusion2 = table(Weekly.pred2, test$Direction)
Weekly.confusion2

Weekly.accuracy2 = round(mean(Weekly.pred2 == test$Direction)*100,2)
Weekly.accuracy2


#Question e
library(MASS)
Weekly.LDA.fit = lda(Direction~Lag2, data=train)
Weekly.LDA.pred = predict(Weekly.LDA.fit, test)
names(lda.pred)
Weekly.LDA.pred$class

LDA.confusion = table(Weeklylda.pred$class, test$Direction)
LDA.confusion


#Question f
Weekly.QDA.fit = qda(Direction~Lag2, data=train)
Weekly.QDA.fit

Weekly.QDA.pred = predict(Weekly.QDA.fit, test)
names(Weekly.QDA.pred)
Weekly.QDA.pred$class

QDA.confusion = table(Weekly.QDA.pred$class, test$Direction)
QDA.confusion
















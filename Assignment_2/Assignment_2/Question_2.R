library(ISLR)
data("Auto")

#Question a
summary(Auto)
attach(Auto)
mpg01 = ifelse( mpg > median(mpg), yes = 1, no = 0)
mpg01
Auto = data.frame(Auto, mpg01)
summary(Auto)


#Question b
pairs(Auto,panel = panel.smooth,upper.panel = NULL)

par(mfrow=c(2,3))
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")


#Question c

# split the data into training set and testing set according to the year
dim(Auto)
train = (year %% 2 == 0)
Auto.train = Auto[train, ]
dim(Auto.train)

Auto.test = Auto[!train,]
mpg01.test <- mpg01[!train]
dim(Auto.test)


#Question d
Auto.fit.LDA = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
Auto.fit.LDA

Auto.pred.LDA = predict(Auto.fit.LDA, Auto.test)
table(Auto.pred.LDA$class, mpg01.test)

mean(Auto.pred.LDA$class != mpg01.test)


#Question e
Auto.fit.QDA = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
Auto.fit.QDA 

Auto.pred.QDA = predict(Auto.fit.QDA, Auto.test)
table(Auto.pred.QDA$class, mpg01.test)

mean(Auto.pred.QDA$class != mpg01.test)


#Question f
Auto.glm = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, family = binomial, subset = train)
summary(Auto.glm)

Auto.prob = predict(Auto.glm, Auto.test, type = "response")
pred.glm = rep(0, length(Auto.prob))
pred.glm[Auto.prob > 0.5] = 1
table(pred.glm, mpg01.test)

mean(pred.glm != mpg01.test)

############## lec 7 Credit data ############

defaultData = read.table("Default.txt",header=T)  


# Logistic Regression

glm.fit=glm(default~balance,data=defaultData,family=binomial)
summary(glm.fit)
predict(glm.fit, list(balance = c(1000,2000)),type="response")


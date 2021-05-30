############## lec 8 Credit data ############

defaultData = read.table("Default.txt",header=T)  


# Logistic Regression

glm.fit=glm(default~student,data=defaultData,family=binomial)
summary(glm.fit)
predict(glm.fit, list(student = c('Yes','No')),type="response")

glm.fit=glm(default~balance+income+student,data=defaultData,family=binomial)
summary(glm.fit)
predict(glm.fit, list(balance = 1500, income = 40000,student = c('Yes')),type="response")



#Question(a)
Auto = read.csv("Auto.csv", na.strings ="?", stringsAsFactors = TRUE)
Auto = na.omit(Auto)
fit = lm(mpg~horsepower, data = Auto)
summary(fit)

# i. we can see the p-value to the f-statistic is 2.2e-16, so there
# is a relationship

# ii. we cam see that the R-square is 0.7945, so it indicate that
# 79% variation in response variable(mpg) is due to the predictor
# variable(horsepower)

# iii. Negative


predict(fit, data.frame(horsepower = 98), interval = 'confidence')
predict(fit, data.frame(horsepower = 98), interval = 'prediction')

#Question(b)
attach(Auto)
plot(horsepower,mpg)
abline(fit,lwd=5,col="blue")

#Question(c)
par(mfrow = c(2,2))
plot(fit)

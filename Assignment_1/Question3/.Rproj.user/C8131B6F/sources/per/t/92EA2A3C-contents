#Question (abc)
set.seed(17)
x = rnorm(100)
error = rnorm(100)

p0 = 11
p1 = 9
p2 = -1
p3 = 17
y = p0 +p1*x + p2*x^2 + p3*x^3 + error

library(leaps)
data = data.frame(x=x, y=y)
fit = regsubsets(y~x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + 
                   I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data, nvmax = 10)
fit_sum = summary(fit)
par(mfrow = c(2, 2))
plot(fit_sum$cp, xlab ="variables", ylab = "C_p", type = "l", col = 3 , lwd = 2)
points(which.min(fit_sum$cp), fit_sum$cp[which.min(fit_sum$cp)], col = "red")

plot(fit_sum$bic, xlab ="variables", ylab = "BIC", type = "l", col = 4 , lwd = 2)
points(which.min(fit_sum$bic), fit_sum$bic[which.min(fit_sum$bic)], col = "red")

plot(fit_sum$adjr2, xlab ="variables", ylab = "R-square", type = "l", col = 5 , lwd = 2)
points(which.max(fit_sum$adjr2), fit_sum$adjr2[which.max(fit_sum$adjr2)], col = "red")

coef(fit, which.min(fit_sum$bic))
coef(fit, which.min(fit_sum$cp))
coef(fit, which.max(fit_sum$adjr2))

#Question (d)

fit = regsubsets(y~x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + 
                   I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data, nvmax = 10, method = "forward")
fit_sum = summary(fit)
par(mfrow = c(2, 2))
plot(fit_sum$cp, xlab ="variables", ylab = "C_p", type = "l", col = 3 , lwd = 2)
points(which.min(fit_sum$cp), fit_sum$cp[which.min(fit_sum$cp)], col = "red")

plot(fit_sum$bic, xlab ="variables", ylab = "BIC", type = "l", col = 4 , lwd = 2)
points(which.min(fit_sum$bic), fit_sum$bic[which.min(fit_sum$bic)], col = "red")

plot(fit_sum$adjr2, xlab ="variables", ylab = "R-square", type = "l", col = 5 , lwd = 2)
points(which.max(fit_sum$adjr2), fit_sum$adjr2[which.max(fit_sum$adjr2)], col = "red")
mtext("forward",side = 3, line = -2, outer = TRUE)

coef(fit, which.min(fit_sum$bic))
coef(fit, which.min(fit_sum$cp))
coef(fit, which.max(fit_sum$adjr2))


fit = regsubsets(y~x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + 
                   I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data, nvmax = 10, method = "backward")
fit_sum = summary(fit)
par(mfrow = c(2, 2))
plot(fit_sum$cp, xlab ="variables", ylab = "C_p", type = "l", col = 3 , lwd = 2)
points(which.min(fit_sum$cp), fit_sum$cp[which.min(fit_sum$cp)], col = "red")

plot(fit_sum$bic, xlab ="variables", ylab = "BIC", type = "l", col = 4 , lwd = 2)
points(which.min(fit_sum$bic), fit_sum$bic[which.min(fit_sum$bic)], col = "red")

plot(fit_sum$adjr2, xlab ="variables", ylab = "R-square", type = "l", col = 5 , lwd = 2)
points(which.max(fit_sum$adjr2), fit_sum$adjr2[which.max(fit_sum$adjr2)], col = "red")
mtext("backward",side = 3, line = -2, outer = TRUE)

coef(fit, which.min(fit_sum$bic))
coef(fit, which.min(fit_sum$cp))
coef(fit, which.max(fit_sum$adjr2))

#Question (e)
library(glmnet)
x=model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data)[, -1]
lasso = cv.glmnet(x,y,alpha = 1)
plot(lasso)

best = lasso$lambda.min
best

fit_lasso = glmnet(x, y, alpha = 1)
predict(fit_lasso, s = best, type = "coefficients")

#Question (f)
p7 = 7
y = p0 + p7* x^7 + error
data2 = data.frame(y = y, x = x)
regfit = regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data2, nvmax = 10)
regfit_sum = summary(regfit)
plot(regfit_sum$cp, xlab = "Number of variables", ylab = "C_p", type = "l", lwd = 2, col = 3)
points(which.min(regfit_sum$cp), regfit_sum$cp[which.min(regfit_sum$cp)], col = "red")
plot(regfit_sum$bic, xlab = "Number of variables", ylab = "BIC", type = "l", lwd = 2, col = 5)
points(which.min(regfit_sum$bic), regfit_sum$bic[which.min(regfit_sum$bic)], col = "red")
plot(regfit_sum$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l", lwd = 2, col = 6)
points(which.max(regfit_sum$adjr2), regfit_sum$adjr2[which.max(regfit_sum$adjr2)], col = "red")

coef(regfit, 1)
coef(regfit, 4)
coef(regfit, 5)

x = model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data2, nvmax = 10)[,-1]
lasso = cv.glmnet(x, y, alpha = 1)
best = lasso$lambda.min
best
fit_lasso = glmnet(x, y, alpha = 1)
predict(fit_lasso, s=best, type = "coefficients")[1:11, ]

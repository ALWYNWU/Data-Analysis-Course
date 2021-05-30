library(MASS)
library(boot)
library(splines)

# Question a
fit = lm(nox ~ poly(dis, 3), data = Boston)
summary(fit)

dislim = range(Boston$dis)
dis.grid = seq(from = dislims[1], to = dislim[2], by = 0.1)
preds = predict(fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, preds, col = "red", lwd = 2)

# Question b
RSS = rep(NA, 10)
for (i in 1:10) {
  fit = lm(nox ~ poly(dis, i), data = Boston)
  RSS[i] = sum(fit$residuals^2)
}
plot(1:10, RSS, xlab = "Degree", ylab = "RSS", type = "l")


# Question c
testMSE <- rep(NA, 10)
for (i in 1:10) {
  fit = glm(nox ~ poly(dis, i), data = Boston)
  testMSE[i] = cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(1:10, testMSE, type = 'l', xlab = "Degree", ylab = "Test MSE")
points(which.min(testMSE), testMSE[which.min(testMSE)], col = 'red', pch = 19)


# Question d
spline.fit = lm(nox ~ bs(dis, df = 4), data = Boston)
summary(spline.fit)
pred = predict(spline.fit, newdata=list(dis=dis.grid), se=T)
plot(Boston$dis, Boston$nox, col="gray")
lines(dis.grid, pred$fit, lwd=2)
lines(dis.grid, pred$fit+2*pred$se, lty="dashed")
lines(dis.grid, pred$fit-2*pred$se, lty="dashed")

attr(bs(Boston$dis,df=4),"knots")


# Question e
res = c()
df.range = 3:16
for (dof in df.range) {
  fit = lm(nox ~ bs(dis, df = dof), data = Boston)
  res = c(res, sum(fit$residuals ^ 2))
}
plot(df.range, res, type = 'l', xlab = 'degree of freedom', ylab = 'RSS')


# Question f
res = c()
for (dof in df.range) {
  fit = glm(nox ~ bs(dis, df = dof), data = Boston)
  testMSE = cv.glm(Boston, fit, K = 10)$delta[1]
  res = c(res, testMSE)
}
plot(df.range, res, type = 'l', xlab = 'degree of freedom', ylab = 'Test MSE')
points(which.min(res) + 2, res[which.min(res)], col = 'red', pch = 19)

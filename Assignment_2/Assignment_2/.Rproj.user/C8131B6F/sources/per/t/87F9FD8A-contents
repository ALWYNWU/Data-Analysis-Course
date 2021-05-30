library(boot)

# Question a
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)


# Question b
plot(x, y)


# Question c
set.seed(1)

Data = data.frame(x, y)
fit.glm.i = glm(y ~ x)
cv.glm(Data, fit.glm.i)$delta[1]


fit.glm.ii <- glm(y ~ poly(x, 2))
cv.glm(Data, fit.glm.ii)$delta[1]

fit.glm.iii <- glm(y ~ poly(x, 3))
cv.glm(Data, fit.glm.iii)$delta[1]

fit.glm.iv <- glm(y ~ poly(x, 4))
cv.glm(Data, fit.glm.iv)$delta[1]


# Question d
set.seed(7)

fit.glm.i = glm(y ~ x)
cv.glm(Data, fit.glm.i)$delta[1]


fit.glm.ii <- glm(y ~ poly(x, 2))
cv.glm(Data, fit.glm.ii)$delta[1]

fit.glm.iii <- glm(y ~ poly(x, 3))
cv.glm(Data, fit.glm.iii)$delta[1]

fit.glm.iv <- glm(y ~ poly(x, 4))
cv.glm(Data, fit.glm.iv)$delta[1]

# Question e
# In word

# Question f
summary(fit.glm.iv)

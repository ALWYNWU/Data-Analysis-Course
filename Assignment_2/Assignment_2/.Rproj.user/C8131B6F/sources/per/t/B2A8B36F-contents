library(MASS)
library(boot)
attach(Boston)

# Question a
res = mean(medv)
res


# Question b
standard = sd(medv) / sqrt(dim(Boston)[1])
standard


# Question c
set.seed(1)
boot.fn = function(data, index) {
  m = mean(data[index])
  return (m)
}
boot(medv, boot.fn, 1000)


# Question d
t.test(medv)
miu.hat = c(22.53 - 2 * 0.4119, 22.53 + 2 * 0.4119)
miu.hat

# Question e
media = median(medv)
media


# Question f
boot.fn = function(data, index) {
  miu = median(data[index])
  return (miu)
}
boot(medv, boot.fn, 1000)

# Question g
percent = quantile(medv, c(0.1))
percent

# Question h
boot.fn = function(data, index) {
  miu = quantile(data[index], c(0.1))
  return (miu)
}
boot(medv, boot.fn, 1000)

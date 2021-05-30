#Question (a)
library(ISLR)
data(College)
set.seed(11)
train = sample(1:dim(College)[1], dim(College)[1] / 2)
test = -train
training_set = College[train, ]
test_set = College[test, ]

# Question (b)
fit = lm(Apps ~ ., data = training_set)
pred = predict(fit, test_set)
mean((pred - test_set$Apps)^2)

train_matrix = model.matrix(Apps ~ ., data = training_set)
test_matrix = model.matrix(Apps ~ ., data = test_set)
grid = 10 ^ seq(4, -2, length = 100)

# Question (c)
library(glmnet)
fit_ridge = glmnet(train_matrix, training_set$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
cv_ridge = cv.glmnet(train_matrix, training_set$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
best = cv_ridge$lambda.min
best

pred_ridge = predict(fit_ridge, s=best, newx = test_matrix)
mean((pred_ridge - test_set$Apps)^2)

# Question (d)
fit_lasso = glmnet(train_matrix, training_set$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
cv_lasso = cv.glmnet(train_matrix, training_set$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
best_lasso = cv_lasso$lambda.min
best

pred_lasso = predict(fit_lasso, s = best_lasso, newx = test_matrix)
mean((pred_lasso - test_set$Apps)^2)

predict(fit_lasso, s = best_lasso, type = "coefficients")

#Question (e)
test.avg = mean(test_set$Apps)
lm_r2 = 1 - mean((pred - test_set$Apps)^2) / mean((test.avg - test_set$Apps)^2)
ridge_r2 = 1 - mean((pred_ridge - test_set$Apps)^2) / mean((test.avg - test_set$Apps)^2)
lasso_r2 = 1 - mean((pred_lasso - test_set$Apps)^2) / mean((test.avg - test_set$Apps)^2)
lm_r2
ridge_r2
lasso_r2

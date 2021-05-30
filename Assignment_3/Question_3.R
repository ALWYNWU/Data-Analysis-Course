require(ISLR); require(tidyverse); require(ggthemes)
require(caret); require(e1071)
library(tree)

# (a)
set.seed(17)

data('OJ')

index = sample(nrow(OJ), 800, replace = FALSE)

train = OJ[index,]
test = OJ[-index,]
purchase.test = OJ$Purchase[-index]

# (b)
tree = tree(Purchase ~ ., train)
summary(tree)

# (c)
tree

# (d)
plot(tree)
text(tree, pretty=0)

# (e)
pred = predict(tree, test, type="class")

table(pred, purchase.test)

test_error = round(mean(pred != purchase.test)*100,2)
test_error

# (f)
cv_tree = cv.tree(tree, FUN = prune.misclass)
cv_tree

# (g)
plot(cv_tree$size, cv_tree$dev, type="b")

# (i)
pruned = prune.misclass(tree, best=2)
summary(pruned)

# (k)
prune.pred = predict(pruned, test, type="class")

table(prune.pred, purchase.test)
prune.test.error = round(mean(prune.pred != purchase.test)*100,2)
prune.test.error

error.df = data.frame(Model_type = c("unPruned","Pruned w/ Term.nodes=2"),Test_Error_rate = c(test_error, prune.test.error))
error.df









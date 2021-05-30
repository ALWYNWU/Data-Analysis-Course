############## lec 8 simulated data ############

library(nnet)
x=matrix(rnorm(100*5),100,5)

y=rnorm(100)
#multinomial
g4=sample(1:4,100,replace=TRUE)
fit3=multinom(g4~x)
summary(fit3)

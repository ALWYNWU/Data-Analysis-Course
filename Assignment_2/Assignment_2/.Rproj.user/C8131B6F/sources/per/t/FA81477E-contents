library(MASS)
library(zoo)
library(boot)
library(np)

# Question a
para = seq(0.1, 5, 0.5)
dislims = range(Boston$dis)
plot(Boston$dis,Boston$nox,xlim = dislims,cex = .5,col = "darkgrey")
RSS = rep(NA, 10)

for (i in 1:10) {
  lines(ksmooth(Boston$dis,Boston$nox,kernel = "box",bandwidth = i,x.points = Boston$dis),lty = 1,lwd = 2)
  
  smooth.res =ksmooth(Boston$dis,Boston$nox,kernel = "box",bandwidth = para.choice[i],x.points = Boston$dis)
  smooth.data = as.data.frame(smooth.res)
  smooth.data = as.data.frame(na.approx(smooth.data))
  
  RSS[i] = sum((ori_data[[2]] - smooth.data[[2]]) ^ 2)
  print(RSS[i])
}

plot(para,RSS,type = 'l',xlab = 'Box-Bandwidth',ylab = 'Box-RSS',col = 'red')


# Question b
para = seq(0.1, 15, 0.5)
n = length(ori_data[[1]])
X = as.numeric(unlist(ori_data[1]))
Y = as.numeric(unlist(ori_data[2]))
fold.MSEs = matrix(0, nrow = 10, ncol = length(para))
case.folds = sample(rep(1:10, length.out = n))

for (fold in 1:10) {
  train.rows = which(case.folds != fold)
  x.train = X[train.rows]
  y.train = Y[train.rows]
  x.test = X[-train.rows]
  y.test = Y[-train.rows]
  
  for (bw in 1:length(para)) {
    fit =npreg(bws = para[bw],txdat = x.train,tydat = y.train,exdat = x.test,eydat = y.test)
    fold.MSEs[fold, bw] = fit$MSE
  }
}

CV_MSE = colMeans(fold.MSEs)
plot(para,CV_MSE,type = 'l',xlab = 'Box-Bandwidth')
Bandwidth = para[which.min(CV_MSE)]
Bandwidth


# Question c
para.choice = seq(0.11, 5, 0.5)
dislims = range(Boston$dis)

plot(Boston$dis,Boston$nox,xlim = dislims,cex = .5,col = "darkgrey")

RSS = rep(NA, 10)

for (i in 1:10) {
  lines(ksmooth(Boston$dis,Boston$nox,kernel = "normal",bandwidth = para.choice[i],x.points = Boston$dis),lty = 1,lwd = 2)
  
  smooth.res =ksmooth(Boston$dis,Boston$nox,kernel = "normal",bandwidth = para.choice[i],x.points = Boston$dis)
  smooth.data = as.data.frame(smooth.res)
  smooth.data = as.data.frame(na.approx(smooth.data))
  
  RSS[i] = sum((ori_data[[2]] - smooth.data[[2]]) ^ 2)
  print(RSS[i])
}

plot(para.choice,RSS,type = 'l',xlab = 'Gau-Bandwidth',ylab = 'Gau-RSS')


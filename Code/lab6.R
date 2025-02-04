# lab6: Non-linear Modeling

library(ISLR)
attach(Wage)

# Polynomial Regression and Step Functions

fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))
fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)
fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
coef(summary(fit.5))
(-11.983)^2
fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)

# Splines

library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")
dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)


# Super smoothing:
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
fit = supsmu(age, wage,bass =0)
lines(fit,col='green',lwd=2)
fit = supsmu(age, wage,bass =5)
lines(fit,lty=2,col= 'red',lwd=2)
fit = supsmu(age, wage,bass =10)
lines(fit,lty=4,col='blue',lwd=2)
legend("bottomright", legend = c("bass = 0", "bass = 5", "bass = 10"), col=c("green","red","blue"), lty=c(1, 2, 4),lwd=2)


# Kernel smoothing:
par(mfrow=c(1,1))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
lines(ksmooth(age, wage, kernel = "box"), lty=4,col='green',lwd=2)
lines(ksmooth(age, wage, kernel = "box", bandwidth = 4), lty=1,col='red',lwd=2)
lines(ksmooth(age, wage, kernel = "box", bandwidth = 8), lty=2,col='blue',lwd=2)
legend("bottomright", legend = c("bandwidth = .5", "bandwidth = 4", "bandwidth = 8"),col=c("green","red","blue"), lty=c(4,1,2),lwd=2)

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
lines(ksmooth(age, wage, kernel = "normal"), lty=4,col='green',lwd=2)
lines(ksmooth(age, wage, kernel = "normal", bandwidth = 4), lty=1,col='red',lwd=2)
lines(ksmooth(age, wage, kernel = "normal", bandwidth = 8), lty=2,col='blue',lwd=2)
legend("bottomright", legend = c("bandwidth = .5", "bandwidth = 4", "bandwidth = 8"), col=c("green","red","blue"),lwd=2,lty=c(4,1,2))

# loess
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

#Question (a)
Carseats = read.table("Carseats.txt",na.strings = "?")
Carseats = na.omit(Carseats)
fit = lm(Sales ~ Price+Urban+US, data= Carseats)
summary(fit)

#Question (e)
fit_2 = lm(Sales~Price + US, data = Carseats)
summary(fit_2)

#Question (g)
confint(fit_2)

#Question (h)
par(mfrow = c(2, 2))
plot(fit_2)

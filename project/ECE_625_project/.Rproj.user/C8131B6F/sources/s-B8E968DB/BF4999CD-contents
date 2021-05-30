# Motorcycle data
# Load the MASS package; data are in mcycle
library(MASS)
attach(mcycle)
plot(mcycle)

par(mfrow=c(2,1))

# Fit orthogonal polynomials
plot(mcycle)
for(k in 2:6) lines(mcycle$times, predict(lm(accel~poly(times,k)), data=mcycle), lty=k-1)
title(sub = "Polynomial fits of degrees 2,...,6 to motorcycle data")
plot(mcycle)
lines(mcycle$times, predict(lm(accel~poly(times,20)), data=mcycle))
title(sub = "Polynomial fit of degree 20 (!) to motorcycle data")


# Fit a smoothing spline
par(mfrow=c(1,1))
fit = smooth.spline(times, accel)
plot(mcycle)
lines(fit, lty=1)
lines(smooth.spline(times, accel, df=5), lty=2)
lines(smooth.spline(times, accel, df=90), lty=4)
legend("bottomright", legend = c("df = 12.21 by GCV", "df=5", "df=90"), lty=c(1,2,4))



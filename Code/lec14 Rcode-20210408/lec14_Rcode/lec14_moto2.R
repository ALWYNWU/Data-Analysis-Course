# Motorcycle data
# Load the MASS package; data are in mcycle
library(MASS)
attach(mcycle)


# Running means and medians:

par(mfrow=c(2,1))

n = length(times)
runningmean = function(k) {
runm = rep(0, n)
for(i in (k+1):(n-k)) runm[i] = mean(accel[(i-k):(i+k)])
runm = runm[(k+1):(n-k)]
}
plot(mcycle)
k = 2; lines(times[(k+1):(n-k)], runningmean(k), lty=2)
k = 9; lines(times[(k+1):(n-k)], runningmean(k), lty=1)
legend("bottomright", legend = c("k=2", "k=9"), lty=c(2,1))

runningmedian = function(k) {
runm = rep(0, n)
for(i in (k+1):(n-k)) runm[i] = median(accel[(i-k):(i+k)])
runm = runm[(k+1):(n-k)]
}
plot(mcycle)
k = 2; lines(times[(k+1):(n-k)], runningmedian(k), lty=2)
k = 9; lines(times[(k+1):(n-k)], runningmedian(k), lty=1)
legend("bottomright", legend = c("k=2", "k=9"), lty=c(2,1))

# Super smoothing:
fit = supsmu(times, accel)
plot(mcycle)
lines(unique(times), fit$y, lty=1)
lines(unique(times), supsmu(times, accel, bass=5)$y, lty=2)
lines(unique(times), supsmu(times, accel, bass=10)$y, lty=4)
legend("bottomright", legend = c("bass = 0", "bass = 5", "bass = 10"), lty=c(1, 2, 4))


# Kernel smoothing:
par(mfrow=c(1,1))
plot(mcycle)
lines(ksmooth(times, accel, kernel = "box"), lty=4)
lines(ksmooth(times, accel, kernel = "box", bandwidth = 4), lty=1)
lines(ksmooth(times, accel, kernel = "box", bandwidth = 8), lty=2)
legend("bottomright", legend = c("bandwidth = .5", "bandwidth = 4", "bandwidth = 8"), lty=c(4,1,2))

plot(mcycle)
lines(ksmooth(times, accel, kernel = "normal"), lty=4)
lines(ksmooth(times, accel, kernel = "normal", bandwidth = 4), lty=1)
lines(ksmooth(times, accel, kernel = "normal", bandwidth = 8), lty=2)
legend("bottomright", legend = c("bandwidth = .5", "bandwidth = 4", "bandwidth = 8"), lty=c(4,1,2))


fit = loess(accel~times)

par(mfrow=c(2,2))
# loess fit, degree = 1, family = "gaussian"
plot(mcycle, xlab = "(a)")
lines(times, predict(loess(accel~times, degree = 1, family = "gaussian")), lty=4)
lines(times, predict(loess(accel~times, degree = 1, span = .2, family = "gaussian")), lty=1)
lines(times, predict(loess(accel~times, degree = 1, span = .4, family = "gaussian")), lty=2)
legend("bottomright", legend = c("span = .75", "span = .2", "span = .4"), lty=c(4,1,2))


# loess fit, degree = 2, family = "gaussian"
plot(mcycle, xlab = "(b)")
lines(times, predict(loess(accel~times, degree = 2, family = "gaussian")), lty=4)
lines(times, predict(loess(accel~times, degree = 2, span = .2, family = "gaussian")), lty=1)
lines(times, predict(loess(accel~times, degree = 2, span = .4, family = "gaussian")), lty=2)
legend("bottomright", legend = c("span = .75", "span = .2", "span = .4"), lty=c(4,1,2))


# loess fit, degree = 1, family = "symmetric"
plot(mcycle, xlab = "(c)")
lines(times, predict(loess(accel~times, degree = 1, family = "symmetric")), lty=4)

lines(times, predict(loess(accel~times, degree = 1, span = .2, family = "symmetric")), lty=1)
lines(times, predict(loess(accel~times, degree = 1, span = .4, family = "symmetric")), lty=2)
legend("bottomright", legend = c("span = .75", "span = .2", "span = .4"), lty=c(4,1,2))

# loess fit, degree = 2, family = "symmetric"
plot(mcycle, xlab = "(d)")
lines(times, predict(loess(accel~times, degree = 2, family = "symmetric")), lty=4)

lines(times, predict(loess(accel~times, degree = 2, span = .2, family = "symmetric")), lty=1)
lines(times, predict(loess(accel~times, degree = 2, span = .4, family = "symmetric")), lty=2)
legend("bottomright", legend = c("span = .75", "span = .2", "span = .4"), lty=c(4,1,2))




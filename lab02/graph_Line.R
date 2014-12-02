par(mfrow=c(1,2))

z <- lm(dist ~ speed, data = cars)
z$coef
plot(cars, pch=16, col="blue", main = "abline")
abline(h = 20)
abline(h = 30)
abline(v = 20)
abline(a = 40, b = 4)
abline(z, lty = 2, lwd = 2)

plot(cars, pch=16, col="blue", main = "abline")
abline(z, lty = 2)

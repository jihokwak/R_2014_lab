par(mfrow=c(1,1))

x <- runif(12); y <- rnorm(12)
i <- order(x); x <- x[i]; y <- y[i]
par(mfrow=c(2, 1))
plot(x, y, pch=16, col="blue", main="2 segments by segments function")
(s <- seq(length(x) - 1))
segments(x[s], y[s], x[s + 2], y[s + 2], lty=1:2)
plot(x, y, pch=16, col="blue", main="3 segments by segments function")
(s <- seq(length(x) - 2))
segments(x[s], y[s], x[s + 3], y[s + 3], lty=1:3)
box(which="outer")

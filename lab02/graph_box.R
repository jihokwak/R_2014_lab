par(mar=c(1,1,1,1))    	                      # Margin
par(oma=c(1,1,1,1))			           # Outer Margin
hist(rnorm(50), axes=F, xlab="", ylab="", main="box")
whichs <- c("outer", "inner", "plot", "figure")	# 영역의 종류
box(which=whichs[1], lty=1)
box(which=whichs[2], lty=2)
box(which=whichs[3], lty=3)
box(which=whichs[4], lty=4)
legend("topright", legend=whichs ,lty=1:4)


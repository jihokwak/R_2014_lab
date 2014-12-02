par(mfrow=c(1,1))

plot(1:9, type="n", axes=F, xlab="", ylab="", main="arrows")
arrows(1, 9, 4, 9, angle=30, length=0.25, code=2)
text(4.5, 9, adj=0, "angle=30, length=0.25, code=2(default)")
arrows(1, 8, 4, 8, length=0.5); text(4.5, 8, adj=0, "length=0.5")
arrows(1, 7, 4, 7, length=0.1); text(4.5, 7, adj=0, "length=0.1")
arrows(1, 6, 4, 6, angle=60); text(4.5, 6, adj=0, "angle=60")
arrows(1, 5, 4, 5, angle=90); text(4.5, 5, adj=0, "angle=90")
arrows(1, 4, 4, 4, angle=120); text(4.5, 4, adj=0, "angle=120")
arrows(1, 3, 4, 3, code=0); text(4.5, 3, adj=0, "code=0")
arrows(1, 2, 4, 2, code=1); text(4.5, 2, adj=0, "code=1")
arrows(1, 1, 4, 1, code=3); text(4.5, 1, adj=0, "code=3")


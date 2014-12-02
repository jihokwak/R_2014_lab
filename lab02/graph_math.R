par(family="AppleGothic")
plot(1:10,1:10, type="n", xlab="", ylab="", main="text")
text(1.5, 9, adj=0, labels="피타고라스의 정리(定理)")
polygon(c(5, 3, 5), c(9, 7, 7))
polygon(c(5, 5, 4.8, 4.8),c(7, 7.2, 7.2, 7))
text(3.64, 8.36, adj=0, labels="c")
text(3.94, 6.67, adj=0, labels="a")
text(5.36, 7.95, adj=0, labels="b")
text(1.5, 8, adj=0, labels=expression(c^2 == a^2+b^2))         # Example expression labels
text(1.5, 6, adj=0, labels=expression(cos(r^2) * e^{-r/6}))
text(2, 3, adj=0.3, labels=expression(z[i]==sqrt(x[i]^2 + y[i]^2)))
text(9, 4, adj=1, labels=expression(
  f(x) == frac(1, sqrt((2 * pi)^n ~~ det(Sigma[x]))) ~~ exp * bgroup("(",
                                                                     -frac(1, 2) ~~ (x - mu)^T * Sigma[x]^-1 * (x - mu), ")")))
text(5, 5, adj=0.5, labels=expression(y==bgroup("(",atop(a ~~ b, c ~~ d),")")))
points(8, 8, pch=16)                                                        # Example position by pos
text(8, 8, "position1", pos=1)
text(8, 8, "position2", pos=2)
text(8, 8, "position3", pos=3)
text(8, 8, "position4", pos=4)
points(8, 6, pch=16)                                                        # Example offset
text(8, 6, "offset1", pos=1, offset=1)
text(8, 6, "offset2", pos=2, offset=1.5)
text(8, 6, "offset3", pos=3, offset=2)
text(8, 6, "offset4", pos=4, offset=2.5)
text(4, 2, "at(4, 2) left/top by adj = c(0, 0)", adj=c(0, 0))           # Example adj by adj(x, y)
text(4, 2, "at(4, 2) center/bottom by adj = c(0.5, 1)", adj=c(0.5, 1))
text(8, 3, "at(8, 3) right/middle by adj = c(1, 0.5)", adj=c(1, 0.5))


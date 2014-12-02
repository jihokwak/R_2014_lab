## lattice package
library(lattice)
data(singer)
histogram(~height|voice.part,data=singer, xlab="height(inches)")
# voice.part is conditional variable
# y~x | A*B 이며 single일 경우는 ~x 형태, y~x는 numeric,  A*B는 factor
ggplot(singer,aes(height,fill=voice.part))+geom_bar(color="black",stat="bin",binwidth=2)+facet_grid(voice.part~.)

attach(mtcars)
gear<-factor(gear,levels=c(3,4,5),labels=c("3 gears", "4 gears", "5 gears"))
cyl<-factor(cyl,levels=c(4,6,8), labels=c("4 cyls", "6 cyls", "8 cyls"))
densityplot(~mpg,xlab="MPG")
densityplot(~mpg|cyl, main="density plot by # of cyliners", xlab="MPG")
bwplot(cyl~mpg|gear, main="boxplot by cylinders and gears")
xyplot(mpg~wt|cyl*gear, main="scatterplot by cylinders and gears")
cloud(mpg~wt*qsec | cyl, main="3D scatter plot by cylinders")
dotplot(cyl~mpg | gear)
splom(mtcars[c(1,3,4,5,6)], main="scatter plot matrix for mtcars data")
detach(mtcars)
ggplot(mtcars,aes(mpg))+geom_density()+xlim(0,45)
ggplot(mtcars,aes(mpg))+geom_density()+facet_grid(.~cyl)+xlim(0,45)
ggplot(mtcars,aes(mpg,wt,color=factor(cyl),fill=factor(cyl)))+geom_point()+facet_grid(.~gear)

mygraph<-densityplot(~height|voice.part,data=singer)
mygraph
update(mygraph,col="red",pch="16",cex=.8,jitter=.05, lwd=2)

str(mtcars)
displacement<-equal.count(mtcars$disp, number=3, overlap=0)
# disp 필드값을 중복되지 않고 3구간으로 나누어 동일한 개수로  
xyplot(mpg~wt|displacement, data=mtcars, main="MPG vs. Weight by engine displacement", layout=c(3,1), aspect=1.5) 
# 3 columns 1 row, aspect ratio(height/width)
ggplot(mtcars,aes(wt,mpg,size=disp))+geom_point()
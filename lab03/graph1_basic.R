## numeric variable
data(iris)
names(iris)<-tolower(names(iris))
colnames(iris)<-c("sl","sw","pl","pw","spe")
attach(iris)
plot(iris)
plot(iris[1:4]) # or plot(iris[-5])
plot(pw,pl, abline(lm(pl~pw)))
plot(pw,pl, main="pl pw",xlab="petal length",ylab="petal width",col="blue")
plot(pw,pl, pch=as.integer(spe))
legend(1.5,2.0,as.character(levels(spe)),pch=1:length(levels(spe)))
legend(locator(1),as.character(levels(spe)),pch=1:length(levels(spe)))


## ggplot으로 선형성을 확인하면...
library(ggplot2)
ggplot(iris,aes(pw,pl))+geom_point()+geom_smooth(method="lm")
ggplot(iris,aes(pw,pl))+geom_point()+geom_smooth(method="loess")
ggplot(iris,aes(pw,pl))+geom_point(aes(color=spe))+geom_smooth(method="loess")
# 어느 모델이 더 적합한지 확인하려면?
lm1<-lm(pl~pw)
loess2<-loess(pl~pw)
summary(lm1) # r squared 확인하고
hat<-predict(loess2)
r_sq_loess<-cor(pl,hat)^2
r_sq_loess


## histogram
par(mfrow=c(2,2))
hist(sl);hist(sw);hist(pl);hist(pw)
plot(density(sl));plot(density(sw));plot(density(pl));plot(density(pw))
library(sm)
sm.density.compare(sl,spe) # 종별 sl density
colfill<-c(2:(1+length(levels(spe))))
legend(locator(1), levels(spe), fill=colfill)

ggplot(iris,aes(sl,fill=spe,color=spe))+geom_line(stat="density")+xlim(0,9)
ggplot(iris,aes(sl,fill=spe))+geom_histogram(binwidth=0.5,color="black")+facet_grid(spe~.)
ggplot(iris,aes(sw,fill=spe))+geom_histogram(binwidth=0.5,color="black")+facet_grid(spe~.)
ggplot(iris,aes(pl,fill=spe))+geom_histogram(binwidth=0.5,color="black")+facet_grid(spe~.)
ggplot(iris,aes(pw,fill=spe))+geom_histogram(binwidth=0.5,color="black")+facet_grid(spe~.)


## boxplot
boxplot(iris[-5])
boxplot.stats(iris$sw)
a<-boxplot(iris$sw,horizontal=T)
text(a$out,rep(1,NROW(a$out)),labels=a$out,pos=1,cex=.5) # text 위치를 (outlier값, 1)로 잡음
boxplot(iris$pl~iris$spe) # boxplot(iris$sl~iris$spe) boxplot(iris$sw~iris$spe) boxplot(iris$pw~iris$spe)
ggplot(iris,aes(spe,pl))+geom_boxplot()

# boxplot에 평균값 표시하기
means<-aggregate(pl~spe,iris,mean)
ggplot(iris, aes(spe,pl))+geom_boxplot()+stat_summary(fun.y=mean, colour="darkred",geom="point",shape=18,size=3,show_guide=F)+geom_text(data = means, aes(label=pl,y=pl-0.08))

## 2개 팩터 고려한 boxplot
data(mtcars)
mtcars$cyl.f<-factor(mtcars$cyl,levels=c(4,6,8),labels=c("4","6","8"))
mtcars$am.f<-factor(mtcars$am,levels=c(0,1),labels=c("auto","standard"))
boxplot(mtcars$mpg~mtcars$am.f * mtcars$cyl.f, col=c("gold","darkgreen"),varwidth=T,xlab="Auto Type",ylab="MPG")
# 이상한 점이 있는데... 확인해 보면...
nrow(mtcars[mtcars$cyl.f==6 & mtcars$am.f=="standard",]) # 방법 1
nrow(mtcars[mtcars$cyl.f==8 & mtcars$am.f=="auto",])
ddply(mtcars,c("cyl.f","am.f"),summarise,count=length(mpg)) # 방법 2
ggplot(mtcars,aes(x=interaction(am.f,cyl.f),y=mpg))+geom_boxplot()


## dotchart
x <- mtcars[order(mtcars$mpg),]
x$cyl <- factor(x$cyl)
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"
dotchart(x$mpg,labels=row.names(x),cex=.7,groups=x$cyl,gcolor="black",color=x$color,pch=19,main= "Gas Mileage for Car Models \n grouped by cylinder",xlab="Miles Per Gallon")

## ggplot으로
mycars<-cbind(vehicle=row.names(mtcars),mtcars)
mycars<-arrange(mycars,cyl,desc(mpg))
mycars
ggplot(mycars,aes(mpg,vehicle,color=factor(cyl)))+geom_point()+facet_grid(.~cyl)+xlim(0,40)+geom_text(aes(label=vehicle),size=3,vjust=1.5) # 방법 1
ggplot(mycars, aes(mpg,vehicle,color=factor(cyl)))+geom_point()+xlim(0,40)+geom_text(aes(label=vehicle),size=3,vjust=1.5) # 방법 2


## categorical variable 
data(Cars93,package="MASS")
str(Cars93)
cars93<-Cars93[,sapply(Cars93,is.factor)]
str(cars93)
attach(cars93)
type.origin<-table(cars93[,c(3,8)]) # contingency table
type.origin
barplot(type.origin,legend.text=T) # stacked
barplot(type.origin,beside=T,legend.text=T) # clustered
temp<-t(type.origin) # transpose
barplot(temp,legend.text=T,las=1,ylim=c(0,25))
p<-ggplot(cars93,aes(Type,fill=Origin))+geom_bar(position="stack",stat="bin")
p
p+guides(fill=guide_legend(rev=T))

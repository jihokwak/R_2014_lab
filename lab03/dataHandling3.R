## factor handling
sizes<-factor(c("small","large","large","small","medium"))
sizes # 레벨은 알파벳 순
sizes<-factor(sizes,levels=c("small","medium","large"))
sizes # 레벨 순서 바꿀 때

sizes1<-revalue(sizes, c(small="S",medium="M",large="L")) 
sizes1 # 레벨명 바꾸기 방법 1, library(plyr) 

levels(sizes1)[levels(sizes1)=="L"]<-"Large"
levels(sizes1)[2]<-"Medium";levels(sizes1)[1]<-"Small" # 레벨명 바꾸기 방법 2
levels(sizes1)<-c("S","M","L") # 레벨명 바꾸기 방법 3

## 새로운 factor field 만들기
d <- data.frame(list(gender=c("M","M","F","M","F","F"),age=c(47,59,21,32,33,24),income=c(55000,88000,32450,76500,123000,45650)));d
d$over25<-ifelse(d$age>25,1,0) 
d # 방법 1
tapply(d$income,list(d$gender,d$over25),mean)
ddply(d,c("gender","over25"),summarise,mean=mean(income))

old<-c("0","1")
new<-factor(c("No","Yes"))
d$new.field<-new[match(d$over25,old)]
d # 방법 2
 
d$another<-interaction(d$gender,d$new.field)
d # 방법 3

d$age2<-cut(d$age,breaks=c(1,20,30,40,50,Inf),right=F,labels=c("20대 미만","20대","30대","40대","50대 이상"))
d # 방법 4-1
d$income2<-cut(d$income,breaks=c(1,50000,100000,Inf),right=F,labels=c("5만 미만","5만 이상 10만 미만","10만 이상"))
d # 방법 4-2
   

## factor handling
char.obj<-"datamanipulation"
factor.obj<-factor(substring(char.obj,1:nchar(char.obj),1:nchar(char.obj)),levels=letters)
factor.obj
table(factor.obj)
factor.obj2<-factor(factor.obj)
factor.obj2


## sqldf 비교
library(sqldf)
newdf <-sqldf("select * from mtcars where carb=1 order by mpg", row.names=TRUE)
newdf
mycar<-mtcars[mtcars$carb==1,];mycar<-mycar[order(mycar$mpg),]
identical(mycar,newdf)
ggplot(mycar,aes(carb,mpg))+geom_point()+geom_text(aes(label=rownames(mycar)),size=3,vjust=2)
newdf<-sqldf("select avg(mpg) as avg_mpg, avg(disp) as avg_disp, gear from mtcars where cyl in (4, 6) group by gear")
newdf
mycar<-mtcars[(mtcars$cyl %in% c(4,6)),]
mycar<-ddply(mycar,"gear",summarise,avg_mpg=mean(mpg),avg_disp=mean(disp))
mycar
mycar<-mycar[,c(2,3,1)]
newdf==mycar
round(newdf,digits=5)==round(mycar,digits=5)

## merging data frame 
d1<-data.frame(kids=c("jack","jill","jillian","john"),states=c("CA","MA","MA","HI"))
str(d1)
d1$kids<-as.character(d1$kids)
d2<-data.frame(kids=c("jill","lillian","jack"),ages=c(10,7,12),stringsAsFactors=F)
d1;d2
d<-merge(d1,d2);d
merge(d1,d2,by="kids",all.x=T) #d1은 모두, d2는 해당되는 것만
merge(d1,d2,by="kids",all.y=T) 
merge(d1,d2,by="kids",all=T)
d3<-data.frame(pals=c("jack","jill","lillian"),ages=c(12,10,7))
str(d3)
d3$pals<-as.character(d3$pals)
merge(d1,d3,by.x="kids",by.y="pals")

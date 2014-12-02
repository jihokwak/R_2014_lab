library(ggplot2);data(ChickWeight)
chick<-ChickWeight
names(chick)<-tolower(names(chick))
str(chick);head(chick)

## 분포 보기
p<-ggplot(chick,aes(weight,fill=diet))
p+geom_histogram(color="black",binwidth=50)+facet_grid(diet~.)
# 또는 
p+geom_bar(color="black",binwidth=50)+facet_grid(diet~.)

## binwidth는 어떻게 구하나요?
# 방법 1
range<-diff(range(chick$weight));range 
# 변수 범위를 구하고
n_class<-1+3.3*log(nrow(chick))
# 계급수를 구한다음
range/n_class
# 변수 범위를 계급수로 나눔

# 방법 2
a<-hist(chick$weight,breaks="FD")
str(a)

# 방법 3
bw<-diff(range(chick$weight)) / (2*IQR(chick$weight)/length(chick$weight)^(1/3))
ggplot(chick,aes(weight))+geom_histogram(binwidth=bw) 

## 특정 구간 분포 보기
p<-ggplot(subset(chick,time==21),aes(weight,fill=diet))
p+geom_bar(color="black",binwidth=50)+facet_grid(diet~.)

## 시간에 따른 체중 변화 보기
p<-ggplot(chick,aes(time,weight,group=diet,color=diet))
p+geom_point()
p+geom_point(alpha=.3)+geom_smooth(alpha=.2,size=1,method="loess")
p+geom_smooth(alpha=.4,size=3,method="loess")
ggplot(chick,aes(time,weight,color=diet,group=chick))+geom_line()
ggplot(chick,aes(time,weight,color=diet,group=chick))+geom_line()+facet_wrap(~diet)

## 바그래프 보기
p<-ggplot(chick,aes(time,weight,fill=diet))
p+geom_bar(stat="identity")
p+geom_bar(stat="identity")+guides(fill=guide_legend(reverse=T))
p+geom_bar(position="dodge",stat="identity")

## 이상한 점을 확인해 보면...
library(plyr);library(dplyr)
# method 1
ddply(chick,"diet",summarise,unique.count=length(unique(chick)),count.record=length(chick),wt.mean=mean(weight))
# method 2
levels(chick$chick)
chick %>%
group_by(diet) %>% summarise(n_distinct(chick),count.record=length(chick),mean.wt=mean(weight))
# method 3
ggplot(chick,aes(diet,weight))+geom_boxplot()



data(diamonds);str(diamonds)

## 변수 분포를 보면서 의미 이해하기
ggplot(diamonds,aes(cut))+geom_bar()
ggplot(diamonds,aes(color))+geom_bar()
ggplot(diamonds,aes(clarity))+geom_bar()
ggplot(diamonds,aes(carat))+geom_bar(binwidth=0.25)+xlim(0,max(5))

## 색상과 투명도 변수의 어떤 값이 좋은 것일까요?
p<-ggplot(diamonds,aes(carat,price,color=color))
p+geom_point() # 어떤 색상이 좋은 것인가요?
p+geom_line()  # 알 수 있나요?
p+geom_smooth(alpha=.4,size=2,method="loess") # 알 수 있습니다.
p<-ggplot(diamonds,aes(carat,price,color=clarity))
p+geom_point() # 어떤 투명도가 좋은 것인가요?
p+geom_smooth(alpah=.4,size=2,method="loess") # 알 수 있습니다.

## boxplot
diamonds$size<-cut(diamonds$carat,breaks=c(0,1,2,3,Inf),right=F,labels=c("1C less","1C more 2C less","2C more 3C less","3C more"))
ggplot(diamonds,aes(x=interaction(cut,size),y=price))+geom_boxplot()+coord_flip()
ggplot(diamonds,aes(x=interaction(color,size),y=price))+geom_boxplot()+coord_flip()
ggplot(diamonds,aes(x=interaction(clarity,size),y=price))+geom_boxplot()+coord_flip()

## 특정 구간 분포 보기
levels(diamonds$size)
ggplot(subset(diamonds,size=="1C less"),aes(price,fill=cut))+geom_bar(color="black",binwidth=500)+facet_grid(cut~.)
ggplot(subset(diamonds,size=="1C more 2C less"),aes(price,fill=cut))+geom_bar(color="black",binwidth=500)+facet_grid(cut~.)
ggplot(subset(diamonds,size=="2C more 3C less"),aes(price,fill=cut))+geom_bar(color="black",binwidth=500)+facet_grid(cut~.)
ggplot(subset(diamonds,size=="3C more"),aes(price,fill=cut))+geom_bar(color="black",binwidth=500)+facet_grid(cut~.)

## 기타 
ggplot(diamonds,aes(carat,fill=cut))+geom_bar(binwidth=0.5,position="fill")

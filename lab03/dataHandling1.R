## 기본 연산 연습
x<-1:10
y<-10:1
z<--5:5
x+y;x+1
x-x;x-1
x*y;x*1
x/y;x/2
x==y;!x==y
is.numeric(x);is.factor(y);!is.factor(y)
z2<-abs(z);z2;sqrt(z2)
mean(x);median(x);sum(x);sd(x);var(x);mad(x);length(x)
seq(1,10,2);rep(x,2)
cut(x,breaks=c(1,4,7,11),right=F)
firstname<-c("Jane")
cat("Hello",firstname,"\n")
quantile(x);quantile(x,probs=c(0.25,0.75))
range(x);diff(x);diff(x,lag=2)
min(x);max(x)
scale(x)


## 데이터 읽어오기(csv file)
getwd()
setwd("~/R Lecture/")
ls();rm(list=ls())
data(iris)
head(iris) # 3개 레코드를 확인하려면?
write.csv(iris,file="newiris.csv",row.names=FALSE) 
# iris를 newiris로 저장
newiris<-read.csv("newiris.csv") 
head(newiris)
# rstudio에서 tool>import dataset
newiris2<-read.table("newiris.csv",sep=",",header=T)
# table로 읽을 때는 추가 인수가 반드시 필요함
# file을 제거할 때는 file.remove("file name")
# RData로 저장 및 읽기
save(newiris2, file="newiris2.RData")
load("newiris2.RData")
summary(newiris)
str(newiris)
dim(newiris)


## 데이터 추가/삭제
iris$x<-1;head(iris)
iris$x<-NULL;head(iris)
df<-data.frame(Sepal.Length=1,Sepal.Width=1,Petal.Length=1,Petal.Width=1,Species="new")
newiris<-rbind(iris,df)
tail(newiris)
newiris<-newiris[-151,]
identical(iris,newiris)
summary(iris);summary(newiris)
newiris$Species<-factor(newiris$Species)
all.equal(iris,newiris)
# setosa는 1, 나머지는 2와 3으로 하려면?
newiris$new[newiris$Species=="setosa"]<-1
newiris$new[newiris$Species=="versicolor"]<-2
newiris$new[newiris$Species=="virginica"]<-3
str(newiris)
# ChickWeight 데이터를 가지고 읽기/쓰기를 해보세요.


## 외부 데이터 읽어오기(html file)
library(XML)
url<-"http://en.wikipedia.org/wiki/World_population"
tbl<-readHTMLTable(url,which=4,stringsAsFactors=FALSE)
head(tbl);class(tbl)
names(tbl)
names(tbl)[2]<-"Country"
names(tbl)[5]<-"Percent"
str(tbl)
library(stringr)
tbl$Rank<-as.numeric(tbl$Rank)
tbl$Population<-str_replace_all(tbl$Population,",","")
tbl$Population<-as.numeric(tbl$Population)
substr(tbl[1,5],3,3)<-"";substr(tbl[4,5],4,4)<-""
substr(tbl$Percent,5,5)<-""
tbl$Percent<-as.numeric(tbl$Percent)
str(tbl)
tbl$new_percent<-cumsum(tbl$Percent) # 방법 1
tbl$new_percent2<-tbl$Population/((1.37e+09)/0.19)
tbl$new_percent2<-cumsum(tbl$new_percent2)*100 # 방법 2
tbl
# 국가별 국방비 자료를 다운로드 해보세요.
url<-"http://en.wikipedia.org/wiki/List_of_countries_by_military_expenditures"
tbl<-readHTMLTable(url,which=3,stringsAsFactors=F)
# 미국과 다른 9개국의 국방비를 비교한다면...
names(tbl)<-c("rank","country","spending","PercentofGDP", "worldshare") # 또는
str(tbl)
tbl$rank<-as.numeric(tbl$rank)
tbl$spending<-as.numeric(tbl$spending)
tbl$PercentofGDP<-as.numeric(tbl$PercentofGDP)
tbl$worldshare<-as.numeric(tbl$worldshare)
sum(tbl$worldshare[3:11])


## 기초 명령어
data(iris);head(iris)
colSums(iris[1:4]) # 열합계 방법 1
apply(iris[1:4],2,sum) # 열합계 방법 2
colMeans(iris[1:4]) # 열 평균 방법 2
apply(iris[1:4],2,mean) # 열 평균 방법 2
rowSums(iris[-5]) # 행 합계 방법 1
apply(iris[-5],1,sum) # 행 합계 방법 2
rowMeans(iris[-5]) # 행 평균 방법 1
apply(iris[-5],1,mean) # 행 평균 방법 2
attach(iris)
tapply(Sepal.Length,Species,sum) # 종별 꽃받침길이 합
tapply(Sepal.Length,Species,mean) # 종별 꽃받침길이 평균
tapply(Sepal.Length,Species,sd) # 종별 꽃받침길이 표준편차
tapply(Sepal.Length,Species,length) # 종별 꽃받침 갯수
temp<-tapply(Sepal.Length,Species,sd)
plot(temp)
barplot(temp)
par(mfrow=c(1,2));plot(temp);barplot(temp)


## 패키지 함수로 해보면...
library(plyr)
ddply(iris,"Species",summarise,mean_sepal.length=mean(Sepal.Length)) 
ddply(iris,"Species", function(x) colMeans(x[,-5]))


## date 추출 예제
leadership$date <- as.Date(leadership$date, "%m/%d/%y")
startdate <- as.Date("2009-01-01")
enddate   <- as.Date("2009-10-31")
newdata <- leadership[which(leadership$date >= startdate &
leadership$date <= enddate),]


## HTML file 읽어오기
library(XML)
# Read and parse HTML file
doc.html <- htmlTreeParse("http://apiolaza.net/babel.html",useInternal = TRUE)
# Extract all the paragraphs (HTML tag is p, starting at the root of the document). Unlist flattens the list to create a character vector.
doc.text <- unlist(xpathApply(doc.html, "//p", xmlValue))
# Replace all \n by spaces
doc.text <- gsub("\\n", " ", doc.text) 
# Join all the elements of the character vector into a single character string, separated by spaces
doc.text <- paste(doc.text, collapse=" ")

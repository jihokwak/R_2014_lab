## RESHAPE 
library(reshape)
data(iris)
iris2<-melt(iris,id="Species") 
# or melt(iris, id.vars="Species"))
head(iris2)
cast(iris2,Species~variable,mean) # 변수별 평균 보기
cast(iris2,Species~variable,length) # 변수별 개수 보기
cast(iris2,Species~variable,length,margins=c("grand_row","grand_col"))
#행과 열별로 소계 구하기
data(airquality)
names(airquality)<-tolower(names(airquality))
airq<-melt(airquality,id=c("month","day")) # 월일을 id로
head(airq)
airq_mean<-cast(airq, month~variable, mean, na.rm=T) #월별 변수들의 평균
airq_mean
airq_sd<-cast(airq, month~variable, sd, na.rm=T) # 월별 변수들의 표준편차
airq_sd
airq_cv<-airq_sd[-1]/airq_mean[-1] # 변동계수 구해보기 (변동 계수가 무엇인가요?)
month<-airq_sd$month;airq_cv<-cbind(month,airq_cv)
airq_cv
airq_range<-cast(airq, month~variable, range, na.rm=T) # 변수값의 범위
airq_range
airq_mean_ozone<-cast(airq,day~month,sum,subset=variable=="ozone",na.rm=T)
airq_mean_ozone # 오존량만 추출 및 아래 처럼 확인
airq[c(airq$month==9 & airq$day==30 & airq$variable=="ozone"),]

# 그리고... reshape package 유용한 쓰임새를 보면...
library(ggplot2);data(economics)
str(economics);econ<-economics
ggplot(econ,aes(date,unemploy))+geom_line()
ggplot(econ,aes(date,uempmed))+geom_line()
ggplot(econ,aes(unemploy,uempmed))+geom_point()+geom_smooth(method=loess)
new.econ<-melt(econ,id="date",measure=c("unemploy","uempmed"))
ggplot(new.econ,aes(date,value))+geom_line()+facet_grid(variable~.,scales="free_y")

# tips data에 관하여 다음 질문에 답해 보세요.
# 어느 요일에 팁을 가장 많이 받았나요?
# 요일별 평균적으로 받은 팁은 얼마인가요?
# 남성고객과 여성 고객중 어느쪽이 팁에 후한편인가요?
# 흡연 여부와 성별 고객의 팁은?

data(tips)
tips<-melt(tips,id=c("day", "time", "sex", "smoker"));head(tips)
newtip<-cast(tips,day~variable,sum)
newtip # 문제 1 답 1
ggplot(newtip, aes(day,tip))+geom_point()
# ggplot(newtip, aes(day,tip))+geom_bar(stat="identity") 문제 1 답 2 
newtip<-cast(tips,day~variable,mean,na.rm=T)
newtip # 문제 2 답 1
ggplot(newtip, aes(day,tip))+geom_bar(stat="identity") # 문제 2 답 2
newtip<-cast(tips,sex~variable,mean,na.rm=T)
newtip # 문제 3 답 1
ggplot(newtip, aes(sex,tip))+geom_bar(stat="identity") # 문제 3 답 2
newtip<-cast(tips,sex+smoker~variable,mean)
newtip # 문제 4 답 1
ggplot(newtip, aes(sex,tip,fill=smoker))+geom_bar(stat="identity",position="dodge")
# 문제 4 답 2
# or cast(tips,sex~smoker~variable,mean)


## plyr 
library(plyr)
ddply(tips,"day",summarise,sum=sum(tip))
ddply(tips,"day",summarise,mean=mean(tip))
ddply(tips,"sex",summarise,mean=mean(tip))
ddply(tips,c("sex","smoker"),summarise,mean=mean(tip))

data(diamonds,package="ggplot2")
# subset 문법
ddply(diamonds,"color",subset,carat==min(carat))
ddply(diamonds,.(color),subset,carat==min(carat))
ddply(diamonds,"color",subset,carat>quantile(carat,0.99))
ddply(diamonds,"color",subset,price>mean(price))
ddply(diamonds,"color",subset,price==max(price))

# 필드 변경/추가 그리고 자료 요약
a<-ddply(diamonds,"color",transform,price=scale(price));head(a) # field 변환
b<-ddply(diamonds,"color",mutate,price2=scale(price));head(b) # field 추가
c<-ddply(diamonds,"color",summarise,mean_price=mean(price));head(c) # 데이터 요약

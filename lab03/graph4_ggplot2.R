# grouped bar graph 
ggplot(ToothGrowth,aes(x=dose,y=len,fill=supp))+geom_bar(stat="identity", position="dodge")+scale_fill_brewer(palette="Pastel1")

# adding text
ggplot(ToothGrowth,aes(dose,len,fill=supp))+geom_bar(stat="identity",position="dodge")+geom_text(aes(label=len),vjust=1.5,color="white") 
# 왜 숫자가 많을까요?
tg<-ddply(ToothGrowth,c("supp","dose"),summarise,length=sum(len))
ggplot(tg,aes(dose,length, fill=supp))+geom_bar(stat="identity",position="dodge")+geom_text(aes(label=length),vjust=1.5,color="white",position=position_dodge(0.5))

# stacked bar graph 
ggplot(ToothGrowth,aes(x=dose,y=len,fill=supp))+geom_bar(stat="identity")+scale_fill_brewer(palette="Pastel1")

# conditional mean
ggplot(ToothGrowth,aes(x=dose,y=len,color=supp))+geom_smooth(method="loess")+scale_fill_brewer(palette="Pastel1")

# line graph (mean), x값이 없을 때는 주의!
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line()
ggplot(tg, aes(x=factor(dose), y=length, colour=supp, group=supp)) + geom_line()

# x축에 2개의 값을, y축에 1개의 값을 고려한 boxplot 그리기
ggplot(ToothGrowth, aes(x=interaction(supp,dose),y=len))+geom_boxplot()

# area graph
library(gcookbook);data(uspopage)
head(uspopage,3)
ggplot(uspopage, aes(Year,Thousands,fill=AgeGroup))+geom_area()+guides(fill=guide_legend(reverse=T))
ggplot(uspopage, aes(Year,Thousands,fill=AgeGroup))+geom_area(color=NA,alpha=.4)+guides(fill=guide_legend(reverse=T))+scale_fill_brewer(palette="Blues")+geom_line(position="stack",size=.2)

# 아래 데이터를 가지고 smooth line을 그려 보세요.
data(Oxboys,package="nlme")
oxboys<-Oxboys; names(oxboys)<-tolower(names(oxboys))
ggplot(oxboys,aes(age,height,group=subject))+geom_line()+geom_smooth(method=loess)
ggplot(oxboys,aes(age,height,group=subject))+geom_line()+geom_smooth(aes(group=1),method=loess)

# weighted ggplot2 (집계 데이터 분석시 활용)
data(midwest)
ggplot(midwest,aes(percwhite,percbelowpoverty))+geom_point()+geom_smooth(method=loess)
ggplot(midwest,aes(percwhite,percbelowpoverty))+geom_point(aes(size=poptotal/1e6))+geom_smooth(method=loess,aes(weight=poptotal/1e6))

# time series data 다수 panel로 보기
data(economics);str(economics);library(reshape)
em1<-melt(economics,id="date")
head(em1)
ggplot(em1,aes(date,value,group=variable))+geom_line()+facet_grid(variable~.,scales="free_y")

em2<-melt(economics,id="date",measure=c("unemploy","uempmed"))
ggplot(em2,aes(date,value,group=variable))+geom_line()+facet_grid(variable~.,scales="free_y")

ggplot(economics, aes(date))+geom_line(aes(y=unemploy,colour="unemploy"))+geom_line(aes(y=uempmed,colour="uempmed"))+scale_colour_hue("variable")

range01 <- function(x) {
	rng <- range(x, na.rm = TRUE)
	(x - rng[1]) / diff(rng)
	}
emp3<-ddply(em2,"variable",transform,value=range01(value))
ggplot(emp3,aes(date,value,color=variable))+geom_line()

# quiz : 상기 함수를 적용하지 않고 다른 방법으로 "unemploy"와 "uempmed" 두 변수를 한 그래프에 그려 보세요.

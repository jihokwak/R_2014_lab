#  특정 연령대별 빈도합 구하기
freq<-count(subset(r_master, age>=20 & age<30))
str(freq) # 빈도칼럼 추가 확인
sum(freq$freq)
# 성별 연령대별 막대 그래프 그리기
ggplot(r_master,aes(age,fill=sex_flag))+geom_bar(binwidth=10)+scale_x_continuous(limits=c(1,70))+guides(fill=guide_legend(reverse=T))
# 수백만개 데이터에서 3만개 표본추출
r_samp <- r_tot[sample(nrow(r_tot),30000),]
# 메뉴별로 주문건수 계산하여 빈도 내림차순으로 정렬한 객체 생성
pop.menu<-arrange(count(mcount,"menu_name"),desc(freq))
# 상위 30개 메뉴 선정하여 그래프 그리기
top30<-head(pop.menu,30)
ggplot(top30,aes(y=reorder(menu_name,freq),x=freq))+geom_point()+ylab("menu")
# 새로운 필드를 만들고 명명하기
object$new_field<-object$existing_field>=0
# 양/음수 그래프 만들기, 범례 없애기
ggplot(data, aes(x=,y=, fill=new_field))+geom_bar(stat="identity", position="identity")+scale_fill_manual(guide=F)
# 또는 but be cautious!
ggplot(cabbage_exp, aes(Date,Weight,fill=factor, order=desc(factor)))+geom_bar(stat="identity")
# 바 그래프에 텍스트 추가
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat="identity")+geom_text(aes(label=Weight),vjust=1.5,color="white")
# y range of plot will auto-adjust
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat="identity")+geom_text(aes(y=Weight+0.1, label=Weight))
# Sort by the day and sex columns to make stacked bar and text
arrange(cabbage_exp, Date, Sex)
# get the cummulative sum 
ddply(data, "Date", transform, label_y=cumsum(weight))
ggplot(data, aes(Date,weight,fill=Sex))+geom_bar(stat="identity")+geom_text(aes(y=label_y,label=weight),vjust=1.5,color="white")
 # Calculate y position, placing it in the middle
ce <- ddply(ce, "Date", transform, label_y=cumsum(Weight)-0.5*Weight)
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) +geom_bar(stat="identity") +geom_text(aes(y=label_y, label=Weight), colour="white")
# reordering
ggplot(data,aes(x=avg,y=reorder(name,avg)))+geom_point(size=3)
# get the names, sorted first by lg, then by avg
nameorder<-tophit$name[order(tophit$lg,tophit$avg)]
# turn name into a facotr, with levels in the order of nameorder
tophit$name<-factor(tophit$name,levels=nameorder)
# x, y variable이 numeric value일 경우는 line graph 사용
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + expand_limits(y=0)
# log 전환
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() +scale_y_log10()
# time series data를 data frame으로 전환 후 그래프 그리기
sunspot<-data.frame(year=as.numeric(time(sunspot.year)),sunspot=as.numeric(sunspot.year))
ggplot(sunspot,aes(year,sunspot))+geom_area()
ggplot(sunspot,aes(year,sunspot))+geom_area(fill="blue",alpha=.2)+geom_line()
# 특정 필드(연도)를 기준으로 새로운 필드(백분율) 생성
ddply(uspopage, "Year", transform, percent=Thousands/sum(Thousands)*100)
# adding a confidence region
ggplot(clim, aes(x=Year, y=Anomaly10y)) +geom_ribbon(aes(ymin=Anomaly10y-Unc10y,ymax=Anomaly10y+Unc10y),alpha=0.2)+geom_line()
# 새로운 조건의 필드 만들기
hw$weightgroup<-cut(hw$weightLb, breaks=c(-Inf,100,Inf), labels=c("<100",">=100"))
> head(hw)
  sex ageYear ageMonth heightIn weightLb weightgroup
1   f   11.92      143     56.3     85.0        <100
2   f   12.92      155     62.3    105.0       >=100
3   f   12.75      153     63.3    108.0       >=100
4   f   13.42      161     59.0     92.0        <100
5   f   15.92      191     62.5    112.5       >=100
6   f   14.25      171     62.5    112.0       >=100
# 일정 연령대를 끊어서 체중과 키를 보고자 할 때(3변수 모두 연속형 변수를 가정)   
ggplot(heightweight,aes(weightLb,heightIn,fill=ageYear))+geom_point(shape=21,size=2.5)+scale_fill_gradient(low="white",high="black",breaks=12:17,guide=guide_legend())
# 3개의 연속형 변수에 1개의 범주형 범수 총 4개 변수를 볼 때
ggplot(heightweight,aes(ageYear,heightIn,size=weightLb,color=sex))+geom_point(alpha=.5)+scale_size_area()
# 등급별 지불 금액을 boxplot으로 보기(x는 범주형, y는 숫자형 변수)
ggplot(r_master,aes(user_grade,amt))+geom_boxplot(aes(group=user_grade))
# Scatter Plot에 회귀선 그리기
sp+geom_point()+stat_smooth(method=lm) # 선형회귀선
sp+geom_point()+stat_smooth(method=lm,level=0.99) # 99% 신뢰구간
sp + geom_point(colour="grey60") + stat_smooth(method=loess) # locally weighted polynomial
# 기존 필드(팩터)를 새로운 필드(벡터)로 전환하기
b$classN[b$class=="benign"]<-0
b$classN[b$class=="malignant"]<-1
# logistic regression 
ggplot(b,aes(V1,classn))+geom_point(position=position_jitter(width=0.3,height=0.06),alpha=0.4,shape=21,size=1.5)+stat_smooth(method=glm,family=binomial)
# modeling -> predict values -> graph
modlinear <- lm(heightIn ~ ageYear, heightweight)
modloess  <- loess(heightIn ~ ageYear, heightweight)
lm_predicted <- predictvals(modlinear, "ageYear", "heightIn")
loess_predicted <- predictvals(modloess, "ageYear", "heightIn")
sp + geom_line(data=lm_predicted, colour="red", size=.8) +geom_line(data=loess_predicted, colour="blue", size=.8)
# labeling points in a scatter plot
ggplot(subset(countries,Year==2009 & healthexp>2000),aes(healthexp,infmortality))+geom_point()+geom_text(aes(label=Name),size=4)
# pairs graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- abs(cor(x, y, use="complete.obs"))
        txt <- format(c(r, 0.123456789), digits=digits)[1]
        txt <- paste(prefix, txt, sep="")
        if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt, cex =  cex.cor * (1 + r) / 2)
}
panel.hist <- function(x, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks
        nB <- length(breaks)
        y <- h$counts
        y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}
pairs(c2009[,2:5], upper.panel = panel.cor,diag.panel  = panel.hist,lower.panel = panel.smooth)
# Convert smoke to a factor and make a new faceted graph
birthwt1$smoke <- factor(birthwt1$smoke)
levels(birthwt1$smoke)
library(plyr) # For the revalue() function
birthwt1$smoke <- revalue(birthwt1$smoke, c("0"="No Smoke", "1"="Smoke"))
ggplot(birthwt1, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +facet_grid(smoke ~ .)
# histogram and density curve
ggplot(faithful,aes(x=waiting,y=..density..))+geom_histogram(fill="cornsilk",color="grey60",size=.2)+geom_density()+xlim(35,105)
# adding means to a boxplot
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot()+stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="white")

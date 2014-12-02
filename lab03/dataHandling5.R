library(hflights);dim(hflights)
head(hflights,3)
library(dplyr)
hflights_df<-tbl_df(hflights) # 일부만 볼 수 있게...
hflights_df

# 1_filter rows with filter()
filter(hflights_df, Month==1, DayOfWeek==1)
# or hflights[hflights$Month==1 & hflights$DayOfWeek==1,] # 두 값이 같은 지 어떻게 확인하죠?
# 2_arrange rows with arrange()
arrange(hflights_df, DayofMonth, Month, Year)
arrange(hflights_df, desc(ArrDelay))
# or hflights[order(hflights$DayofMonth, hflights$Month, hflights$Year),]
# or hflights[order(desc(hflights$ArrDelay)),]
# 3_select columns with select()
select(hflights_df, Year, Month, DayOfWeek)
select(hflights_df, Year:DayOfWeek)
select(hflights_df, -(Year:DayOfWeek))
# 4_add new columns with mutate()
mutate(hflights_df, gain=ArrDelay-DepDelay, speed=Distance/AirTime*60)
transmute(hflights_df, gain=ArrDelay-DepDelay,speed=Distance/AirTime*60)
mutate(hflights_df,gain=ArrDelay-DepDelay, gain_per_hour=gain/(AirTime/60))
transmute(hflights_df,gain=ArrDelay-DepDelay,gain_per_hour=gain/(AirTime/60))
# 5_summarise values with summarise()
summarize(hflights_df, delay=mean(DepDelay,na.rm=T))
# 6_sampling
sample_n(hflights_df,30)
sample_frac(hflights_df,0.01)

# Grouped operations
planes<-group_by(hflights_df, TailNum)
delay<-summarise(planes, count=n(), dist=mean(Distance,na.rm=T), delay=mean(ArrDelay,na.rm=T))
delay<-filter(delay,count>20,dist<2000)
library(ggplot2)
ggplot(delay,aes(dist,delay))+geom_point(aes(size=count),alpha=1/2)+stat_smooth(method="loess")+scale_size_area()
destinations<-group_by(hflights_df, Dest)
summarize(destinations, planes=n_distinct(TailNum), flights=n())
daily<-group_by(hflights_df, Year, Month, DayofMonth)
(per_day<-summarise(daily, flights=n()))
(per_month<-summarise(per_day, flights=sum(flights)))
(per_year<-summarise(per_month, flights=sum(flights)))

# chaining
a1<-group_by(hflights, Year, Month, DayofMonth)
a2<-select(a1, Year:DayofMonth, ArrDelay, DepDelay)
a3<-summarise(a2, arr=mean(ArrDelay, na.rm=T), dep=mean(DepDelay, na.rm=T))
a4<-filter(a3, arr>30 | dep>30)
a4 # 방법 1

b<-filter(summarise(select(group_by(hflights,Year,Month,DayofMonth),Year:DayofMonth, ArrDelay, DepDelay), arr=mean(ArrDelay,na.rm=T),dep=mean(DepDelay,na.rm=T)), arr>30 | dep>30) # 방법 2

identical(a4,b)

hflights %>%
group_by(Year,Month,DayofMonth) %>%
select(Year:DayofMonth, ArrDelay, DepDelay) %>%
summarise(arr=mean(ArrDelay,na.rm=T),dep=mean(DepDelay,na.rm=T)) %>%
filter(arr>30 | dep>30) # 방법 3

# 연습해 보세요
data(Batting,package="Lahman")
games<-ddply(Batting,"playerID",summarise,total=sum(G))
head(arrange(games,desc(total)), 5)

players<-group_by(Batting, playerID)
games<-summarise(players, total=sum(G))
head(arrange(games,desc(total)), 5)

Batting %>%
group_by(playerID) %>%
summarise(total = sum(G)) %>%
arrange(desc(total)) %>%
head(5)

library(car)
scatterplotMatrix(~mpg+disp+drat+wt, data=mtcars, spread=F, lty.smooth=2)
scatterplotMatrix(~mpg+disp+drat+wt | cyl, data=mtcars, spread=F, diagonal="histogram")
library(gclus)
mydata<-mtcars[c(1,3,5,6)]
mydata.corr<-abs(cor(mydata))
mycolors<-dmat.color(mydata.corr) # matrix에 색상 도입
myorder<-order.single(mydata.corr) # 상관성이 높은 객체를 이웃시킴
cpairs(mydata,myorder,panel.colors=mycolors,gap=.5)

# high density scatter plot
set.seed(1234)
n<-10000
c1<-matrix(rnorm(n, mean=0, sd=.05), ncol=2)
c2<-matrix(rnorm(n, 3, 2), ncol=2)
mydata<-rbind(c1,c2)
mydata<-as.data.frame(mydata)
names(mydata)<-c("x","y")
with(mydata,plot(x,y,pch=19))
with(mydata, smoothScatter(x,y))
ggplot(mydata,aes(x,y))+geom_point(alpha=.05)

# 3D scatter plot
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt,disp,mpg)
s3d<-scatterplot3d(wt,disp,mpg,pch=16,highlight.3d=T,type="h")
fit<-lm(mpg~wt+disp)
s3d$plane3d(fit)
library(rgl)
plot3d(wt,disp,mpg,col="red", size=5)

# bubble chart
r<-sqrt(cyl/pi)
symbols(wt,mpg,circle=r,inches=0.3,fg="white",bg="lightblue")
text(wt,mpg,rownames(mtcars),cex=0.6)

# 상관 분석표 그리기
data(mtcars)
mtcars.cor<-cor(mtcars) # 상관분석의 전제는 무엇인가요?
mtcars.cor
mtcars.cor<-round(mtcars.cor, digits=2)
mtcars.cor
library(corrplot)
corrplot(mtcars.cor,method="shade",shade.col=NA,tl.col="black",tl.srt=45,addCoef.col="black")
corrplot(mtcars.cor) # 차이점을 보세요
corrplot(mtcars.cor,method="shade")
corrplot(mtcars.cor,method="shade",shade.col=NA)
corrplot(mtcars.cor,method="shade",shade.col=NA,tl.col="black")
corrplot(mtcars.cor,method="shade",shade.col=NA,tl.col="black",tl.srt=45)
corrplot(mtcars.cor,method="shade",shade.col=NA,tl.col="black",tl.srt=45,addCoef.col="black")
# 더 세련되게 하고 싶다면(하지만 권하기는 좀?)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mtcars.cor,method="shade",shade.col=NA,tl.col="black",tl.srt=45,col=col(200),addCoef.col="black",addcolorlabel="no",order="AOE")

# Mosaic Plot for categorical variable
library(vcd)
data(Titanic)
ftable(Titanic)
mosaic(Titanic,shade=T,legend=T) # OR
mosaic(~Class+Sex+Age+Survived,data=Titanic,shade=T,legend=T)
# 또는 
mosaicplot(Titanic,color=T)
mosaicplot(~Class+Survived,data=Titanic,color=T)
mosaicplot(~Class+Sex+Survived,data=Titanic,color=T)

data(UCBAdmissions)
UCBAdmissions
ftable(UCBAdmissions)
dimnames(UCBAdmissions)
mosaic(~Admit+Gender+Dept,data=UCBAdmissions)
mosaic(~Dept+Gender+Admit,data=UCBAdmissions,highlighting="Admit",highlighting_fill=c("lightblue","pink"),direction=c("v","h","v"))
mosaic(~Dept+Gender+Admit,data=UCBAdmissions,highlighting="Admit",highlighting_fill=c("lightblue", "pink"),direction=c("v", "v", "h"))

# heat map
data(presidents);head(presidents)
class(presidents)
pres<-data.frame(rating=as.numeric(presidents),year=as.numeric(floor(time(presidents))),quarter=as.numeric(cycle(presidents)))
head(pres)
p<-ggplot(pres,aes(year,quarter,fill=rating))
p+geom_tile()
p+geom_raster()

# interactive plot
library(iplots);library(JGR) # mac은 JGR install
attach(mtcars)
cylinders<-factor(cyl)
gears<-factor(gear)
transmission<-factor(am)
ihist(mpg)
ibar(gears)
iplot(mpg, wt)
ibox(mtcars[c("mpg", "wt", "qsec", "disp", "hp")])
ipcp(mtcars[c("mpg", "wt", "qsec", "disp", "hp")])
imosaic(transmission, cylinders)
detach(mtcars)

# creating a map
library(maps);library(ggplot2)
states_map<-map_data("state") 
# 지도 데이터를 데이터 프레임으로
# county,france,italy,nz,usa,world,world2
head(states_map)
ggplot(states_map,aes(long,lat,group=group))+geom_polygon(fill="white",color="black")
# 또는 ggplot(states_map,aes(long,lat,group=group))+geom_path()+coord_map("mercator")
world_map<-map_data("world")
head(world_map)
sort(unique(world_map$region)) # sort는 왜 했을까요?
east_asia<-map_data("world",region=c("Japan","China","North Korea","South Korea"))
ggplot(east_asia,aes(long,lat,group=group,fill=region))+geom_polygon(color="black")

# 동남아시아를 그려보면...
south_asia<-map_data("world", region=c("Vietnam","Myanmar","Cambodia","Thailand","Malaysia","Indonesia" ,"China","Laos"))
ggplot(south_asia,aes(long,lat,group=group,fill=region))+geom_polygon(color="black")
nz1 <- map_data("world", region="New Zealand")
nz1 <- subset(nz1, long > 0 & lat > -48) # Trim off islands
ggplot(nz1, aes(x=long, y=lat, group=group)) + geom_path()
nz2<-map_data("nz")
ggplot(nz2, aes(long,lat,group=group))+geom_path()

# creating a choropleth map
data(USArrests);str(USArrests)
crimes<-data.frame(state=tolower(rownames(USArrests)),USArrests) # row.names 동일
head(USArrests);head(crimes)
states_map<-map_data("state")
crime_map<-merge(states_map,crimes,by.x="region",by.y="state") # merge하면 order가 바뀌므로 반드시 arrange할 것
head(crime_map)
library(plyr)
crime_map<-arrange(crime_map,group,order);head(crime_map) # group is a grouping variable for each polygon, the order to connect each point within a group
ggplot(crime_map,aes(long,lat,group=group,fill=Assault))+geom_polygon(color="black")+coord_map("polyconic")

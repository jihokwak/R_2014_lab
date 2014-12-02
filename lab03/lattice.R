library(lattice);library(nutshell)

data(births2006.smpl);birth<-births2006.smpl

head(birth)

names(birth)<-tolower(names(birth))

birth_table<-table(birth$dob_wk)

birth_table

barchart(birth_table,ylab=”Day of Week”,col=”black”)

#ggplot(birth,aes(dob_wk))+geom_bar()

birth_table2<-table(WK=birth$dob_wk,MM=birth$dmeth_rec)

birth_table2

birth_table2<-birth_table2[,-2]

birth_table2

barchart(birth_table2,ylab=”Day of Week”)

barchart(birth_table2,horizontal=F,groups=F,xlab=”Day of Week”,col=”black”)

#ggplot(birth,aes(dob_wk,fill=dmeth_rec))+geom_bar(position=”dodge”)

histogram(~dbwt|dplural,data=birth,layout=c(1,5),col=”black”)

#ggplot(birth,aes(dbwt))+geom_bar(binwidth=100)+facet_grid(dplural~.)

histogram(~dbwt|dmeth_rec,data=birth,layout=c(1,3),col=”black”)

#ggplot(birth,aes(dbwt))+geom_bar(binwidth=100)+facet_grid(dmeth_rec~.)

densityplot(~dbwt|dplural,data=birth,layout=c(1,5),plot.points=F,col=”black”)

#ggplot(birth,aes(dbwt))+geom_density()+facet_grid(dplural~.)

densityplot(~dbwt,groups=dplural,data=birth,plot.points=F)

#ggplot(birth,aes(dbwt,color=dplural))+geom_density()

dotplot(~dbwt|dplural,data=birth,layout=c(1,5),plot.points=F,col=”black”)

xyplot(dbwt~dob_wk,data=birth,col=”black”)

xyplot(dbwt~dob_wk|dplural,data=birth,layout=c(1,5),col=”black”)

xyplot(dbwt~wtgain,data=birth,col=”black”)

#ggplot(birth,aes(wtgain,dbwt))+geom_point()

xyplot(dbwt~wtgain|dplural,data=birth,layout=c(1,5),col=”black”)

ggplot(birth,aes(wtgain,dbwt))+geom_point()+facet_grid(dplural~.)

smoothScatter(birth$wtgain,birth$dbwt)

#ggplot(birth,aes(wtgain,dbwt))+geom_point(alpha=.003)

boxplot(dbwt~apgar5,data=birth,ylab=”dbwt”,xlab=”apgar5″)

#ggplot(birth,aes(x=factor(apgar5),dbwt))+geom_boxplot()

boxplot(dbwt~dob_wk,data=birth,ylab=”dbwt”,xlab=”Day of Week”)

#ggplot(birth,aes(x=factor(dob_wk),dbwt))+geom_boxplot()

fac<-factor(birth$dplural)

res<-birth$dbwt

t4<-tapply(res,fac,mean,na.rm=T);t4

t5<-tapply(birth$dbwt,INDEX=list(birth$dplural,birth$sex),FUN=mean,na.rm=T)

barplot(t4,ylab=”DBWT”);barplot(t5,beside=T,ylab=”DBWT”)

#library(plyr)

#t4_1<-ddply(birth,”dplural”,summarise,mean_dbwt=mean(dbwt,na.rm=T))

#ggplot(t4_1,aes(dplural,mean_dbwt))+geom_bar(stat=”identity”)

#t4_2<-ddply(birth,c(“dplural”,”sex”),summarise,mean_dbwt=mean(dbwt,na.rm=T))

#ggplot(t4_2,aes(dplural,mean_dbwt,fill=sex))+geom_bar(position=”dodge”,stat=”identity”)
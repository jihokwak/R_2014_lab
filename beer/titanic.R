rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R/titanic')
getwd()
dir()

library(data.table)
library(ggplot2)


titanic <- read.csv("titanic.csv")

#분석 편의상 data.table로 변환한다. 
titanic.dt <- as.data.table(titanic)

names(titanic.dt)
head(titanic.dt)


titanic.dt$survived <- as.factor(titanic.dt$survived)

#15세 미만을 아이로 본다. 
titanic.dt[,isminor:="adult"]
titanic.dt[age < 15,isminor:="child"]
titanic.dt$isminor <- as.factor(titanic.dt$isminor)

# 등급별 생존률 
titanic.dt[, length(which(survived == 1))/nrow(.SD), by=pclass]

# 성별 생존률
titanic.dt[, length(which(survived == 1))/nrow(.SD), by=sex]

#등급/성별 생존률 
survived_pclass_sex <- titanic.dt[, list(cntsurv=length(which(survived == 1)), cntdie=length(which(survived == 0))), 
                                  by=list(pclass, sex)][,list(psurvived=cntsurv/(cntsurv + cntdie)),by=list(pclass, sex)]

ggplot(survived_pclass_sex, aes(pclass, sex)) +
  geom_tile(aes(fill=psurvived)) + scale_fill_gradient2("생존률") + xlab("승객등급") + ylab("성별")

#성별, 등급, 나이별 생존률 
survived_pclass_sex_isminor<- titanic.dt[,list(cntsurv=length(which(survived == 1)), cntdie=length(which(survived == 0))),by=list(pclass, sex, isminor)][,list(psurvived=cntsurv/(cntsurv + cntdie)),by=list(pclass, sex, isminor)]

survived_pclass_sex_isminor$sex_age <- apply(survived_pclass_sex_isminor[,list(sex,isminor)], 1, paste, collapse="_")

library(scales)
ggplot(survived_pclass_sex_isminor, aes(pclass, sex_age)) +
  geom_tile(aes(fill=psurvived)) + scale_fill_gradient2("생존률", low=muted("white"), high=muted("blue")) + xlab("승객등급") + ylab("성별과 나이")
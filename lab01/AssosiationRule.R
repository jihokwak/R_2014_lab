install.packages("arules") 
install.packages("Matrix")
library("arules")
setwd("C:\Users\kosmo023\Dropbox\201409_bigdataDaeWoo")
asso=read.table("ex421.txt",header=T ,fileEncoding = "EUC-KR")
asso
trans<-as.matrix(asso,"Transaction")
rules1<-apriori(trans,parameter=list(supp=0.4,conf=0.6,  target="rules"))
rules1
inspect(sort(rules1))

rules2<-apriori(trans,parameter=list(suppor=0.6))

#?˜ë¯? ?—†?Š” ê²°ê³¼ë¥? ? œê±°í•˜ê¸? ?œ„?•´?„œ ?™¼ìª? ë¶€ë¶„ì´ ?—†?Š” ê²ƒì„ ê²°ê³¼?—?„œ ëº?-
rules2.sub_1=subset(rules1,subset=lhs %pin% ""  & lift>0.6)
inspect(sort( rules2.sub_1)[1:3])
inspect(sort(rules2))
result=as(sort( rules2.sub_1)[1:3], "data.frame")
result

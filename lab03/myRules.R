# preparing data
library(arules)
read.transactions(file="",sep=",") # POS자료 읽어오기
data(Groceries);groceries<-Groceries
summary(groceries)
inspect(groceries[1:5]) # 5건의 거래(영수증)
itemFrequency(groceries[,1:3]) # 3개 상품의 지지도
itemFrequencyPlot(groceries,support=0.1)
itemFrequencyPlot(groceries,topN=20)
image(groceries[1:5])
image(sample(groceries,100))

# finding rules
apriori(groceries) 
# default support=0.1,confidence=0.8
groceryrule<-apriori(groceries,parameter=list(support=0.006,confidence=0.25,minlen=2));groceryrule 
# 퍼래미터 기준은 무엇일까요?
60/9835, 2개 상품 이상 조건
summary(groceryrule)
inspect(groceryrule[1:3]) # rule을 한번 보자
inspect(sort(groceryrule,by="lift")[1:5])
berryrule<-subset(groceryrule,items %in% "berries") 
# %in% c("berries","yogurt") : 둘 중 하나
# %pin% "fruit" : tropical fruit or citrus fruit
# %ain% c("berries,"yogurt") : 둘다
inspect(berryrule)

# 그래프로 표현
library(arulesViz)
plot(berryrule,measure=c("support","confidence"),shading="lift",interactive=F)
plot(berryrule,measure=c("support","confidence"),shading="lift",interactive=T)

# saving rules (문법에 주의!)
write(groceryrule,file="groceryrule.csv",sep=",",row.names=F)
groceryrule_df<-as(groceryrule,"data.frame")

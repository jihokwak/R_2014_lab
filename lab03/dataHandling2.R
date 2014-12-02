data(Cars93,package="MASS")
head(Cars93);str(Cars93)

## data frame에서 데이터 추출
mycar<-Cars93[,-c(15:17)] # 열추출
mycar<-mycar[c(mycar$Manufacturer=="Honda" & mycar$Price>=15),] # 행추출
mycar # 방법 1

mycar2<-subset(Cars93,select=-c(15:17)) # 열추출
mycar2<-subset(mycar2, Price>=15 & Manufacturer=="Honda") # 행추출
mycar2 # 방법 2

# 동일한 df 확인하려면
all.equal(mycar,mycar2);identical(mycar,mycar2) 

## 문제 1) 현대 자동차만 추출해 보세요
## 문제 2) 가장 비싼 자동차를 추출해 보세요
## 문제 3) 4기통 자동차 중 도시연비가 가장 좋은 차는?

# 문제 1 답)
mycar<-Cars93;names(mycar)<-tolower(names(mycar))
table(mycar$manufacturer) # 제조사 확인 후
hyundai<-mycar[mycar$manufacturer=="Hyundai",] # 현대차 추출 (문제 1 방법 1) 
hyundai # 확인 
hyundai2<-subset(mycar,manufacturer=="Hyundai") # 현대차 추출 (문제 1 방법 2)
identical(hyundai,hyundai2)

# 문제 2 답)
range(mycar$price) # 가격 범위를 확인 후
maxprice<-mycar[mycar$price>=61.9,] # 최고가 추출 (문제 2 방법 1)
maxprice # 확인
maxprice<-mycar[rev(order(mycar$price)),] # 또는 가격으로 정렬
head(maxprice,1) # 최고가 추출 (문제 2 방법 2)
p<-ggplot(mycar,aes(make,price))+geom_point()
p+geom_text(aes(label=make),size=3,vjust=2) # 최고가 추출 (문제 2 방법 3-1)
p+coord_flip()+theme(axis.text.y=element_text(size=5))+geom_text(aes(label=make),size=2,vjust=2) # 최고가 추출 (문제 2 방법 3-2)
expensive<-arrange(mycar,desc(price))
top3<-head(expensive,3)
top3 # 최고가 추출 (문제 2 방법 4)
# by the way... 왜 여러가지 방법을 보여주는 것일까요?

# 문제 3 답)
cyl4<-mycar[mycar$cylinders=="4",] # 4기통 차량을 살펴보고
range(cyl4$mpg.city) # 도시연비를 확인 후 최종 명령문 입력
cyl4<-cyl4[cyl4$cylinders=="4" & cyl4$mpg.city>=42,] 
cyl4 # 4기통 차량 중 연비 가장 좋은차 (문제 3 방법 1)
mycar2<-mycar[mycar$cylinders=="4",]
mycar3<-mycar2[rev(order(mycar2$mpg.city)),]
head(mycar3,1) # 4기통 차량 중 연비 가장 좋은차 (문제 3 방법 2)
p<-ggplot(subset(mycar,cylinders=="4"), aes(make,mpg.city))+geom_point()
p+geom_text(aes(label=make),size=3)+coord_flip()
# 4기통 차량 중 연비 가장 좋은차 (문제 3 방법 3)

# 기타 함수
str(Cars93) 
Cars93.num<-Cars93[,sapply(Cars93,is.numeric)] # 숫자형 필드만 추출
str(Cars93.num)
Cars93.fac<-Cars93[,sapply(Cars93,is.factor)]
str(Cars93.fac) # 팩터형 필드만 추출

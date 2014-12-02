# KNN
data(iris)
set.seed(1)
ind<-sample(2,nrow(iris),replace=T,prob=c(0.7,0.3)) 
traindata<-iris[ind==1,]
testdata<-iris[ind==2,]
train_label<-traindata[,5];test_label<-testdata[,5]
train_data<-traindata[,-5];test_data<-testdata[,-5]

library(class)
m<-knn(train_data,test_data,cl=train_label,k=10)
table(m,test_label);x<-table(m,test_label)
sum(diag(x))/sum(x)

# party
library(party)
data(iris)
ind<-sample(2,nrow(iris),replace=T,prob=c(0.7,0.3)) 
# 퀴즈 : 1부터 100까지의 수 중에서 3개를 비복원 추출해 보세요.
traindata<-iris[ind==1,]
testdata<-iris[ind==2,]
myformula<-Species~.
iris_ctree<-ctree(myformula,data=traindata)
plot(iris_ctree)
# 또는 plot(iris_ctree,type="simple")
table(predict(iris_ctree),traindata$Species)
x<-table(predict(iris_ctree),traindata$Species)
sum(diag(x))/sum(x) # 정확도 확인_1
prop.table(x) # 정확도 확인_2
library(caret);confusionMatrix(x) # 정확도 확인_3
testpred<-predict(iris_ctree,newdata=testdata)
y<-table(testpred,testdata$Species)
sum(diag(y))/sum(y)

data(Sonar,package="mlbench")
head(Sonar);summary(Sonar)
prop.table(table(Sonar$Class))
set.seed(1234)
ind<-sample(2,nrow(Sonar),replace=T,prob=c(0.7,0.3)) 
traindata<-Sonar[ind==1,]
testdata<-Sonar[ind==2,]
sonar.party<-ctree(Class~., data=traindata)
plot(sonar.party)
x<-table(predict(sonar.party),traindata$Class)
sum(diag(x))/sum(x)
# Sonar 돌려 보니 과적합이 나오는데 party 알고리즘에서는 어떻게 해결하나요?

# rpart
library(rpart)
set.seed(1234)
ind<-sample(2,nrow(iris),replace=T,prob=c(0.7,0.3)) 
traindata<-iris[ind==1,]
testdata<-iris[ind==2,]
m<-rpart(Species~., data=traindata)
m
plot(m,compress=T);text(m) # plot 1
library(rpart.plot);prp(m, type=4, extra=2) # plot 2
library(DMwR);prettyTree(m) # plot 3
x<-table(predict(m,type="class"),traindata$Species);x
sum(diag(x))/sum(x) # 방법 1
y<-table(predict(m,newdata=testdata,type="class"),testdata$Species);y
sum(diag(y))/sum(y)
printcp(m) # 방법 2
1-0.62857*0.045455;1-0.62857*0.12121
head(predict(m,newdata=iris,type="class"))

# randomForest
library(randomForest)
rf<-randomForest(Species~.,data=iris,importance=T);rf
head(predict(rf,newdata=iris))
importance(rf)
plot(rf)
varImpPlot(rf)
# 주의 사항
# 범주형 변수의 level은 32개 미만이어야 하며, 
# 결측치가 있을 경우 na.action=na.omit 입력

# SVM
library(kernlab)
data(spam);str(spam)
idx<-sample(2,nrow(spam),replace=T,prob=c(0.8,0.2))
train<-spam[idx==1,]
test<-spam[idx==2,]
m<-ksvm(type~.,data=train,kernel="vanilladot")
pred<-predict(m,test)
x<-table(pred,test$type)
sum(diag(x))/sum(x) # 방법 1
agreement<-pred==test$type
table(agreement) 
prop.table(table(agreement)) # 방법 2
m2<-ksvm(type~.,data=train,kernel="rbfdot")
pred2<-predict(m2,test)
agreement2<-pred2==test$type
table(agreement2)
prop.table(table(agreement2))

 
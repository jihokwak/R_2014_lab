# 분포
x=rnorm(100)
hist(x,probability=TRUE,col=gray(.9),main="normal mu=0,sigma=1")
curve(dnorm(x),add=T) # 정규분포
x=rchisq(100,5)
hist(x,probability=T,ylim=c(0,0.2))
curve(dchisq(x,5),add=T) # 카이제곱 분포

# 자료의 탐색
library(UsingR);library(psych)
x=c(6,10,12,12,15,21,22,33,37)
simple.eda(x)
describe(x)

# 정규성 검증 
norm<-read.csv("1_normality.csv",sep=",",header=T)
group1<-norm[norm$group==1,];group2<-norm[norm$group==2,]
shapiro.test(group1$score);shapiro.test(group2$score)
par(mfrow=c(1,2))
hist(group1$score);hist(group2$score)
qqnorm(group1$score);qqline(group1$score)
library(psych);describe(group1$score);describe(group2$score)

# 표본을 이용한 모평균 검정 (t test)
data(iris)
t.test(iris$Sepal.Length, mu=5.8) 
# 귀무가설 : 세팔 길이의 모평균은 5.8이다
t.test(iris$Sepal.Length, mu=5.8, conf.level=0.99) 
# 신뢰수준을 99% 적용
setosa<-subset(iris,select=Sepal.Length,Species=="setosa")
virginica<-subset(iris, select=Sepal.Length,Species=="virginica")
t.test(setosa,virginica) 
# 귀무가설 : 두 군의 평균은 차이가 없다 
t.test(y~x, data) # y numeric, x dichotomous
t.test(y1,y2) # both numeric
# 앞에서 활용한 norm 자료를 가지고 독립 t 검정을 수행해 보세요.
t.test(group1$score,group2$score)
# 등분산 검정 (귀무가설 : 두 군의 분산은 같다)
library(car);leveneTest(norm$score,norm$group) # 경고 메시지 없애려면?, 그리고 문법에 주의!
t.test(norm$score~factor(norm$group))
t.test(group1$score,group2$score)

# 3군의 평균 비교 = 일원배치 분산분석
anova.samp<-read.csv("4_anova.csv")
# 1.정규성 검정
group1<-anova.samp[anova.samp==1,];group2<-anova.samp[anova.samp$group==2,];group3<-anova.samp[anova.samp$group==3,]
shapiro.test(group1$score);shapiro.test(group2$score);shapiro.test(group3$score) 
# 2.등분산 검정
library(car);leveneTest(anova.samp$score,factor(anova.samp$group)) 
# 3.분산분석
oneway.test(anova.samp$score~anova.samp$group)
# 4.사후 검정 (Tukey, bonferroni)                                                                                          
TukeyHSD(aov(anova.samp$score~factor(anova.samp$group)))
pairwise.t.test(anova.samp$score,factor(anova.samp$group),p.adjust.method="bonferroni")
# R에서도 다중비교 상황을 보정해서 0.05값 기준 결과 제시
boxplot(anova.samp$score~anova.samp$group) 

# 카이제곱 검정 sum{(관측빈도-기대빈도)^2 / 기대빈도}
chi_data<-read.csv("5_chisq.csv", sep=",", header=T);str(chi_data)
attach(chi_data)
chi_table<-xtabs(~obesity+diabetes) # table 만드는 다른 방법은 없나요? 
chi_table # 1 비만체중, 2 정상체중, 1 당뇨, 2 정상 
chisq.test(chi_table)$expected
chisq.test(chi_table)
# 귀무가설 : 두 변수는 연관성이 없다.(당뇨와 비만은 연관성이 없다.)
library(vcd);exp(oddsratio(chi_table))
# 비만 체중인 사람이 당뇨가 있을 가능성은 정상 체중인의 4.3배

# contingency table data로 카이제곱 검정하기
female<-c(18,102) # 여성 120명중 편두통이 있음 18명, 편두통 없음 102명
male<-c(10, 110)  # 남성은 편두통 있음 10명, 편두통 없음 110명
편두통과 성별 발병률과 어떤 관련이 있는가?
migraine<-cbind(female,male);migraine
chisq.test(migraine)

# 상관분석
plot(x) #모든 변수의 연관성을 산포도로 보기
cor(x) #모든 변수간 상관관계 보기
cor.test(변수,변수) # 두변수간 상관관계의 유의성 확인
library(psych)
corr.test() # 모든 변수간 상관관계 및 유의성 확인
library(corrgram)
corrgram(cor(iris[,1:4]), type="corr", upper.panel=panel.conf)

# 편상관분석
install.packages("ppcor")
library(ppcor)
pcor.test(x,y,z)

# 단순회귀분석 
# 샘플 파일에서 허리둘레로 BMI를 예측하는 회귀식을 구하세요.
sample<-read.csv("6_sample.csv",sep=",",header=T)
library(ggplot2);ggplot(sample, aes(waistline, BMI))+geom_point()+geom_smooth(method=lm) # 선형성 확인
lm_sample<-lm(BMI~waistline, data=sample)
summary(lm_sample) # 회귀식, 설명력, 회귀계수 확인 
resid<-resid(lm_sample);par(mfrow=c(1,2))
qqnorm(resid);qqline(resid)
plot(resid) # 기본가정 검정 
# waistline이 130일 경우 BMI는?
-3.97611+0.33135*130 # 방법 1
df<-data.frame(waistline=130)
predict(lm_sample, newdata=df) # 방법 2

## 기타 사항
library(lmtest);bptest(lm_sample) # 오차의 등분산 검정
names(summary(lm_sample))
summary(lm_sample)$r.squared;summary(lm_sample)$adj.r.squared
# pull out p-value from linear regression
lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}
lmp(lm_sample)
 
# 다중회귀분석
library(car)
options(digits=2)
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
fit2<-step(fit, direction="both")
summary(fit);summary(fit2)
anova(fit, fit2) # 두 모형의 차이가 없는데 어떤 모형을 선택해야 하나요? # 방법 1
library(forecast)
rbind(fit=CV(fit),fit2=CV(fit2)) # 방법 2
fit2
# 인구 500, 문맹률 1.5일 경우 살인사건 범죄율은?
1.651550+0.000224*500+4.080737*1.5 # 방법 1
df<-data.frame(Population=500,Illiteracy=1.5)
predict(fit2,newdata=df) # 방법 2
vif(fit2)
sqrt(vif(fit2))>2 # 다중공선성 확인
library(QuantPsyc);lm.beta(fit2) # 표준화 계수 확인
## 모형의 가정 검정
plot(states)
library(corrgram);corrgram(cor(states),type="corr",upper.panel=panel.conf)
resid<-fit2$residuals
plot(resid);acf(resid) # 오차의 독립성 검정_방법1
library(lmtest);dwtest(fit2,alt="two.sided")
durbinWatsonTest(fit2) # 오차의 독립성 검정_방법2 
qqnorm(resid);qqline(resid) # 오차의 정규성 검정_방법1
shapiro.test(resid) # 오차의 정규성 검정_방법2
par(mfrow=c(1,2))
plot(states$Population,resid);plot(states$Illiteracy,resid) # 오차의 등분산성 검정_방법1
ncvTest(fit2) # 오차의 등분산성 검정_방법2
library(lmtest);bptest(fit2)


# 샘플 파일에서 수축기 혈압과 유의하게 관련된 변수를 밝혀보세요.
sample<-read.csv(file.choose(),header=T,sep=",") 
head(sample);sample<-sample[2:7]
attach(sample)
m<-lm(SBP~., data=sample)  
summary(m)
m2<-step(m,direction="both")
summary(m2);anova(m,m2)

# 단순회귀 vs. 다항회귀
data(women);head(women);plot(women)
fit<-lm(weight~height, data=women)
summary(fit)
women$weight
fitted(fit)
resid(fit)
plot(women$height,women$weight);abline(fit) 
pred<-data.frame(height=58)
predict(fit,newdata=pred)
fit2<-lm(weight~height+I(height^2), data=women)
summary(fit2)
plot(women$height,women$weight);lines(women$height,fitted(fit2))
pred<-data.frame(height=58)
predict(fit2,newdata=pred)
fit3<-lm(weight~height+I(height^2)+I(height^3), data=women)
summary(fit3)

# 문제 풀어 보세요 (아래 데이터를 가지고 적절한 회귀모형을 찾으세요)
x<-c(1:9)
y<-c(5,3,2,3,4,6,10,12,18)
z<-data.frame(x,y)
plot(z) # 적합선을 미리 그려 보시고... ggplot(z,aes(x,y))+geom_point()+geom_smooth(method=loess)
fit<-lm(y~x+I(x^2),data=z) # 다르게 쓰는 방법은 아십니까?
lines(fitted(fit))
summary(fit)
pred<-data.frame(x=10)
predict(fit,newdata=pred)

# logistic regression (인구성장, 소문이 퍼지는 속도 등의 데이터)
library(MASS);data(menarche)
str(menarche)
pimple<-menarche;names(pimple)[3]<-"acne"
plot(acne/Total~Age, data=pimple)
glm.out<-glm(cbind(acne,Total-acne)~Age,family=binomial(),data=pimple) # cbind로 성공과 실패를 코딩, logit은 default 
lines(pimple$Age, glm.out$fitted, type="l", col="red")
title(main="Pimple Data with Fitted Logistic Regression Line")
summary(glm.out)
# 반응 변수는 log odds이므로 연령계수에 지수함수 적용을 잊지 마세요.
exp(1.632) # 1살 증가할 때마다 pimple을 가질 odds는 5.11배 증가
exp(glm.out$coef)
exp(confint(glm.out)) # 그런데 우리가 알고 싶은 것은 특정 연령에 여드름이 생길 확률이므로...
df<-data.frame(Age=10);df
predict(glm.out,df,type="response")

# 그런데 데이터 분포를 처음에 바로 알 수 있었을까요?
# 이 그래프를 만들기 전에 다른 그래프를 그려 보세요.
p<-ggplot(pimple, aes(Age, acne))+geom_point()
p+coord_fixed()
ggplot(pimple,aes(Age,acne/Total))+geom_point()+geom_smooth(method=loess)
ggplot(pimple,aes(Age,acne/Total))+geom_point()+geom_smooth(method=glm,family=binomial) 
anova(glm.out,test="Chisq") # 나이변수로 관측치와 적합치 차가 크게 줄어듦
1-pchisq(3693.884-26.703, 24-23) 
# p-value 산출(잔차의 편차, 자유도)
# 귀무가설 : 회귀모형으로 설명할 수 없다. 

data(Affairs, package="AER")
summary(Affairs)
table(Affairs$affairs)
Affairs$ynaffair[Affairs$affairs  > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair,levels=c(0,1),labels=c("No","Yes"))
table(Affairs$ynaffair)
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children +religiousness + education + occupation +rating,data=Affairs,family=binomial())
summary(fit.full)
fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + rating, data=Affairs, family=binomial())
summary(fit.reduced)
anova(fit.reduced, fit.full, test="Chisq") 
# 두 모형은 차이가 없으므로 간결한 모형을 선택
coef(fit.reduced) # log(odds)값이므로 
exp(coef(fit.reduced)) # odds 값으로 전환
# 수치를 해석해 보세요
# 혼외 정사 오즈비(승산비)는 다른 요소를 상수로 고정시킬 경우 결혼연수가 1년 증가할 때마다 1.106배 증가한다. 예측변수에 0이 없으므로 절편은 무의미. 
# 다른 방법exp(cbind(OR=coef(fit.reduced),confint(fit.reduced)))
testdata <- data.frame(rating=c(1, 2, 3, 4, 5), age=mean(Affairs$age),yearsmarried=mean(Affairs$yearsmarried),religiousness=mean(Affairs$religiousness))
testdata
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata
testdata <- data.frame(rating=mean(Affairs$rating),age=seq(17, 57, 10),yearsmarried=mean(Affairs$yearsmarried),religiousness=mean(Affairs$religiousness))
testdata
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata
testdata<-data.frame(rating=3, age=30, yearsmarried=3, religiousness=1)
testdata$prob<-predict(fit.reduced, newdata=testdata, type="response")
testdata
1-pchisq(675.38-615.36,4)
library(ResourceSelection)
hoslem.test(Affairs$ynaffair,fitted(fit.reduced))
# 경고문을 없애려면 어떻게 해야 할까요? 함수를 살펴보세요.

# MLR
library(nnet)
m<-multinom(Species~., data=iris)
summary(m)
head(fitted(m)) # 각 행이 각 분류에 속할 확률
predict(m, newdata=iris[c(1,51,101),], type="class") # iris 각 행을 추출하여 분류 확인
predict(m, newdata=iris[c(1,51,101),], type="probs") # 각 분류에 속할 확률 예측시
predicted<-predict(m,newdata=iris)
sum(predicted==iris$Species)/NROW(predicted) # 정확도 확인_1
xtabs(~predicted + iris$Species)

# Poisson regression
d1=read.table("http://quantedu.com/wp-content/uploads/2014/05/job.txt",header=T)
str(d1);head(d1,4)
d2=within(d1, {Race <- factor(Race)})
pm1= glm(Job ~ Race + Income+GPA, family = "poisson", data = d2)
summary(pm1) # 유의미 변수 선택 방법 1
exp(cbind(coef=coef(pm1), confint(pm1))) # 유의미 변수 선택 방법 2
pm2= glm(Job ~ Race + Income, family = "poisson", data = d2);summary(pm2)
s1= data.frame(Income = mean(d2$Income), Race = factor(1:3));s1
s2= data.frame(Income = mean(d2$Income)*1.5, Race = factor(1:3));s2
predict(pm2, s1, type = "response", se.fit = TRUE)$fit
predict(pm2, s2, type = "response", se.fit = TRUE)$fit
## 문제 풀이
data(breslow.dat, package="robust")
names(breslow.dat)
summary(breslow.dat[c(6,7,8,10)])
opar<-par(no.readonly=T)
par(mfrow=c(1,2))
attach(breslow.dat)
hist(sumY,breaks=20, xlab="seizure count", main="distribution of seizures")
boxplot(sumY~Trt, xlab="treatment", main="group comparison")
par(opar)
fit<-glm(sumY~Base+Age+Trt, data=breslow.dat, family=poisson())
summary(fit)
coef(fit) # 포아송 리그레션에서 종속변수는 조건 평균에 로그를 취한 값이므로 (log(람다)) 발작회수에 로그를 취한 것보다는 발작회수를 알고 싶으므로
exp(coef(fit))
library(qcc) # overdispersion test package
qcc.overdispersion.test(breslow.dat$sumY, type="poisson")
# overdispersion 존재를 확인하고서 (p-value <0.05)
fit.od<-glm(sumY~Base+Age+Trt, family=quasipoisson(), data=breslow.dat)
summary(fit.od)









####################### 회귀모형에 대한 기본 가정 검토 #######################
library(gvlma) # global validation of linear model assumptions
gvmodel<-gvlma(fit)
summary(gvmodel)
#Multicollinearity(다중 공선성)
#일반적으로, square root of VIF > 2 이면, 다중공선성 문제가 있는 것으로... 
library(car)
vif(fit)
sqrt(vif(fit))>2 # 확인하고
#Outliers
outlierTest(fit)
#Influential observations
cutoff<-4/(nrow(states)-length(fit$coefficients)-1)
plot(fit,which=4,cook.levels=cutoff)
abline(h=cutoff,lty=2,col="red") # or
avPlots(fit,ask=FALSE,onepage=TRUE,id.method="identify")
influencePlot(fit,id.method="identify",main="Influence Plot",sub="Circle size is proportional to Cook's distance")
########################## Corrective Measures ########################
# 1. deleting outliers for normality assumption(기입 오류, 규정 오류, 이해 부족)
# 2. transforming variables for normality,lineartiy,homoscedasticity
library(car)
summary(powerTransform(states$Murder))
boxTidwell(Murder~Population+Illiteracy,data=states)
# 3. adding or deleting variables for multicollinearity
# 4. trying a differnt approach
# 다중공선성->ridge regression,극단치->robust regression,정규성->비모수 회귀분석
# 오차의 독립성->time series/multi-level regression,최소제곱법-> glm
############################ Comparing Models ##########################
fit1<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
fit2<-lm(Murder~Population+Illiteracy, data=states)
anova(fit2,fit1) # requires nested model 
AIC(fit1,fit2)   # doesn't require
########################### variable selection #########################
library(MASS)
stepAIC(fit1,direction="backward")
stepAIC(fit1,direction="both")
library(leaps)
leaps<-regsubsets(Murder~Population+Illiteracy+Income+Frost, data=states,nbest=4)    
plot(leaps, scale="adjr2")

# 다중회귀분석관련 몇가지 함수
options(digits=2)
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
cor(states)
library(car);scatterplotMatrix(states,spread=FALSE,lty.smooth=2)
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,data=states)
summary(fit)
fit<-lm(mpg~hp+wt+hp:wt, data=mtcars)
summary(fit)
install.packages("effects");library(effects)
plot(effect("hp:wt",fit,list(wt=c(2.2,3.2,4.2))),multiline=T)

fit<-glm(ynaffair~age+yearsmarried+religiousness+rating, family=binomial(), data=Affairs)
fit.od<-glm(ynaffair~age+yearsmarried+religiousness+rating, family=quasibinomial(), data=Affairs)
pchisq(summary(fit.od)$dispersion * fit$df.residual, fit$df.residual, lower=F) # p>0.05, overdispersion is not a problem

# glmRob() : robust logistic regression, outliers and influential observations
# mlogit() : multinomial logistic regression, 3 categories
# lrm()    : ordinal logistic regression, poor/good/excellent, rms packages

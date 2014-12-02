######################### Sample handling #########################
options(digits=2)
Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose","David Jones", "Janice Markhammer", "Cheryl Cushing","Reuven Ytzrhak", "Greg Knox", "Joel England","Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student,Math,Science,English,stringsAsFactors=FALSE)
roster
# 상기 데이터에서 수학, 과학, 영어 3과목의 점수 합계에 대한 평가를 상위 20%는 "A" 다음 20%는 "B" 그 다음 20%는 "C" 그 다음 20%는 "D"로 평가한 평가표를 만들어 보시오.  

z<-scale(roster[,2:4])
z
roster$score<-rowMeans(z) # or apply(z, 1, mean)
roster
y<-quantile(roster$score, c(.8,.6,.4,.2))
y
roster$grade[roster$score >= y[1]] <- "A"
roster$grade[roster$score < y[1] & roster$score >= y[2]] <- "B"
roster$grade[roster$score < y[2] & roster$score >= y[3]] <- "C"
roster$grade[roster$score < y[3] & roster$score >= y[4]] <- "D"
roster$grade[roster$score < y[4]] <- "F"
roster

data(mtcars)
# 상기 데이터에서 트랜스 미션 타입(am)과 실린더를 기준으로 "mpg", "hp", "wt" 변수들의 갯수, 평균, 표준편차를 구하시오.
library(plyr)  # 방법 1
mpg_sum<-ddply(mtcars, c("am","cyl"), summarize, n=length(mpg),mean=mean(mpg),sd=sd(mpg))
hp_sum<-ddply(mtcars, c("am","cyl"), summarize, n=length(hp),mean=mean(hp),sd=sd(hp))
wt_sum<-ddply(mtcars, c("am","cyl"), summarize, n=length(wt),mean=mean(wt),sd=sd(wt))

library(reshape) # 방법 2
dfm<-melt(mtcars, id.vars=c("am","cyl"), measure.vars=c("mpg","hp","wt"))
mystats<-function(x) {c(n=length(x), mean=mean(x), sd=sd(x))}
cast(dfm, am+cyl+variable~., mystats) # cast(dfm, am+cyl~variable, mystats)

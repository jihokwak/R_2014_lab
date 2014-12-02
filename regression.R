rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R/regression')
getwd()
dir()


regression <- read.csv("regression.csv")
regression
plot(regression$height ~ regression$weight, main="평균키와 몸무게", xlab="Height", ylab="Weight")

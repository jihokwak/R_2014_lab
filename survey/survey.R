rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R/survey')
getwd()
dir()

#Sys.setlocale("LC_COLLATE","ko_kr.UTF-8")
#options(encoding="UTF-8")
#localeToCharset()


library(MCMCpack,  quietly = T)
library(lubridate, quietly = T)
library(reshape2,  quietly = T)
library(ggplot2,   quietly = T)
library(ggmcmc,    quietly = T)
library(plyr,      quietly = T)


surveys <- read.csv("seoul_survey_2014_U8.csv")

surveys_new <- within(surveys, {
  N   <- 표본크기
  N_p <- 표본크기 * 박원순 /100
  N_j <- 표본크기 * 정몽준 /100
  N_e <- 표본크기 * (100 - 박원순 - 정몽준)/100 
  dt  <- ymd(조사시작)
})



ggplot(melt(surveys_new, id.vars='dt', measure.vars =c('박원순', '정몽준')), aes(dt, value)) + 
  geom_point(aes(colour=variable)) + 
  stat_smooth(aes(colour=variable), method = "loess") + 
  scale_y_continuous("Support Ratio(%)", limits=c(0,100)) + 
  scale_color_discrete("Candidates", labels=c("Park", "Jung")) + 
  ggtitle("Supports of Two Candidates \n(Loess fitting)") + 
  scale_x_datetime(breaks='7 days') 



surveys_new <- surveys_new[order(surveys_new$dt,decreasing=F),]
surveys_new$N <- with(surveys_new, {N_p + N_j + N_e})


surveys_new_aggr <- ddply(surveys_new, "조사기관", 
                          summarize, N_p=sum(N_p), N_j=sum(N_j), N_e=sum(N_e), N=sum(N))


alpha <- c(1,1,1)


baye_diffs <- c()
freq_diff <- c()
ci <- c()
#sequential learning  
for(i in 1:nrow(surveys_new)){
  obs <- unlist(surveys_new[i, c("N_p", "N_j", "N_e")])
  post <- MCmultinomdirichlet(obs, alpha, mc=10000)
  baye_diffs <- append(baye_diffs, round(mean(post[,1] - post[,2]), 3))
  alpha <- (alpha + obs)
  p_1 <- obs[1]/sum(obs)
  p_2 <- obs[2]/sum(obs)
  conf_interval <- qnorm(0.975) * 
    1/sqrt(sum(obs)) * sqrt(p_1 *(1- p_1) + p_2 * (1 - p_2) + 2 * p_1 * p_2)
  freq_diff <- append(freq_diff, p_1 - p_2)
  ci <- append(ci, conf_interval)
}




diff_dist <- data.frame(diffs_val=as.numeric((post[,1] - post[,2])))

mdiff <- mean(diff_dist$diffs_val)

ggplot(diff_dist, aes(diffs_val)) + geom_histogram(binwidth=0.001) + 
  geom_vline(x=get('mdiff',envir =.GlobalEnv)) + 
  scale_x_continuous(breaks=round(c(seq(0.10, 0.15, by=0.01), 
                                    get('mdiff',envir =.GlobalEnv)),3)) + 
  xlab(expression(mean~of~theta[p]-theta[j])) 



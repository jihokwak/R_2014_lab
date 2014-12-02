rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R/weblog')
getwd()
dir()

library(ggplot2)

df = read.table('weblog.log')

df
help(read.table)

colnames(df)=c('host','ident','authuser','date','time','request','status','bytes','duration')

df$date=as.Date(df$date,"[%d/%b/%Y")

head(df)

table(df$status)

table(df$date)

table(df$time)



reqs=as.data.frame(table(df$date))

df$date

ggplot(data=reqs, aes(x=df$date, y=Freq))+ geom_line() + xlab('Date') + ylab('Requests') + opts(title='Traffic to Site')

df
ggplot(data=df, aes(x=status)) + geom_bar()+ xlab('Status') + ylab('Count') 
+ opts(title='Status')

png("imagename.png")
print(p)
dev.off()

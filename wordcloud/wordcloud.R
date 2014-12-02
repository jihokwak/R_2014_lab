rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R/wordcloud')
getwd()
dir()


install.packages("wordcloud")

require(wordcloud)
require(RColorBrewer)

datain <- read.csv("wordcloud_ryu.csv", colClasses=c("character", "numeric"))
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud.png", width=1000,height=1000)
names(datain)

wordcloud(datain$name,datain$ranking, scale=c(8,.4),min.freq=1, max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

dev.off()
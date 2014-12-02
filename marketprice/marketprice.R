rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R/marketprice')
getwd()
dir()

library(ggplot2)
library(plyr)

market_price <- read.csv("marketprice.csv",fileEncoding = "UTF-8")

#market_price <- read.csv("http://dl.dropbox.com/u/8686172/marketprice.csv",fileEncoding = "UTF-8")

ddply(market_price, .(A_NAME, M_TYPE_NAME), summarize, mean_price = mean(A_PRICE))

# ggplot(market_price, aes(x=A_NAME, y=A_PRICE, colour=M_TYPE_NAME)) +
# geom_boxplot(outlier.size=0) 



system.time({
  a1 <- ddply(market_price, .(A_NAME, M_TYPE_NAME), summarize, mean_price = mean(A_PRICE))
})


# for Linux on my case
library(doMC)
registerDoMC()

library(sqldf)
sqldf("SELECT * FROM market_price LIMIT 5;")

system.time({
  a3 <- ddply(market_price, .(A_NAME, M_TYPE_NAME), summarize, mean_price = mean(A_PRICE), 
              .parallel = TRUE)
})



library(data.table)

market_price.dt <- data.table(market_price)  #--- (1)

market_price.dt[2, list(M_NAME)]  #--- (2) 

market_price[2, "M_NAME", drop = F]

market_price.dt[, list(avg = mean(A_PRICE)), by = list(M_TYPE_NAME, A_NAME)]

setkey(market_price.dt, A_NAME)
market_price.dt["고등어"]

head(market_price.dt)

setkey(market_price.dt)

market_price.dt[A_NAME == "고등어", ]

# market_price.dt['고등어']
head(market_price.dt)


library(sqldf)
sqldf("SELECT * FROM market_price LIMIT 5;")

market_price_group_by <- sqldf("SELECT M_TYPE_NAME, A_NAME, avg(A_PRICE) as mean_price FROM market_price GROUP BY M_TYPE_NAME, A_NAME")

library(reshape2)

head(iris)

iris.melt <- melt(iris, id = "Species", value.name = "cm")

head(iris.melt)
library(ggplot2)
ggplot(iris.melt, aes(Species, cm)) + geom_point(aes(colour = variable)) + scale_color_discrete("Species")


load("pew.RData")
head(raw)


paw <- raw[, -c(8, 9, 10, 11)] ; paw
melted <- melt(paw, measure.vars = c(2, 3, 4, 5, 6, 7), variable.name = "income", 
               value.name = "freq")

head(melted)

tail(melted)

library(ggplot2)

ggplot(melted, aes(religion, freq)) + geom_bar(aes(fill = income), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45))

library(dplyr)

??ddply
melted <- ddply(melted, .(religion), mutate, freq_sum = sum(freq))

melted <- ddply(melted, .(religion, income), summarise, freq_precent = freq/freq_sum)
head(melted)

ggplot(melted, aes(religion, freq_precent)) + geom_bar(aes(fill = income), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45))

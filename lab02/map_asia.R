library("maps")
library("mapdata")

par(mfrow = c(1, 1))
map('worldHires', region=c('South Korea', 'North Korea', 'Japan', 'China'))
map('worldHires', region=c('South Korea'), col='blue', add=TRUE, fill=TRUE)
map('worldHires', region=c('North Korea'), col='red', add=TRUE, fill=TRUE)
map('worldHires', region=c('Japan'), col='black',add=TRUE, fill=TRUE)
map('worldHires', region=c('China'), col='yellow',add=TRUE, fill=TRUE)

data <- data.frame(
  date = c('2013-12-01','2013-12-02','2013-12-03','2013-12-04','2013-12-05','2013-12-06','2013-12-07','2013-12-10'),
  count = c(1,2,3,4,5,3,5,7),
  price = c(500,3000,10000,50,600,30,100,560),
  store = c('강남점','신림점','홍대점','건대점','신촌점', '강남점', '신림점', '홍대점')
)


SELECT SUM(price) FROM data GROUP BY store


##PRICE GROUP BY SUM
aggregate(data$price, by=list(data$store), FUN=sum)
##COUNT GROUP BY SUM
aggregate(data$count, by=list(data$store), FUN=sum)


library(data.table)
data <- data.table(data)
data[, sum(price), by="store"]

data.table 을 사용하는 이유는 data.frame보다 속도도 빠르며 유용한 메소드를 제공한다

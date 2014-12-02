#https://dev.twitter.com/apps/new 

library(ROAuth)
library(twitteR)
cainfo <- system.file("CurlSSL", "cacert.pem", package = "RCurl")


cred <- OAuthFactory$new(consumerKey="XXXXXXXXXXXXXX",
                         consumerSecret="YYYYYYYYYYYYYYYYY",
                         requestURL="https://api.twitter.com/oauth/request_token",
                         accessURL="http://api.twitter.com/oauth/access_token",
                         authURL="http://api.twitter.com/oauth/authorize")


cred$handshake(cainfo=cainfo)


#save(cred, file="twitteR_credentials")

#load("twitteR_credentials")

registerTwitterOAuth(cred)


z <- getCurRateLimitInfo(cainfo=cainfo)
z$getRemainingHits()


###

# 팔로워 분포  -----------------------------------------------------------------



gogamza <- getUser("gogamza",cainfo=cainfo)
gogamza.friends <- gogamza$getFriends(cainfo=cainfo)
#gogamza.friendsID <- gogamza$getFriendIDs()
gogamza.followers <- gogamza$getFollowers(cainfo=cainfo)


fcnt <- c()
idnames <- c()
location <- c()
ids <- c()
friends <- c()
statuscnt <- c()
for(i in gogamza.followers){
  fcnt <- append(fcnt, i$followersCount)
  idnames <- append(idnames, i$name)
  location <- append(location, i$location)
  ids <- append(ids, i$id)
  friends <- append(friends, i$friendsCount)
  statuscnt <- append(statuscnt, i$statusesCount)
}


gogamza.followers.df <- data.frame(id=ids, names=idnames,followers=fcnt, friends=friends, statusCount=statuscnt)

cor(gogamza.followers.df[,c(3,4)])

library(ggplot2)
#plot graph 
fol <- ggplot(gogamza.followers.df, aes(followers, friends)) + 
  geom_point(aes(size=statusCount), alpha=I(0.8), colour="red") + 
  geom_text(aes(label=names, size=followers + friends), hjust=1, vjust=-1, angle=25) + scale_size(range=c(3,7))


# 나를 팔로잉 하는  사람들의 팔로워 분포 

ggplot(gogamza.followers.df) +  geom_histogram(aes(log10(followers), fill="log of followers"), alpha=I(0.7)) + 
  geom_histogram(aes(log10(friends), fill="log of friends"), alpha=I(0.7)) + scale_fill_hue("팔로워 그리고 친구")

#전체 그래프 
tiff(file="followers.tiff" ,width = 800, height = 600, units = "px",type="windows", family="Dotum", antialias="cleartype" ,compression="lzw")
fol
dev.off()


gogamza.followers.df.medium <- subset(gogamza.followers.df, followers <= 20000)

# 팔로워 2만명 이하 
tiff(file="followers2.tiff" ,width = 800, height = 600, units = "px",type="windows", family="Dotum", antialias="cleartype" ,compression="lzw")
fo2 <- fol %+% gogamza.followers.df.medium
fo2 + geom_smooth()
dev.off()

gogamza.followers.df.small <- subset(gogamza.followers.df, followers <= 6000)

# 팔로워 6천명 이하 
tiff(file="followers3.tiff" ,width = 800, height = 600, units = "px",type="windows", family="Dotum", antialias="cleartype" ,compression="lzw")

fo3 <- fo2 %+% gogamza.followers.df.small
fo3 + geom_smooth()
dev.off()

# 팔로워 천명 이하 
gogamza.followers.df.tiny <- subset(gogamza.followers.df, followers <= 1000)

tiff(file="followers4.tiff" ,width = 800, height = 600, units = "px",type="windows", family="Dotum", antialias="cleartype" ,compression="lzw")

fo4 <- fo3 %+% gogamza.followers.df.tiny
fo4 + geom_smooth()
dev.off()

#####

library(igraph)
gogamza <- getUser("gogamza",cainfo=cainfo)

gogamza.followers <- gogamza$getFollowers(cainfo=cainfo)

followerscnt <- sapply(gogamza.followers, function(x) {x$followersCount})

followers <- sapply(gogamza.followers, function(x) {x$name})

#팔로워가 많은 순으로 정렬해서 top 20명만 추출 
followers.df <- data.frame(followers, followerscnt)
top_followers <- order(followers.df, decreasing=T)[1:20]


followermatrix <- as.matrix(data.frame(followers=followers[top_followers], me="gogamza"))

# secondfollowers <- data.frame()
# 
# for(i in gogamza.followers[top_followers]){
#   foll <- i$getFollowers(cainfo=cainfo)
#   followerscnt <- sapply(foll, function(x) {x$followersCount})
#   followers <- sapply(foll, function(x) {x$name})
#   idx <- order(data.frame(followers, followerscnt), decreasing=T)[1:5]
#   secondfollowers <- rbind(secondfollowers,data.frame(sapply(foll[idx], name), i$name))
# }


sna <- graph.edgelist(followermatrix, directed=F)


plot(sna,vertex.label=V(sna)$name, layout=layout.random,  vertex.label.cex=1, vertex.label.family="Verdana")



library(tm)
library(KoNLP)
library(wordcloud)

gogamza <- userTimeline(user="gogamza", n=300, cainfo=cainfo)

gogamzatw <- c()
for(i in 1:length(gogamza)){
  gogamzatw <- append(gogamzatw, gogamza[[i]]$text)
}


gogamzatw <- gsub("[[:space:]]"," ", gogamzatw)

gogamzaNoun <- sapply(gogamzatw, extractNoun,USE.NAMES=F)

gogamzaNoun <- unlist(gogamzaNoun, use.name=F)

gogamzaNoun <- gogamzaNoun[-which(gogamzaNoun %in% stopwords("english"))]
gogamzaNoun <- Filter(function(x){nchar(x)>=2}, gogamzaNoun)
gogamzaNoun <- gsub("[[:punct:]]", "", gogamzaNoun)

#gogamzaNoun <- Filter(function(x){nchar(x)>=2}, gogamzaNoun)



wordcount <- table(gogamzaNoun)
pal <- brewer.pal(8,"Dark2")


wordcloud(names(wordcount),freq=wordcount,scale=c(4,0.3),min.freq=10,
          random.order=T,rot.per=.1,colors=pal)
# K means clustering
data(iris)
newiris<-iris
newiris$Species<-NULL;head(newiris)
kc<-kmeans(newiris,3);kc;head(kc)
table(kc$cluster,iris$Species)
plot(newiris[c("Sepal.Length","Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length","Sepal.Width")],col=1:3,pch=8,cex=2)
# 3개 군집이 적절한지 어떻게 알수 있을까요?

p<-princomp(newiris,cor=T)
summary(p)
plot(p,type="l") # 또는
mydata<-scale(newiris)
wss<-0
for(i in 1:15)wss[i]<-sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within group sum of squares")

# Hierarchical Clustering
dim(iris);idx<-sample(150, 40)
idx
irissample<-iris[idx,]
irissample
irissample$Species<-NULL
hc<-hclust(dist(irissample), method="ave")
# default is complete
# others ("ward","single","mcquitty","median","centroid")
hc
plot(hc, hang=-1, labels=iris$Species[idx])

#there is a relatively long distance between cluster 3 (at about
0.10) and cluster 4 (at about 0.17). This suggests that this segmentation
of the data, yielding three clusters, might be a good division. Also
notice point F in the dendrogram. Whenever a single point merges
high up in a dendrogram, this is an indication that it seems different
from the rest, which we might call an “outlier,” and want to investigate
it.


library(gcookbook)
c2<-subset(countries,Year==2009)
c2<-c2[complete.cases(c2),]
set.seed(201)
c2<-c2[sample(1:nrow(c2),25),]
head(c2)
rownames(c2)<-c2$Name # 숫자로 보이지 않도록
c2<-c2[,4:7]
head(c2)
c3<-scale(c2) 
hc<-hclust(dist(c3)) 
# default distance calcualtion "euclidean" 
# ("maximum","manhattan","canberra","binary","minkowski")
plot(hc)
plot(hc, hang=-1)

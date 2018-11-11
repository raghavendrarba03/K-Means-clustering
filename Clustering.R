install.packages("C50",dependencies = TRUE)
library("C50")
str(mtcars)
mtcars_scaled=scale(mtcars)
library(cluster)
Cluster_avg <- agnes(mtcars_scaled, metric="euclidean", method = "average")
plot(Cluster_avg, which=2, main="Dendrogram of 'mtcars' (Average)")
rect.hclust(Cluster_avg, k=4)
coef(Cluster_avg)
Cluster_Ward <- agnes(mtcars_scaled, metric="euclidean", method = "ward")
plot(Cluster_Ward, main="Dendrogram of 'mtcars' (Ward)", which=2)
coef(Cluster_Ward)
mtcars$cluster= cutree(Cluster_Ward, k=4)
c=C5.0(mtcars[1:11],as.factor(mtcars[,12]))
plot(c)


#K means Clustering

str(mtcars)
cars_sc=scale(mtcars)
library(stats)
set.seed(345); k3=kmeans(cars_sc, centers=3)
library(cluster)
clusplot(x=cars_sc, clus=k3$cluster,labels=2)
k3
twss <- kmeans(cars_sc,centers=1)$tot.withinss
for (i in 2:(nrow(cars_sc)-1)) twss[i] <- kmeans(cars_sc,centers=i)$tot.withinss
plot(twss, type="b", xlab="Number of Clusters",ylab="Total Within groups SS")
set.seed(345); k4=kmeans(cars_sc, centers=4)
clusplot(x=cars_sc,clus=k4$cluster)
k4
mtcars$cluster=k4$cluster
library(C50)
d=C5.0(mtcars[1:11],as.factor(mtcars[,12]))
plot(d)

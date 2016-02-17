library(e1071)
library(cluster)
library(class)
library(amap)
library(clue)
library(graphics)
library(fpc)
library(mclust)

wt <- read.table("/home/yassine/EMSE 2015-2016/Data Science/Classification/TP Classification/column_3C.dat")

#Examination of Data------------------------------------------------------------------------------------------
summary(wt)
boxplot(wt)
plot(wt.learning,col=c("red","blue","green")[as.numeric(wt.learning[,7])])


#Learning and Test set----------------------------------------------------------------------------------------
n <- dim(wt)[1]
p <- dim(wt)[2]
learning.Set <- setdiff(1:n,3*(1:(n/3)))
test.Set <- 3*(1:(n/3))
wt.learning <- wt[learning.Set,]
wt.test <- wt[test.Set,]

#Standarazing the data --------------------------------------------------------------------------------------

wt.learning.cs <- wt.learning[,-7]    #remove qualitative varibles from learning set
wt.learning.cs <- scale(wt.learning.cs, center=TRUE, scale=TRUE)                   #center and scale the learning set 
wt.learning.cs <- as.data.frame(wt.learning.cs)
wt.learning.cs[,7] <-wt.learning[,7]                                #put back the qualitative varibles

wt.test.cs <- wt.test[,-7]    #remove qualitative varibles from learning set
wt.test.cs <- scale(wt.test.cs, center=TRUE, scale=TRUE)                   #center and scale the learning set 
wt.test.cs <- as.data.frame(wt.test.cs)
wt.test.cs[,7] <-wt.test[,7]                                #put back the qualitative varibles

#Classification-----------------------------------------------------------------------------------------------

#Kmeans
km <- kmeans(wt.learning[,1:6], 3)
plot(wt.learning.cs[,1],wt.learning.cs[,2], col=c("red","blue","green")[km$cluster])
plot(wt.learning.cs[,1],wt.learning.cs[,2], col=c("red","blue","green")[as.numeric(wt.learning[,7])])
points(km$centers[,c(1,2)], col=1:3, pch=8, cex=2)
table(km$cluster, wt.learning[,7])

#Kmeans on Standarized data
km.cs <- kmeans(wt.learning.cs[,1:6], 3)
plot(wt.learning.cs[,1],wt.learning.cs[,2], col=c("red","blue","green")[km.cs$cluster])
plot(wt.learning.cs[,1],wt.learning.cs[,2], col=c("red","blue","green")[as.numeric(wt.learning.cs[,7])])
points(km.cs$centers[,c(1,2)], col=1:3, pch=8, cex=2)
table(km.cs$cluster, wt.learning.cs[,7])



#Hierchical Clustering
distance <- dist(wt.learning[,-7], method="euclidean")
cluster <- hclust(distance, method="average")

plot(cluster, hang=-1, label=wt.learning[,7])
rect.hclust(cluster,k=3 , border = c("red","blue", "green"))

#Hierchical Clustering on Standarized Data
distance.cs <- dist(wt.learning.cs[,-7], method="euclidean")
cluster.cs <- hclust(distance.cs, method="average")

plot(cluster.cs, hang=-1, label=wt.learning.cs[,7])
rect.hclust(cluster.cs,k=3 , border = c("red","blue", "green"))

#Mclust
mc <- Mclust(wt.learning[,-7], 3)
plot(mc, what=c('classification'),dimens=c(1,2))
plot(wt.learning[,1],wt.learning[,3], col=c("red","blue","green")[as.numeric(wt.learning[,7])])
table( mc$classification,wt.learning[,7])

#Mclust on Standarized Data
mc.cs <- Mclust(wt.learning.cs[,-7], 3)
table(wt.learning.cs[,7], mc.cs$classification)

#DBSCAN
cluster.dbscan <- dbscan(wt.learning[,-7], eps=8, MinPts=3)
table(cluster.dbscan$cluster,wt.learning[,7])

#knn
pred <- knn(wt.learning[,-7], wt.test[,-7], wt.learning[,7], k = 5, prob=TRUE)
attributes(.Last.value)
table(pred,wt.test[,7]) 

#knn of Standarized data
pred.cs <- knn(wt.learning.cs[,-7], wt.test.cs[,-7], wt.learning.cs[,7], k = 3, prob=TRUE)
attributes(.Last.value)
table(pred,wt.test.cs[,7]) 

#SVM
colnames(wt.learning)
## classification mode default with factor response:
model <- svm(V7 ~ ., data = wt.learning)
summary(model)

# test with train data
pred.svm <- predict(model,wt.test)

# Check accuracy:
table(pred.svm, wt.test[,7])

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(wt.learning[,-7])),
     col = as.integer(wt.learning[,7]),
     pch = c("o","+")[1:150 %in% model$index + 1])

#SVM avec différents Kernels
#best Cost & gammma
best.svmradial <- best.svm(V7~., data = wt.learning, type="C", kernel="radial",cost=c(1,10,100,1000),gamma=2^(-1:1),cross=5)
best.svmlin <- best.svm(V7~., data = wt.learning, type="C", kernel="linear",cost=c(1,10,100,1000),cross=5)
best.svmpoly <- best.svm(V7~., data = wt.learning, type="C", kernel="polynomial",degree=c(2,3,4),cross=5)

summary(best.svmradial)
pred.svm.radial <- predict(best.svmradial,wt.test)
table(pred.svm.radial, wt.test[,7])

summary(best.svmlin)
pred.svm.lin <- predict(best.svmlin,wt.test)
table(pred.svm.lin, wt.test[,7])

summary(best.svmpoly)
pred.svm.poly <- predict(best.svmpoly,wt.test)
table(pred.svm.poly, wt.test[,7])

#Regression logistique
lg <- read.table("/home/yassine/EMSE 2015-2016/Data Science/Classification/TP Classification/column_2C.dat")

#Learning and Test set
n <- dim(lg)[1]
p <- dim(lg)[2]
learning.Set <- setdiff(1:n,3*(1:(n/3)))
test.Set <- 3*(1:(n/3))
lg.learning <- lg[learning.Set,]
lg.test <- lg[test.Set,]

glm.out = glm(V7~ ., family=binomial, data=lg.learning)
summary(glm.out)

pred.glm <- predict(glm.out,type="response",data=lg.test)
vect <- rep(0,length(pred.glm))
vect[pred>0.5]<-1
vect[pred<0.5]<-1
plot(vect)

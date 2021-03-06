getwd()
setwd("./MitAnal")
dir()
kos<-read.csv("./data/dailykos.csv")
str(kos[,-1])

# Change the data type to matrix
kosMatrix = as.matrix(kos)
str(kosMatrix)

# Turn matrix into a vector
kosVector = as.vector(kosMatrix[,-1])
str(kosVector)

kosVector2 = as.vector(kos)
str(kosVector2)

# Compute distances
distance = dist(kosMatrix[,-1], method = "euclidean")

# Hierarchical clustering
clusterIntensity = hclust(distance, method="ward.D")
plot(clusterIntensity)

k=7

rect.hclust(clusterIntensity, k , border = "blue")
kosClusters = cutree(clusterIntensity, k)
head(kosClusters)

tapply(kosMatrix,kosClusters,mean)
table(kosClusters)

kosClust1<-subset(kos,kosClusters == 1)
kosClust2<-subset(kos,kosClusters == 2)
kosClust3<-subset(kos,kosClusters == 3)
kosClust4<-subset(kos,kosClusters == 4)
kosClust5<-subset(kos,kosClusters == 5)
kosClust6<-subset(kos,kosClusters == 6)
kosClust7<-subset(kos,kosClusters == 7)


#or do this: kosClustHeir = split(kos[,-1], kosClusters)   ##cluster 1 can be accessed HierCluster[[1]], cluster 2 HierCluster[[2]]
tail(sort(colMeans(kosClust1[-1])))
tail(sort(colMeans(kosClust2[-1])))
tail(sort(colMeans(kosClust3[-1])))
tail(sort(colMeans(kosClust4[-1])))
tail(sort(colMeans(kosClust5[-1])))
tail(sort(colMeans(kosClust6[-1])))
tail(sort(colMeans(kosClust7[-1])))



##2.1

k = 7

# Run k-means
set.seed(1000)
KMC = kmeans(kos[,-1], centers = k,
             #iter.max = 1000
             )
str(KMC)

# Extract clusters
kosKclusters = KMC$cluster
KMC$centers[1]

kos.by.Clust = split(kos[,-1],kosKclusters)

for (i in 1:k){print(nrow(kos.by.Clust[[i]]))}  #gives count of each cluster


for (i in 1:k){  #will show each clusters largest word's, sorted with largest at the tail
  print (i)
  print(tail(sort(colMeans(kos.by.Clust[[i]]))))
}

tableOut<-table(kosClusters, KMC$cluster) #kosClusters was derrived from heirarchical found way up above.  KMC is a k-means
margin.table(tableOut, c(1,2))

ftable(kosClusters, KMC$cluster)


##New section HW #6
getwd()
airlines<-read.csv("/home/brian/Projects/MitAnal/data/AirlinesCluster.csv")
str(airlines)
summary(airlines)
install.packages("caret")
library(caret)
preproc = preProcess(airlines)  #normalizes the data
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

#hclust

distance<-dist(airlinesNorm,method="euclidian")
hierClust<-hclust(distance,method="ward.D")
plot(hierClust)
rect.hclust(hierClust, k=5 , border = "blue")
str(airlines)
airClust<-cutree(hierClust,k=5)
table(airClust)
tapply(airlines$Balance, airClust, mean)
tapply(airlines$QualMiles, airClust, mean)
tapply(airlines$BonusMiles, airClust, mean)
tapply(airlines$BonusTrans, airClust, mean)
tapply(airlines$FlightMiles, airClust, mean)
tapply(airlines$FlightTrans, airClust, mean)
tapply(airlines$DaysSinceEnroll, airClust, mean)
lapply(split(airlines, airClust), colMeans) #alternative way of getting at same data


# Specify number of clusters
k = 5

# Run k-means
set.seed(88)
KMC = kmeans(airlinesNorm, centers = k, iter.max = 1000)
str(KMC)
objects(KMC)
table(KMC$cluster) #gives count of each cluster

# Extract clusters
AirKclusters = KMC$cluster  #associated cluster with observations.  length should equal data nrow
KMC$centers
lapply(split(airlines, KMC$cluster), colMeans) #shows the cluster averages by non-normed data

#####New problem, HW 6

claims<-read.csv("./data/reimbursement.csv")
str(claims)
summary(claims)
(rowSums(claims[,-c(1,13:14)]))
NumConditions = rowSums(claims[,2:12])> 0  # the answer included an evaluation of every IV included in an OR statement wihthin a table.  claim$alz ==1 | claim$stroke ==1
mean(NumConditions )


corTable<-cor(claims)
sort(corTable)

hist(claims$reimbursement2009)


claims$reimbursement2008 = log(claims$reimbursement2008+1)
claims$reimbursement2009 = log(claims$reimbursement2009+1)
log(0)
hist(claims$reimbursement2009)
table((claims$reimbursement2009==0))
90498/(367507+90498)


##split data

set.seed(144)
spl = sample(1:nrow(claims), size=0.7*nrow(claims))
train = claims[spl,]
test = claims[-spl,]


#Linear regression
lm.claims<-lm(reimbursement2009~.,data=train)
summary(lm.claims)

lmPred<-predict(lm.claims, newdata=test)

SSE<-sum((lmPred-test$reimbursement2009)^2)

SST
RMSE<-sqrt(SSE/(nrow(test)-ncol(test)))

RMSE<-sqrt(SSE/(nrow(test)))
1-SSE/SST   #R^2
###alternative: rmse.lm = sqrt(mean((lmPred - test$reimbursement2009)^2))


SSE=sum((test$reimbursement2009 - lmPred)^2)
SST =sum((test$reimbursement2009 - mean(claims$reimbursement2009))^2)  #the mean comes from the baseline model
1-SSE/SST  # this is R^2

baseline.pred = mean(train$reimbursement2009)
sqrt(mean((baseline.pred - test$reimbursement2009)^2))

#smart baseline model where 2008 and 2009 costs are predicted to be equal
# shoud look something like 2008 -2009 squred, averaged, then sqrt'd
baseline.pred = sqrt(mean((train$reimbursement2009-train$reimbursement2008)^2))

##cluster
#the following removes the dependent variable
train.limited = train
train.limited$reimbursement2009 = NULL
test.limited = test
test.limited$reimbursement2009 = NULL

library(caret)

preproc = preProcess(train.limited)

train.norm = predict(preproc, train.limited)

test.norm = predict(preproc, test.limited)
summary(test.norm)
mean(test.norm$arthritis)

mean(train.norm$arthritis)

###k-means


k =3

# Run k-means
set.seed(144)
KM = kmeans(train.norm, centers = k)


str(KM)

# Extract clusters

train.norm.Kclusters = KM$cluster  #associated cluster with observations.  length should equal data nrow
KM$centers  #shows k-means clusters with varialbes
lapply(split(train, KM$cluster), colMeans) #shows the cluster averages by non-normed data
mean(train.norm$age)


##3.5
library(flexclust)

km.kcca = as.kcca(KM, train.norm)

cluster.train = predict(km.kcca)
cluster.test = predict(km.kcca, newdata=test.norm)
table(cluster.test)

train1<-subset(train,cluster.train==1)
train2<-subset(train,cluster.train==2)
train3<-subset(train,cluster.train==3)

mean(train1$reimbursement2009)
mean(train2$reimbursement2009)
mean(train3$reimbursement2009)

trainClust.split = split(train, cluster.train)   ##cluster 1 can be accessed HierCluster[[1]], cluster 2 HierCluster[[2]]
trainClust.split[[1]]

test1<-subset(test,cluster.test==1)
test2<-subset(test,cluster.test==2)
test3<-subset(test,cluster.test==3)


lm1<-lm(reimbursement2009~.,train1)
lm2<-lm(reimbursement2009~.,train2)
lm3<-lm(reimbursement2009~.,train3)

lm1$coef
lm2$coef
lm3$coef

pred.test1<-predict(lm1,newdata=test1)
pred.test2<-predict(lm2,newdata=test2)
pred.test3<-predict(lm3,newdata=test3)

mean(pred.test1)
mean(pred.test2)
mean(pred.test3)
SSE<-sum((pred.test1-test1$reimbursement2009)^2)

rmse.lm1 = sqrt(mean((pred.test1 - test1$reimbursement2009)^2))
rmse.lm2 = sqrt(mean((pred.test2 - test2$reimbursement2009)^2))
rmse.lm3 = sqrt(mean((pred.test3 - test3$reimbursement2009)^2))


all.predictions = c(pred.test1, pred.test2, pred.test3)
all.outcomes = c(test1$reimbursement2009, test2$reimbursement2009, test3$reimbursement2009)

rmse.all = sqrt(mean((all.predictions - all.outcomes)^2))




####new problem 2016


stocks<-read.csv("/home/brian/Projects/MitAnal/data/StocksCluster.csv")
str(stocks)
summary(stocks)
head(stocks)
tail(stocks)

"""For the first 11 variables, the value stored is a proportional change in
  stock value during that month. For instance, a value of 0.05 means the stock
  increased in value 5% during the month, while a value of -0.02 means the
  stock decreased in value 2% during the month."""

table(stocks$PositiveDec)
cor(stocks[,1:11])    #...0.19167279
colMeans(stocks)

#logistic
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel<-glm(PositiveDec~.,data=stocksTrain,family=binomial)
summary(StocksModel)

predLog1<-predict(StocksModel,newdata=stocksTrain,type='response')
tbl<-table(stocksTrain$PositiveDec,predLog1>.5)
tbl
(tbl[1]+tbl[4])/sum(tbl)

#test set log predictinos

predTestLog1<-predict(StocksModel,newdata=stocksTest,type='response')
tbl<-table(stocksTest$PositiveDec,predTestLog1>.5)
tbl
(tbl[1]+tbl[4])/sum(tbl)

#test set baseline predicting most common score
table(stocksTest$PositiveDec)
1897/(1577+1897)

#make new data set without d.v.

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL

limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#standardizing data (score - mean/ SD)
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
colMeans(normTrain)
colMeans(normTest)

par(mfrow=c(1,2))
hist(stocksTrain[,1])
hist(stocksTest[,1])


# Run k-means
k =3
set.seed(144)
KM = kmeans(normTrain, centers = k)
str(KM)
library(flexclust)

km.kcca = as.kcca(KM, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

stocksTrain1<-subset(stocksTrain,clusterTrain==1)
stocksTrain2<-subset(stocksTrain,clusterTrain==2)
stocksTrain3<-subset(stocksTrain,clusterTrain==3)

stocksTest1<-subset(stocksTest,clusterTest==1)
stocksTest2<-subset(stocksTest,clusterTest==2)
stocksTest3<-subset(stocksTest,clusterTest==3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)


StocksModel1<-glm(PositiveDec~.,family=binomial,data=stocksTrain1)
StocksModel2<-glm(PositiveDec~.,family=binomial,data=stocksTrain2)
StocksModel3<-glm(PositiveDec~.,family=binomial,data=stocksTrain3)

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

StocksModel1$coefficients
StocksModel2$coefficients
StocksModel3$coefficients

rbind(StocksModel1$coefficients>0,StocksModel2$coefficients>0,StocksModel3$coefficients>0)## this makes a grid showing each variable's sign on each cluster


PredictTest1<-predict(StocksModel1,newdata=stocksTest1,type='response')
PredictTest2<-predict(StocksModel2,newdata=stocksTest2,type='response')
PredictTest3<-predict(StocksModel3,newdata=stocksTest3,type='response')

tbl<-table(stocksTest1$PositiveDec,PredictTest1>.5)
tbl<-table(stocksTest2$PositiveDec,PredictTest2>.5)
tbl<-table(stocksTest3$PositiveDec,PredictTest3>.5)
tbl
(tbl[1]+tbl[4])/sum(tbl)

#combining predictions

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

tbl<-table(AllOutcomes,AllPredictions>.5)
tbl
(tbl[1]+tbl[4])/sum(tbl)



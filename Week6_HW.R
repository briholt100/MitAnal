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

airlines<-read.csv("./data/AirlinesCluster.csv")
str(airlines)
summary(airlines)
install.packages("caret")
library(caret)
preproc = preProcess(airlines)
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
table(cluster.test,)


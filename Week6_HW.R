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

kosClust = split(kos[,-1],kosKclusters)
for (i in 1:7){print(nrow(kosClust[[i]]))}


for (i in 1:7){
  print (i)
  print(tail(sort(colMeans(kosClust[[i]]))))
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
table(KMC$cluster)

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
1-SSE/SST   #R^2
###alternative: rmse.lm = sqrt(mean((lmPred - test$reimbursement2009)^2))


SSE=sum((test$reimbursement2009 - lmPred)^2) 
SST =sum((test$reimbursement2009 - mean(claims$reimbursement2009))^2)  #the mean comes from the baseline model
1-SSE/SST  # this is R^2 

baseline.pred = mean(train$reimbursement2009)
sqrt(mean((baseline.pred - test$reimbursement2009)^2))




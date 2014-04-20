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


#or do this: kosClust = split(kos, kosClusters)   ##cluster 1 can be accessed HierCluster[[1]], cluster 2 HierCluster[[2]]
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


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
distance = dist(kosVector, method = "euclidean")

# Hierarchical clustering
clusterIntensity = hclust(distance, method="ward.D")

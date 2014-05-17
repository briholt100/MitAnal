# Final part 3 for MitAnal
getwd()
stocks<-read.csv("nasdaq_returns.csv")
head(stocks)
dim(stocks)
(table(stocks$industry))
table(stocks$ret2000.12<= -.1)

table(stocks$industry,stocks$ret2008.10<= -.1)

tapply(stocks$ret2008.10,stocks$industry,mean)
tapply(stocks$ret2000.02,stocks$industry,mean)


##new df

limited<-stocks
dim(limited)

limited<-limited[,-c(1:3)]
head(limited)
sort(apply(limited,2,mean))


# Compute distances
distance = dist(limited, method = "euclidean")

# Hierarchical clustering
clusterIntensity = hclust(distance, method="ward.D")
plot(clusterIntensity)
k=5
rect.hclust(clusterIntensity, k , border = "blue")

limited.clusters = cutree(clusterIntensity, k)

head(limited)
length(limited.clusters)
nrow(stocks)
dim(limited)
length(limited)
str(limited)
limited<-limited[,-121]
limited<-data.frame(cbind(limited,as.factor(limited.clusters)))

tapply(limited,limited$limited.clusters,mean)
lapply(split(limited, limited.clusters), colMeans) #alternative way of getting at same data
stocks.clustered<-split(stocks, limited.clusters)
head(stocks.clustered[[1]])

stock.with.cluster<-data.frame(cbind(stocks,limited.clusters))

tapply(stock.with.cluster,stock.with.cluster$limited.clusters,mean)
table(stock.with.cluster$industry,stock.with.cluster$limited.clusters)

table(stock.with.cluster$subindustry,stock.with.cluster$limited.clusters)
table(stock.with.cluster$ret2000.02,stock.with.cluster$limited.clusters)

tapply(stock.with.cluster$ret2000.02,stock.with.cluster$limited.clusters,mean)


tapply(stock.with.cluster$ret2000.03,stock.with.cluster$limited.clusters,mean)
tapply(stock.with.cluster$ret2005.05,stock.with.cluster$limited.clusters,mean)
tapply(stock.with.cluster$ret2009.10,stock.with.cluster$limited.clusters,mean)
colMeans(stock.with.cluster$ret2009.12)
lapply(split(stock.with.cluster$ret2009.12, stock.with.cluster$limited.clusters), colMeans) #alternative way of getting at same data


#Ploting these clusters

boxplot(stock.with.cluster$ret2000.02~stock.with.cluster$limited.clusters)


library(ggplot2)
ggplot(stock.with.cluster, aes(x=ret2000.02, y=limited.clusters)) + geom_point()  



# Run k-means
k=5
set.seed(144)
KMC = kmeans(limited, centers = k, 
             #iter.max = 1000
)
str(KMC)
objects(KMC)
table(KMC$cluster)
kmean.limited<-cbind(limited,KMC$cluster)


table(kmean.limited[,121],kmean.limited[,122])


stocks<-cbind(stocks,limited.clusters,KMC$cluster)
dim(stocks)

stocks[stocks$stock_symbol=="AAPL",124:125]
stocks[stocks$stock_symbol=="AMZN",124:125]
stocks[stocks$stock_symbol=="MSFT",124:125]
stocks[stocks$stock_symbol=="TROW",124:125]


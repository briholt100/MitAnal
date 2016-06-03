library(ggplot2)
library(maps)
library(ggmap)


getwd()
school<-"I:/My Data Sources/mooc/MitAnalytic"
setwd(school)
home<-getwd()
setwd(paste0(home, "/mooc/MitAnal"))
dater<-getwd()
setwd(paste0(dater, "/MitAnal"))
dir()


statesMap = map_data("state")
str(statesMap)
table(statesMap$group)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") + coord_map("mercator")

polling<-read.csv("./data/PollingImputed.csv")
str(polling)
table(polling$Year)
Train<-subset(polling,polling$Year!=2012)
Test<-subset(polling,polling$Year==2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
str(predictionDataFrame)
table(predictionDataFrame$Test.State,predictionDataFrame$TestPredictionBinary)

apply(predictionDataFrame[,1:2],2,mean)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
dim(statesMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+
  geom_polygon(color = "black",linetype=1,size=1,alpha=0.3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")

predictionMap[predictionMap$Test.State=="Iowa",]
str(predictionMap)


############

parole<-read.csv("./data/parole (1).csv")
str(parole)
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
table(parole$violator)
table(parole$male,parole$violator)
130/(545+130)
14/nrow(parole)
14/(14+64)

table(parole$crime[parole$state=="2"])#2 = kentucku

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5,color="blue")
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male~.)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(.~male)
ggplot(data = parole, aes(x = age, fill = as.factor(male))) + geom_histogram(binwidth = 5)
ggplot(data = parole, aes(x = age, fill = as.factor(male))) + geom_histogram(binwidth = 5,position="identity",alpha=0.5)


ggplot(data = parole, aes(x = time.served)) + geom_histogram(color="blue")
ggplot(data = parole, aes(x = time.served,fill=as.factor(crime))) + geom_histogram(binwidth = 1,position="identity",alpha=0.5)



########
edges<-read.csv("./data/edges.csv")
str(edges)
users<-read.csv("./data/users.csv")
str(users)
edges<-lapply(edges,as.factor)

(table(edges$V1))
table(users$school,users$gender)

#install.packages("igraph")
library(igraph)

g = graph.data.frame(edges, FALSE, users)
class(g)
plot(g, vertex.size=5, vertex.label=NA)

table(degree(g)>9)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
  sort(degree(g))

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "green"

plot(g, vertex.label=NA)



V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label=NA)


s
tweets<-read.csv("./data/tweets.csv",stringsAsFactors=F)
str(tweets)



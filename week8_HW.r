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


#updated week 7


install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)


# Read in the data
tweets<-read.csv("./data/tweets.csv",stringsAsFactors=F)
str(tweets)


#load stop words
sw = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "would", "should", "could", "ought", "i'm", "you're", "he's", "she's", "it's", "we're", "they're", "i've", "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll", "you'll", "he'll", "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't", "mustn't", "let's", "that's", "who's", "what's", "here's", "there's", "when's", "where's", "why's", "how's", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very")
length(stopwords("english"))

# Create corpus

corpus = Corpus(VectorSource(tweets$Tweet))

# Look at corpus
corpus

as.character(corpus[[1]])
corpus[[1]]$content
corpus[[1]][1]

# Convert to lower-case

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]]$content
corpus[[1]][1]

# Remove punctuation

corpus = tm_map(corpus, removePunctuation)

as.character(corpus[[1]])
corpus[[1]]$content
corpus[[1]][1]

# Look at stop words
stopwords("english")[1:10]

# Remove stopwords

corpus = tm_map(corpus, removeWords, c( 'apple',stopwords("english")))

as.character(corpus[[1]])

# Stem document

#corpus = tm_map(corpus, stemDocument)

as.character(corpus[[1]])
corpus[[1]]$content
corpus[[1]][1]

# Create matrix

frequencies = DocumentTermMatrix(corpus)

frequencies

# Look at matrix

inspect(frequencies[1000:1005,505:515])


# Convert to a data frame

allTweets = as.data.frame(as.matrix(frequencies))

# Make all variable names R-friendly

colnames(allTweets) = make.names(colnames(allTweets))


#install.packages('wordcloud')
#library(wordcloud)

wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2, 0.25))

wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2, 0.25),color= c("red", "green", "blue"))
library(RColorBrewer)
display.brewer.all()

wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2, 0.25),colors=brewer.pal(9, "Blues")[c(5:9)])

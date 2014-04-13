#Load data
school<-"I:/My Data Sources/mooc/MitAnalytic"
setwd(school)
home<-getwd()
setwd(paste0(home, "/mooc/MitAnal"))
dater<-getwd()
setwd(paste0(dater, "/MitAnal"))
dir()


wiki<-read.csv("data/wiki.csv",stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)


##load libraries:
#install.packages("flexclust")
#install.packages("ggplot2")
#install.packages("maps")
#install.packages("ggmap")
#install.packages("igraph")
#install.packages("wordcloud")
#install.packages("RColorBrewer")

library(tm)
library(SnowballC)
library(flexclust)
library(ggplot2)
library(maps)
library(ggmap)
library(igraph)
library(wordcloud)
library(RColorBrewer)



str(wiki)
table(wiki$Vandal)


corpusAdded = Corpus(VectorSource(wiki$Added))

corpusAdded[[1]]


# Pre-process data
#corpusAdded <- tm_map(corpusAdded, tolower)

#corpusAdded <- tm_map(corpusAdded, removePunctuation)

corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))

corpusAdded <- tm_map(corpusAdded, stemDocument)

# Look at first 
corpusAdded[[1]]

dtmAdded<-DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded<-removeSparseTerms(dtmAdded, 0.997)

wordsAdded<-as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

###########
corpusRemoved = Corpus(VectorSource(wiki$Removed))

corpusRemoved[[1]]


# Pre-process data
#corpusRemoved <- tm_map(corpusRemoved, tolower)

#corpusRemoved <- tm_map(corpusRemoved, removePunctuation)

corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))

corpusRemoved <- tm_map(corpusRemoved, stemDocument)

# Look at first 
corpusRemoved[[3]]

dtmRemoved<-DocumentTermMatrix(corpusRemoved)
dtmRemoved

sparseRemoved<-removeSparseTerms(dtmRemoved, 0.997)

wordsRemoved<-as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)

wikiWords<-cbind(wordsRemoved,wordsAdded)
str(wikiWords)

#
wikiWords$Vandal = as.factor(wiki$Vandal)
library(caTools)

set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)

train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

table(test$Vandal)
618/(nrow(test))

618+545

library(rpart)
library(rpart.plot)
wikiCART<-rpart(Vandal~.,data=train,method="class")
prp(wikiCART)

prediction<-predict(wikiCART,newdata=test,type="class")

table(test$Vandal,prediction)

(618+12)/nrow(test)


##trying a different approach 2.1

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, spl==TRUE)

wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART2<-rpart(Vandal~.+HTTP,data=wikiTrain2,method="class")
prp(wikiCART2)

prediction2<-predict(wikiCART2,newdata=wikiTest2,type="class")

table(wikiTest2$Vandal,prediction2)

(609+57)/nrow(wikiTest2)


wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiTrain3= subset(wikiWords2, spl==TRUE)

wikiTest3 = subset(wikiWords2, spl==FALSE)

wikiCART3<-rpart(Vandal~.,data=wikiTrain3,method="class")
prp(wikiCART3)

prediction3<-predict(wikiCART3,newdata=wikiTest3,type="class")

table(wikiTest3$Vandal,prediction3)

(514+248)/nrow(wikiTest3)


####

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4= subset(wikiWords3, spl==TRUE)

wikiTest4 = subset(wikiWords3, spl==FALSE)

wikiCART4<-rpart(Vandal~.,data=wikiTrain4,method="class")
prp(wikiCART4)

prediction4<-predict(wikiCART4,newdata=wikiTest4,type="class")

table(wikiTest3$Vandal,prediction4)

(595+241)/nrow(wikiTest4)


###neww set

trials<-read.csv("data/clinical_trial.csv",stringsAsFactors=FALSE)
summary(trials)
nchar(trials[which.max(nchar(trials[,2])),2])



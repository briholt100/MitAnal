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

set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)

train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

table(test$Vandal)
618/(nrow(test))

618+545

wikiCART<-rpart(Vandal~.,data=train,method="class")
prp(wikiCART)

prediction<-predict(wikiCART,newdata=test,type="class")

table(test$Vandal,prediction)

(618+12)/nrow(test)

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
str(trials)
nchar(trials[which.max(nchar(trials[,2])),2])

table(nchar(trials[,2])<1)
trials[which.min(nchar(trials[,1])),1]


#### Create corpusTitle

corpusTitle = Corpus(VectorSource(trials$title))

corpusTitle[[1]]


# Pre-process data
corpusTitle <- tm_map(corpusTitle, tolower)

corpusTitle <- tm_map(corpusTitle, removePunctuation)

corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))

corpusTitle <- tm_map(corpusTitle, stemDocument)

# Look at first title
corpusTitle[[1]]

# Create matrix

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle

# Remove sparse terms
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle

# Create data frame
dtmTitle = as.data.frame(as.matrix(dtmTitle))###
str(dtmTitle)

#### Create corpusAbstract

corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusAbstract[[1]]


# Pre-process data
corpusAbstract <- tm_map(corpusAbstract, tolower)

corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusAbstract <- tm_map(corpusAbstract, stemDocument)

# Look at first abstract
corpusAbstract[[1]]

# Create matrix

dtmAbstract = DocumentTermMatrix(corpusAbstract)

# Remove sparse terms
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

# Create data frame
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))###
str(dtmAbstract)

which.max(apply(dtmAbstract,2,sum))
  
#the following is in preparation of merging the two df.
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm<-cbind(dtmTitle,dtmAbstract)
dtm$trial<-trials$trial
summary(dtm)


##build the model

set.seed(144)
spl = sample.split(dtm$trial, 0.7)

train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

table(train$trial)
730/(730+572)

trialCART<-rpart(trial~.,data=train,method="class")
prp(trialCART)

cartPrediction<-predict(trialCART,data=train)[,2]
head(cartPrediction)

summary(cartPrediction)

table(train$trial,cartPrediction>=.5)
(631+441)/nrow(train)

#sensitivity 
#sensitivity is TP over that row 
441/(131+441)
# specificity is TN over that row 
631/(631+99)


#predict onto test set

trialCART<-rpart(trial~.,data=train,method="class")
prp(trialCART)

predTest<-predict(trialCART,newdata=test)[,2]  #this might need a class type
head(predTest)

#FN are a problem because they won't be caught by an expert reviewer, 
#so we try to reduce them by reducing the threshold
table(test$trial,predTest>=.5)
table(test$trial)
table(predTest>=.5)
apply(table(test$trial,predTest>=.5),2,sum)

(261+162)/nrow(test)

#ROC calc
library(ROCR)
predROCR = prediction(predTest, test$trial)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values



##new problem set.

emails<-read.csv("data/emails.csv",stringsAsFactors=F)
str(emails)
summary(emails)
table(emails$spam)
head(emails$text)
nchar(emails[which.max(nchar(emails[,1])),1])
which.min(nchar(emails[,1]))


#### Create corpus

corpus = Corpus(VectorSource(emails$text))

corpus[[1]]


# Pre-process data
corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, stemDocument)

# Look at first email
corpus[[1]]

# Create matrix

dtm = DocumentTermMatrix(corpus)
dtm

# Remove sparse terms
spdtm = removeSparseTerms(dtm, 0.95)

# Create data frame
emailsSparse = as.data.frame(as.matrix(spdtm))###
str(emailsSparse)

# Make all variable names R-friendly

colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))
emailsSparse$spam<-emails$spam
head(emailsSparse)
stemSums<-colSums(emailsSparse[emailsSparse$spam == 1,])
table(stemSums>=1000)

sort(colSums(subset(emailsSparse, spam == 1)))

emailsSparse$spam = as.factor(emailsSparse$spam)

set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)
str(train)
#For each model, obtain the predicted spam probabilities for the training set. 
#Be careful to obtain probabilities instead of predicted classes, because we will be 
#using these values to compute training set AUC values. Recall that you can obtain 
#probabilities for CART models by not passing any type parameter to the predict() function, 
#and you can obtain probabilities from a random forest by adding the argument type="prob". 
#For CART and random forest, you need to select the second column of the output 
#of the predict() function, corresponding to the probability of a message being spam.

#Log
spamLog<-glm(spam~.,data=train,family="binomial")
summary(spamLog)




###training
#Predict Log
SpamPredLog<-predict(spamLog,type="response")
table(SpamPredLog<0.00001)
table(SpamPredLog>0.99999)
table(SpamPredLog<0.00001,SpamPredLog>0.99999)
table(SpamPredLog >= 0.00001 & SpamPredLog <= 0.99999)

table(train$spam,SpamPredLog>=.5)
(3052+954)/nrow(train)

#auc
library(ROCR)

predROCR = prediction(SpamPredLog, train$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#CART
spamCART<-rpart(spam~.,data=train,method="class")
prp(spamCART)

#Predict CART
SpamPredCart<-predict(spamCART,newdata=train)[,2]
head(SpamPredCart)
table(train$spam,SpamPredCart>=.5)
(2885+894)/nrow(train)


predROCR = prediction(SpamPredCart, train$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#randomForest
library(randomForest)
set.seed(123)
spamRF<-randomForest(spam~., data=train)
prp(spamRF)
plot(spamRF)

#Predict RF
SpamPredRF<-predict(spamRF,newdata=train,type="prob")[,2]
head(SpamPredCart)
table(train$spam,SpamPredRF>=.5)
(3046+958)/nrow(train)



predROCR = prediction(SpamPredRF, train$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values






###TESTING

#Predict Log
SpamPredLog<-predict(spamLog,newdata=test,type="response")
table(SpamPredLog<0.00001)
table(SpamPredLog>0.99999)
table(SpamPredLog<0.00001,SpamPredLog>0.99999)
table(SpamPredLog >= 0.00001 & SpamPredLog <= 0.99999)

table(test$spam,SpamPredLog>=.5)
(1257+376)/nrow(test)

#auc
library(ROCR)

predROCR = prediction(SpamPredLog, test$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#CART
spamCART<-rpart(spam~.,data=train,method="class")
prp(spamCART)

#Predict CART
SpamPredCart<-predict(spamCART,newdata=test)[,2]
head(SpamPredCart)
table(test$spam,SpamPredCart>=.5)
(1228+386)/nrow(test)


predROCR = prediction(SpamPredCart, test$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#randomForest
library(randomForest)
set.seed(123)
spamRF<-randomForest(spam~., data=train)
prp(spamRF)
plot(spamRF)

#Predict RF
SpamPredRF<-predict(spamRF,newdata=test,type="prob")[,2]
head(SpamPredCart)
table(test$spam,SpamPredRF>=.5)
(1290+386)/nrow(test)


predROCR = prediction(SpamPredRF, test$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#New section
#wordCount = rowSums(as.matrix(spdtm))
wordCount = rowSums(as.matrix(dtm))
hist(wordCount,breaks=100,xlim=c(0,2000))
hist(log(wordCount))

emailsSparse$logWordCount<-log(wordCount)
boxplot(logWordCount~spam, data=emailsSparse)

spl #done earlier
train2 = subset(emailsSparse, spl == TRUE)
test2 = subset(emailsSparse, spl == FALSE)

#CART
spam2CART<-rpart(spam~.,data=train2,method="class")
prp(spam2CART)

predSpam2Cart<-predict(spam2CART,newdata=test2)[,2]
table(test2$spam,predSpam2Cart>=.5)
(1214+384)/nrow(test2)

predROCR = prediction(predSpam2Cart, test2$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#randomForest
set.seed(123)
spam2RF<-randomForest(spam~.,data=train2)[,2]
SpamPred2RF<-predict(spam2RF,newdata=test2,type="prob")[,2]
table(test2$spam,SpamPred2RF>=.5)
(1296+383)/nrow(test2)

spredROCR = prediction(SpamPred2RF, test2$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values



###n-grams
install.packages("RTextTools")
library(RTextTools)
dtm2gram = create_matrix(as.character(corpus), ngramLength=2)
dtm2gram

spdtm2gram<-removeSparseTerms(dtm2gram, 0.95)


emailsSparse2gram<-as.data.frame(as.matrix(spdtm2gram))
colnames(emailsSparse2gram) = make.names(colnames(emailsSparse2gram))
emailsCombined = cbind(emailsSparse, emailsSparse2gram)

spl #done earlier
trainCombined = subset(emailsCombined, spl == TRUE)
testCombined = subset(emailsCombined, spl == FALSE)

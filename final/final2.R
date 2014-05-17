# Final part 2 for MitAnal
getwd()
nytimes<-read.csv("nytimes.csv",stringsAsFactors=FALSE)
str(nytimes)
table(nytimes$popular)
105/nrow(nytimes)

cor(nchar(nytimes$headline),nytimes$popular)

nytimes$popular<-as.factor(nytimes$popular)
nytimes$type<-as.factor(nytimes$type)

articles<-nytimes

library(caTools)
set.seed(144)
spl<-sample.split(nytimes$popular,SplitRatio = .7)
#spl
train<-subset(nytimes,spl==T)
test<-subset(nytimes,spl==F)


#log model

pop.glm1<-glm(popular~print+type+word.count,data=train,family="binomial")
summary(pop.glm1)


trial<-data.frame(1,"News",682)
colnames(trial)<-c("print","type","word.count")
trial
predict(pop.glm1,newdata=trial,type="response")
pop.glm1$coef
-2.5075573108 + (-0.8468333174) + 0.9055929357 +  0.0002599972 * 682  #=-2.27148

1/(1+exp(-(-2.27148)))
#the log response function:   1/1 +e^-(-2.27)

exp(-0.8468333)  #beta coef are the log odds for that variable.  By taking e^beta you get odds, all other variables equal


##test set predictions next

predictTest<-predict(pop.glm1,newdata=test,type="response")
table(test$popular,predictTest>.5)


library(ROCR)
ROCRpred<-prediction(predictTest,test$popular)
ROCRperf<-performance(ROCRpred, "tpr","fpr")
plot(ROCRperf,colorize =T, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7),)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)


# Define cross-validation experiment
library(rpart)
library(rpart.plot)
library(caret)
set.seed(144)
fitControl = trainControl( method = "cv", number = 10 ) #cv=cross val, 10= folds
cartGrid = expand.grid( .cp = (1:50)*0.01)  #this commands uses the increments of .01, but 50 times

# Perform the cross validation
train(popular~print+type+word.count, data=train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

#build cart model and plot it
popularTreeCV = rpart(popular~print+type+word.count, data=train, method="class", control=rpart.control(cp = 0.01))
prp(popularTreeCV)


###text analytics

library(tm)
library(SnowballC)


# Create corpus
corpus = Corpus(VectorSource(articles$snippet))  #note that I changed dataframe name

# Look at corpus
corpus

# Convert to lower-case
corpus = tm_map(corpus, tolower)

corpus[[1]]

# Remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]

# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple
corpus = tm_map(corpus, removeWords, stopwords("english"))

corpus[[1]]

# Stem document 
corpus = tm_map(corpus, stemDocument)

corpus[[1]]

# Create matrix
dtm = DocumentTermMatrix(corpus)

dtm

# Look at matrix 
inspect(dtm[100:105,505:515])

# Check for sparsity
findFreqTerms(dtm, lowfreq=20)
findFreqTerms(dtm, lowfreq=173)
findFreqTerms(dtm, highfreq=10000)


# Remove sparse terms
spdtm = removeSparseTerms(dtm, 0.95)
spdtm


# Convert to a data frame
articleText = as.data.frame(as.matrix(spdtm))

sort(colSums(articleText),decreasing=T)

# Make all variable names R-friendly
#colnames(articleText) = make.names(colnames(articleText))


head(articleText)
articleText<-cbind(articleText, articles$print, articles$type, articles$word.count, articles$popular)

new.names<-colnames(articleText)
new.names<-c(new.names,"print","type","word.count","popular")
colnames(articleText)<-new.names

#split using spl from above
trainText<-subset(articleText,spl==T)
testText<-subset(articleText,spl==F)

str(trainText)

# train basic log with all variables 

glmText<-glm(popular~.,data=trainText,family="binomial")
summary(glmText)


ROCRpred<-prediction(predict(glmText,newdata=testText,type="response"),testText$popular)
ROCRperf<-performance(ROCRpred, "tpr","fpr")
plot(ROCRperf,colorize =T, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7),)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)


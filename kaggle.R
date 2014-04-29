##Must change dataframe into integers for K-means and Hclust

###########################
#   Note about file names
#   Kaggle_test & Kaggle_train are the original data from competition
#   trainSource and testSource have had their data imputed down in this code..  
###########################



getwd()
setwd("./mooc/MitAnalytic")
setwd("./MitAnal")  #dater
dir()
#trainSource<-read.csv("./data/Kaggle_train.csv",na.strings="",stringsAsFactors=T)


##################################################
trainSource<-read.csv("./data/trainSource.csv",stringsAsFactors=T)
##################################################
str(trainSource)
s################################################################################################
################################################################################################
testSource<-read.csv("./data/Kaggle_test.csv",na.strings="",stringsAsFactors=T)
################################################################################################
################################################################################################


library(caTools)
set.seed(1000)
split<-sample.split(trainSource$Happy,SplitRatio = .7)
train<-subset(trainSource,split==T)
test<-subset(trainSource,split==F)
summary(train)
str(train)
str(test)
summary(test)









#create new variable, which is a sum of NA's

sumNA<-rep(0,nrow(trainSource))
for (i in 1:nrow(trainSource)){
  for(j in 1:ncol(trainSource)){
    if(is.na(trainSource[i,j]) ==T) {
      sumNA[i]<-sumNA[i]+1
    }      
  }
}
#sumNA
trainSource$sumNA<-sumNA
rm(sumNA)


#create new variable, a ratio of $vote to # of Questions  VQ_ratio
###1 varialbe has no NA's.  So, 109 questions.  Ratio will be $votes/109
sort(trainSource$votes/109)
summary(trainSource[1:9])  #7 variables not like the others
trainSource$VQ_ratio<-trainSource$votes/109
which(trainSource$VQ_ratio<=.19)
boxplot(log(trainSource$VQ_ratio+1))
##convert YOB to int

install.packages("lubridate")
library(lubridate)
trainSource$YOB<-as.Date(as.character(trainSource$YOB),format="%Y")
trainSource$YOB<-year(trainSource$YOB)
table((trainSource$YOB))
old<-trainSource$YOB<1936
trainSource[old,1:3]

#####################################big issue with outliers here

##reorder training Income
#table(train$Income)

trainSource$Income<-relevel(trainSource$Income,ref="over $150,000")
trainSource$Income<-relevel(trainSource$Income,ref="$100,001 - $150,000")
trainSource$Income<-relevel(trainSource$Income,ref="$75,000 - $100,000")
trainSource$Income<-relevel(trainSource$Income,ref="$50,000 - $74,999")
trainSource$Income<-relevel(trainSource$Income,ref="$25,001 - $50,000")
trainSource$Income<-relevel(trainSource$Income,ref="under $25,000")

levels(trainSource$EducationLevel)
trainSource$EducationLevel<-relevel(trainSource$EducationLevel,ref="Current K-12")

trainSource$HouseholdStatus<-relevel(trainSource$HouseholdStatus,ref="Single (no kids)")


install.packages("mice")
library(mice)

# Multiple imputation
#set.seed(144)
summary(trainSource)
imputed = complete(mice(trainSource[,2:109]))
summary(imputed)
trainSource[,2:109] = imputed
summary(trainSource)
write.csv(trainSource, "trainSource.csv", row.names=FALSE) 



#baesline train predcition of happy variable
table(train$Happy)
1823/nrow(train)  #  0.563
table(test$Happy)
781/nrow(test)  #  0.563

table(trainSource$Happy)
2604/nrow(trainSource)
#sumObj<-(summary(train[]))
#str(sumObj)
#sumObj[1,24]  #I believe generally at the end of every summary column, if there are NA's, they will be there.  



hist(as.numeric(train$Income),xlab=levels(train$income))
#happy.by.incomeLog<-glm(Happy~Income,data=train,family=binomial)
#summary(happy.by.incomeLog)
boxplot(as.numeric(train$Income)~as.numeric(train$Happy))
#be sure to evaluate the data for outliers.


#answers <- read.csv("train.csv", stringsAsFactors=FALSE)
#answers[answers==''] <- "IGNORED"



##OUTLIERS
par(mfrow = c(4,4))
for(i in 1:length(train)){plot(train[,i],xlab=colnames(train[i]))}
par(mfrow = c(1,1))
plot(train[,111])
hist(train[,111])
plot(train$sumNA,col=train$Gender)

plot((train$Income))


boxplot(train$sumNA~train$Happy)
tapply(train$sumNA,as.factor(train$Happy),mean)
boxplot(train$Happy~train[,111])

boxplot(train[,110])

cov(train,use="complete.obs")


table(train$Happy,train$sumNA>=45)



##all variables
Happy.all.Log<-glm(Happy~. - UserID,data=train,family="binomial")
summary(Happy.all.Log)
objects(Happy.all.Log)

ss <- coef(summary(Happy.all.Log))
head(ss)
#Take only the rows you want:
  ss_sig <- ss[ss[,"Pr(>|z|)"]<0.1,]
sigNames<-rownames(ss_sig)


Happy.all.CART<-rpart(Happy~. - UserID,data=train)
prp(Happy.all.CART)

Happy.All.CART.Predict<-predict(Happy.all.CART,newdata=test)
table(test$Happy,Happy.All.CART.Predict>=.5)
(228+704)/(nrow(test))


library(randomForest)
Happy.all.RF<-randomForest(Happy~. - UserID,data=train)
plot(Happy.all.RF)

Happy.All.RF.Predict<-predict(Happy.all.RF,newdata=test)
table(test$Happy,Happy.All.RF.Predict>=.5)
(379+606)/(nrow(test))



HappyLog.mod1<-glm(Happy~YOB+HouseholdStatus+EducationLevel+Q124122+Q120194+Q119334+Q118237+Q116953+Q116441
+Q116197+Q115602+Q115899+Q115390+Q114961+Q114517+Q113584+Q111848+Q108342+Q107869+Q102674+Q102289+Q101162+Q101596+Q100680
+Q98197,data=train,family=binomial)

summary(HappyLog.mod1)

library(rpart)
library(rpart.plot)
HappyCART.mod1<-rpart(Happy~YOB+HouseholdStatus+EducationLevel+Q124122+Q120194+Q119334+Q118237+Q116953+Q116441
      +Q116197+Q115602+Q115899+Q115390+Q114961+Q114517+Q113584+Q111848+Q108342+Q107869+Q102674+Q102289+Q101162+Q101596+Q100680
      +Q98197,data=train)
prp(HappyCART.mod1)

HappyLog.mod1_predictions<-predict(HappyLog.mod1,newdata=test,type="response")

HappyLog.CART1_predictions<-predict(HappyCART.mod1,newdata=testSource)
output<-table(test$Happy,HappyLog.CART1_predictions >= .5)
(228+704)/(nrow(test))


submission4 = data.frame(UserID = testSource$UserID, Probability1 = HappyLog.CART1_predictions)  #CART Model1
write.csv(submission4, "submission4.csv", row.names=FALSE) 



for(i in 1:length(HappyLog.mod1_predictions)){
  if(is.na(HappyLog.mod1_predictions[i])==T){
    HappyLog.mod1_predictions[i]<-sample(1:1000, 1)/1000
  }
}


submission2 = data.frame(UserID = test$UserID, Probability1 = HappyLog.mod1_predictions)
write.csv(submission2, "submission2.csv", row.names=FALSE) 



HappyLog.mod2<-glm(Happy~YOB
                   +Income
                   +HouseholdStatus
                   +EducationLevel
                   +Q124122
                   +Q121699
                   +Q120472
                   +Q120194
                   +Q119334
                   +Q118237
                   +Q116441
                   +Q116197
                   +Q115777
                   +Q115390
                   +Q114961
                   +Q115195
                   +Q114517
                   +Q114386
                   +Q111848
                   +Q108856
                   +Q107869
                   +Q105840
                   +Q102089
                   +Q101162
                   +Q101596
                   +Q100680
                   +Q99716
                   +Q99581
                   +Q98869
                   +Q98197,data=train,family=binomial)

summary(HappyLog.mod2)
objects(HappyLog.mod2)


###TRY STEP
HappyLogMod2Step<-step(HappyLog.mod2)
#Happy ~ YOB + HouseholdStatus + Q120194 + Q119334 + Q118237 + 
#Q116441 + Q116197 + Q115390 + Q114961 + Q114386 + Q111848 + 
 # Q108856 + Q107869 + Q102089 + Q101162 + Q100680 + Q99581 + 
  #Q98869
#

sumNA<-rep(0,nrow(testSource))
for (i in 1:nrow(testSource)){
  for(j in 1:ncol(testSource)){
    if(is.na(testSource[i,j]) ==T) {
      sumNA[i]<-sumNA[i]+1
    }      
  }
}
#sumNA
testSource$sumNA<-sumNA
rm(sumNA)


#create new variable, a ratio of $vote to # of Questions  VQ_ratio
###1 varialbe has no NA's.  So, 109 questions.  Ratio will be $votes/109
sort(testSource$votes/109)
summary(testSource[1:9])  #7 variables not like the others
testSource$VQ_ratio<-testSource$votes/109
which(testSource$VQ_ratio<=.19)
boxplot(log(testSource$VQ_ratio+1))
##convert YOB to int

install.packages("lubridate")
library(lubridate)
testSource$YOB<-as.Date(as.character(testSource$YOB),format="%Y")
testSource$YOB<-year(testSource$YOB)
table((testSource$YOB))
old<-testSource$YOB<1936
testSource[old,1:3]

#####################################big issue with outliers here

##reorder training Income
#table(train$Income)

testSource$Income<-relevel(testSource$Income,ref="over $150,000")
testSource$Income<-relevel(testSource$Income,ref="$100,001 - $150,000")
testSource$Income<-relevel(testSource$Income,ref="$75,000 - $100,000")
testSource$Income<-relevel(testSource$Income,ref="$50,000 - $74,999")
testSource$Income<-relevel(testSource$Income,ref="$25,001 - $50,000")
testSource$Income<-relevel(testSource$Income,ref="under $25,000")

levels(testSource$EducationLevel)
testSource$EducationLevel<-relevel(testSource$EducationLevel,ref="Current K-12")

testSource$HouseholdStatus<-relevel(testSource$HouseholdStatus,ref="Single (no kids)")



summary(testSource)
imputed = complete(mice(testSource[,2:109]))
summary(imputed)
testSource[,2:109] = imputed
summary(testSource)
write.csv(testSource, "testSource.csv", row.names=FALSE) 

testSource<-read.csv("./data/testSource.csv",stringsAsFactors=T)
trainSource<-read.csv("./data/trainSource.csv",stringsAsFactors=T)

HappyLog.mod2_predictions<-predict.glm(HappyLog.mod2,newdata=testSource,type="response")

output<-table(test$Happy,HappyLog.mod2_predictions >= .5)
(387+609)/(nrow(test))

submission3 = data.frame(UserID = testSource$UserID, Probability1 = HappyLog.mod2_predictions)
write.csv(submission3, "submission3.csv", row.names=FALSE) 




library(ROCR)
ROCRpredict<-prediction(votlog_predictions,gerber$voting)
ROCRperf<-performance(ROCRpredict, "tpr","fpr")
auc = as.numeric(performance(ROCRpredict, "auc")@y.values)







# Run k-means.  First load trainSource.csv which has imputed and made variables.  Split them using caTools into
# train and test.  This code is up top.

#convert train to integers or numeric? ########works as either numeric or integer
#to convert factors into integers, you must first turn the characters.  No idea why.  
###
#Exclude UserID[,1], YOB[,2], Happy[,8],votes[,110], sumNA[,111],VQ_ratio[,112]
####

#as.numeric.factor <- function(x) {(as.numeric(levels(x)))[x]}
#as.numeric(levels(train[,9]))[train[,9]]



head(train[,c(1:9,110:112)])
trainMatrix<-data.matrix(train[,c(3:7,9:109)]) ###excludes Devependent variable and userID
testMatrix<-data.matrix(test[,c(3:7,9:109)]) ###excludes Devependent variable and userID
head(trainMatrix)
str(trainMatrix)
#trainMatrix$Gender<-as.numeric(levels(trainMatrix$Gender))[as.integer(trainMatrix$Gender)]
#trainMatrix<-data.matrix(as.numeric(levels(trainMatrix)))  #######this isn't working. NA's introduced by coersion

# Compute distances
distance = dist(trainMatrix, method = "euclidean")

# Turn matrix into a vector
#kosVector = as.vector(kosMatrix[,-1])
#str(kosVector)

#kosVector2 = as.vector(kos)
#str(kosVector2)

# Compute distances
#distance = dist(kosMatrix[,-1], method = "euclidean")

# Hierarchical clustering
clusterIntensity = hclust(distance, method="ward.D")
plot(clusterIntensity)

k=6
rect.hclust(clusterIntensity, k , border = "red")

happyClusters = cutree(clusterIntensity, k)
head(happyClusters)
length(happyClusters)
nrow(trainMatrix)
tapply(train$Happy,happyClusters,mean)
table(happyClusters)

####
###Make cluster subsets in both train and test
happyClust1<-subset(train,happyClusters == 1)
happyClust2<-subset(train,happyClusters == 2)
happyClust3<-subset(train,happyClusters == 3)
happyClust4<-subset(train,happyClusters == 4)
happyClust5<-subset(train,happyClusters == 5)
happyClust6<-subset(train,happyClusters == 6)

happyClust1.test<-subset(test,happyClusters == 1)
happyClust2.test<-subset(test,happyClusters == 2)
happyClust3.test<-subset(test,happyClusters == 3)
happyClust4.test<-subset(test,happyClusters == 4)
happyClust5.test<-subset(test,happyClusters == 5)
happyClust6.test<-subset(test,happyClusters == 6)



###Model using clustered training set.
Happy.hclust.log1<-glm(Happy~Q118237+Q101162,data=happyClust1,family=binomial)
Happy.hclust.log2<-glm(Happy~Q118237+Q101162,data=happyClust2,family=binomial)
Happy.hclust.log3<-glm(Happy~Q118237+Q101162,data=happyClust3,family=binomial)
Happy.hclust.log4<-glm(Happy~Q118237+Q101162,data=happyClust4,family=binomial)
Happy.hclust.log5<-glm(Happy~Q118237+Q101162,data=happyClust5,family=binomial)
Happy.hclust.log6<-glm(Happy~Q118237+Q101162,data=happyClust6,family=binomial)

####
#predictions  ###this doesn't work..wrong numbers of data points leads to NA's
Pred.Happy.hclust.log1<-predict(Happy.hclust.log1,newdata=happyClust1.test)
Pred.Happy.hclust.log2<-predict(Happy.hclust.log2,newdata=happyClust2.test)
Pred.Happy.hclust.log3<-predict(Happy.hclust.log3,newdata=happyClust3.test)
Pred.Happy.hclust.log4<-predict(Happy.hclust.log4,newdata=happyClust4.test)
Pred.Happy.hclust.log5<-predict(Happy.hclust.log5,newdata=happyClust5.test)
Pred.Happy.hclust.log6<-predict(Happy.hclust.log6,newdata=happyClust6.test)

k=6
set.seed(1000)
KMC = kmeans(trainMatrix, centers = k, iter.max = 1000)
str(KMC)

# Extract clusters
trainKclusters = KMC$cluster
KMC$centers[6]

train.by.Clust = split(trainMatrix,trainKclusters)

for (i in 1:k){print(mean(train.by.Clust[[i]]))}  #gives count of each cluster


for (i in 1:k){  #will show each clusters largest word's, sorted with largest at the tail
  print (i)
  print(tail(sort(colMeans(train.by.Clust[[i]]))))
}




# Extract clusters

train.norm.Kclusters = KM$cluster  #associated cluster with observations.  length should equal data nrow
KM$centers  #shows k-means clusters with varialbes
lapply(split(train, KM$cluster), colMeans) #shows the cluster averages by non-normed data
mean(train.norm$age)


##3.5
library(flexclust)

km.kcca = as.kcca(KM, train.norm)

cluster.train = predict(km.kcca)
cluster.test = predict(km.kcca, newdata=test.norm)
table(cluster.test)

train1<-subset(train,cluster.train==1)
train2<-subset(train,cluster.train==2)
train3<-subset(train,cluster.train==3)

mean(train1$reimbursement2009)
mean(train2$reimbursement2009)
mean(train3$reimbursement2009)

trainClust.split = split(train, cluster.train)   ##cluster 1 can be accessed HierCluster[[1]], cluster 2 HierCluster[[2]]
trainClust.split[[1]]

test1<-subset(test,cluster.test==1)
test2<-subset(test,cluster.test==2)
test3<-subset(test,cluster.test==3)


lm1<-lm(reimbursement2009~.,train1)
lm2<-lm(reimbursement2009~.,train2)
lm3<-lm(reimbursement2009~.,train3)

lm1$coef
lm2$coef
lm3$coef

pred.test1<-predict(lm1,newdata=test1)
pred.test2<-predict(lm2,newdata=test2)
pred.test3<-predict(lm3,newdata=test3)

mean(pred.test1)
mean(pred.test2)
mean(pred.test3)
SSE<-sum((pred.test1-test1$reimbursement2009)^2)


all.predictions = c(pred.test1, pred.test2, pred.test3)
all.outcomes = c(test1$reimbursement2009, test2$reimbursement2009, test3$reimbursement2009)











#http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/#more-61075

library(MASS)
data(crabs)
data(crabsgp)
str(crabs)
brks<-quantile(crabs$BD,probs=seq(0,1,.2))
range(crabs$BD)
nrow(crabs)
table(cut(crabs$BD, breaks<-c(0,11,14,15,18,22), dig.lab=4))

grp<-(cut(crabs$BD, breaks<-c(0,11,14,15,18,22), dig.lab=4))
cases<-table(grp)
avgBD<-tapply(crabs$BD,grp,mean)

#http://web.stat.ufl.edu/~presnell/Courses/sta4504-2000sp/R/R-CDA.pdf
#see pg 19, 20 for "wegith" "offset" "cases"



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
#testSource<-read.csv("./data/Kaggle_test.csv",na.strings="",stringsAsFactors=T)
trainKaggle<-read.csv("./data/Kaggle_train.csv",na.strings="",stringsAsFactors=T)
testKaggle<-read.csv("./data/Kaggle_test.csv",na.strings="",stringsAsFactors=T)
testKaggle$testType<-rep("test",nrow(testKaggle))
trainKaggle$testType<-rep("train",nrow(trainKaggle))

all.data<-rbind(trainKaggle[,-8],testKaggle)


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
sort(trainSource$votes/109)
summary(trainSource[1:9])  #7 variables not like the others
trainSource$VQ_ratio<-trainSource$votes/109
which(trainSource$VQ_ratio<=.19)
boxplot(log(trainSource$VQ_ratio+1))
##convert YOB to int
testSource$VQ_ratio<-testSource$votes/109




#install.packages("lubridate")
library(lubridate)
trainSource$YOB<-as.Date(as.character(trainSource$YOB),format="%Y")
trainSource$YOB<-year(trainSource$YOB)
testSource$YOB<-as.Date(as.character(testSource$YOB),format="%Y")
testSource$YOB<-year(testSource$YOB)
table((trainSource$YOB))
table((testSource$YOB))

all.data$YOB<-as.Date(as.character(all.data$YOB),format="%Y")
all.data$YOB<-year(all.data$YOB)
table((all.data$YOB))


for(i in 1:length(trainSource$YOB)){
  if (!is.na(trainSource$YOB[i])){
    if(trainSource$YOB[i] < 1931 | trainSource$YOB[i]>2001) {
      trainSource$YOB[i]<-NA
      print(trainSource$YOB[i])
    }
  }
}

for(i in 1:length(testSource$YOB)){
  if (!is.na(testSource$YOB[i])){
    if(testSource$YOB[i] < 1931 | testSource$YOB[i]>2001) {
      testSource$YOB[i]<-NA
      print(testSource$YOB[i])
    }
  }
}


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

#######for testSource
testSource$Income<-relevel(testSource$Income,ref="over $150,000")
testSource$Income<-relevel(testSource$Income,ref="$100,001 - $150,000")
testSource$Income<-relevel(testSource$Income,ref="$75,000 - $100,000")
testSource$Income<-relevel(testSource$Income,ref="$50,000 - $74,999")
testSource$Income<-relevel(testSource$Income,ref="$25,001 - $50,000")
testSource$Income<-relevel(testSource$Income,ref="under $25,000")

#######for all.data
all.data$Income<-relevel(all.data$Income,ref="over $150,000")
all.data$Income<-relevel(all.data$Income,ref="$100,001 - $150,000")
all.data$Income<-relevel(all.data$Income,ref="$75,000 - $100,000")
all.data$Income<-relevel(all.data$Income,ref="$50,000 - $74,999")
all.data$Income<-relevel(all.data$Income,ref="$25,001 - $50,000")
all.data$Income<-relevel(all.data$Income,ref="under $25,000")

levels(all.data$EducationLevel)
all.data$EducationLevel<-relevel(all.data$EducationLevel,ref="Current K-12")
all.data$HouseholdStatus<-relevel(all.data$HouseholdStatus,ref="Single (no kids)")



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

#for testSource
imputed = complete(mice(testSource[,2:109]))
summary(imputed)
testSource[,2:109] = imputed
summary(testSource)
write.csv(testSource, "testSource.csv", row.names=FALSE) 


##################################################
trainSource<-read.csv("./data/trainSource.csv",stringsAsFactors=T)
##################################################
dim(trainSource)
################################################################################################
################################################################################################
testSource<-read.csv("./data/testSource.csv",na.strings="",stringsAsFactors=T)
################################################################################################
################################################################################################
dim(testSource)


library(caTools)
#set.seed(1)
split<-sample.split(trainSource$Happy,SplitRatio = .7)
train<-subset(trainSource,split==T)
test<-subset(trainSource,split==F)
summary(train)
str(train)
str(test)
summary(test)

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

table(trainSource$YOB,trainSource$Income)


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
  ss_sig <- ss[ss[,"Pr(>|z|)"]<0.05,]
sigNames<-rownames(ss_sig)
#sigNames<-sub("2|4|5","",sigNames)
#sigNames<-sigNames[-c(2:3)]
sigNames<-sub("Q","+Q",sigNames)
cat(sigNames)

train.glm1<-glm(Happy~Income+ HouseholdStatus+ EducationLevel +Q122120 +Q121011 +Q120014 +Q119334 +Q118237 +Q116797 +Q116881 
                +Q116441 +Q114386 +Q113181 +Q108855 +Q108856 +Q108754 +Q108342 +Q108343 +Q107869 +Q102906 +Q102289 +Q101162 
                +Q100562 +Q99581 +Q98869 +Q98197,data=train,family=binomial)
summary(train.glm1)

HappyLog.train.glm1.predictions<-predict(train.glm1,newdata=test,type="response")
table(test$Happy,HappyLog.train.glm1.predictions>=.5)
(360+605)/nrow(test)

testSourceHappyLog.glm1<-predict(train.glm1,newdata=testSource,type="response")
submission6 = data.frame(UserID = testSource$UserID, Probability1 = testSourceHappyLog.glm1)  
write.csv(submission6, "submission6.csv", row.names=FALSE) 

train.glm1.step<-step(train.glm1)
summary(train.glm1.step)


Happy.all.CART<-rpart(Happy~. - UserID,data=train)
prp(Happy.all.CART)

Happy.All.CART.Predict<-predict(Happy.all.CART,newdata=test)
table(test$Happy,Happy.All.CART.Predict>=.5)
(228+704)/(nrow(test))

Happy.svd.CART<-rpart(Happy~Q108754+
                        YOB+Q108342+votes+
                        sumNA+Q108950+Q106993+
                        Q109367+Q117186+Q108856,data=train)

prp(Happy.svd.CART)
Happy.svd.CART.Predict<-predict(Happy.svd.CART,newdata=test)


table(test$Happy,Happy.svd.CART.Predict>=.5)
(218+570)/(nrow(test)) #.568



library(randomForest)
Happy.all.RF<-randomForest(Happy~. - UserID,data=train)
plot(Happy.all.RF)

Happy.All.RF.Predict<-predict(Happy.all.RF,newdata=test)
table(test$Happy,Happy.All.RF.Predict>=.5)
(379+606)/(nrow(test)) #.71
(378+603)/(nrow(test)) #.707


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



HappyCART.mod2<-rpart(Happy~Income+ HouseholdStatus+ EducationLevel +Q122120 +Q121011 +Q120014 +Q119334 +Q118237 +Q116797 +Q116881 
                      +Q116441 +Q114386 +Q113181 +Q108855 +Q108856 +Q108754 +Q108342 +Q108343 +Q107869 +Q102906 +Q102289 +Q101162 
                      +Q100562 +Q99581 +Q98869 +Q98197,data=trainSource)
prp(HappyCART.mod2)

HappyLog.CART2_predictions<-predict(HappyCART.mod2,newdata=testSource)
output<-table(test$Happy,HappyLog.CART2_predictions >= .5)
(218+707)/(nrow(test))

submission7 = data.frame(UserID = testSource$UserID, Probability1 = HappyLog.CART2_predictions)  #CART Model1
write.csv(submission7, "submission7.csv", row.names=FALSE) 


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
trainMatrix<-data.matrix(train[,c(2:7,9:112)]) ###excludes Devependent variable and userID
testMatrix<-data.matrix(test[,c(2:7,9:112)]) ###excludes Devependent variable and userID
trainSourceMatrix<-data.matrix(trainSource[,c(2:7,9:112)]) ###excludes Devependent variable and userID
testSourceMatrix<-data.matrix(testSource[,c(2:111)]) ###excludes Devependent variable and userID

####The following is an attempt to make sure testSourceMatrix has same # of variables as train
####IF they are equal, then there will be an error.  If the colnames are off, you'll see the first problem.
for (i in 1:ncol(trainMatrix)){
  if(colnames(trainMatrix[i])==colnames(testSourceMatrix[i])){print(T)} else{print(colnames(trainMatrix[i]))}
}

dim(trainMatrix)
dim(testMatrix)
dim(testSourceMatrix)
summary(testSourceMatrix)


# Compute distances
distance = dist(trainMatrix, method = "euclidean")
heatmap(trainMatrix)
plot(rowMeans(trainMatrix),,xlab="Row",ylab="Row Mean",pch=19)
plot(colMeans(trainMatrix),xlab="Column",ylab="Column Mean",pch=19)


#############Principal comp
pc<-prcomp(trainSourceMatrix)
objects(pc)
dim(pc$x)
plot(pc)
biplot(pc)
summary(pc)

Happy.PC.Log1<-glm(Happy~pc$x[,1]+pc$x[,2],data=trainSource,family=binomial)
summary(Happy.PC.Log1)

Happy.PC.Log1.pred<-predict(Happy.PC.Log1,newdata=testSource,type="response")
Happy.PC.Log1.pred<-Happy.PC.Log1.pred[1:1980]
table(test$Happy,Happy.PC.Log1.pred>=.5)

submission11 = data.frame(UserID = testSource$UserID, Probability1 =Happy.PC.Log1.pred)  #Model using trainSource,Princ comp, glm
write.csv(submission11, "submission11.csv", row.names=FALSE) 



prc<-princomp(trainMatrix)
objects(prc)
prc$scores[,1]

Happy.PrC.Log2<-glm(Happy~prc$scores[,1]+prc$scores[,2],data=train,family=binomial)
summary(Happy.PrC.Log2)
Happy.PrC.Log2.pred<-predict(Happy.PrC.Log2,newdata=test,type="response")
table(test$Happy,Happy.PC.Log1.pred[1:1386][>=.5])








svd1 <- svd(scale(trainMatrix))
which.max(svd1$v[,1])
colnames(train[4])
which.max(svd1$v[,10])
colnames(train[99])

plot(svd1$d,xlab="Column",ylab="Singluar value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)
plot(svd1$v[,1],pch=19,xlab="Column",ylab="First right singluar vector")
plot(svd1$v[,2],pch=19,xlab="Column",ylab="Second right singluar vector")
plot(svd1$v[,3],pch=19,xlab="Column",ylab="third right singluar vector")
plot(svd1$v[,4],pch=19,xlab="Column",ylab="forth right singluar vector")
plot(svd1$v[,5],pch=19,xlab="Column",ylab="fift right singluar vector")
plot(svd1$v[,6],pch=19,xlab="Column",ylab="sixth right singluar vector")
plot(svd1$v[,7],pch=19,xlab="Column",ylab="seventh right singluar vector")
plot(svd1$v[,8],pch=19,xlab="Column",ylab="eighth right singluar vector")
plot(svd1$v[,9],pch=19,xlab="Column",ylab="ninth right singluar vector")
plot(svd1$v[,10],pch=19,xlab="Column",ylab="tenth right singluar vector")
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10])%*% t(svd1$v[,1:10])
heatmap(approx10)


svd.glm1<-glm(Happy~Q99982+Q110740+Gender+Q104996+Q98059+Q100689
              +Q96024+YOB+Q107491+Income,data=trainSource,family=binomial)
summary(svd.glm1)
svd.glm1.pred<-predict(svd.glm1,newdata=test,type="response")
table(test$Happy,svd.glm1.pred>=.5)
(168+650)/nrow(test)

svdCart.mod1<-rpart(Happy~Q99982+Q110740+Gender+Q104996+Q98059+Q100689
                    +Q96024+YOB+Q107491+Income,data=trainSource)
prp(svdCart.mod1)

# Turn matrix into a vector
#kosVector = as.vector(kosMatrix[,-1])
#str(kosVector)

#kosVector2 = as.vector(kos)
#str(kosVector2)

# Compute distances
#distance = dist(kosMatrix[,-1], method = "euclidean")

# Hierarchical clustering
distanceSource = dist(trainSourceMatrix, method = "euclidean")
distanceTrain= dist(trainMatrix, method = "euclidean")

clusterIntensity = hclust(distanceTrain, method="ward.D")
plot(clusterIntensity)
objects(clusterIntensity)
k=2
rect.hclust(clusterIntensity, k , border = "blue")

happyClusters = cutree(clusterIntensity, k)
head(happyClusters)
length(happyClusters)
nrow(trainMatrix)
avgwt<-tapply(train$Happy,happyClusters,mean)
table(happyClusters)
happyClusters<-as.factor(happyClusters)

happyClustWeight<-0
for (i in 1:nrow(train)){
  if (happyClusters[i]==1){
    happyClustWeight[i]<-0.5779967
  }
  else {
    if (happyClusters[i]==2){
      happyClustWeight[i]<-0.5568783
    }
  else {
    if (happyClusters[i]==3){
      happyClustWeight[i]<-0.5690299
    }
  else {
    if (happyClusters[i]==4){
      happyClustWeight[i]<-0.5527638
    }
  }
}
}
}
class(happyClustWeight)
trainTrial<-cbind(train,happyClusters,happyClustWeight)
head(trainTrial)

L1<-glm(Happy~ happyClustWeight-UserID,data=trainTrial,family=binomial)
summary(L1)
L2<-glm(Happy~.-UserID,data=trainTrial,family=binomial,subset=happyClusters==1)
summary(L2)

####
###Make cluster subsets in both train and test
happyClust1<-subset(train,happyClusters == 1)
happyClust2<-subset(train,happyClusters == 2)
happyClust3<-subset(train,happyClusters == 3)
happyClust4<-subset(train,happyClusters == 4)
happyClust5<-subset(train,happyClusters == 5)
happyClust6<-subset(train,happyClusters == 6)
happyClust7<-subset(train,happyClusters == 7)
happyClust8<-subset(train,happyClusters == 8)

happyClust1.test<-subset(testSource,happyClusters == 1)
happyClust2.test<-subset(testSource,happyClusters == 2)
happyClust3.test<-subset(testSource,happyClusters == 3)
happyClust4.test<-subset(testSource,happyClusters == 4)
happyClust5.test<-subset(testSource,happyClusters == 5)
happyClust6.test<-subset(testSource,happyClusters == 6)
happyClust7.test<-subset(testSource,happyClusters == 7)
happyClust8.test<-subset(testSource,happyClusters == 8)
nrow(happyClust7.test)

###Model using clustered training set.
Happy.hclust.log1<-glm(Happy~Q118237+Q101162,data=happyClust1,family=binomial)
Happy.hclust.log2<-glm(Happy~Q118237+Q101162,data=happyClust2,family=binomial)
Happy.hclust.log3<-glm(Happy~Q118237+Q101162,data=happyClust3,family=binomial)
Happy.hclust.log4<-glm(Happy~Q118237+Q101162,data=happyClust4,family=binomial)
Happy.hclust.log5<-glm(Happy~Q118237+Q101162,data=happyClust5,family=binomial)
Happy.hclust.log6<-glm(Happy~Q118237+Q101162,data=happyClust6,family=binomial)
Happy.hclust.log7<-glm(Happy~Q118237+Q101162,data=happyClust7,family=binomial)
Happy.hclust.log8<-glm(Happy~Q118237+Q101162,data=happyClust8,family=binomial)
####

Pred.Happy.hclust.log1<-predict(Happy.hclust.log1,newdata=happyClust1.test,type="response")
Pred.Happy.hclust.log2<-predict(Happy.hclust.log2,newdata=happyClust2.test,type="response")
Pred.Happy.hclust.log3<-predict(Happy.hclust.log3,newdata=happyClust3.test,type="response")
Pred.Happy.hclust.log4<-predict(Happy.hclust.log4,newdata=happyClust4.test,type="response")
Pred.Happy.hclust.log5<-predict(Happy.hclust.log5,newdata=happyClust5.test,type="response")
Pred.Happy.hclust.log6<-predict(Happy.hclust.log6,newdata=happyClust6.test,type="response")
Pred.Happy.hclust.log7<-predict(Happy.hclust.log7,newdata=happyClust7.test,type="response")

all.predictions = c(Pred.Happy.hclust.log1, Pred.Happy.hclust.log2, Pred.Happy.hclust.log3,Pred.Happy.hclust.log4,
                    Pred.Happy.hclust.log5,Pred.Happy.hclust.log6,Pred.Happy.hclust.log7)
length(all.predictions)



k=4
set.seed(1000)
KMC = kmeans(trainMatrix, centers = k, iter.max = 1000)
str(KMC)

# Extract clusters
trainKclusters = KMC$cluster
KMC$centers

train.by.Clust = split(trainMatrix,trainKclusters)

for (i in 1:k){print(sum(train.by.Clust[[i]]))}  #gives count of each cluster


for (i in 1:k){  #will show each clusters largest word's, sorted with largest at the tail
  print (i)
  print(tail(sort(colMeans(train.by.Clust[[i]]))))
}




# Extract clusters

train.norm.Kclusters = KMC$cluster  #associated cluster with observations.  length should equal data nrow
KMC$centers  #shows k-means clusters with varialbes
lapply(split(train, KMC$cluster), colMeans) #shows the cluster averages by non-normed data
mean(train.norm$Happy)


##3.5
library(flexclust)

dim(trainMatrix) = c(nrow(trainMatrix), ncol(trainMatrix))
dim(testMatrix) = c(nrow(testMatrix), ncol(testMatrix))
dim(testSourceMatrix) = c(nrow(testSourceMatrix), ncol(testSourceMatrix))


km.kcca = as.kcca(KMC, trainMatrix)

cluster.train = predict(km.kcca)
cluster.test = predict(km.kcca, newdata=testMatrix)
table(cluster.train)
table(cluster.test)

################
train1<-subset(train,cluster.train==1)
train2<-subset(train,cluster.train==2)
train3<-subset(train,cluster.train==3)
train4<-subset(train,cluster.train==4)
#train5<-subset(train,cluster.train==5)
#train6<-subset(train,cluster.train==6)

mean(train1$Happy)
mean(train2$Happy)
mean(train3$Happy)
mean(train4$Happy)


trainClust.split = split(train, cluster.train)   ##cluster 1 can be accessed HierCluster[[1]], cluster 2 HierCluster[[2]]
trainClust.split[[1]]

trainSource1<-subset(trainSource,cluster.train==1)
trainSource2<-subset(trainSource,cluster.train==2)
trainSource3<-subset(trainSource,cluster.train==3)
trainSource4<-subset(trainSource,cluster.train==4)



test1<-subset(test,cluster.test==1)
test2<-subset(test,cluster.test==2)
test3<-subset(test,cluster.test==3)
test4<-subset(test,cluster.test==4)

testSource1<-subset(testSource,cluster.test==1)
testSource2<-subset(testSource,cluster.test==2)
testSource3<-subset(testSource,cluster.test==3)
testSource4<-subset(testSource,cluster.test==4)

lm1<-glm(Happy~Income+ HouseholdStatus+ EducationLevel +Q122120 +Q121011 +Q120014 +Q119334 +Q118237 +Q116797 +Q116881                        +Q116441 +Q114386 +Q113181 +Q108855 +Q108856 +Q108754 +Q108342 +Q108343 +Q107869 +Q102906 +Q102289 +Q101162+Q100562 +Q99581 +Q98869 +Q98197,trainSource1,family="binomial")
lm2<-glm(Happy~Income+ HouseholdStatus+ EducationLevel +Q122120 +Q121011 +Q120014 +Q119334 +Q118237 +Q116797 +Q116881                        +Q116441 +Q114386 +Q113181 +Q108855 +Q108856 +Q108754 +Q108342 +Q108343 +Q107869 +Q102906 +Q102289 +Q101162+Q100562 +Q99581 +Q98869 +Q98197,trainSource2,family="binomial")
lm3<-glm(Happy~Income+ HouseholdStatus+ EducationLevel +Q122120 +Q121011 +Q120014 +Q119334 +Q118237 +Q116797 +Q116881                        +Q116441 +Q114386 +Q113181 +Q108855 +Q108856 +Q108754 +Q108342 +Q108343 +Q107869 +Q102906 +Q102289 +Q101162+Q100562 +Q99581 +Q98869 +Q98197,trainSource3,family="binomial")
lm4<-glm(Happy~Income+ HouseholdStatus+ EducationLevel +Q122120 +Q121011 +Q120014 +Q119334 +Q118237 +Q116797 +Q116881                        +Q116441 +Q114386 +Q113181 +Q108855 +Q108856 +Q108754 +Q108342 +Q108343 +Q107869 +Q102906 +Q102289 +Q101162+Q100562 +Q99581 +Q98869 +Q98197,trainSource4,family="binomial")

#lm5<-glm(Happy~.,train5,family="binomial")
#lm6<-glm(Happy~.,train6,family="binomial")



pred.test1<-predict(lm1,newdata=test1,type="response")
pred.test2<-predict(lm2,newdata=test2,type="response")
pred.test3<-predict(lm3,newdata=test3,type="response")
pred.test4<-predict(lm3,newdata=test4,type="response")
#pred.test5<-predict(lm3,newdata=test5)
#pred.test6<-predict(lm3,newdata=test6)

mean(pred.test1)
mean(pred.test2)
mean(pred.test3)
mean(pred.test4)
SSE<-sum((pred.test1-test1$reimbursement2009)^2)

pred.testSource1<-predict(lm1,newdata=testSource1,type="response")
pred.testSource2<-predict(lm2,newdata=testSource2,type="response")
pred.testSource3<-predict(lm3,newdata=testSource3,type="response")
pred.testSource4<-predict(lm3,newdata=testSource4,type="response")


all.predictions = c(pred.testSource1, pred.testSource2, pred.testSource3,pred.testSource4)

table(test$Happy,all.predictions>=.5)
length(all.predictions)

submission9 = data.frame(UserID = testSource$UserID, Probability1 = all.predictions)  #Model using train, clusterin glms
write.csv(submission9, "submission9.csv", row.names=FALSE) 

submission10 = data.frame(UserID = testSource$UserID, Probability1 = all.predictions)  #Model using trainSource, clusterin glms
write.csv(submission10, "submission10.csv", row.names=FALSE) 




head(submission5)




###############tables sniffing for odd stuff######
table(all.data$YOB,all.data$Education,exclude=NULL)
boxplot(table(all.data$YOB,all.data$Income,exclude=NULL))

table(all.data$Q113181, all.data$Q98197,exclude=NULL)


getwd()
setwd("./mooc/MitAnalytic")
setwd("./MitAnal")
dir()
trainSource<-read.csv("./data/Kaggle_train.csv",na.strings="",stringsAsFactors=T)
#create new variable, which is a sum of NA's

sumNA<-rep(0,nrow(trainSource))
for (i in 1:nrow(trainSource)){
  for(j in 1:ncol(trainSource)){
    if(is.na(trainSource[i,j]) ==T) {
      sumNA[i]<-sumNA[i]+1
    }      
  }
}
sumNA
trainSource$sumNA<-sumNA
rm(sumNA)


#create new variable, a ratio of $vote to # of Questions  VQ_ratio
###1 varialbe has no NA's.  So, 109 questions.  Ratio will be $votes/109
sort(trainSource$votes/109)
summary(trainSource[1:9])  #7 variables not like the others
trainSource$VQ_ratio<-trainSource$votes/109
which(trainSource$VQ_ratio<=.19)

##convert YOB to int
install.packages("lubridate")
library(lubridate)
trainSource$YOB<-as.Date(as.character(trainSource$YOB),format="%Y")
year(trainSource$YOB)
table(year(trainSource$YOB))
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

library(caTools)
set.seed(1000)
split<-sample.split(trainSource$Happy,SplitRatio = .7)
train<-subset(trainSource,split==T)
test<-subset(trainSource,split==F)
str(train)
str(test)

#baesline train predcition of happy variable
table(train$Happy)
1823/nrow(train)  #  0.563
table(test$Happy)
781/nrow(test)  #  0.563

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
rownames(ss_sig)


HappyLog.mod1<-glm(Happy~YOB+HouseholdStatus+EducationLevel+Q124122+Q120194+Q119334+Q118237+Q116953+Q116441
+Q116197+Q115602+Q115899+Q115390+Q114961+Q114517+Q113584+Q111848+Q108342+Q107869+Q102674+Q102289+Q101162+Q101596+Q100680
+Q98197,data=train,family=binomial)

summary(HappyLog.mod1)

HappyLog.mod1_predictions<-predict(HappyLog.mod1,newdata=test,type="response")
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
###gives an error, one suggestion is to remove NA from original data

HappyLog.mod2_predictions<-predict.glm(HappyLog.mod2,newdata=test,type="response")

output<-table(test$Happy,HappyLog.mod2_predictions >= .5)
(74+142)/(74+38+44+142)

submission3 = data.frame(UserID = test$UserID, Probability1 = HappyLog.mod2_predictions)
write.csv(submission3, "submission3.csv", row.names=FALSE) 




library(ROCR)
ROCRpredict<-prediction(votlog_predictions,gerber$voting)
ROCRperf<-performance(ROCRpredict, "tpr","fpr")
auc = as.numeric(performance(ROCRpredict, "auc")@y.values)
















#HappyStep<-step(HappyLog)
summary(HappyStep)
table(train$Q113181,train$Q98197)  #Important to exclude one or the other



plot(train$YOB,train$Gender)
#cor(as.numeric(train$Q98197),as.numeric(train$Q113181),use="complete.obs",method="spearman") #mediate, pray


#after step analysis
Step:  AIC=-6307.43
Happy ~ UserID + YOB + Gender + HouseholdStatus + Party + Q122769 + 
  Q122770 + Q121700 + Q121011 + Q120194 + Q120012 + Q120014 + 
  Q119334 + Q119650 + Q118237 + Q116797 + Q116441 + Q116197 + 
  Q115777 + Q115610 + Q115611 + Q115899 + Q114961 + Q113992 + 
  Q113583 + Q113584 + Q109367 + Q108855 + Q108617 + Q108754 + 
  Q108342 + Q108343 + Q107869 + Q106388 + Q106389 + Q105655 + 
  Q102906 + Q102674 + Q102687 + Q102289 + Q102089 + Q101162 + 
  Q100680 + Q100562 + Q99982 + Q99716 + Q98869 + Q98197

Df Sum of Sq    RSS     AIC
- YOB              1    0.2273 752.36 -6308.2
- Q105655          2    0.6172 752.75 -6308.2
- Q106388          2    0.6288 752.77 -6308.1
- UserID           1    0.2474 752.38 -6308.1
- Gender           2    0.6545 752.79 -6308.0
- Q115777          2    0.6690 752.81 -6307.9
- Q108617          2    0.6871 752.82 -6307.8
- Q113992          2    0.7194 752.86 -6307.7
- Q119650          2    0.7610 752.90 -6307.5
<none>                         752.14 -6307.4
- Q109367          2    0.7991 752.94 -6307.3
- Q108342          2    0.8077 752.95 -6307.2
- Q102089          2    0.8192 752.96 -6307.1
- Q116197          2    0.8526 752.99 -6307.0
- Q121700          2    0.8599 753.00 -6306.9
- Party            5    2.0188 754.16 -6306.9
- Q115611          2    0.8767 753.01 -6306.8
- Q122770          2    0.8980 753.04 -6306.7
- Q98197           2    0.9289 753.07 -6306.6
- Q120012          2    0.9472 753.08 -6306.5
- Q116797          2    1.0027 753.14 -6306.2
- Q113583          2    1.0473 753.18 -6306.0
- Q115610          2    1.1452 753.28 -6305.4
- Q108754          2    1.1498 753.29 -6305.4
- Q108343          2    1.2223 753.36 -6305.0
- Q100680          2    1.2351 753.37 -6305.0
- Q99982           2    1.2879 753.43 -6304.7
- Q99716           2    1.3007 753.44 -6304.6
- Q113584          2    1.3356 753.47 -6304.4
- Q106389          2    1.3641 753.50 -6304.3
- Q114961          2    1.4081 753.55 -6304.1
- Q108855          2    1.5085 753.65 -6303.5
- Q122769          2    1.5211 753.66 -6303.5
- Q102687          2    1.5717 753.71 -6303.2
- Q100562          2    1.5753 753.71 -6303.2
- Q102674          2    1.6541 753.79 -6302.8
- Q120194          2    1.6762 753.81 -6302.7
- Q121011          2    1.8120 753.95 -6302.0
- Q115899          2    1.8348 753.97 -6301.8
- Q102906          2    2.5727 754.71 -6298.0
- Q116441          2    2.6007 754.74 -6297.8
- Q98869           2    2.9507 755.09 -6296.0
- Q120014          2    3.7092 755.85 -6292.1
- HouseholdStatus  6    6.5860 758.72 -6285.1
- Q119334          2    5.4842 757.62 -6282.8
- Q102289          2    6.3137 758.45 -6278.5
- Q107869          2    7.8908 760.03 -6270.4
- Q101162          2   13.4887 765.63 -6241.5
- Q118237          2   17.2440 769.38 -6222.2



###lm of NA

lmHappyNA<-glm(Happy~sumNA,data=train,family="binomial")
summary(lmHappyNA)





pisaTest <- na.omit(pisaTest)
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")


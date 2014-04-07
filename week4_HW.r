()
school<-"I:/My Data Sources/mooc/MitAnalytic"
setwd(school)
home<-getwd()
setwd(paste0(home, "/mooc/MitAnal"))
dater<-getwd()
setwd(paste0(dater, "/MitAnal"))
dir()
gerber = read.csv("./data/gerber.csv")
str(gerber)
table(gerber$vo)[2]/sum(table(gerber$vo))
table(gerber$civi,gerber$vot)
table(gerber$haw,gerber$vot)
table(gerber$neigh,gerber$vot)

votlog<-glm(voting~hawthorne+civicduty+neighbors+self,data=gerber,family="binomial")
summary(votlog)
votlog_predictions<-predict(votlog,type="response")

output<-table(votlog_predictions >= .5, gerber$voting)
overAll_accur<-(output[1,1]+output[2,2])/(sum(output))

library(ROCR)
ROCRpredict<-prediction(votlog_predictions,gerber$voting)
ROCRperf<-performance(ROCRpredict, "tpr","fpr")
auc = as.numeric(performance(ROCRpredict, "auc")@y.values)
######

library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors+sex, data=gerber, cp=0.0)
prp(CARTmodel3)

table(gerber$sex,gerber$vot)
table(gerber$civ,gerber$vot)

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4,digits = 6)
objects(CARTmodel4)

CARTmodel5 = rpart(voting ~ control+sex, data=gerber, cp=0.0)
prp(CARTmodel5,digits = 6)
summary(CARTmodel5)

abs(.296638-.34)

LogModelSex = glm(voting ~ control+sex, data=gerber,family="binomial")
summary(LogModelSex)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")


##part2

letters<-read.csv("data/letters_ABPR.csv")
str(letters)
table(letters$letter)
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
split<-sample.split(letters$isB,SplitRatio = .5)
head(split,10)
train<-subset(letters,split==T)
str(train)
test<-subset(letters,split==F)
str(test)
#baseline
table(train$isB)
1175/(383+1175)


CARTb = rpart(isB ~ . - letter, data=train, method="class")
prp(CARTb)

cartPred<-predict(CARTb,test,type="class")
table(test$isB,cartPred)
(1118+340)/(1118+340+57+43)


library(randomForest)
set.seed(1000)

isBforest = randomForest(isB ~ . - letter, data=train )
plot(isBforest)

# Make predictions
PredictForest = predict(isBforest, newdata = test)
table(test$isB, PredictForest)

(1165+374)/(1165+10+374+9)
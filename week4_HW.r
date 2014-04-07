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
train<-subset(letters,split==T)
test<-subset(letters,split==F)
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

letters$letter = as.factor(letters$letter)
set.seed(2000)
split<-sample.split(letters$letter,SplitRatio = .5)
train2<-subset(letters,split==T)
test2<-subset(letters,split==F)
table(test2$letter)

cartLetter2 = rpart(letter ~ . - isB, data=train2 , method ="class")
#summary(cartLetter2)
prp(cartLetter2)
cartPred2<-predict(cartLetter2, newdata=test2, type ="class")
table(test2$letter,cartPred2)
(348+318+363+340)/nrow(test2)
table(cartPred2)
92+76+106+96
head(train2)

set.seed(1000)
forestLetter2 = randomForest(letter ~ . - isB, data=train2 , method ="class")
predForest2<-predict(forestLetter2,newdata=test2)
table(test2$letter,predForest2)
(390+380+393+364)/nrow(test2)


#part 3

data(state)
statedata = data.frame(state.x77)
str(statedata)

lifelm<-lm(Life.Exp~.,data=statedata)
summary(lifelm)
predictLife<-predict(lifelm,data=statedata)

SSE<-  sum((statedata$Life.Exp-predictLife)^2)

lifelm2<-lm(Life.Exp~Population+Murder+Frost+HS.Grad,data=statedata)
summary(lifelm2)
predictLife2<-predict(lifelm2,data=statedata)
SSE2<-  sum((statedata$Life.Exp-predictLife2)^2)

cartLife<-rpart(Life.Exp~.,data=statedata,control=rpart.control(minbucket=5))
prp(cartLife)

cartLifePred<-predict(cartLife,data=statedata)
SSE<-sum((statedata$Life.Exp-cartLifePred)^2)

cartLife2<-rpart(Life.Exp~Area,data=statedata,control=rpart.control(minbucket=1))
prp(cartLife2)

cartLifePred2<-predict(cartLife2,data=statedata)
SSE5<-sum((statedata$Life.Exp-cartLifePred2)^2)


library(caret)
set.seed(111)

# Define cross-validation experiment
fitControl = trainControl( method = "cv", number = 10 ) #cv=cross val, 10= folds
cartGrid = expand.grid( .cp = (1:50)*0.01)  #this commands uses the increments of .01, but 50 times

# Perform the cross validation
train(Life.Exp~.,data=statedata,  method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

cartLife3<-rpart(Life.Exp~.,data=statedata,control=rpart.control(cp=.11))
prp(cartLife3)

cartLifePred3<-predict(cartLife3,data=statedata)
SSE7<-sum((statedata$Life.Exp-cartLifePred3)^2)


set.seed(111)

# Define cross-validation experiment
fitControl = trainControl( method = "cv", number = 10 ) #cv=cross val, 10= folds
cartGrid = expand.grid( .cp = (1:50)*0.01)  #this commands uses the increments of .01, but 50 times

# Perform the cross validation
train(Life.Exp~Area,data=statedata,  method = "rpart", trControl = fitControl, tuneGrid = cartGrid )
#objects(output)
cartLife4<-rpart(Life.Exp~Area,data=statedata,control=rpart.control(cp=.06))
prp(cartLife4)

cartLifePred4<-predict(cartLife4,data=statedata)
SSE8<-sum((statedata$Life.Exp-cartLifePred4)^2)


###part 4
census<-read.csv("data/census.csv")
str(census)
library(caTools)

set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split == TRUE)
test = subset(census, split == FALSE)

over50kLog<-glm(over50k~.,data=train,family=binomial)
summary(over50kLog)

over50LogPredict<-predict(over50kLog,newdata=test,type="response")
table(test$over50k,over50LogPredict>=.5)
sum(9051,1888)/nrow(test)

#baseline for test set
table(test$over50k)
9713/nrow(test)

install.packages("ROCR")
library(ROCR)


ROCRpredTest = prediction(over50LogPredict, test$over50k)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)


library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

over50kCart<-rpart(over50k~.,data=train,method="class")
prp(over50kCart)

over50CartPred<-predict(over50kCart,newdata=test, type="class")
table(test$over50k,over50CartPred)
sum(9243,1596)/nrow(test)

### to calc the ROC, must change to type "prob"
over50CartPred<-predict(over50kCart,newdata=test, type="prob")
ROCRpred<-prediction(over50CartPred[,2],test$over50k)
ROCRperf<-performance(ROCRpred, "tpr","fpr")
plot(ROCRperf,colorize =T, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7),)

auc = as.numeric(performance(ROCRpred, "auc")@y.values)

set.seed(1)

trainSmall = train[sample(nrow(train), 2000), ]
library(randomForest)
set.seed(1)
over50forest = randomForest(over50k ~ . -nativecountry, data=trainSmall )

over50ForPred<-predict(over50forest,newdata=test)
table(test$over50k,over50ForPred)
sum(8871,2027)/nrow(test)

vu = varUsed(over50forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(over50forest$forest$xlevels[vusorted$ix]))
varImpPlot(over50forest)

install.packages("caret")
library(caret)
set.seed(2)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
fitControl = trainControl( method = "cv", number = 10 ) #cv=cross val, 10= folds


# Perform the cross validation
set.seed(2)
train(over50k~.,data=train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )



# Week 4 - "Judge, Jury, and Classifier" Lecture


# VIDEO 4

# Read in the data
getwd()
school<-"I:/My Data Sources/mooc/MitAnalytic"
setwd(school)
home<-getwd()
setwd(paste0(home, "/mooc/MitAnal"))
dater<-getwd()
setwd(paste0(dater, "/MitAnal"))
dir()
stevens = read.csv("./data/stevens.csv")
str(stevens)

# Split the data
library(caTools)
set.seed(3000)
split = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, split==TRUE)
Test = subset(stevens, split==FALSE)

# Install rpart library
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, method="class", data = Train, control=rpart.control(minbucket=25))
prp(StevensTree)

table(stevens$Resp)
# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)

# ROC curve
library(ROCR)

PredictROC = predict(StevensTree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)


auc = as.numeric(performance(pred, "auc")@y.values)

StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, method="class", data = Train, control=rpart.control(minbucket=5))
prp(StevensTree2)

StevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, method="class", data = Train, control=rpart.control(minbucket=100))
prp(StevensTree3)


# VIDEO 5 - Random Forests

# Install randomForest package
#install.packages("randomForest")
library(randomForest)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Try again
set.seed(200)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(41+75)/(41+36+18+75)
(40+74)/(40+37+19+74)



# VIDEO 6

# Install cross-validation packages
#install.packages("class")
library(class)


#install.packages("ggplot2")
library(ggplot2)
#install.packages("Rcpp")

#install.packages("plyr")
library(plyr)
#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)

# Define cross-validation experiment
fitControl = trainControl( method = "cv", number = 10 ) #cv=cross val, 10= folds
cartGrid = expand.grid( .cp = (1:50)*0.01)  #this commands uses the increments of .01, but 50 times

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, method="class", data = Train, control=rpart.control(cp = 0.18))
prp(StevensTreeCV)
# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)


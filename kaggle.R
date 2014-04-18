getwd()
setwd("./MitAnal")
dir()
train<-read.csv("./data/Kaggle_train.csv")
test<-read.csv("./data/Kaggle_test.csv")
str(train)
table(train$Happy,train$Income)

HappyLog<-glm(Happy~.,data=train,family="binomial")
summary (HappyLog)

boxplot(train$YOB~train$Gender)
?boxplot

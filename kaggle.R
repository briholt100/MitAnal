getwd()
setwd("./MitAnal")
dir()
train<-read.csv("./data/Kaggle_train.csv")
test<-read.csv("./data/Kaggle_test.csv")
str(train)
table(train$Happy,train$Income)

HappyLog<-lm(Happy~.,data=train,family="binomial")
summary (HappyLog)
happyStep<-step(HappyLog)

plot(train$YOB,train$Gender)
cor(as.numeric(train$Q98197),as.numeric(train$Q113181),use="complete.obs",method="spearman") #mediate, pray
summary(train)


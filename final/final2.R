# Final part 2 for MitAnal
getwd()
nytimes<-read.csv("nytimes.csv",stringsAsFactors=FALSE)
str(nytimes)
table(nytimes$popular)
105/nrow(nytimes)

cor(nchar(nytimes$headline),nytimes$popular)

nytimes$popular<-as.factor(nytimes$popular)
nytimes$type<-as.factor(nytimes$type)

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


predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

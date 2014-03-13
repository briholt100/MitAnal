getwd()
home<-getwd()
setwd(paste0(home, "/MitAnal"))
wine<-read.csv("wine.csv")
str(wine)
summary(wine)
model1<-lm(wine$Price~wine$AGST,data=wine)
summary(model1)
plot(model1)
model1$residuals
SSE<-sum(model1$residuals^2)
model2<-lm(wine$Price~wine$AGST+wine$HarvestRain,data=wine)
summary(model2)
SSE<-sum(model2$residuals^2)
model3<-lm(wine$Price~AGST+HarvestRain+WinterRain+
             Age+FrancePop,data=wine)
summary(model3)
SSE<-sum(model3$residuals^2)

model5<-lm(wine$Price~HarvestRain+WinterRain,data=wine)
summary(model5)
SSE<-sum(model5$residuals^2)
SSE
model4<-lm(wine$Price~AGST+HarvestRain+WinterRain+
             Age,data=wine)
summary(model4)
SSE<-sum(model4$residuals^2)
SSE
cor(wine$WinterRain,wine$Price)
cor(wine$Age,wine$FrancePop)
cor(wine)


wine_test<-read.csv("wine_test.csv")
str(wine_test)
predictTest<-predict(model4,newdata=wine_test)
predictTest
class(predictTest)

SSE=sum((wine_test$Price - predictTest)^2) 
SST =sum((wine_test$Price - mean(wine$Price))^2)  #the mean comes from the baseline model

1-SSE/SST

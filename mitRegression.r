getwd()
school<-"I:/My Data Sources/mooc/MitAnalytic"
home<-getwd()
setwd(paste0(home, "/MitAnal"))
setwd(school)


#
#Wine
######

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

1-SSE/SST  # this is R^2 


#
#moneyball
###########

base<-read.csv("baseball.csv")
str(base)
moneyball<-subset(base,Year<2002)
str(moneyball)
moneyball$rd<-(moneyball$RS-moneyball$RA)
wins<-lm(W~rd,data=moneyball)
summary(wins)
model<-lm(RS~OBP+SLG, data=base)
summary(model)


teamRank <- c(1,2,3,3,4,4,4,4,5,5)
wins2012<-c(94,88,95,88,93,94,98,97,93,94)
wins2013<-c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)


#
#NBA  Week 2 recitiation
#######

nba<-read.csv("NBA_train.csv")
str(nba)
range(nba$Seas)
table(nba$W, nba$Playoffs)
range(nba$W)
nba$PTSdiff <- (nba$PTS-nba$oppPTS)
winsReg<-lm(W~PTSdiff,data=nba)
summary(winsReg)




pointsReg<-lm(PTS~X2PA+X3PA+FTA+AST+ORB+DRB+TOV+STL+BLK,data=nba)
summary(pointsReg)
SSE<-sum(pointsReg$residuals^2)
RMSE<-sqrt(SSE/nrow(nba))

pointsReg4<-lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL,data=nba)
summary(pointsReg4)

nba_test<-read.csv("NBA_test.csv")

PointsPredictions<-predict(pointsReg4,newdata=nba_test)





##2nd week homework

data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)

model1<-lm(Life.Exp~Population+Income+Illiteracy+Murder+HS.Grad+Frost+ Area,data=statedata)
summary(model1)
SSE<-sum(model1$residuals^2)
plot(statedata$Income, statedata$Life.Exp)

model2<-lm(Life.Exp~Population+Income+Illiteracy+Murder+HS.Grad+Frost,data=statedata)
summary(model2)
SSE<-sum(model2$residuals^2)

model3<-lm(Life.Exp~Population+Income+Murder+HS.Grad+Frost,data=statedata)
summary(model3)

model4<-lm(Life.Exp~Population+Murder+HS.Grad+Frost,data=statedata)
summary(model4)
sort(predict(model4))
statedata[which.min(statedata$Life.Exp),]
sort(model4$residuals)


#
#week 2 climate data
######

climate<-read.csv("climate_change.csv")
str(climate)
range(climate$Ye)
climate_train<-subset(climate,Year<2007)
climTrain<-lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data=climate_train)
summary(climTrain)
cor(climate_train)

climTrain2<-lm(Temp~MEI+N2O+TSI+Aerosols,data=climate_train)
summary(climTrain2)

climTrain_step<-step(climTrain)
summary(climTrain_step)
climTrain_step$anova

#train and predict data.  climate train data includes data prior to 2007.   we will now test on data after

climate_test<-subset(climate,Year>2006)
str(climate_test)
tempPredict <-predict(climTrain_step,newdata=climate_test)
#R^2
SSE<-sum((tempPredict-climate_test$Temp)^2)
SST<-sum((mean(climate_train$Temp)-climate_test$Temp)^2)
1-(SSE/SST)


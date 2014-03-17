getwd()
school<-"I:/My Data Sources/mooc/MitAnalytic"
home<-getwd()
setwd(paste0(home, "/mooc/MitAnal"))
setwd(school)

#CPS
##########
getwd()
CPS<-read.csv("CPSData.csv")
MetroAreaMap<-read.csv("MetroAreaCodes.csv")
CountryMap<-read.csv("CountryCodes.csv")
str(CountryMap)
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)
summary(CPS)
str(read.csv("CPSData.csv"))

sort(table(CPS$MetroArea))

sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))

tt<-sort(tapply(CPS$Race =="Asian",CPS$MetroArea,mean))

tt[tt>.19]

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=T))


CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

summary(CPS)
sort(tapply(CPS$Country))


sort(tapply(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country == "United States", mean,na.rm=T))


(table(CPS$MetroArea,CPS$Country == "Somalia"))

data(state)

statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)



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

######
#PISA
######
getwd()
home<-getwd()
setwd(paste0(home, "/MOOC/mitanal/data"))

pisaTrain<-read.csv("pisa2009train.csv")
pisaTest<-read.csv("pisa2009test.csv")
str(pisaTrain)
str(pisaTest)
tapply(pisaTrain$readingS,pisaTrain$male,mean)
summary(pisaTrain)
pisaTrain <- na.omit(pisaTrain)

pisaTest <- na.omit(pisaTest)
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore<-lm(readingScore~.,data=pisaTrain)
summary(lmScore)

SSE<-sum(lmScore$residuals^2)
RMSE<-sqrt(SSE/nrow(pisaTrain))

predTest<-predict(lmScore, newdata=pisaTest)
summary(predTest)
SSE<-sum((predTest-pisaTest$readingScore)^2)
SST
RMSE<-sqrt(SSE/nrow(pisaTest))

mean(pisaTrain$readingScore)  #predicted test score using pisaTrain (after changing values for raceeth)
SST<-sum((mean(pisaTrain$readingScore)-pisaTest$readingScore)^2)
1-SSE/SST

#############
##FluTrain

FluTrain<-read.csv("./data/FluTrain.csv")
str(FluTrain)
head(FluTrain$Week)
FluTrain$Week<-as.Date(FluTrain$Week)
table1<-tapply(FluTrain$ILI,FluTrain$Week,sum)
which.max(subset(table1,FluTrain$Week<"2010-01-01"))

table2<-tapply(FluTrain$Queries,FluTrain$Week,sum)
table2<-(subset(table2,FluTrain$Week<"2010-01-01"))
which.max(table2)
FluTrain[FluTrain$Week=="2010-10-17",]

hist(FluTrain$ILI)
plot(log(FluTrain$ILI),FluTrain$Que)

FluTrend1<-lm(log(ILI)~Queries,data=FluTrain)
summary(FluTrend1)

cor(log(FluTrain$ILI),FluTrain$Que)^2

FluTest<-read.csv("./data/FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest1[grep("2012-03-11",FluTest$Week)] #finds record of march 11, 2012 in fluTest, then pulls our prediction
#Observed ILI compared to estimated:
(FluTest$ILI[11]-PredTest1[11])/FluTest$ILI[11]


SSE<-sum((PredTest1-FluTest$ILI)^2)
SST
RMSE<-sqrt(SSE/nrow(FluTest))

library(zoo)
FluTest<-read.csv("./data/FluTest.csv")
FluTrain<-read.csv("./data/FluTrain.csv")

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)
plot(log(FluTrain$ILILag2),log(FluTrain$ILI))

FluTrend2<-lm(log(ILI)~Queries+log(ILILag2),data=FluTrain)
summary(FluTrend2)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)

#must fill in first two data sets given teh time series lag
head(FluTest)
FluTest$ILILag2[1] <- FluTrain$ILI[416]
FluTest$ILILag2[2] <- FluTrain$ILI[417]



PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE<-sum((FluTrend2$residuals)^2)
SSE<-sum((PredTest2-FluTest$ILI)^2)
SST
RMSE<-sqrt(SSE/nrow(FluTest))

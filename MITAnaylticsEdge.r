getwd()
dir()
read.csv(file="WHO.csv")
WHO<-read.csv(file="WHO.csv")
str(WHO)
summary(WHO$Over60)
plot(WHO$GNI, WHO$FertilityRate)
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)
Outliers
nrow(Outliers)
Outliers[c("Country", "GNI","FertilityRate")]

hist(WHO$CellularSubscribers)
boxplot(WHO$CellularSubscribers~WHO$Region)
table(WHO$Region)
tapply(WHO$Over60, WHO$Region, min)
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=T)
mean(WHO$Over60)


usda<-read.csv("USDA.csv")
summary(usda)
usda[which.max(usda$Sodium),2]
HighSodium <- subset(usda,usda$Sodium>10000)
usda[match("CAVIAR",usda$Descrip),]
sd(usda$Sodium,na.rm=T)

head(read.csv("mvtWeek1.csv"))
mvt<-read.csv("mvtWeek1.csv")
str(mvt)
DateConvert <- as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
str(mvt)
mvt$Date = DateConvert
tapply(mvt$Arrest,mvt$Month,count)
table(mvt$Arrest,mvt$Month)
hist(mvt$Date, breaks=100)
boxplot(mvt$Date,range=0,horizontal=T)
table(mvt$Arrest,mvt$Year)





IBM<-read.csv("IBMStock.csv")
GE<-read.csv("GEStock.csv")
ProcterGamble<-read.csv("ProcterGambleStock.csv")
CocaCola<-read.csv("CocaColaStock.csv")
Boeing<-read.csv("BoeingStock.csv")
str
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
str(Boeing)
summary(IBM)
str(Boeing)

plot (CocaCola$Date,CocaCola$StockPrice,type="l",col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2, lty=2, col="turquoise")

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(GE$Date[301:432],GE$Stock[301:432],col="green")
lines(IBM$Date[301:432],IBM$Stock[301:432],col="purple")
lines(Boeing$Date[301:432],Boeing$Stock[301:432],col="blue")
lines(ProcterGamble$Date[301:432],ProcterGamble$Stock[301:432],col="black")
abline(v=as.Date(c("1997-09-01")), lwd=2, lty=2, col="orange")
abline(v=as.Date(c("1997-11-01")), lwd=2, lty=2, col="orange")
abline(v=as.Date(c("2005-01-01")), lwd=2, lty=2, col="orange")

tapply(GE$Stock,months(GE$Date),mean)
mean(CocaCola$Stock)

ap<-read.csv(file="AnonymityPoll.csv")
str(ap)
summary(ap)
table(ap$Smart,ap$Inter,exclude=NULL)


limited<-subset(ap,ap$Inter == 1 | ap$Smart == 1) 
nrow(limited)
summary(limited)
table(limited$Privacy.Law,exclude = NULL)
str(limited)
hist(limited$Age)
plot(limited$Age, limited$Info.On.Internet)
table(limited$Age, limited$Info)

plot(jitter(limited$Age), jitter(limited$Info.On.Internet),pch=20,cex=.8)
cor(limited$Age,limited$Info,use="complete.obs")

tapply(limited$Info.,limited$Smart,summary)
tapply(limited$Tried.,limited$Smart,table,exclude =NULL)
max(table(limited$Age, limited$Info.On.Internet,exclude =NULL))

cps<-read.csv(file="CPSData.csv")
str(cps)
max(table(cps$Indust))
sort(table(cps$State))
sum(table(cps$Citizenship,exclude =NULL))

summary(cps)
table(cps$Race,cps$Hisp)
table(cps$Regi,!is.na(cps$Married))
table(cps$Sex,!is.na(cps$Married))
table(cps$Age,!is.na(cps$Married))
table(cps$Citiz,!is.na(cps$Married))

addmargins(table(cps$State,sort(is.na(cps$Metro))))

table(cps$State, is.na(cps$MetroAreaCode))
addmargins(table(cps$Reg, is.na(cps$MetroAreaCode)))
sort(tapply(is.na(cps$Metro),cps$State,mean))
MetroAreaMap<-read.csv("MetroAreaCodes.csv")
CountryMap<-read.csv("CountryCodes.csv")
str(MetroAreaMap)
str(CountryMap)
levels(MetroAreaMap$Metro)
summary(MetroAreaMap)

# Final for MitAnal
getwd()
elantra<-read.csv("elantra.csv")
str(elantra)
summary(elantra)
elantra$factor.month<-as.factor(elantra$Month)

# Subset data into training set and test set
train = subset(elantra, Year < 2013)
test = subset(elantra, Year >= 2013)
str(train)

#regression using Unemployment, CPI_all, CPI_energy and Queries

sales.lm1 <- lm(ElantraSales~ Unemployment + CPI_all + CPI_energy + Queries, data = train)
summary(sales.lm1)



# lm using Month as well as Unemployment, CPI_all, CPI_energy and Queries

sales.lm2 <- lm(ElantraSales~ Month + Unemployment + CPI_all + CPI_energy + Queries, data = train)
summary(sales.lm2)

table(elantra$Month)
#jan times month factor:
abs(1*110.69 - 5*110.69)


#use month as factor.

sales.lm3 <- lm(ElantraSales~ factor.month + Unemployment + CPI_all + CPI_energy + Queries, data = train)
summary(sales.lm3)

#correlations:
cor(elantra[,1:7])


step(sales.lm3)


sales.lm3 <- lm(ElantraSales~ factor.month + Unemployment + CPI_all + CPI_energy, data = train)

SSE<-sum(sales.lm3$residuals^2)  #general

predict.test<-predict(sales.lm3,newdata=test)
SSE=sum((test$ElantraSales - predict.test)^2) 

mean(train$ElantraSales)

SST<-sum((mean(train$ElantraSales)-test$ElantraSales)^2)
1-SSE/SST   #R^2


#what is the largest absolute error that we make in our test set prediction?  

which.max(sort(abs(test$ElantraSales - predict.test)))
table(test$factor.month)

sort(abs(test$ElantraSales[test$factor.month==3] - predict.test[test$factor.month==3]))
test[5,]
























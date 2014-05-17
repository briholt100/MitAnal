# Final part 3 for MitAnal
getwd()
stocks<-read.csv("nasdaq_returns.csv")
str(stocks)
dim(stocks)
(table(stocks$industry))
table(stocks$ret2000.12<= -.1)

table(stocks$industry,stocks$ret2008.10<= -.1)

tapply(stocks$ret2008.10,stocks$industry,mean)
tapply(stocks$ret2000.02,stocks$industry,mean)

# Final for MitAnal

elantra<-read.csv("elantra.csv")
str(elantra)
summary(elantra)

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

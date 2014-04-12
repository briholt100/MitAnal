#Load data
school<-"I:/My Data Sources/mooc/MitAnalytic"
setwd(school)
home<-getwd()
setwd(paste0(home, "/mooc/MitAnal"))
dater<-getwd()
setwd(paste0(dater, "/MitAnal"))
dir()

wiki<-read.csv("data/wiki.csv",stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
str(wiki)

()
school<-"I:/My Data Sources/mooc/MitAnalytic"
setwd(school)
home<-getwd()
setwd(paste0(home, "/mooc/MitAnal"))
dater<-getwd()
setwd(paste0(dater, "/MitAnal"))
dir()
gerber = read.csv("./data/gerber.csv")
str(gerber)
table(gerber$vo)[2]/sum(table(gerber$vo))
table(gerber$civi,gerber$vot)
table(gerber$haw,gerber$vot)
table(gerber$neigh,gerber$vot)

votlog<-glm(voting~hawthorne+civicduty+neighbors+self,data=gerber,family="binomial")
summary(votlog)
votlog_predictions<-predict(votlog,type="response")

output<-table(votlog_predictions >= .5, gerber$voting)
overAll_accur<-(output[1,1]+output[2,2])/(sum(output))



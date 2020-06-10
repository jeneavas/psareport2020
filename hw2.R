setwd("C:/Users/Пыжак Юлия/Desktop/майнор/отчет1")
dir("отчет1")

mydata <- (read.table("hw2.csv", sep = ";", header = TRUE))
long <- as.character(mydata$life_longivity)
long <- as.numeric(long)
pollution <- as.character(mydata$air_pollution)
pollution <- as.numeric(pollution)
hap <- as.numeric(as.character(mydata$happiness))
med <- as.numeric(as.character(mydata$medicine_exp))
urb <- as.numeric(as.character(mydata$urbanization))
fail <- as.numeric(as.character(mydata$failed_ind))
country <- as.character(mydata$Country)
class(mydata[0,8])
head(mydata)

library(rpart)
credit.res <- rpart(cluster ~ life_logivity + air_pollution + medicine_exp + urbanization + failed_ind, data = mydata, method='class', control=rpart.control(minsplit=7,minbucket=5,maxdepth=9))

credit.res

library(rpart.plot)
rpart.plot(credit.res, type=2, extra=1, roundint = F)
table(mydata[,8], predict(credit.res, mydata[,-1], type="class"))
predict(credit.res, mydata[,-1], type="class")

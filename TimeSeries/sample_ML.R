
#load in the CSV data
#activate appropriate libraries
library(ggplot2)
library(forecast)
library(tseries)
library(plyr)
library(caret)
library(kernlab)


#get the data file
dataURL <- "~/Desktop/RDataFiles/CSVs/JulDecTSAllDays.csv"

#for server data use this foramt
dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/CSVs/JulDecTSAllDays.csv"
Data <- read.csv(dataURL, header = TRUE, as.is = FALSE, sep = "," )

#turn industry into a factor
Data$Industry <- as.factor(Data$Industry)

#set the seed for the random function for reproducibility, you can choose any number
set.seed(1235)

#partition the data sets
inTrain <- createDataPartition(y=Data$Field,p=0.60, list=FALSE)

training <- data[inTrain,]
testing <- data[-inTrain,]


#perform logistic regression on data
logData <- glm(Data$Outcome ~ Data$Questions,family="binomial")


#after model is built test it on the Test data
newdata <- data.frame(Test)
predict(model,newdata)


#predict model creates lists, this is how you unpack them
pred.Binds <- unlist(pred1[[1]][[1]])
pred.Binds <- pred.Binds[1:7,1]

pred.Leads <- unlist(pred1[[1]][[2]])
pred.Leads <- pred.Leads[1:7,1]



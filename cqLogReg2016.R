install.packages("lme4")
library(lme4)
packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz" 
install.packages(packageurl, repos=NULL, type="source")
install.packages("caret")
library("caret")


#libraries for reshaping and plotting data
library(pbkrtest)
library(caret)

library(ggplot2)    #for plotting
library(reshape2)   # for making clean R panels
library(rpart)      # for decision tree packages
library(rpart.plot)
library(dplyr)      #use SQL like commands

library(Metrics)
library(pROC)

#load the file for the 2016 data
url <- "K:/Sandbox/R/Data IO/CQ 2016/TestFile_CQ_2016_ina.csv"
cqDataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


#remove NA rows from the data
cqData <- na.omit(cqDataRaw)

#view the data distribution
table(cqData$Q19)
table(cqData$Q20)

#isolate the outcomes
tempOutcome <- cqData[,c(23,24)]

#make function to separate outcomes from predictors
`%notin%` <- function(x,y) !(x %in% y)
allNames <- names(cqData)
allNames
outcomeNames <- c("Q19", "Q20", "Loop", "Row.ID", "ResponseID", "Company")
predictorNames <- allNames[allNames %notin% outcomeNames]
predictorNames


#create the data set without the outcome variables
predictorVars <- cqData[, c(5:22,25)]

#create the dependent variables
Q19 <- cqData$Q19
Q20 <- cqData$Q20


#Q19 is intend to purchase
#Q20 is recommend

#convert outcomes to factors
Q19 <- as.factor(as.character(Q19))
Q20 <- as.factor(as.character(Q20))


#combine outscomes with predictors for data frame
glm19 <- cbind(predictorVars,Q19)
glm20 <- cbind(predictorVars,Q20)

head(glm19)
head(glm20)


#testing and training data for regression
#this creates testing and training data sets
#inputs df = dataframe to split, outcomename = outcome name, tr = name of training data set
#te = name of testing dataset, p = partition ratio (usually .70)
splitData <- function(df,outcomeName, tr, te, p){
  set.seed(1000)
  splitIndex <- createDataPartition(df[,outcomeName], 
                                    p=p, list = FALSE, times = 1)
  trainDf <- df[splitIndex,]
  testDf <- df[-splitIndex,]
  assign(tr, trainDf, envir = .GlobalEnv)
  assign(te, testDf, envir = .GlobalEnv)
}

#use  the function above to make the segment splits
splitData(glm19,"Q19", "train19Df", "test19Df", .70)

str(train19Df)



#start the modeling process for Q19
#set the outcome name
outcome <- "Q19"

#set up the model
objControl <- trainControl(method = 'cv', number = 10, savePredictions = TRUE)
objModel <- train(train19Df[,predictorNames], train19Df[,outcome],
                  method = 'glm',
                  family = "binomial",
                  trControl = objControl)


#check coef look for significance
summary(objModel)
summary(objModel)$coef

#make predictions
predictions19 <- predict(object = objModel, test19Df[,predictorNames])


#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = F))

#check the accuracy of the model
confusionMatrix(predictions19, test19Df$Q19)


#start the modeling process for Q20
#split the data
#use  the function above to make the segment splits
splitData(glm20,"Q20", "train20Df", "test20Df", .70)


#set the outcome name
outcome <- "Q20"

#set up the model
objControl <- trainControl(method = 'cv', number = 10, savePredictions = TRUE)
objModel <- train(train20Df[,predictorNames], train20Df[,outcome],
                  method = 'glm',
                  family = "binomial",
                  trControl = objControl)


#check coef look for significance
#get all info and then just coefficients
summary(objModel)
summary(objModel)$coef

#make predictions
predictions20 <- predict(object = objModel, test20Df[,predictorNames])


#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = F))

#check the accuracy of the model
confusionMatrix(predictions20, test20Df$Q20)



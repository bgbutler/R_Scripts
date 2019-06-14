
#libraries for reshaping and plotting data
library(ggplot2)    #for plotting
library(reshape2)   # for making clean R panels
library(rpart)      # for decision tree packages
library(rpart.plot)
library(dplyr)      #use SQL like commands
library(caret)
library(Metrics)
library(pROC)

#load the file
url <- "K:/Sandbox/R/Data IO/CQ 07-28.csv"
cqDataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#remove NA rows from the data
cqData <- na.omit(cqDataRaw)

#view the data distribution
table(cqData$Q19)

#remove the 4's from the outcome questions, Q19 and Q20
#should remove ~ 2500 rows
cqDataClean <- filter(cqData, Q19 != '4', Q20 != '4')

table(cqDataClean$Q20)

#isolate the outcomes
tempOutcome <- cqData[,c(24,25)]

#make function to separate outcomes from predictors
`%notin%` <- function(x,y) !(x %in% y)
allNames <- names(cqDataClean)
allNames
outcomeNames <- c("Q19", "Q20", "Loop", "Row.ID", "ResponseID", "Industry", "Company")
predictorNames <- allNames[allNames %notin% outcomeNames]
predictorNames

#change the characters of the data for the Q's this will cause a warning for the NA's
#do not do this until you have dropped the 4's from the outcome variables
factorToNumeric <- function(f) as.numeric(levels(f))[as.integer(f)]
cols <- c(6:ncol(cqDataClean))
cqDataClean[cols] <- lapply(cqDataClean[cols], factorToNumeric)


#check for NA coercions 
cqDataClean <- na.omit(cqDataClean)

#create the data set without the outcome variables
predictorVars <- cqDataClean[, c(1:23,26:30)]

#create the dependent variables
Q19 <- cqDataClean$Q19
Q20 <- cqDataClean$Q20


#apply the changes to the groupings based on the analysis
#convert Q19, Q20  to binary from 1 to 7
#change the q19 values
Q19 <- ifelse(Q19 >3,1,0)
Q20 <- ifelse(Q20 >3,1,0)


#Q19 is intend to purchase
#Q20 is recommend

#convert outcomes to factors
Q19 <- as.factor(Q19)
Q20 <- as.factor(Q20)


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



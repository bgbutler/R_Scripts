#regression analyses of data
.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

library(Rcpp)
library(dplyr)
library(plyr)
library(ggplot2)
library(caret)
library(Metrics)
library(pROC)
library(rpart)
library(rpart.plot)
library(gbm)
library(e1071)
library(glmnet)


#get the files  with location
url <- "C:/Users/n846490/Documents/DataScience/balanceSheetAggQ2.csv"
balanceAgg <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get the attrition information
url <- "C:/Users/n846490/Documents/DataScience/branchHeadCountsNullRem.csv"
branchHead <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get checking production and related info
url <- "C:/Users/n846490/Documents/DataScience/juneDataReducedClean.csv"
branchData <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

branchData <- select(branchData, BranchNumber, Q2BookedNewChkgCnts, Q2BookedChkNewUSD, Q2TotalChkRev,
                     TotalBizChkMM, SBLoanBal)

names(branchData) <- c("Branch.Number", "Q2NewCkCounts", "Q2NewCkRevenue", "Q2CkMarginRevenue",
                       "Q2BizCkMMRevenue", "Q2SBLoanBal")



#create the clean na function
na.zero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}



#get the key fields and then drop the rest
branchHeadkey <- select(branchHead, Branch.Number, BankersAct, ServiceAct, BankersAttr, ServiceAttr)

#apply the Na cleanup function
branchHeadkey <- as.data.frame(apply(branchHeadkey, 2, na.zero))

#clean up a little by removinv extra data
rm(branchHead)


#clean up csat
#clean csat and convert it to usable buckets
balanceAgg$CSat <- as.character(balanceAgg$CSat)
balanceAgg$CSat <- gsub("%", "", balanceAgg$CSat)
balanceAgg$CSat <- as.numeric(balanceAgg$CSat)

#define the high performance field
#this is for those in performance level 3 and above
balanceAgg$HighPerf <- ifelse(balanceAgg$PerfLevel>3,1,0)

#merge the data
q2Data <- inner_join(balanceAgg, branchHeadkey, by = "Branch.Number")
q2 <- left_join(q2Data, branchData, by = "Branch.Number")


#output the file to csv
# Write the data to clean .CSV file
write.csv(q2, file="C:/Users/n846490/Documents/DataScience/q2PerfData.csv", row.names=FALSE)


##########################################################
#begin modeling

m1 <- lm(CSat ~ BankersAttr + ServiceAttr, data = q2)
summary(m1)


#headcount active does not impact CSat
m2 <- lm(CSat ~ BankersAct + ServiceAct, data = q2)
summary(m2)

#Nnot highly correlated
m3 <- lm(Q2NewCkRevenue ~ Q2NewCkCounts, data = q2)
summary(m3)


m4 <- lm(CSat ~ Q2NewCkCounts, data = q2)
summary(m4)

m5 <- lm(CSat ~ Home + BizTotal + ConsTotal + Invest, data = q2)
summary(m5)

#convert highperf to a factor
q2$HighPerfFactor <- as.factor(as.character(q2$HighPerf))


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
splitData(q2,"HighPerf", "trainDf", "testDf", .70)


#set the outcome name
outcome <- "HighPerf"
predictorNames <- c("Home", "BizTotal", "ConsTotal", "Invest", "Q2NewCkCounts", "Q2NewCkRevenue",
                    "Q2CkMarginRevenue", "Q2BizCkMMRevenue", "Q2SBLoanBal", "CSat")




#set up the model
objControl <- trainControl(method = 'cv', number = 5, savePredictions = TRUE)
objModel <- train(trainDf[,predictorNames], trainDf[,outcome],
                  method = 'glm',
                  family = "binomial",
                  trControl = objControl)


#check coef look for significance
summary(objModel)
summary(objModel)$coef


#make predictions
predictions <- predict(object = objModel, testDf[,predictorNames])


#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = T))

#check the accuracy of the model
confusionMatrix(predictions, testDf$HighPerf)


############################################################################
#create a high CSat cut 75
q2$HighCSat <- ifelse(q2$CSat>75,1,0)
q2$HighCSat <- as.factor(as.character(q2$HighCSat))


#use  the function above to make the segment splits
splitData(q2,"HighCSat", "train2Df", "test2Df", .70)


#set the outcome name
outcome <- "HighCSat"
predictorNames <- c("Home", "BizTotal", "ConsTotal", "Invest", "Q2NewCkCounts", "Q2NewCkRevenue",
                    "Q2CkMarginRevenue", "Q2BizCkMMRevenue", "Q2SBLoanBal")


#set up the model
objControl <- trainControl(method = 'cv', number = 5, savePredictions = TRUE)
objModel <- train(train2Df[,predictorNames], train2Df[,outcome],
                  method = 'glm',
                  family = "binomial",
                  trControl = objControl)



#check coef look for significance
summary(objModel)
summary(objModel)$coef


#make predictions
predictions <- predict(object = objModel, testDf[,predictorNames])


#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = T))

#check the accuracy of the model
confusionMatrix(predictions, testDf$HighPerf)



#random
m7 <- lm(CSat ~ Lat + Lon, data = q2)
summary(m7)


#csat
predictorNames <- c("BankersAct", "ServiceAct", "BankersAttr", "ServiceAttr")



#get one feed and merge it

#get the files  with location
url <- "C:/Users/n846490/Documents/DataScience/oneDataClean.csv"
ONE <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)





#merge on to q2
q2One <- left_join(q2, ONE, by = "Branch.Number")

q2One$X <- NULL

q2One <- na.omit(q2One)


#use  the function above to make the segment splits
#convert the high perf to numerical
q2One$HighPerfNum <- as.numeric(as.character(q2One$HighPerf))


splitData(q2One,"VHighPerfFactor", "train3Df", "test3Df", .75)


predictorNames <- c("UniqueUsers", "ExitSummaryProcess",
                    "ExploreNotesUpdate", "UniqueCustomerSearches")

outcome <- "HighPerfNum"

#newer high perf based on 4 or 5
#this is for those in performance level 3 and above
q2One$VHighPerf <- ifelse(q2One$PerfLevel>4,1,0)

outcome <- "VHighPerfFactor"

q2One$VHighPerfFactor <- as.factor(q2One$VHighPerf)


#set up the model
objControl <- trainControl(method = 'cv', number = 5, savePredictions = TRUE)
objModel <- train(train3Df[,predictorNames], train3Df[,outcome],
                  method = 'glm',
                  family = "binomial",
                  preProcess = c("center", "scale"),
                  trControl = objControl)


#try glmnet
objControl <- trainControl(method = 'cv', number = 5, returnResamp = 'none')
objModel <- train(train3Df[,predictorNames], train3Df[,outcome],
                  method = 'glmnet',
                  metric = "RMSE",
                  preProcess = c("center", "scale"),
                  trControl = objControl)




#SMOTE the data
library(DMwR)

train3Df <- SMOTE(VHighPerfFactor ~ ., train3Df, perc.over = 100, perc.under = 200)

prop.table(table(train3Df$VHighPerfFactor))



#check coef look for significance
summary(objModel)
summary(objModel)$coef


#make predictions
predictions <- predict(object = objModel, test3Df[,predictorNames])


#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = F))


#create a data frame for comparing
check <- data.frame(predictions,test3Df[,outcome])
check$inseg <- ifelse(check$predictions > .50,1,0)
colnames(check) <- c("Prob", "Actual","Predict")
check$Actual <- as.factor(check$Actual)
check$Predict <- as.factor(check$Predict)


confusionMatrix(check$Predict, check$Actual)



#check the accuracy of the model
confusionMatrix(predictions, test3Df$VHighPerfFactor)


#### new models
#cleanup

rm(balanceAgg); rm(branchData); rm(branchHeadkey)
rm(ONE); rm(q2); rm(q2Data)

#write data to CSV

# Write the data to clean .CSV file
write.csv(q2, file="C:/Users/n846490/Documents/DataScience/q2OneCombined.csv", row.names=FALSE)

#try caret on multivariate

splitData(q2One,"Q2NewCkCounts", "train3Df", "test3Df", .75)

outcome <- "Q2NewCkCounts"

predictorNames <- c("UniqueUsers", "ExitSummaryProcess",
                    "ExploreNotesUpdate", "UniqueCustomerSearches")

objControl <- trainControl(method = 'cv', number = 5, returnResamp = 'none')
objModel <- train(train3Df[,predictorNames], train3Df[,outcome],
                  method = 'lm',
                  metric = "RMSE",
                  preProcess = c("center", "scale"),
                  trControl = objControl)


#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = F))

#make predictions
predictions <- predict(object = objModel, test3Df[,predictorNames])


#create a data frame for comparing
check <- data.frame(predictions,test3Df[,outcome])
names(check) <- c("Predict", "Actual")
check$SqError <- (check$Actual - check$Predict)^2



#model revenue


splitData(q2One,"Q2NewCkRevenue", "train3Df", "test3Df", .75)

outcome <- "Q2NewCkRevenue"

predictorNames <- c("UniqueUsers",
                    "ExploreNotesUpdate", "UniqueCustomerSearches")

objControl <- trainControl(method = 'cv', number = 5, returnResamp = 'none')
objModel <- train(train3Df[,predictorNames], train3Df[,outcome],
                  method = 'lm',
                  metric = "RMSE",
                  trControl = objControl)


#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = F))


#make predictions
predictions <- predict(object = objModel, test3Df[,predictorNames])


#create a data frame for comparing
check <- data.frame(predictions,test3Df[,outcome])
names(check) <- c("Predict", "Actual")
check$SqError <- (check$Actual - check$Predict)^2

g1 <- ggplot(check, aes(x = Predict, y  = Actual, size = SqError)) + geom_point()
g1



#make a big model
data <- q2One[,c(5:20,25:30)]







#get the data
`%notin%` <- function(x,y) !(x %in% y)
allNames <- names(data)
allNames

toRemove <- c("Branch.Name", "Lon", "Lat", "BranchName", "District", "Region", "Branch.Number")

dataNames <- allNames[allNames %notin% toRemove]
dataNames


#get clean data
#get the files  with location
url <- "C:/Users/n846490/Documents/DataScience/q2OneCombined.csv"
data <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

data <- data[,c(5,7:20)]

data$HighPerf <- as.factor(data$HighPerf)
data$HighPerf <- ifelse(data$HighPerf == 1,"High", "Low")


outcome <- "HighPerf"

predictorNames <- allNames[allNames %notin% outcome]
predictorNames

splitData(data,"HighPerf", "trainDf", "testDf", .75)

#try gbm
objControl <- trainControl(method = 'cv', number = 5, 
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           returnResamp = 'none')
objModel <- train(trainDf[,predictorNames], trainDf[,outcome],
                  method = 'gbm',
                  metric = "ROC",
                  preProcess = c("center", "scale"),
                  trControl = objControl)


summary(objModel)
print(objModel)

#make predictions
predictions <- predict(object = objModel, testDf[,predictorNames], type = "raw")
head(predictions)


print(postResample(pred = predictions, obs=as.factor(testDf[,outcome])))

predictions <- predict(object = objModel, testDf[,predictorNames], type = 'prob')
head(predictions)


auc <- roc(ifelse(testDf[,outcome]=="High",1,0), predictions[[2]])
print(auc$auc)


#auc for the GBM model of performance is .69, not a very good model
data <- data[,c(1:5,7:15)]

allNames <- names(data)
allNames

outcome <- "Q2CkMarginRevenue"
predictorNames <- allNames[allNames %notin% outcome]
predictorNames

#try glmnet
objControl <- trainControl(method = 'cv', number = 5, 
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           returnResamp = 'none')
objModel <- train(trainDf[,predictorNames], trainDf[,outcome],
                  method = 'gbm',
                  metric = "ROC",
                  preProcess = c("center", "scale"),
                  trControl = objControl)


splitData(data,"Q2CkMarginRevenue", "trainDf", "testDf", .75)


objControl <- trainControl(method = 'cv', number = 3, 
                           returnResamp = 'none')
objModel <- train(trainDf[,predictorNames], trainDf[,outcome],
                  method = 'glmnet',
                  metric = "RMSE",
#                  preProcess = c("center", "scale"),
                  trControl = objControl)

summary(objModel)

small.lambda.index <- which(objModel$lambda == objModel$lambda.min)
small.lambda.index

plot(varImp(objModel, scale = F))
plot(objModel, xvar='lambda')
coef(objModel)

predictions <- predict(object = objModel, testDf[,predictorNames])

auc <- roc(testDf[,outcome], predictions)
print(auc$auc)



#depending on Center and Scaling, different variables have different importance
#try a linear with some key attributes

predictorNames <- c("ServiceAttr", "ServiceAct", "BankersAttr", "CSat", "BankersAct")
predictorNames


objControl <- trainControl(method = 'cv', number = 3, 
                           returnResamp = 'none')

objModel <- train(trainDf[,predictorNames], trainDf[,outcome],
                  method = 'lm',
                  metric = "RMSE",
                  trControl = objControl)



summary(objModel)
plot(objModel)


m12 <- lm(Q2CkMarginRevenue ~ ServiceAttr + ServiceAct + BankersAttr + CSat + BankersAct, data = trainDf)
summary(m12)

predictions <- predict(object = objModel, testDf[,predictorNames])


#final linear test against performance


predictorNames <- c("Q2NewCkCounts", "CSat")
outcome <- "HighPerf"


splitData(data,"HighPerf", "trainDf", "testDf", .75)


objControl <- trainControl(method = 'cv', number = 5, 
                           returnResamp = 'none')



objControl <- trainControl(method = 'cv', number = 5, 
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           returnResamp = 'none')


objModel <- train(trainDf[,predictorNames], trainDf[,outcome],
                  method = 'glmnet',
                  metric = "ROC",
                  trControl = objControl)

summary(objModel)
print(objModel)

coef(objModel, s = 0.1)

plot(objModel)

predictions <- predict(object = objModel, testDf[,predictorNames])

predictions <- ifelse(predictions == "High", 1, 0)
actual <- ifelse(testDf$HighPerf == "High",1,0)

auc <- roc(actual, predictions)
print(auc$auc)

confusionMatrix(predictions, testDf$HighPerf)



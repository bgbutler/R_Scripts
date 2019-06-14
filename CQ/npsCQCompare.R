#compare cq to NPS

library(stringi)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(boot)
library(caret)
library(Metrics)
library(pROC)

#load the file
url <- "K:/Sandbox/Bryan Projects/CQ/CQCompanyDataforNPS.csv"
cqDataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

url <- "K:/Sandbox/Bryan Projects/CQ/NPS_baseline01262016v2.csv"
npsData <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

cqData <- select(cqDataRaw, Company,Industry2,CQScore, GoodToTotal)

allData <- cbind(cqData,npsData)

npsData

#remove the zeros
allDataClean <- filter(allData, scores > 0|scores <0)

newNames <- c("Company", "Industry2", "CQ", "GoodPercent", "Names","NPS")
colnames(allDataClean) <- newNames

m1 <- lm(data = allDataClean, NPS ~ CQ + GoodPercent + Industry2)
summary(m1)

m2 <- lm(data = allDataClean, NPS ~ CQ + GoodPercent -1)
summary(m2)

pairs(allDataClean[,c(6,3,4)])


#test the glm
m3 <- glm(NPS ~ CQ + GoodPercent -1 ,data = allDataClean, family = "gaussian")
summary.glm(m3)

#create training and testing sets
lmData <- allDataClean[,c(3,4,6)]

set.seed(1000)
splitIndex <- createDataPartition(lmData[,outcomeName], 
                                  p=.75, list = FALSE, times = 1)
trainDf <- glmnetData[splitIndex,]
testDf <- glmnetData[-splitIndex,]



hist(logSqNOS)

m4 <- lm(data = trainDf, NPS ~ CQ + GoodPercent-1)
summary(m4)

lmPredictions <- predict(object = m4,testDf[,c(1,2)])


plot(log(testDf[,3]^2), lmPredictions)

newPredDF <- cbind(log(testDf[,3]^2),lmPredictions)
newPredDF

CQ <- lmData$CQ

logSqNOS <- log(lmData$NPS^2)

Percent <- lmData$GoodPercent

logDF <- as.data.frame(cbind(logSqNOS,Percent,CQ))

pairs(logDF)

plot(m4, which = 2)

plot(allDataClean$CQ, log(allDataClean$NPS))


#cross validate the glm to improve
m3CV1 <- cv.glm(allDataClean, m3, K = 5)
m3CV1$delta


#glmnet
#create a partition of the data
glmnetData <- allDataClean[,c(3,4,6)]

outcomeName <- "NPS"
predictorNames <- names(glmnetData[names(glmnetData) != outcomeName])
predictorNames


set.seed(1000)
splitIndex <- createDataPartition(glmnetData[,outcomeName], 
                                  p=.75, list = FALSE, times = 1)
trainDf <- glmnetData[splitIndex,]
testDf <- glmnetData[-splitIndex,]

objControl <- trainControl(method = "cv", number = 3, returnResamp = "none")

objModel <- train(trainDf[,predictorNames], trainDf[,outcomeName],
                  method = "glmnet",
                  metric = "RMSE",
                  trControl = objControl)

predictions <- predict(object = objModel, testDf[,predictorNames])



auc <- roc(testDf[,outcomeName], predictions)
print(auc$auc)

postResample(pred=predictions, obs=testDf[,outcomeName])

plot(varImp(objModel), scale=F)
summary(objModel)

plot(objModel)

x <- as.matrix(trainDf[,predictorNames])
y <- as.matrix(trainDf[,outcomeName])

fit <- glmnet(x,y)

plot(fit)

print(fit)

coef(fit, s = .08)

summaryDf <- cbind(testDf$NPS, predictions)
summaryDf

plot(summaryDf)



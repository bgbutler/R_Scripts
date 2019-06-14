



#script to analyze survey data


.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

#these packages are more data cleaning and tefor text analytics
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(readxl)
library(SnowballC)
library(tm)
library(RTextTools)
library(stringi)
library(wordcloud)
library(RWeka)


#this series  of pacakges is for modeling
library(caret)
library(Metrics)
library(pROC)
library(rpart)
library(rpart.plot)
library(gbm)
library(e1071)
library(glmnet)
library(ROCR)


url <- "C:/Users/n846490/Documents/DataScience/CSVs/NewBranchDataCleanQuestions.csv"
sat <- read.csv(url, header = TRUE, sep=",", na.strings=c("", "NA"))


#make the hour a variable
sat$HourStarted <- substr(sat$TimeSurveyStarted,1,2)

#remove the ":"
sat$HourStarted <- gsub(":","",sat$HourStarted)

sat$HourStarted <- as.numeric(sat$HourStarted)


#clean up the date field and create a duration factor for survey
sat$TenureDate <- as.character(sat$TenureDate)
sat$TenureDate <- as.Date(sat$TenureDate, format = "%m/%d/%Y")

sat$SurveyDate <- as.character(sat$SurveyDate)
sat$SurveyDate <- as.Date(sat$SurveyDate, format = "%m/%d/%Y")

sat$CustDuration <- as.numeric((as.Date("2016-10-31") - sat$TenureDate)/365)

satClean <- filter(sat, CustDuration > 0)

satClean$Day <- as.POSIXlt(satClean$SurveyDate)$mday

############################################################
#clean up the satisfaction levels
satClean <- filter(sat, GeneralSatisfaction != "")

satClean$GeneralSatisfaction <- ordered(satClean$GeneralSatisfaction,
                                        levels = c("Highly Dissatisfied",
                                                   "Dissatisfied", "Neutral",
                                                   "Satisfied", "Highly Satisfied"))






satClean$GeneralSatisfaction <- ifelse(satClean$GeneralSatisfaction == "Highly Dissatisfied",  1, ifelse(
    satClean$GeneralSatisfaction == "Dissatisfied", 2, ifelse(satClean$GeneralSatisfaction == "Neutral", 3,ifelse(
        satClean$GeneralSatisfaction == "Satisfied", 4,ifelse(satClean$GeneralSatisfaction == "Highly Satisfied", 5,NA)))))

#####################################################################################
#make the conversion a function

convertComments <- function(x) {x <- ifelse(x == "Highly Dissatisfied",  1, ifelse(
    x == "Dissatisfied", 2, ifelse(x == "Neutral", 3,ifelse(
        x == "Satisfied", 4,ifelse(x == "Highly Satisfied", 5,NA)))))
return(x)
}



convertRec <- function(x) {x <- ifelse(x == "Definitely Will Not",  1, ifelse(
    x == "Likely Will Not", 2, ifelse(x == "May or May Notl", 3,ifelse(
        x == "Likely Will", 4,ifelse(x == "Definitely Will", 5,NA)))))
return(x)
}

###########################################################################


#apply the conversion

for (i in 46:53) {
    sat[,i] <- convertComments(sat[,i])
}
#this is a test
head(sat$GenuineListening)

sat$SatisfactionResolutionProblem <- convertComments(sat$SatisfactionResolutionProblem)
sat$SatisfactionRecommendations <- convertComments(sat$SatisfactionRecommendations)

sat$GeneralSatisfaction <- convertComments(sat$GeneralSatisfaction)


#create a month factor for plotting
sat$Month <- format(sat$SurveyDate, format = "%b")
sat$Month <- as.factor(sat$Month)


sat$Month <- ordered(sat$Month, levels = c("Jul","Aug","Sep", "Oct"))


#need to adjust the EHI levels in the survey:
levels(satClean$EHI)[levels(satClean$EHI)=="'$ - $24,999'"] <- "Less than $50K"

levels(satClean$EHI)[levels(satClean$EHI)=="$ - $24,999"] <- "Less than $50K"

levels(satClean$EHI)[levels(satClean$EHI)=="'$0 - $24,999'"] <- "Less than $50K"
levels(satClean$EHI)[levels(satClean$EHI)=="$0 - $24,999"] <- "Less than $50K"

levels(satClean$EHI)[levels(satClean$EHI)=="'$25,000 - $49,999'"] <- "Less than $50K"
levels(satClean$EHI)[levels(satClean$EHI)=="'$50,000 - $74,999'"] <- "$50K - $100K"
levels(satClean$EHI)[levels(satClean$EHI)=="'$75,000 - $99,999'"] <- "$50K - $100K"

levels(satClean$EHI)[levels(satClean$EHI)=="$25,000 - $49,999"] <- "Less than $50K"
levels(satClean$EHI)[levels(satClean$EHI)=="$50,000 - $74,999"] <- "$50K - $100K"
levels(satClean$EHI)[levels(satClean$EHI)=="$75,000 - $99,999"] <- "$50K - $100K"


levels(satClean$EHI)[levels(satClean$EHI)=="'$100,000 - $124,999'"] <- "$100K - $150K"
levels(satClean$EHI)[levels(satClean$EHI)=="'$125,000 - $149,999'"] <- "$100K - $150K"
levels(satClean$EHI)[levels(satClean$EHI)=="'$150,000 - $174,999'"] <- "$150K - $200K"
levels(satClean$EHI)[levels(satClean$EHI)=="'$175,000 - $199,999'"] <- "$150K - $200K"

levels(satClean$EHI)[levels(satClean$EHI)=="$100,000 - $124,999"] <- "$100K - $150K"
levels(satClean$EHI)[levels(satClean$EHI)=="$125,000 - $149,999"] <- "$100K - $150K"
levels(satClean$EHI)[levels(satClean$EHI)=="$150,000 - $174,999"] <- "$150K - $200K"
levels(satClean$EHI)[levels(satClean$EHI)=="$175,000 - $199,999"] <- "$150K - $200K"

levels(satClean$EHI)[levels(satClean$EHI)=="$200,000 - $224,999"] <- "$200K - $250K"
levels(satClean$EHI)[levels(satClean$EHI)=="$225,000 - $249,999"] <- "$200K - $250K"
levels(satClean$EHI)[levels(satClean$EHI)=="$250,000 - $274,999"] <- "$250K - $300K"
levels(satClean$EHI)[levels(satClean$EHI)=="$275,000 - $299,999"] <- "$250K - $300K"

levels(satClean$EHI)[levels(satClean$EHI)=="'$200,000 - $224,999'"] <- "$200K - $250K"
levels(satClean$EHI)[levels(satClean$EHI)=="'$225,000 - $249,999'"] <- "$200K - $250K"



levels(satClean$EHI)[levels(satClean$EHI)=="$300,000 - $324,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$325,000 - $349,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$350,000 - $374,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$375,000 - $399,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$400,000 - $424,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$425,000 - $449,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$450,000 - $474,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$475,000 - $499,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$500,000 - $524,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$525,000 - $549,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$550,000 - $574,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$575,000 - $599,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$600,000 - $624,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$625,000 - $649,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$650,000 - $674,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$675,000 - $699,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$700,000 - $724,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$725,000 - $749,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$750,000 - $774,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$775,000 - $799,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$800,000 - $824,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$825,000 - $849,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$850,000 - $874,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$875,000 - $899,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$900,000 - $924,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$925,000 - $949,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$950,000 - $974,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$975,000 - $999,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$1,150,000 - $1,174,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$2,000,000 - $2,024,999"] <- "$300K+"


levels(satClean$EHI)[levels(satClean$EHI)=="$300K+"] <- "$300,000+"
levels(satClean$EHI)[levels(satClean$EHI)=="-"] <- "NA"


#now order it

satClean$EHI <- ordered(satClean$EHI,
                        levels = c(NA, "Less than $50K", "$50K - $100K", "$100K - $150K",
                                   "$150K - $200K", "$200K - $250K", "$250K - $300K",
                                   "$300K+"))


satClean$OverallExperience <- ordered(satClean$OverallExperience,
                                      levels = c("Highly Dissatisfied",
                                                 "Dissatisfied", "Neutral",
                                                 "Satisfied", "Highly Satisfied"))


#ensure the right factors are there


satRed <- select(satClean,
                 OverallExperience,
                 Wouldyoudescribethewaitas...,
                 GenuineListening,
                 KnowledgeProductsServices,
                 SolutionAddressedNeed,
                 TellerMetNeed,
                 SpeedofTransaction,
                 SincerelyThanking,
                 WelcomingEnvironment,
                 BranchAppearance,
                 CompareSantanderToOther,
                 LikelyRecommend,
                 IdenitifyPersonalFinancialNeeds,
                 Region)


#check for missing numbers 
sapply(satRed,function(x)any(is.na(x)))

#####################################################################################
#make the conversion a function

convertComments <- function(x) {x <- ifelse(x == "Highly Dissatisfied",  1, ifelse(
    x == "Dissatisfied", 2, ifelse(x == "Neutral", 3,ifelse(
        x == "Satisfied", 4,ifelse(x == "Highly Satisfied", 5,NA)))))
return(x)
}



convertRec <- function(x) {x <- ifelse(x == "Definitely Will Not",  1, ifelse(
    x == "Likely Will Not", 2, ifelse(x == "May or May Notl", 3,ifelse(
        x == "Likely Will", 4,ifelse(x == "Definitely Will", 5,NA)))))
return(x)
}

###########################################################################


#apply the conversion

for (i in 3:11) {
    satRed[,i] <- convertComments(satRed[,i])
}
#this is a test
head(sat$GenuineListening)


satRed$LikelyRecommend <- as.character(satRed$LikelyRecommend)
satRed$LikelyRecommend <- convertRec(satRed$LikelyRecommend)


colnames(satRed)[2] <- "WaitUnreasonable"

#convert unreasonable wait into dummy
satRed$WaitUnreasonable <- as.character(satRed$WaitUnreasonable)
satRed$WaitUnreasonable <- ifelse(satRed$WaitUnreasonable == "Unreasonable",1,0)

#make identify opersonal financial needs a dummy
satRed$IdenitifyPersonalFinancialNeeds <- ifelse(satRed$IdenitifyPersonalFinancialNeeds == "Yes",1,0)


#fill in the NAs with the row means
data <- satRed[,c(3:11)]

k <- which(is.na(data), arr.ind = TRUE)
data[k] <- rowMeans(data, na.rm=TRUE)[k[,1]]


#this collects the non 1 to 5 questions
satOther <- satRed[,c(1,2,12,13,14)]

#drop the likely recommend column from other
satOther <- satOther[,-3] 


#drop the compare to other
data <- data[,-9]

satModel <- cbind(satOther, data)

#make outcomes binary factors
satModel$LikelyRecommend <- ifelse(satModel$LikelyRecommend > 4, 1,0)
satModel$LikelyRecommend <- as.factor(satModel$LikelyRecommend)

satModel$OverallExperience <- as.character(satModel$OverallExperience)

satModel$OverallExperience <- convertComments(satModel$OverallExperience)
satModel$OverallExperience <- ifelse(satModel$OverallExperience > 4, 1,0)
satModel$OverallExperience <- as.factor(ifelse(satModel$OverallExperience == 1,"High", "Low"))

#check for near zero variance
nearZeroVar(satModel, saveMetrics = TRUE)

##################################################
#te = name of testing dataset, p = partition ratio (usually .70)
#this is a function only and is called later in the analysis

splitData <- function(df,outcomeName, tr, te, p){
    set.seed(1000)
    splitIndex <- createDataPartition(df[,outcomeName], 
                                      p=p, list = FALSE, times = 1)
    trainDf <- df[splitIndex,]
    testDf <- df[-splitIndex,]
    assign(tr, trainDf, envir = .GlobalEnv)
    assign(te, testDf, envir = .GlobalEnv)
}

highSat <- satModel


#check the number of NA
#convert NA to 0 for modeling
sum(is.na(highSat$WaitUnreasonable))
highSat$WaitUnreasonable <- ifelse(is.na(highSat$WaitUnreasonable),0,highSat$WaitUnreasonable)

#convert NA to zero for Identify Personal Financial Needs
sum(is.na(highSat$IdenitifyPersonalFinancialNeeds))
highSat$IdenitifyPersonalFinancialNeeds <- ifelse(is.na(highSat$IdenitifyPersonalFinancialNeeds),
                                                        0, highSat$IdenitifyPersonalFinancialNeeds)


#make the region a dummy variable
highSatDummy <- dummyVars("~.", data = highSat)
highSat <- as.data.frame(predict(highSatDummy,highSat))

#remove the region.northeastern ne to make constant clean
highSat <- highSat[,-6]

#clean up dummification
colnames(highSat)[1] <- "OverallExperience"
highSat$OverallExperience <- as.factor(ifelse(highSat$OverallExperience == 1,"High", "Low"))

#remove the low
highSat <- highSat[,-2]


Outcome1 <- "OverallExperience"

Outcome2 <- "LikelyRecommend"



################################################################
##################   First mModel OVerall Experience


predictorNames <- names(highSat)[names(highSat) != Outcome1]
predictorNames <- predictorNames[-11]
predictorNames



#use  the function above to make the segment splits
splitData(highSat,Outcome1, "trainDf", "testDf", .70)

#set up the model
objControl <- trainControl(method = 'cv',
                           number = 5,
                           savePredictions = TRUE,
                           returnResamp = 'none',
                           summaryFunction = twoClassSummary,
                           classProbs = T)
objModel <- train(trainDf[,predictorNames], trainDf[,Outcome1],
                  method = 'glm',
                  family = "binomial",
                  metric = "ROC",
                  trControl = objControl)

#check coef look for significance
summary(objModel)

#check for variable importance
plot(varImp(objModel, scale = T))


#make predictions
predictionsRaw <- predict(object = objModel, testDf[,predictorNames], type = "raw")
head(predictionsRaw)
print(postResample(pred=predictionsRaw, obs = as.factor(testDf[,Outcome1])))

#check the accuracy of the model
confusionMatrix(predictionsRaw, testDf$OverallExperience)

#get the roc
predictionsProb <- predict(object = objModel, testDf[,predictorNames], type = 'prob')
auc <- roc(ifelse(testDf[,Outcome1] == "High", 1,0), predictionsProb[[1]])
auc

##########################################################################
############# Second Model Likely to Recommend

##################   First mModel OVerall Experience

likeRecommend <- satModel[,-1]

#check the number of NA
#convert NA to 0 for modeling
sum(is.na(likeRecommend$WaitUnreasonable))
likeRecommend$WaitUnreasonable <- ifelse(is.na(likeRecommend$WaitUnreasonable),0,likeRecommend$WaitUnreasonable)

#convert NA to zero for Identify Personal Financial Needs
sum(is.na(likeRecommend$IdenitifyPersonalFinancialNeeds))
likeRecommend$IdenitifyPersonalFinancialNeeds <- ifelse(is.na(likeRecommend$IdenitifyPersonalFinancialNeeds),
                                                  0, likeRecommend$IdenitifyPersonalFinancialNeeds)


#convert likely Recommend NA
sum(is.na(likeRecommend$LikelyRecommend))
likeRecommend$LikelyRecommend <- as.character(likeRecommend$LikelyRecommend)
likeRecommend$LikelyRecommend <- ifelse(is.na(likeRecommend$LikelyRecommend),
                                                        0, likeRecommend$LikelyRecommend)
likeRecommend$LikelyRecommend <- ifelse(likeRecommend$LikelyRecommend == "1", "Recommend", "No")


likeRecommend$LikelyRecommend <- as.factor(likeRecommend$LikelyRecommend)

Outcome2 <- "LikelyRecommend"

predictorNames <- names(likeRecommend)[names(likeRecommend) != Outcome2]
predictorNames <- predictorNames[-11]
predictorNames



#use  the function above to make the segment splits
splitData(likeRecommend, Outcome2, "trainDf", "testDf", .70)

#set up the model
objControl <- trainControl(method = 'cv',
                           number = 5,
                           savePredictions = TRUE,
                           returnResamp = 'none',
                           summaryFunction = twoClassSummary,
                           classProbs = T)
objModel <- train(trainDf[,predictorNames], trainDf[,Outcome2],
                  method = 'glm',
                  family = "binomial",
                  metric = "ROC",
                  trControl = objControl)

#check coef look for significance
summary(objModel)

#check for variable importance
plot(varImp(objModel, scale = T))


#make predictions
predictionsRaw <- predict(object = objModel, testDf[,predictorNames], type = "raw")
head(predictionsRaw)
print(postResample(pred=predictionsRaw, obs = as.factor(testDf[,Outcome2])))

#check the accuracy of the model
confusionMatrix(predictionsRaw, testDf$LikelyRecommend)

#get the roc
predictionsProb <- predict(object = objModel, testDf[,predictorNames], type = 'prob')
rm(auc)
auc <- roc(ifelse(testDf[,Outcome2] == "Recommend", 1,0), predictionsProb[[1]])
auc


s1 <- 0
s2 <- 1
s3 <- 4
s4 <- 4
s5 <- 4
s6 <- 4
s7 <- 4
s8 <- 4
s9 <- 5
s10<- 4
B0 <- summary(objModel)$coefficients[1,1]
B1 <- summary(objModel)$coefficients[2,1]
B2 <- summary(objModel)$coefficients[3,1]
B3 <- summary(objModel)$coefficients[4,1]
B4 <- summary(objModel)$coefficients[5,1]
B5 <- summary(objModel)$coefficients[6,1]   
B6 <- summary(objModel)$coefficients[7,1]
B7 <- summary(objModel)$coefficients[8,1]    
B8 <- summary(objModel)$coefficients[9,1]
B9 <- summary(objModel)$coefficients[10,1]
B10 <- summary(objModel)$coefficients[11,1]    
    
    
    

P <- B0 + B1*s1 + B2*s2 + B3*s3 + B3*s3 + B4*s4 + B5*s5 + B6*s6 + B7*s7 + B8*s8 + B9*s9 + B10*s10

H <- 1/(1 + exp(P))
H



o1 <- exp(22.3775 + B1*3)
o2 <- exp(22.3775 + B1*4)
o1
o2

p1 <- o1/(1+o1)
p2 <- o2/(1+o2)
p1
p2








#this is for the cluster prediction of the ADA data

library(dplyr)
library(ggplot2)
library(class)  #for knn
library(caret)  #machine learning package
library(Metrics)
library(pROC)
library(rpart)
library(rpart.plot)
library(partykit)


#get the data
#this is the reduced questions with straighliners removed
url <- "Q:/Analytics/ADA/adaSurveyClean.csv"
dataRaw <- read.csv(url, header = TRUE, sep=",", na.strings = 0,as.is = FALSE)

#build the matrix
adaMatRed <- dataRaw[,c(20:35)]

#use this matrix to include HHI
adaMatRed <- dataRaw[,c(19:35)]

#full data matrix with some columns removed
adaMatRed <- dataRaw[,c(2:51)]



#make dummy vars for segments
#make factors out of outcomes and clusters
adaMatRed$Cluster <- as.factor(as.character(adaMatRed$Cluster))

dmy <- dummyVars("~.", data = adaMatRed)
dmyDf <- data.frame(predict(dmy, newdata = adaMatRed))
head(dmyDf,10)

#save the segments as values for other modeling
tempOutcome <- dmyDf[,c(16:22)]

#with HHI
tempOutcome <- dmyDf[,c(26:32)]

tempOutcome <- dmyDf[,c(60:66)]


#make function to separate outcomes from predictors
`%notin%` <- function(x,y) !(x %in% y)
allNames <- names(dmyDf)
allNames
outcomeNames <- c("Cluster.1", "Cluster.2", "Cluster.3",
                  "Cluster.4", "Cluster.5", "Cluster.6", "Cluster.7")
predictorNames <- allNames[allNames %notin% outcomeNames]
predictorNames

#make factors from outcomes
#do in three steps
#make character
dmyDf$Cluster.1 <- as.character(dmyDf$Cluster.1)
dmyDf$Cluster.2 <- as.character(dmyDf$Cluster.2)
dmyDf$Cluster.3 <- as.character(dmyDf$Cluster.3)
dmyDf$Cluster.4 <- as.character(dmyDf$Cluster.4)
dmyDf$Cluster.5 <- as.character(dmyDf$Cluster.5)
dmyDf$Cluster.6 <- as.character(dmyDf$Cluster.6)
dmyDf$Cluster.7 <- as.character(dmyDf$Cluster.7)

#change outcome from 1,0
dmyDf$Cluster.1 <- ifelse(dmyDf$Cluster.1==1, "Seg1", "Other")
dmyDf$Cluster.2 <- ifelse(dmyDf$Cluster.2==1, "Seg2", "Other")
dmyDf$Cluster.3 <- ifelse(dmyDf$Cluster.3==1, "Seg3", "Other")
dmyDf$Cluster.4 <- ifelse(dmyDf$Cluster.4==1, "Seg4", "Other")
dmyDf$Cluster.5 <- ifelse(dmyDf$Cluster.5==1, "Seg5", "Other")
dmyDf$Cluster.6 <- ifelse(dmyDf$Cluster.6==1, "Seg6", "Other")
dmyDf$Cluster.7 <- ifelse(dmyDf$Cluster.7==1, "Seg7", "Other")

#make factor
dmyDf$Cluster.1 <- as.factor(dmyDf$Cluster.1)
dmyDf$Cluster.2 <- as.factor(dmyDf$Cluster.2)
dmyDf$Cluster.3 <- as.factor(dmyDf$Cluster.3)
dmyDf$Cluster.4 <- as.factor(dmyDf$Cluster.4)
dmyDf$Cluster.5 <- as.factor(dmyDf$Cluster.5)
dmyDf$Cluster.6 <- as.factor(dmyDf$Cluster.6)
dmyDf$Cluster.7 <- as.factor(dmyDf$Cluster.7)

#this is without HHI factors
Segment1 <- dmyDf[,c(1:16)]
Segment2 <- dmyDf[,c(1:15,17)]
Segment3 <- dmyDf[,c(1:15,18)]
Segment4 <- dmyDf[,c(1:15,19)]
Segment5 <- dmyDf[,c(1:15,20)]
Segment6 <- dmyDf[,c(1:15,21)]
Segment7 <- dmyDf[,c(1:15,22)]

#with HHI factors
Segment1 <- dmyDf[,c(1:26)]
Segment2 <- dmyDf[,c(1:25,27)]
Segment3 <- dmyDf[,c(1:25,28)]
Segment4 <- dmyDf[,c(1:25,29)]
Segment5 <- dmyDf[,c(1:25,30)]
Segment6 <- dmyDf[,c(1:25,31)]
Segment7 <- dmyDf[,c(1:25,32)]

#with ALL factors
Segment1 <- dmyDf[,c(1:60)]
Segment2 <- dmyDf[,c(1:59,61)]
Segment3 <- dmyDf[,c(1:59,62)]
Segment4 <- dmyDf[,c(1:59,63)]
Segment5 <- dmyDf[,c(1:59,64)]
Segment6 <- dmyDf[,c(1:59,65)]
Segment7 <- dmyDf[,c(1:59,66)]


#testing and training data for regression
#this is a function for use later
splitData <- function(df,outcomeName, tr, te, p){
  set.seed(1000)
  splitIndex <- createDataPartition(df[,outcomeName], 
                                    p=p, list = FALSE, times = 1)
  trainDf <- df[splitIndex,]
  testDf <- df[-splitIndex,]
  assign(tr, trainDf, envir = .GlobalEnv)
  assign(te, testDf, envir = .GlobalEnv)
}
#create the new dataset for all vars
gl1 <- cbind(dmyDf[1:59],tempOutcome[1])
gl2 <- cbind(dmyDf[1:59],tempOutcome[2])
gl3 <- cbind(dmyDf[1:59],tempOutcome[3])
gl4 <- cbind(dmyDf[1:59],tempOutcome[4])
gl5 <- cbind(dmyDf[1:59],tempOutcome[5])
gl6 <- cbind(dmyDf[1:59],tempOutcome[6])
gl7 <- cbind(dmyDf[1:59],tempOutcome[7])

#try glmnet with HHI
gl1 <- cbind(dmyDf[1:25],tempOutcome[1])
gl2 <- cbind(dmyDf[1:25],tempOutcome[2])
gl3 <- cbind(dmyDf[1:25],tempOutcome[3])
gl4 <- cbind(dmyDf[1:25],tempOutcome[4])
gl5 <- cbind(dmyDf[1:25],tempOutcome[5])
gl6 <- cbind(dmyDf[1:25],tempOutcome[6])
gl7 <- cbind(dmyDf[1:25],tempOutcome[7])



#use  the function above to make the segment splits
splitData(gl1,"Cluster.1", "train1Df", "test1Df", .70)
splitData(gl2,"Cluster.2", "train2Df", "test2Df", .70)
splitData(gl3,"Cluster.3", "train3Df", "test3Df", .70)
splitData(gl4,"Cluster.4", "train4Df", "test4Df", .70)
splitData(gl5,"Cluster.5", "train5Df", "test5Df", .70)
splitData(gl6,"Cluster.6", "train6Df", "test6Df", .70)
splitData(gl7,"Cluster.7", "train7Df", "test7Df", .70)


############################################
#model and predictions for each cluster
outcome <- "Cluster.1"
objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none')
objModel <- train(train1Df[,predictorNames], train1Df[,outcome],
                  method = 'glmnet',
                  metric = "RMSE",
                  trControl = objControl)

predictions <- predict(object = objModel, test1Df[,predictorNames])

#create a data frame for comparing
check <- data.frame(predictions,test1Df[,outcome])
check$inseg <- ifelse(check$predictions > .50,1,0)
colnames(check) <- c("Prob", "Actual","Predict")
confusionMatrix(check$Predict, check$Actual)

#get the AUC for the glmnet
auc <- roc(test1Df[,outcome], predictions)
print(auc$auc)

#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = F))

#############################################################
#model and predictions for each cluster
outcome <- "Cluster.2"
objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none')
objModel <- train(train2Df[,predictorNames], train2Df[,outcome],
                  method = 'glmnet',
                  metric = "RMSE",
                  trControl = objControl)

predictions <- predict(object = objModel, test2Df[,predictorNames])

#create a data frame for comparing
check <- data.frame(predictions,test2Df[,outcome])
check$inseg <- ifelse(check$predictions > .50,1,0)
colnames(check) <- c("Prob", "Actual","Predict")
confusionMatrix(check$Predict, check$Actual)

#get the AUC for the glmnet
auc <- roc(test2Df[,outcome], predictions)
print(auc$auc)

#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = F))

#############################################################
#model and predictions for each cluster
outcome <- "Cluster.3"
objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none')
objModel <- train(train3Df[,predictorNames], train3Df[,outcome],
                  method = 'glmnet',
                  metric = "RMSE",
                  trControl = objControl)

predictions <- predict(object = objModel, test3Df[,predictorNames])

#create a data frame for comparing
check <- data.frame(predictions,test3Df[,outcome])
check$inseg <- ifelse(check$predictions > .50,1,0)
colnames(check) <- c("Prob", "Actual","Predict")
confusionMatrix(check$Predict, check$Actual)

#get the AUC for the glmnet
auc <- roc(test3Df[,outcome], predictions)
print(auc$auc)

#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = F))

#############################################################
#model and predictions for each cluster
outcome <- "Cluster.4"
objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none')
objModel <- train(train4Df[,predictorNames], train4Df[,outcome],
                  method = 'glmnet',
                  metric = "RMSE",
                  trControl = objControl)

predictions <- predict(object = objModel, test4Df[,predictorNames])

#create a data frame for comparing
check <- data.frame(predictions,test4Df[,outcome])
check$inseg <- ifelse(check$predictions > .50,1,0)
colnames(check) <- c("Prob", "Actual","Predict")
confusionMatrix(check$Predict, check$Actual)

#get the AUC for the glmnet
auc <- roc(test4Df[,outcome], predictions)
print(auc$auc)

#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = F))

#############################################################
#model and predictions for each cluster
outcome <- "Cluster.5"
objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none')
objModel <- train(train5Df[,predictorNames], train5Df[,outcome],
                  method = 'glmnet',
                  metric = "RMSE",
                  trControl = objControl)

predictions <- predict(object = objModel, test5Df[,predictorNames])

#create a data frame for comparing
check <- data.frame(predictions,test5Df[,outcome])
check$inseg <- ifelse(check$predictions > .50,1,0)
colnames(check) <- c("Prob", "Actual","Predict")
confusionMatrix(check$Predict, check$Actual)

#get the AUC for the glmnet
auc <- roc(test5Df[,outcome], predictions)
print(auc$auc)

#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = F))

#######################################
#model and predictions for each cluster
outcome <- "Cluster.6"
objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none')
objModel <- train(train6Df[,predictorNames], train6Df[,outcome],
                  method = 'glmnet',
                  metric = "RMSE",
                  trControl = objControl)

predictions <- predict(object = objModel, test6Df[,predictorNames])

#create a data frame for comparing
check <- data.frame(predictions,test6Df[,outcome])
check$inseg <- ifelse(check$predictions > .50,1,0)
colnames(check) <- c("Prob", "Actual","Predict")
confusionMatrix(check$Predict, check$Actual)

#get the AUC for the glmnet
auc <- roc(test6Df[,outcome], predictions)
print(auc$auc)

#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = F))

################################################
#model and predictions for each cluster
outcome <- "Cluster.7"
objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none')
objModel <- train(train7Df[,predictorNames], train7Df[,outcome],
                  method = 'glmnet',
                  metric = "RMSE",
                  trControl = objControl)

predictions <- predict(object = objModel, test7Df[,predictorNames])

#create a data frame for comparing
check <- data.frame(predictions,test7Df[,outcome])
check$inseg <- ifelse(check$predictions > .50,1,0)
colnames(check) <- c("Prob", "Actual","Predict")
confusionMatrix(check$Predict, check$Actual)

#get the AUC for the glmnet
auc <- roc(test7Df[,outcome], predictions)
print(auc$auc)

#check for variable importance
summary(objModel)
plot(varImp(objModel, scale = F))

################################################
#make the logistic regressions

splitData(Segment1,"Cluster.1", "train1Df", "test1Df", .70)
splitData(Segment2,"Cluster.2", "train2Df", "test2Df", .70)
splitData(Segment3,"Cluster.3", "train3Df", "test3Df", .70)
splitData(Segment4,"Cluster.4", "train4Df", "test4Df", .70)
splitData(Segment5,"Cluster.5", "train5Df", "test5Df", .70)
splitData(Segment6,"Cluster.6", "train6Df", "test6Df", .70)
splitData(Segment7,"Cluster.7", "train7Df", "test7Df", .70)
#Values have been converted Cluster values to factors for training/testing


#make the actual models
log1 <- train(Cluster.1 ~ RegularDentistVisits +
                          FeelResponsibleSinceGo +
                          GoAboveBeyondHealthy +
                          NotKnowWhereFindNewDentist +
                          BadFeedbackPreventsVisits +
                          RoutineAppointmentNotFinStressful +
                          IBestJudgeOralCareNeeds +
                          FearPainPreventsVisits +
                          GeneticMakeupDetermines +
                          AnxietyFindNewProblemsPrevents + 
                          PressureFromLovedOnesToGo + 
                          TakeGoodCareNotNeedDentist +
                          EmergencyProcedureNotFinStressful +
                          FindingTimeVisitDifficult +
                          PainfulProblemScheduleVisit +
                          GoWhenExperiencePain + 
                          MoreKnowledgeableThanOthers +
                          HHI.60000to79999,
              data = train1Df,
              method = "glm")


predictions1 <- predict(object = log1, newdata = test1Df, type = "prob")
predictions1$seg <- ifelse(predictions1$Seg1 >.50, "Seg1", "Other")
confusionMatrix(predictions1$seg, test1Df$Cluster.1)
#############

log2 <- train(Cluster.2 ~ FearPainPreventsVisits +
                AnxietyFindNewProblemsPrevents + 
                KnowHowToFindInformation +
                NotKnowWhereFindNewDentist + 
                RegularDentistVisits +
                TakeGoodCareNotNeedDentist +
                PainfulProblemScheduleVisit,
              data = train2Df,
              method = "glm")

predictions2 <- predict(object = log2, newdata = test2Df, type = "prob")
predictions2$seg <- ifelse(predictions2$Seg2 >.50, "Seg2", "Other")
confusionMatrix(predictions2$seg, test2Df$Cluster.2)
#################################

log3 <- train(Cluster.3 ~ DentistUnnecessaryExceptProblem +
                EmergencyProcedureNotFinStressful +
                DentalEffortNotWorthTime +
                FeelResponsibleSinceGo + 
                HHI.Under.20000 + 
                TakeGoodCareNotNeedDentist +
                JudgedBasedTeethLook +
                WhiteTeethHealthyTeeth + 
                GoWhenExperiencePain +
                NotKnowWhereFindNewDentist + 
                PainfulProblemScheduleVisit +
                RoutineAppointmentNotFinStressful +
                RegularDentistVisits,
              data = train3Df,
              method = "glm")

predictions3 <- predict(object = log3, newdata = test3Df, type = "prob")
predictions3$seg <- ifelse(predictions3$Seg3 >.50, "Seg3", "Other")
confusionMatrix(predictions3$seg, test3Df$Cluster.3)
#################################

log4 <- train(Cluster.4 ~ FearPainPreventsVisits +
                HHI.Under.20000 +
                FeelResponsibleSinceGo +
                PrioritizedLovedOnes + 
                AnxietyFindNewProblemPrevents + 
                MoreKnowledgeableThanOthers + 
                SmellSoundTastePreventsVisits +
                PainfulProblemScheduleVisit + 
                FindingTimeVisitDifficult + 
                RoutineAppointmentNotFinStressful +
                DentalEffortNotWorthTime +
                TeethGoodForCareerSuccess + 
                EmergencyProcedureNotFinStressful,
              data = train4Df,
              method = "glm")


predictions4 <- predict(object = log4, newdata = test4Df, type = "prob")
predictions4$seg <- ifelse(predictions4$Seg4 >.50, "Seg4", "Other")
confusionMatrix(predictions4$seg, test4Df$Cluster.4)

##############################
log5 <- train(Cluster.5 ~ EmergencyProcedureNotFinStressful +
                RoutineAppointmentNotFinStressful +
                HHI.140000to179999 + 
                ProudTeethGumsFeel +
                TakeGoodCareNotNeedDentist +
                GeneticMakeupDetermines,
              data = train5Df,
              method = "glm")

predictions5 <- predict(object = log5, newdata = test5Df, type = "prob")
predictions5$seg <- ifelse(predictions5$Seg5 >.50, "Seg5", "Other")
confusionMatrix(predictions5$seg, test5Df$Cluster.5)
#####################################

log6 <- train(Cluster.6 ~ TakeGoodCareNotNeedDentist +
                JudgedBasedTeethLook +
                GeneticMakeupDetermines + 
                NotKnowWhereFindNewDentist + 
                PainfulProblemScheduleVisit + 
                FearPainPreventsVisits +
                RegularDentistVisits +
                RoutineAppointmentNotFinStressful +
                AnxietyFindNewProblemPrevents +
                FindingTimeVisitDifficult,
                data = train6Df,
              method = "glm")


predictions6 <- predict(object = log6, newdata = test6Df, type = "prob")
predictions6$seg <- ifelse(predictions6$Seg6 >.50, "Seg6", "Other")
confusionMatrix(predictions6$seg, test6Df$Cluster.6)

###############################

log7 <- train(Cluster.7 ~ RegularDentistVisits +
                FeelResponsibleSinceGo +
                DentalEffortNotWorthTime + 
                EmergencyProcedureNotFinStressful +
                RoutineAppointmentNotFinStressful +
                PainfulProblemScheduleVisit + 
                AnxietyFindNewProblemsPrevents, 
              data = train7Df,
              method = "glm")


predictions7 <- predict(object = log7, newdata = test7Df, type = "prob")
predictions7$seg <- ifelse(predictions7$Seg7 >.50, "Seg7", "Other")
confusionMatrix(predictions7$seg, test7Df$Cluster.7)




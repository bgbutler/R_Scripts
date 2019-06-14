## perform linear regression on CSat Scoring


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
#make the conversion functions

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
    satClean[,i] <- convertComments(satClean[,i])
}
#this is a test
head(satClean$GenuineListening)

satClean$SatisfactionResolutionProblem <- convertComments(satClean$SatisfactionResolutionProblem)
satClean$SatisfactionRecommendations <- convertComments(satClean$SatisfactionRecommendations)

satClean$GeneralSatisfaction <- convertComments(satClean$GeneralSatisfaction)

#get the key data
satRed <- select(satClean,
                 BranchID,
                 SurveyScore,
                 Wouldyoudescribethewaitas...,
                 GenuineListening,
                 KnowledgeProductsServices,
                 SolutionAddressedNeed,
                 TellerMetNeed,
                 SpeedofTransaction,
                 SincerelyThanking,
                 WelcomingEnvironment,
                 BranchAppearance,
                 IdenitifyPersonalFinancialNeeds)


sum(is.na(satRed$SurveyScore))

colnames(satRed)[3] <- "WaitUnreasonable"



#convert the text to values
#convert unreasonable wait into dummy
satRed$WaitUnreasonable <- as.character(satRed$WaitUnreasonable)
satRed$WaitUnreasonable <- ifelse(satRed$WaitUnreasonable == "Unreasonable",1,0)


#convert NA to 0 for modeling
sum(is.na(satRed$WaitUnreasonable))
satRed$WaitUnreasonable <- ifelse(is.na(satRed$WaitUnreasonable),0,satRed$WaitUnreasonable)

#make identify opersonal financial needs a dummy
satRed$IdenitifyPersonalFinancialNeeds <- as.character(satRed$IdenitifyPersonalFinancialNeeds)
satRed$IdenitifyPersonalFinancialNeeds <- ifelse(satRed$IdenitifyPersonalFinancialNeeds == "Yes",1,0)



#convert NA to 0 for modeling
sum(is.na(satRed$SolutionAddressedNeed))

sum(is.na(satBranch$SolutionAddressedNeed))


satBranch <- satRed %>% group_by(BranchID) %>% 
    summarise(
        Score = mean(SurveyScore),
        WaitUnreasonable = mean(WaitUnreasonable, na.rm = T),
        GenuineListening = mean(GenuineListening, na.rm = T),
        KnowledgeProductsServices = mean(KnowledgeProductsServices, na.rm = T),
        SolutionAddressedNeed = mean(SolutionAddressedNeed, na.rm = T),
        TellerMetNeed = mean(TellerMetNeed, na.rm = T),
        SpeedofTransaction = mean(SpeedofTransaction, na.rm = T),
        SincerelyThanking = mean(SincerelyThanking, na.rm = T),
        WelcomingEnvironment = mean(WelcomingEnvironment, na.rm = T),
        BranchAppearance = mean(BranchAppearance, na.rm = T),
        IdenitifyPersonalFinancialNeeds = mean(IdenitifyPersonalFinancialNeeds, na.rm = T))

#convert the dummies back to 1 and zero, use .49 as the cutoff
satBranch$WaitUnreasonable <- ifelse(satBranch$WaitUnreasonable > .10,1,0)

#convert the dummies back to 1 and zero, use .49 as the cutoff
satBranch$IdenitifyPersonalFinancialNeeds <- ifelse(satBranch$IdenitifyPersonalFinancialNeeds > .60,1,0)

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


Outcome1 <- "Score"


predictorNames <- names(satBranch)[names(satBranch) != Outcome1]
predictorNames <- predictorNames[-1]
predictorNames


#use  the function above to make the segment splits
splitData(satBranch, Outcome1, "trainDf", "testDf", .70)


#use  the function above to make the segment splits
set.seed(1000)
splitIndex <- createDataPartition(satBranch$Score, p=.7, list = FALSE, times = 1)

df = satBranch
tr = "trainDf"
te = "testDf"


#set up the model
objModel <- lm(log(Score) ~ WaitUnreasonable + 
                  GenuineListening + 
                  KnowledgeProductsServices +
                  SolutionAddressedNeed + 
                  TellerMetNeed + 
                  SpeedofTransaction + 
                  SincerelyThanking + 
                  WelcomingEnvironment + 
                  BranchAppearance + 
                  IdenitifyPersonalFinancialNeeds,
                  data = trainDf)

#check coef look for significance
summary(objModel)

#check for variable importance
plot(varImp(objModel, scale = T))



plot(trainDf$SpeedofTransaction, trainDf$Score, data = trainDf)



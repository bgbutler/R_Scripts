# this is for analyzing NPS Data

## @knitr loadLibs

# text mining and basic libraries
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(reshape2)
library(scales)
library(SnowballC)          # Porter's word stemming
library(tm)                 # Text Cleaning and Processing
library(RTextTools)         # Automatic Text Classification via Supervised Learning
library(stringi)            # For processing strings
library(wordcloud)          # Plot word clouds
library(RWeka)              # Machine Learning Java Library for Tokenization
library(tidytext)           # Additional Text Processing Library (new from RStudio)
library(tidyr)              
library(knitr)
library(dendextend)         # Making Dendograms
library(dendextendRcpp)     # Supporting package for dendextend
library(qdapRegex)          # function to remove words less than 3 letters
library(DT)
library(lime)


# Modeling Libraries
library(caret)
library(Metrics)
library(pROC)
library(naivebayes)         # Naive Bayes package
library(klaR)               # Alternative NB package
library(e1071)              # Windows specific modeling package best NB package
library(caTools)            # Utility function to help modeling split
library(randomForest)

# Manipulating Data
library(dplyr)

## @knitr loadData
# getwd()
setwd('N:/Bryan/Marketing')
dataURL <- 'NPS_Q22019.csv'

# read the files from csv
nps <- read.csv(dataURL, stringsAsFactors = F)


# use this theme for any ggplots
# standardizes the theme for plots in hanover colors
theme_bryan <- function () { 
  theme(axis.text.x = element_text(size = 14, color = 'darkgreen', angle = 0),
        axis.text.y = element_text(size = 14, color = 'darkgreen'),
        plot.title = element_text(color = 'darkgreen', size = 14, face = 'bold'),
        legend.position = 'right',
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 10, color='darkgreen', face = 'bold'),
        strip.background = element_rect(fill = 'gray69'))
}


#  plotting function
# hanover colors
hanover <- c('green', 'orange', 'light blue', 'grey', 'blue', 
             'light green', 'orangered', 'darkgray', 'darkgreen', 'royalblue4')



# function for dealing with strength
# map the words to values
convertComments <- function(x) {as.numeric(ifelse(x == 'Strongly Agree',  10, ifelse(
  x == 'Agree', 8, ifelse(x == 'Neutral', 5,ifelse(
    x == 'Disagree', 3,ifelse(x == 'Strongly Disagree', 1,NA))))))
}

# create the promoter field
# map the likely recommmend score to promoter
nps$Promoter <- ifelse(nps$LikelyRecommend > 8, 'Promoter', ifelse(
  nps$LikelyRecommend > 6 & nps$LikelyRecommend <9, 'Passive', 'Detractor'))



# make role bins from the many roles
nps$RoleGroup <- ifelse(nps$PrimaryRole == "Producer", "FrontLine", 
                        ifelse(nps$PrimaryRole == "Account Manager/CSR", "FrontLine",
                        ifelse(nps$PrimaryRole ==  "Administrative Assistant","Admin",
                        ifelse(nps$PrimaryRole ==  "Claims","FrontLine",
                        ifelse(nps$PrimaryRole ==  "LOB Manager","Admin",
                        ifelse(nps$PrimaryRole ==  "Marketing","MktgSales",
                        ifelse(nps$PrimaryRole ==  "Office Manager","Admin",
                        ifelse(nps$PrimaryRole ==  "Operations Manager","Admin",
                        ifelse(nps$PrimaryRole ==  "Principal","Exec Mgt",
                        ifelse(nps$PrimaryRole ==  "Producer","FrontLine",
                        ifelse(nps$PrimaryRole ==  "Sales Manager","MktgSales",
                        ifelse(nps$PrimaryRole ==  "Senior Executive","Exec Mgt",
                               "Admin"))))))))))))



# order role group
nps$RoleGroup <- ordered(nps$RoleGroup, levels = c("FrontLine", "Exec Mgt",
                                                   "Admin", "MktgSales"))

# order the promoter from lowest to highest
nps$Promoter <- ordered(nps$Promoter, levels = c("Detractor", 
                                                 "Passive", 
                                                 "Promoter"))


# convert the columns to numbers
# apply this function to the recommend Questions
# check the column numbers - ORDER MATTERS
for (i in seq(5, 23, by=2)) {nps[,i] <- convertComments(nps[,i])}


# get the main columns
# this is non-text data
questions <- nps %>% dplyr::select(EnrolledinCSC,
                            RoleGroup,
                            PrimaryRole,
                            Promoter,
                            Q2_RangeofProducts,
                            Q3_QuotePlatformEasy,
                            Q4_FieldSalesRepRegVisits,
                            Q5_UWResponsive,
                            Q6_ProcessHomeRepCostAccurate,
                            Q7_HomeEvalHassleFree,
                            Q8_BillingEasyUnderstand,
                            Q9_AgencySupCenExcellent,
                            Q10_ClaimServFairResponsive,
                            Q11_NBRenPricingStable)

# melt the data for plotting
npsMelt <- melt(questions, id = c('RoleGroup', 'EnrolledinCSC', 'PrimaryRole', 'Promoter'),
                 variable.name = 'Question',
                 value.name = 'Score')

# head(npsMelt)


## @knitr overallPlot
# questions overall
# makes a boxplot
g1 <- ggplot(npsMelt, aes(x = Question, y = Score,
                                fill = Question)) + coord_flip(expand = T) + 
  geom_boxplot()  +
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
  theme_bryan() + 
  theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(0,10, by = 2)) + 
  ggtitle('Overall Scores')


# violin plot
g2 <- ggplot(npsMelt, aes(x = Question, y = Score,
                          fill = Question)) + coord_flip(expand = T) + 
  geom_violin() +
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
  theme_bryan() + 
  theme(legend.position = 'none') + 
  theme(axis.text.y = element_blank()) + 
  scale_y_continuous(breaks = seq(0,10, by = 2)) + 
  ggtitle('Overall Scores Showing Density of Values')



# plot both plots on one to get a view
ggarrange(g1, g2, widths = c(5.5,3), nrow = 1, ncol = 2)




## @knitr statusPlot
# questions by status
datatable(nps %>% dplyr::count(Promoter, sort = T))
g3 <- ggplot(npsMelt, aes(x = Question, y = Score,
                          fill = Question)) + coord_flip(expand = T) + 
  geom_violin() +
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
  theme_bryan() + theme(legend.position = 'none') + labs(title='Overall Scores',size = 8, face = 'bold') +
  facet_wrap(~Promoter) + 
  scale_y_continuous(breaks = seq(0,10, by = 2)) +  
  ggtitle('Scores By Promoter')
g3



## @knitr rolePlot
# questions by role
# Get a view of claims by zip
datatable(nps %>% dplyr::count(RoleGroup, sort = T))

g4 <- ggplot(npsMelt, aes(x = Question, y = Score,
                          fill = RoleGroup)) +  
  geom_violin() + coord_flip(expand = T) +
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
  theme_bryan() + theme(legend.position = 'bottom') + labs(title='Overall Scores',size = 8, face = 'bold') +
  facet_wrap(~RoleGroup, nrow = 1) +
  scale_y_continuous(breaks = seq(0,10, by = 2)) +  
  ggtitle('Scores by Role')
g4


## @knitr cscPlot
# CSC enrollment
datatable(nps %>% dplyr::count(EnrolledinCSC, sort = T))


g3 <- ggplot(npsMelt, aes(x = Question, y = Score,
                          fill = EnrolledinCSC)) +  
  geom_violin() + coord_flip(expand = T) +
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
  theme_bryan() + theme(legend.position = 'bottom') + labs(title='Overall Scores',size = 8, face = 'bold') +
  facet_wrap(~EnrolledinCSC, nrow = 1) +
  scale_y_continuous(breaks = seq(0,10, by = 2)) + 
  ggtitle('Scores by Enrolled in CSC')
g3



## @knitr prepNPS
# get NPS data as a table
npsScore <- nps %>% select(BillingState, 
                           RoleGroup,
                           EnrolledinCSC,
                           LikelyRecommend)


npsScore$Promoter <- ifelse(npsScore$LikelyRecommend > 8,1,0)
npsScore$Detractor <- ifelse(npsScore$LikelyRecommend < 7,1,0)
npsScore$Passive <- ifelse(npsScore$LikelyRecommend > 6 & npsScore$LikelyRecommend < 9,1,0)

# function to group and summarize
# takes in a data frame and a variable for faceting
# summarizes by promoter
calc_Parts <- function(dat, fac_var){
      dat %>% group_by_(fac_var) %>%
      summarise(
      TotPromoters = sum(Promoter),
      TotDetractors = sum(Detractor),
      TotPassive = sum(Passive)
      )}

# function to make final calculations
# takes in a data fram and returns percentages
calc_NPS <- function(dat){
  dat$Sample <- dat$TotPromoters + dat$TotDetractors + dat$TotPassive
  dat$NPS <- (dat$TotPromoters/dat$Sample - dat$TotDetractors/dat$Sample) * 100
  return(dat)
}

# use state as facet variable
npsState <- calc_Parts(npsScore, "BillingState")
npsState <- calc_NPS(npsState)
# npsState

# use role as facet variable
npsRole <- calc_Parts(npsScore, "RoleGroup")
npsRole <- calc_NPS(npsRole)
# npsRole

# use CSC as facet variable
npsCSC <- calc_Parts(npsScore, "EnrolledinCSC")
npsCSC <- calc_NPS(npsCSC)
# npsCSC


# function to handle the data
# it takes two meltid variables and a dataframe
# returns a melted dataframe
meltData <- function(dat, meltid1, meltid2){
       melt(dat, id = c(meltid1, meltid2),
       variable.name = 'NPSComponent',
       value.name = 'Value')
}


# fuction to make the factors and filter out sample
# takes in a dataframe makes factors from variables
factorFilter <- function(dat){
  dat$NPSComponent <- factor(dat$NPSComponent, 
                             levels = c('Sample', 'TotPromoters','TotPassive','TotDetractors'))
  dat %>% filter(NPSComponent != 'Sample')
  return(dat)
}

# make text grouping df
# takes in a dataframe in a grouping variable
textGrouping  <- function(dat, groupVar){
  dat %>% group_by_(groupVar) %>%
    summarize(NPS = max(NPS),
              Value = max(Value))
}


#  plotting function
hanover <- c('green', 'orange', 'light blue', 'grey', 'blue')

# function to plot the NPS by grouping variable
# takes in a dataframe, main plotting variable, xvalue and a keyvalue for the title
plotNPS <- function(df, textdf, pltvar, textvar, key_grouping) {
  g3 <- ggplot(df, aes(x = reorder(pltvar, Value), y = Value)) +  
    geom_bar(aes(fill = NPSComponent), stat = 'identity') + coord_flip(expand = T) +
    theme_bryan() + theme(legend.position = 'bottom') + 
    ggtitle(paste('NPS Scores by ', key_grouping, sep = "")) + 
    scale_fill_manual(values = hanover) + 
    geom_text(data = textdf, aes(x = textvar, y = Value, label=paste("NPS ",round(NPS,1), sep = "")), 
              nudge_y = -4, color = 'darkgreen')
  return(g3)
}


## @knitr stateNPS
# create first dataset with billing state apply functions
meltState <- meltData(npsState, 'BillingState', 'NPS')
pltMelt <- meltState %>% filter(NPSComponent != 'Sample')
stateNPS <- textGrouping(meltState, 'BillingState')

# plot by BillingState
plotNPS(pltMelt, stateNPS, pltMelt$BillingState, stateNPS$BillingState, 'State')

## @knitr roleNPS
# check the role
meltRole <- meltData(npsRole, 'RoleGroup', 'NPS')
pltMelt <- factorFilter(meltRole)
pltMelt <- meltRole %>% filter(NPSComponent != 'Sample')
roleNPS <- textGrouping(meltRole, 'RoleGroup')


# plot by RoleGroup
# use the melted dataframe to plot the NPS by role group
plotNPS(pltMelt, roleNPS, pltMelt$RoleGroup, roleNPS$RoleGroup, 'RoleGroup')

## @knitr cscNPS
# CSC
meltCSC <- meltData(npsCSC, 'EnrolledinCSC', 'NPS')
pltMelt <- factorFilter(meltCSC)
pltMelt <- meltCSC %>% filter(NPSComponent != 'Sample')
cscNPS <- textGrouping(meltCSC, 'EnrolledinCSC')


# plot by CSC enrollment
# use the melted dataframe to plot the NPS by CSC enrollment
plotNPS(pltMelt, cscNPS, pltMelt$EnrolledinCSC, cscNPS$EnrolledinCSC, 'CSC Enrollment')

## @knitr weightNPS
# Weight States
# calculated the weighted NPS by states
# this is the NPS for the data
weightNPS <- npsState
totalSample <- as.numeric(weightNPS %>% summarise(sum(Sample)))
weightNPS$Weight <- weightNPS$Sample/totalSample
weightNPS$WeightedNPS <- weightNPS$Weight * weightNPS$NPS

newNPS <- as.numeric(weightNPS %>% summarise(sum(WeightedNPS)))



## @knitr modelNPS
# get the matrix for modeling
# this gets the questions with numerical values
modelNPS <- nps[,c(4,5,7,9,11,13,15,17,19,21,23)]
# colnames(modelNPS)

# make dummies for CSC and RoleGroup
modelNPS$FrontLine <- ifelse(nps$RoleGroup == 'FrontLine',1,0)
modelNPS$CSC <- ifelse(nps$EnrolledinCSC == 'Yes',1,0)

# fill in NAs with Row means
# for NA's for each respondant, use row means
# this represents the respondant better than columns means
data <- modelNPS[,c(2:10)]

# get the NAs
k <- which(is.na(data), arr.ind = TRUE)
# fill NA with row means
data[k] <- rowMeans(data, na.rm=TRUE)[k[,1]]

# make a clean dataframe
npsModel <- cbind(modelNPS$LikelyRecommend, data, modelNPS[,c(11,12)])
# colnames(npsModel)

# rename the outcome and drop the odd name
# make the outcome a factor for logistic regression
npsModel$LikelyRecommend <- as.factor(ifelse(npsModel$`modelNPS$LikelyRecommend` > 8,'Yes','No'))
npsModel <- npsModel[,-1]

# check the NAs
# colSums(is.na(npsModel))

# drop the NAs exist as whole rows (2)
npsModel <- drop_na(npsModel)


#this is a function only and is called later in the analysis
# the function takes in a dataframe, outcome name, name for training set (tr)
# name for testing set(te) and split ratio (p)
# returns a training and testing set to the global environment
splitData <- function(df,outcomeName, tr, te, p){
  set.seed(1000)
  splitIndex <- createDataPartition(df[,outcomeName], 
                                    p=p, list = FALSE, times = 1)
  trainDf <- df[splitIndex,]
  testDf <- df[-splitIndex,]
  assign(tr, trainDf, envir = .GlobalEnv)
  assign(te, testDf, envir = .GlobalEnv)
}

# get the outcome variable
outcome <- 'LikelyRecommend'

# get the predictor names by excluding the outcome variable
predictorNames <- names(npsModel)[names(npsModel) != outcome]
# predictorNames

#use  the function above to make the segment splits
splitData(npsModel,outcome, "trainDf", "testDf", .70)


#set up the model
objControl <- trainControl(method = 'cv',
                           number = 5,
                           savePredictions = TRUE,
                           returnResamp = 'none',
                           summaryFunction = twoClassSummary,
                           classProbs = T)

# fit the model to logistic regression
objModel <- train(trainDf[,predictorNames], trainDf[,outcome],
                  method = 'glm',
                  family = "binomial",
                  metric = "ROC",
                  trControl = objControl)


## @knitr glmOut
#check coef look for significance
summary(objModel)

#check for variable importance
plot(varImp(objModel, scale = T))


#make predictions
predictionsRaw <- predict(object = objModel, testDf[,predictorNames], type = "raw")
# head(predictionsRaw)
print(postResample(pred=predictionsRaw, obs = as.factor(testDf[,outcome])))

#check the accuracy of the model with a confusion matrix
confusionMatrix(predictionsRaw, testDf$LikelyRecommend)

# creates and prints the AUC
predictionsProb <- predict(object = objModel, testDf[,predictorNames], type = 'prob')
auc <- roc(ifelse(testDf[,outcome] == "Yes", 1,0), predictionsProb[[1]])
auc


## @knitr redGLM
# get some sample results
# get the average
# newWords <- newWords [! newWords %in% keep]

# if columns are insignificant, then drop them
dropColumns <- c('Q2_RangeofProducts',
                 'Q6_ProcessHomeRepCostAccurate',
                 'Q7_HomeEvalHassleFree',
                 'Q8_BillingEasyUnderstand',
                 'Q10_ClaimServFairResponsive',
                 'FrontLine')


# reset the predictor names to the new set, exclude columns to drop
newPredictors <- predictorNames[! predictorNames %in% dropColumns]
newPredictors


#use  the function above to make the segment splits
splitData(npsModel,outcome, "trainDf", "testDf", .70)


#set up the model
objControl <- trainControl(method = 'cv',
                           number = 5,
                           savePredictions = TRUE,
                           returnResamp = 'none',
                           summaryFunction = twoClassSummary,
                           classProbs = T)

# train the new reduced model
objModel <- train(trainDf[,newPredictors], trainDf[,outcome],
                  method = 'glm',
                  family = "binomial",
                  metric = "ROC",
                  trControl = objControl)


## @knitr redGLMOut
#check coef look for significance
summary(objModel)

# check for variable importance
plot(varImp(objModel, scale = T))


# make predictions
predictionsRaw <- predict(object = objModel, testDf[,newPredictors], type = "raw")
# head(predictionsRaw)
print(postResample(pred=predictionsRaw, obs = as.factor(testDf[,outcome])))

# check the accuracy of the model
# use a confusion matrix
confusionMatrix(predictionsRaw, testDf$LikelyRecommend)

# make the AUC calculation
predictionsProb <- predict(object = objModel, testDf[,newPredictors], type = 'prob')
auc <- roc(ifelse(testDf[,outcome] == "Yes", 1,0), predictionsProb[[1]])
auc


## @knitr sensitivity
## analyze some cases
# get the average person
# df = mydata[,!(names(mydata) %in% drop)]

trainMat <- trainDf[,!(names(trainDf) %in% dropColumns)]
# str(trainMat)

avgNPS <- trainDf %>% dplyr::group_by(LikelyRecommend) %>%
          summarise(
          Q2_RangeofProducts = mean(Q2_RangeofProducts),
          Q3_QuotePlatformEasy = mean(Q3_QuotePlatformEasy),
          Q4_FieldSalesRepRegVisits = mean(Q4_FieldSalesRepRegVisits),
          Q5_UWResponsive = mean(Q5_UWResponsive),
          Q6_ProcessHomeRepCostAccurate = mean( Q6_ProcessHomeRepCostAccurate),
          Q7_HomeEvalHassleFree = mean(Q7_HomeEvalHassleFree),
          Q8_BillingEasyUnderstand = mean(Q8_BillingEasyUnderstand),
          Q9_AgencySupCenExcellent = mean(Q9_AgencySupCenExcellent),
          Q10_ClaimServFairResponsive = mean(Q10_ClaimServFairResponsive),
          Q11_NBRenPricingStable = mean(Q11_NBRenPricingStable))
str(avgNPS)


### check the avg value
avgNo <- predict(object = objModel, avgNPS[1,newPredictors], type = 'prob')
print('Probability of No using average scores for a no: ')
avgNo

avgYes <- predict(object = objModel, avgNPS[2,newPredictors], type = 'prob')
print('Probability of Yes using average scores for a Yes: ')
avgYes



# increase UW responsive
uwPlus <- avgNPS[1,]
print("Average Scores for a 'No'")
str(uwPlus)
UWBase <- predict(object = objModel, uwPlus[1,newPredictors], type = 'prob')
print('New Probabilities')
UWBase

uwPlus1 <- avgNPS[1,]
uwPlus1$Q5_UWResponsive <- uwPlus1$Q5_UWResponsive + 1
print("Increase UW Responseve score by 1 for Avg No")
str(uwPlus1)
# check the avg value
probPlus1 <- predict(object = objModel, uwPlus1[1,newPredictors], type = 'prob')
print('New Probabilities after increasing UW score by 1')
probPlus1


uwPlus2 <- avgNPS[1,]
uwPlus2$Q5_UWResponsive <- uwPlus1$Q5_UWResponsive + 2
print("Increase UW Responseve score by 2 for Avg No")
str(uwPlus2)
# check the avg value
probPlus2 <- predict(object = objModel, uwPlus2[1,newPredictors], type = 'prob')
print('New Probabilities after increasing UW score by 2')
probPlus2


## @knitr marginalRole
options(digits = 2)

frontlineNPS <- trainDf %>% dplyr::group_by(LikelyRecommend, FrontLine) %>%
  summarise(
    Q2_RangeofProducts = mean(Q2_RangeofProducts),
    Q3_QuotePlatformEasy = mean(Q3_QuotePlatformEasy),
    Q4_FieldSalesRepRegVisits = mean(Q4_FieldSalesRepRegVisits),
    Q5_UWResponsive = mean(Q5_UWResponsive),
    Q6_ProcessHomeRepCostAccurate = mean( Q6_ProcessHomeRepCostAccurate),
    Q7_HomeEvalHassleFree = mean(Q7_HomeEvalHassleFree),
    Q8_BillingEasyUnderstand = mean(Q8_BillingEasyUnderstand),
    Q9_AgencySupCenExcellent = mean(Q9_AgencySupCenExcellent),
    Q10_ClaimServFairResponsive = mean(Q10_ClaimServFairResponsive),
    Q11_NBRenPricingStable = mean(Q11_NBRenPricingStable))


frontlineNPS$FrontLine <- ifelse(frontlineNPS$FrontLine == 1, 
                                 "Yes", "No")


# str(frontlineNPS)
glimpse(frontlineNPS)

### check the avg value
# No, non-FL
avgnFlNo <- predict(object = objModel,frontlineNPS[1,newPredictors], type = 'prob')
print('Probability of No using average scores for a Non-FrontLine: ')
avgnFlNo


avgFlNo <- predict(object = objModel,frontlineNPS[2,newPredictors], type = 'prob')
print('Probability of No using average scores for a FrontLine: ')
avgFlNo


avgnFlYes <- predict(object = objModel,frontlineNPS[3,newPredictors], type = 'prob')
print('Probability of Yes using average scores for a Non-FrontLine: ')
avgnFlYes

avgFlYes <- predict(object = objModel,frontlineNPS[4,newPredictors], type = 'prob')
print('Probability of Yes using average scores for a FrontLine: ')
avgFlYes



frontlineNPS$LikelyRecommend <- as.character(frontlineNPS$LikelyRecommend)

flMelt <- melt(frontlineNPS, id = c('LikelyRecommend', 'FrontLine'),
     variable.name = 'Question',
     value.name = 'Value')

options(digits = 2)
datatable(flMelt %>% dplyr::group_by(LikelyRecommend, FrontLine) %>% 
            summarise(AvgScore = mean(round(Value,2))))





## @knitr LIME
# Run lime() on training set
RedTrainDf <- trainDf[,newPredictors]

explainer <- lime::lime(
  x              = RedTrainDf, 
  model          = objModel, 
  bin_continuous = FALSE)

redTestDf <- testDf[,newPredictors]

# Run explain() on explainer
explanation <- lime::explain(
  redTestDf[1:6,], 
  explainer    = explainer, 
  n_labels     = 1, 
  n_features   = 6,
  kernel_width = 0.75)


print('Scores for case 1')
redTestDf %>% 
  slice(1) %>%
  glimpse()



plot_features(explanation) +
  labs(title = "LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 6 Cases Shown")






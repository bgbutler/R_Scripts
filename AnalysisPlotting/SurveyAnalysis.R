


## @knitr loadLibraries
# analyze survey
# set java for rJava
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_251\\jre')



library(ggplot2)
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
library(plotly)

library(qdapRegex)          # function to remove words less than 3 letters
library(readr)
library(stringr)
library(textclean)
library(forcats)
library(topicmodels)


# Modeling Libraries
library(caret)
library(Metrics)
library(pROC)
library(naivebayes)         # Naive Bayes package
library(klaR)               # Alternative NB package
library(e1071)              # Windows specific modeling package best NB package
library(caTools)            # Utility function to help modeling split
library(randomForest)
library(RColorBrewer)
library(GGally)
library(dplyr)
library(lime)

## @knitr getData
# set the wd
setwd('C:/Users/bbutler/Documents/DataScience/NLP')


# get the data
# list.files()
allData = read_csv('BottomLineSurvey10282020Red.csv')





## @knitr cleanData
# remove na columns Lockbox, Lockbox2, wires
data = select(allData, -c(Lockbox, Lockbox1, Lockbox2, Wires, Wires1, Wires2, Alert))


# get relevant columns
data = select(data, 1:39)


# date clean up
data$CompletedDate =  gsub( " .*$", "", data$CompletedDate)
data$CompletedDate = as.Date(data$CompletedDate, format = '%m/%d/%Y')

data$LastLogOn = as.Date(data$LastLogOn, format = '%m/%d/%Y')
data$CreationDate = as.Date(data$CreationDate, format = '%m/%d/%Y')

data$Started =  gsub( " .*$", "", data$Started)
data$Started = as.Date(data$Started, format = '%m/%d/%Y')

# change the segment names
data$MarketSegment = ifelse(data$MarketSegment == 'SMB', 'Small Biz', 'Commercial')


## @knitr dataFunctions
### FUNCTIONS TO CONVERT DATA
# function to convert words to numbers
convertComments = function(x) {x = ifelse(x == 'Regularly', 7, 
                              ifelse(x == 'Occasionally', 4, 
                              ifelse(x == 'Rarely', 2,
                              ifelse(x == 'Never', 0, NA))))
                  return(x)
  }


# satisfaction to scores
convertSat = function(x) {x = ifelse(x == 'Extremely Satisfied', 7, 
                                          ifelse(x == 'Satisfied', 5, 
                                          ifelse(x == 'Neutral', 4,
                                          ifelse(x == 'Dissatisfied', 3,
                                          ifelse(x == 'Extremely Dissatisfied', 0, NA)))))
return(x)
}


# convert yes no questions
convertYN = function(x) {
  ifelse(x == 'Yes', 1,0)
}





# convert hms to a portion of hours
hhmmss2dec <- function(x) {
  as.character(x)
  xlist <- strsplit(x,split=":")
  h <- as.numeric(sapply(xlist,"[",1))
  m <- as.numeric(sapply(xlist,"[",2))
  s <- as.numeric(sapply(xlist,"[",3))
  xdec <- (h+(m/60)+(s/60/60))*60
  return(xdec)
}


# function that orders responses
order_response <- function(x) {
  ordered(x, levels = c(NA, 'Never', 'Rarely', 'Occasionally', 'Regularly'))
  
}


# order the satisfaction also
order_sat <- function(x){
  ordered(x, levels = c('Extremely Satisfied', 'Satisfied', 'Neutral','Dissatisfied', 'Extremely Dissatisfied', NA))
}


# create a promoter function
promoter_func <- function(x, high, low) {
  ifelse(x > high, 'Promoter',
         ifelse(x < low, 'Detractor', 'Passive'))
}


# create promoter and other
promoter_func1 <- function(x, highCutoff) {
  ifelse(x > highCutoff, 'Promoter', 'Other')
}


# create a high sat function for OSAT
osat_func <- function(x, high, middle, low) {
  ifelse(x > high, 'High Sat',
         ifelse(
           x > middle & x < high , 'Satisfied',
           ifelse(
           x > low & x < middle, 'Neutral', 'Dissatisfied'
         )))
}

order_osat <- function(x){
  ordered(x, levels = c('High Sat', 'Satisfied', 'Neutral','Dissatisfied'))
}
  

## @knitr plotFunctions
######   FUNCTIONS TO HANDLE DATA AND PLOTTING
# select a palette
easternPal = c('lightsteelblue1', 'royalblue4', 'sienna1', 'skyblue')


# use this theme
theme_bryan <- function () { 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 0),
        legend.position = 'bottom',
        axis.text.y = element_text(color = 'blue'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = 'darkblue', size = 10, face = 'bold'),
        strip.background = element_rect(fill = 'light blue'))
}



# function to plot 10 point scale
osatPlot10 <- function(df=data, segment=MarketSegment, y) {
  g = ggplot(df, aes(x = OSAT, y = y, fill = segment)) +
    geom_violin() + scale_fill_manual(values = easternPal) + 
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
    stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") +
    theme_bryan()
  return(g)
  
}




# plot function on 7 point scale
geomPlot7 <- function(df, x, segment) {
  g = ggplot(df, aes(x = x, y = segment, fill = segment)) +
    geom_violin() + scale_fill_manual(values = easternPal) + 
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7)) +
    stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") +
    theme_bryan()
  return(g)
}



# multi-dimensional plot inputs: x = Effiency, y = MarketSegment, fill = MarketSegment, facet = LoanPay
multi_plot <- function(df, x, y, fill, facet) {
  df1 <- data.frame(x = x, y = y, fill = fill, facet = facet) 
  g = ggplot(df1, aes(x = x, y = y, fill = fill)) + 
    geom_violin() + scale_fill_manual(values = easternPal) + 
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7)) + facet_wrap(~facet) + 
    stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    theme_bryan()
  return(g)
}


# multi-dimensional plot inputs: x = Effiency, y = MarketSegment, fill = MarketSegment, facet = LoanPay
# test the multi_plot
# multi_plot(df = data, x = data$Efficiency, y = data$MarketSegment, fill = data$MarketSegment, facet = data$LoanPay)
multi_plot10 <- function(df, x, y, fill, facet) {
  df1 <- data.frame(x = x, y = y, fill = fill, facet = facet) 
  g = ggplot(df1, aes(x = x, y = y, fill = fill)) + 
    geom_violin() + scale_fill_manual(values = easternPal) + 
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) + facet_wrap(~facet) + 
    stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    theme_bryan()
  return(g)
}










## @knitr checkCorrelations
###### Begin analysis
# make a nice pairplot
pairs <-  data %>% dplyr::select(OSAT, Efficiency, Effort, Emotion, MarketSegment)

# use the code below for better colors
# ggpairs(pairs, aes(color = MarketSegment, alpha = 0.5)) + scale_fill_manual(values = easternPal)

p <- ggpairs(pairs, aes(color = MarketSegment, alpha = .6))

for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values = c('blue', 'orange')) + 
      scale_color_manual(values = c('blue', 'darkorange'))
  }
}

p


## @knitr checkCorrelations1
# pairplots
numbers <-  data %>% select(OSAT, TotalAccounts, NumberACHCompanies, NumberUsers, MarketSegment)
p <- ggpairs(numbers, aes(color = MarketSegment, alpha = .6))

for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values = c('blue', 'orange')) + 
      scale_color_manual(values = c('blue', 'darkorange'))
  }
}

p





## @knitr osatPlots
# order the OSAT plots
#  Apply the functions
# order the responses
# df[cols] <- lapply(df[cols], FUN)
# wifi[4:9] <- lapply(wifi[4:9], A)

data[23:28] = lapply(data[23:28], order_response)

data[29:34] = lapply(data[29:34], order_sat)


# show the overall responses
# make a plot to show n's

## @knitr surveyResponses
g = ggplot(data, aes(x = MarketSegment, fill = MarketSegment))+
  geom_bar(stat = 'count') + scale_fill_manual(values = easternPal) +
  scale_y_continuous(breaks = c(0,25,50,75,100,125,150)) +
  theme_bryan()

ggplotly(g, tooltip = c("x","y")) %>% layout(showlegend = TRUE)


## @knitr osatViolin
# OSAT plots
osatPlot10(data,segment = data$MarketSegment, y = data$MarketSegment)



## @knitr osatJoin
# join community
osatPlot10(data,segment = data$MarketSegment, y = data$JoinUserCommunity)

## @knitr osatAcctTransfer
# Account Transfers
osatPlot10(df = data, y = data$AccountTransfers, segment = data$MarketSegment)


## @knitr osatAch
# ACH Origination
osatPlot10(df = data, y = data$ACHOrigination, segment = data$MarketSegment)

## @knitr osatBal
# BalTransctRpt
osatPlot10(df = data, y = data$BalTransctRpt, segment = data$MarketSegment)

## @knitr osatBillPay
# Bill Pay
osatPlot10(df = data, y = data$BillPay, segment = data$MarketSegment)

## @knitr osatPositive
# positive pay
osatPlot10(df = data, y = data$PositivePay, segment = data$MarketSegment)

## @knitr osatLoans
# Loans
osatPlot10(df = data, y = data$Loans, segment = data$MarketSegment)



# get osat data
osatData = data %>% select(
  RespondentId,
  OSAT,
  ACHOrigination,
  BalTransctRpt,
  BillPay,
  PositivePay,
  Loans,
  MarketSegment
)


osatMelt = melt(osatData, id = c('RespondentId', 'MarketSegment', 'OSAT'),
                variable.name = 'Question',
                value.name = 'Frequency')

# reset the rankings
order_response(osatMelt$Frequency)


multi_plot10 <- function(df, x, y, fill, facet) {
  df1 <- data.frame(x = x, y = y, fill = fill, facet = facet) 
  g = ggplot(df1, aes(x = x, y = y, fill = fill)) + 
    geom_violin() + scale_fill_manual(values = easternPal) + 
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) + facet_wrap(~facet) + 
    stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    theme_bryan()
  return(g)
}



g = ggplot(osatMelt, aes(x = Frequency, fill = MarketSegment)) + 
  geom_bar(stat = 'count', alpha = 0.8) + scale_fill_manual(values = easternPal) + 
  facet_wrap(~Question) + 
  theme_bryan()
g


g = ggplot(osatMelt, aes(x = OSAT, fill = Frequency)) + 
  geom_bar(stat = 'count') + scale_fill_manual(values = easternPal) + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
  facet_wrap(~Question) + 
  theme_bryan()
g






## @knitr efficiencyPlots
# questions on Efficiency
multi_plot(df = data, x = data$Efficiency, y = data$MarketSegment, fill = data$MarketSegment, facet = data$Loans1)


## @knitr effAccount
multi_plot(df = data, x = data$Efficiency, y = data$MarketSegment, fill = data$MarketSegment, facet = data$AccountTransfers1)

## @knitr effAch
multi_plot(df = data, x = data$Efficiency, y = data$MarketSegment, fill = data$MarketSegment, facet = data$ACHOrigination1)

## @knitr effTrans
multi_plot(df = data, x = data$Efficiency, y = data$ACHOrigination, fill = data$MarketSegment, facet = data$BalTransactRpt1)

## @knitr effBillPay
multi_plot(df = data, x = data$Efficiency, y = data$ACHOrigination, fill = data$MarketSegment, facet = data$BillPay1)


## @knitr effortPlots
# Effort
multi_plot(df = data, x = data$Effort, y = data$LoanPay, fill = data$MarketSegment, facet = data$Loans1)

## @knitr effortXfer
multi_plot(df = data, x = data$Effort, y = data$MarketSegment, fill = data$MarketSegment, facet = data$AccountTransfers1)

## @knitr effortAch
multi_plot(df = data, x = data$Effort, y = data$MarketSegment, fill = data$MarketSegment, facet = data$ACHOrigination1)

## @knitr effortTrans
multi_plot(df = data, x = data$Effort, y = data$ACHOrigination, fill = data$MarketSegment, facet = data$BalTransactRpt1)

## @knitr effortBillPay
multi_plot(df = data, x = data$Effort, y = data$AccountTransfers, fill = data$MarketSegment, facet = data$BillPay1)


## @knitr emotionPlots
# Emotion
multi_plot(df = data, x = data$Emotion, y = data$LoanPay, fill = data$MarketSegment, facet = data$Loans1)

## @knitr emotionAcct
multi_plot(df = data, x = data$Emotion, y = data$MarketSegment, fill = data$MarketSegment, facet = data$AccountTransfers1)

## @knitr emotionAch
multi_plot(df = data, x = data$Emotion, y = data$MarketSegment, fill = data$MarketSegment, facet = data$ACHOrigination1)

## @knitr emotionTrans
multi_plot(df = data, x = data$Emotion, y = data$ACHOrigination, fill = data$MarketSegment, facet = data$BalTransactRpt1)

## @knitr emotionBillPay
multi_plot(df = data, x = data$Emotion, y = data$AccountTransfers, fill = data$MarketSegment, facet = data$BillPay1)





## @knitr effortSat
#### EFFORT and SATISFACTION LEVELS
# subset the data for melting
effort1 = select(data, RespondentId, Effort, AccountTransfers1, ACHOrigination1, BalTransactRpt1, BillPay1, PositivePay1, Loans1, MarketSegment)


# score the frequency
# # df[cols] <- lapply(df[cols], FUN)


effort1[3:8] = lapply(effort1[3:8], convertSat)


effortMelt = melt(effort1, id = c('RespondentId', 'MarketSegment'),
                variable.name = 'Question',
                value.name = 'Score')


multi_plot(df = effortMelt, x = effortMelt$Score, y = effortMelt$Question,
           fill = effortMelt$MarketSegment, facet = effortMelt$MarketSegment)


## @knitr efficiencySat
####### EFFICIENCY ##########
efficiency1 = select(data, RespondentId, Efficiency, AccountTransfers1, ACHOrigination1, BalTransactRpt1, BillPay1, PositivePay1, Loans1, MarketSegment)

efficiency1[3:8] = lapply(efficiency1[3:8], convertSat)


efficiencyMelt = melt(efficiency1, id = c('RespondentId', 'MarketSegment'),
                  variable.name = 'Question',
                  value.name = 'Score')


multi_plot(df = efficiencyMelt, x = efficiencyMelt$Score, y = efficiencyMelt$Question,
           fill = efficiencyMelt$MarketSegment, facet = efficiencyMelt$MarketSegment)


## @knitr emotionSat
###### EMOTION ###########
emotion1 = select(data, RespondentId, Emotion, AccountTransfers1, ACHOrigination1, BalTransactRpt1, BillPay1, PositivePay1, Loans1, MarketSegment)

emotion1[3:8] = lapply(emotion1[3:8], convertSat)


emotionMelt = melt(emotion1, id = c('RespondentId', 'MarketSegment'),
                      variable.name = 'Question',
                      value.name = 'Score')


multi_plot(df = emotionMelt, x = emotionMelt$Score, y = emotionMelt$Question,
           fill = emotionMelt$MarketSegment, facet = emotionMelt$MarketSegment)







## @knitr modelOsat
# model OSAT
# get a susbset

osat = data %>% dplyr::select(OSAT, MarketSegment, TotalAccounts, NumberACHCompanies, NumberUsers,
                       AccountTransfers1, ACHOrigination1, BalTransactRpt1, BillPay1, PositivePay1, Loans1)


# convert words to values
#apply the conversion

for (i in 6:11){
  osat[,i] <- convertSat(osat[,i])
}


# lots of NA - fill in with ROW mean
#fill in the NAs with the row means
osatData = as.matrix(osat[,c(3:11)])

k = which(is.na(osatData), arr.ind = TRUE)
osatData[k] = rowMeans(osatData, na.rm=TRUE)[k[,1]]


osatData = as.data.frame(osatData)


# rebind the osat score
osatData$OSAT = osat$OSAT
osatData$MarketSegment = osat$MarketSegment


# round to all integer whole numbers
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}


# apply the rounding
osatClean = round_df(osatData, digits = 0)

# check for correlations
p <- ggpairs(osatClean, aes(color = MarketSegment, alpha = 1))


for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values = c('blue', 'orange')) + 
      scale_color_manual(values = c('blue', 'darkorange'))
  }
}

# p



# make a highsat category
osatClean$HighSat = ifelse(osatClean$OSAT > 8, "HighSat", "LowSat")

# need a dummy for MarketSegment
osatClean$SMB = ifelse(osatClean$MarketSegment == 'SMB', 1,0)

# reorganize into final data set
osatModel = select(osatClean,
                    HighSat,
                    SMB,
                    TotalAccounts,
                    NumberACHCompanies,
                    NumberUsers,
                    AccountTransfers1,
                    BalTransactRpt1,
                    BillPay1,
                    PositivePay1,
                    Loans1)

# make highsat a factor
osatModel$HighSat = as.factor(osatModel$HighSat)

# te = name of testing dataset, p = partition ratio (usually .70)
# this is a function only and is called later in the analysis
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
outcome <- 'HighSat'

# get the predictor names by excluding the outcome variable
predictorNames = names(osatModel)[names(osatModel) != outcome]
# predictorNames


#use  the function above to make the segment splits
splitData(osatModel,outcome, "trainDf", "testDf", .70)

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



#check coef look for significance
summary(objModel)

## @knitr modelVarImp
#check for variable importance
plot(varImp(objModel, scale = T))

#make predictions
predictionsRaw <- predict(object = objModel, testDf[,predictorNames], type = "raw")
head(predictionsRaw)

print(postResample(pred=predictionsRaw, obs = as.factor(testDf[,outcome])))


## @knitr modelMatrix
#check the accuracy of the model with a confusion matrix
confusionMatrix(predictionsRaw, testDf$HighSat)


## @knitr modelAuc
# creates and prints the AUC
predictionsProb <- predict(object = objModel, testDf[,predictorNames], type = 'prob')
auc <- roc(ifelse(testDf[,outcome] == "HighSat", 1,0), predictionsProb[[1]])
auc

## @knitr reducedModel
##### Make a reduced model
predictorNames1 = c('TotalAccounts', 'BalTransactRpt1', 'Loans1', 'AccountTransfers1')

#set up the model
objControl <- trainControl(method = 'cv',
                         number = 5,
                         savePredictions = TRUE,
                         returnResamp = 'none',
                         summaryFunction = twoClassSummary,
                         classProbs = T)

# fit the model to logistic regression
objModel <- train(trainDf[,predictorNames1], trainDf[,outcome],
                  method = 'glm',
                  family = "binomial",
                  metric = "ROC",
                  trControl = objControl)


## @knitr redModelSummary
#check coef look for significance
summary(objModel)


## @knitr redModelVarImp
#check for variable importance
plot(varImp(objModel, scale = T))


#make predictions
predictionsRaw <- predict(object = objModel, testDf[,predictorNames1], type = "raw")
# head(predictionsRaw)

print(postResample(pred=predictionsRaw, obs = as.factor(testDf[,outcome])))

## @knitr redModelMatrix
#check the accuracy of the model with a confusion matrix
confusionMatrix(predictionsRaw, testDf$HighSat)

## @knitr redModelAuc
# creates and prints the AUC
predictionsProb <- predict(object = objModel, testDf[,predictorNames1], type = 'prob')
auc <- roc(ifelse(testDf[,outcome] == "HighSat", 1,0), predictionsProb[[1]])
auc


## @knitr modelValues
# get average values
avgOsat <- trainDf %>% group_by(HighSat) %>%
  summarise(
    BalTransactRpt1 = mean(BalTransactRpt1),
    Loans1 = mean(Loans1),
    TotalAccounts = mean(TotalAccounts),
    AccountTransfers1 = mean(AccountTransfers1))
str(avgOsat)

## @knitr redModelHigh
# avg high Sat
avgHigh = predict(object = objModel, avgOsat[1,predictorNames1], type = 'prob')
print('Probability of High Sat using average scores for a High Rating: ')
avgHigh

## @knitr redModelLow
# avg low sat
avgLow <- predict(object = objModel, avgOsat[2,predictorNames1], type = 'prob')
print('Probability of Low Sat using average scores for a Low Rating: ')
avgLow


## @knitr lime
# Run lime() on training set
trainDf1 <- trainDf[,predictorNames1]

# make the explainer
explainer <- lime::lime(
  x              = trainDf1, 
  model          = objModel, 
  bin_continuous = FALSE)

testDf1 <- testDf[,predictorNames1]

# Run explain() on explainer
explanation <- lime::explain(
  testDf1[1:6,], 
  explainer    = explainer, 
  n_labels     = 1, 
  n_features   = 6,
  kernel_width = 0.75)


# print('Scores for case 1')
# testDf1 %>% 
#   slice(1) %>%
#   glimpse()



plot_features(explanation) +
  labs(title = "LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 6 Cases Shown")



## @knitr textAnalytics
###### Start Text analytics
## Subset to text fields and outcomes

textDf = data %>% select(OSAT,
                         Efficiency,
                         Effort,
                         Emotion,
                         ExplainRating,
                         MoreEffortless,
                         NeedsImprovement,
                         JoinUserCommunity)

# convert to lowercase
textDf$ExplainRating <- tolower(textDf$ExplainRating)
textDf$MoreEffortless <- tolower(textDf$MoreEffortless)
textDf$NeedsImprovement <- tolower(textDf$NeedsImprovement)


# expand contractions
textDf$ExplainRating <- qdap::replace_contraction(textDf$ExplainRating, ignore.case = TRUE, sent.cap = FALSE)

stop_words <- stopwords("english")
keep <- c("no", "more", "not", "can't", "cannot", "isn't", "aren't", "wasn't",
          "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't")
newWords <- stop_words [! stop_words %in% keep]


# myDict = qdapDictionaries::contractions

# small words function
smallWds <- function(x) {rm_nchar_words(x, "1,2")}

# clean possessives
possessive <- function (x) {gsub("'s", " ", x)}

# clean and process
text <- VCorpus(VectorSource(textDf$ExplainRating))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(smallWds))


# make the clustering functions
create_distance = function(df){
  as.dist(1 - cov2cor(cov(df[,c(2:words)], 
                          method = "pearson", 
                          use = "pairwise.complete.obs")))
}


# make clustering function
create_cluster = function(df, k, title, max){
  options(scipen = 999)
  par(mar = c(2.5, 0.5, 1.0, 7))
  d <- dist(df, method = "euclidean")
  hc <- hclust(d)
  dend <- d %>% hclust %>% as.dendrogram
  labels_cex(dend) <- 1.25
  dend %>% 
    color_branches(k=k) %>%
    color_labels() %>%
    highlight_branches_lwd(4) %>% 
    plot(horiz=TRUE, main = title, axes = T, xlim = c(max,0))
}

## @knitr clusterOSAT
# start cluster analysis
# requires DTM
# make the DTM
DTM <- DocumentTermMatrix(text)
dtmDF <- as.data.frame(as.matrix(DTM))
                       
# need to be careful of the column name
dtmDF$OSAT <- as.factor(as.character(textDf$OSAT))

# clean up OSAT column
clusDf <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF$OSAT), sum)

colnames(clusDf)[1] = 'OSAT'


# clustering works off row names
rownames(clusDf) <- clusDf[,1]

# Create a correlation matrix and make it a distance matrix
words <- length(clusDf) 

# create_distance(clusDf)





create_cluster(clusDf, k = 4, "Word Clusters by OSAT Score", max = 50)

## @knitr clusterEffort
#######    Effort
# need to be careful of the column name
# start with a clean dtmDF
dtmDF <- as.data.frame(as.matrix(DTM))

dtmDF$Effort <- as.factor(as.character(textDf$Effort))

# clean up column
clusDf <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF$Effort), sum)

colnames(clusDf)[1] = 'Effort'


# clustering works off row names
rownames(clusDf) <- clusDf[,1]


# create_distance(clusDf)

# make the plot
create_cluster(clusDf, k = 4, "Word Clusters by Effort Score", max = 75)



## @knitr clusterEfficiency
#### Efficiency
# start with a clean dtmDF
dtmDF <- as.data.frame(as.matrix(DTM))

dtmDF$Efficiency <- as.factor(as.character(textDf$Efficiency))

# clean up column
clusDf <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF$Efficiency), sum)

colnames(clusDf)[1] = 'Efficiency'


# clustering works off row names
rownames(clusDf) <- clusDf[,1]


# create_distance(clusDf)

# make the plot
create_cluster(clusDf, k = 4, "Word Clusters by Efficiency Score", max = 75)


## @knitr clusterEmotion
#### Emotion
# start with a clean dtmDF
dtmDF <- as.data.frame(as.matrix(DTM))

dtmDF$Emotion <- as.factor(as.character(textDf$Emotion))

# clean up column
clusDf <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF$Emotion), sum)

colnames(clusDf)[1] = 'Emotion'


# clustering works off row names
rownames(clusDf) <- clusDf[,1]


# create_distance(clusDf)

# make the plot
create_cluster(clusDf, k = 4, "Word Clusters by Emotion Score", max = 75)





## @knitr textTokenize
########################################################################
#### Tokenizing and Plotting Functions
tokens <- 5

myGramTokenizer <- function(x) {RWeka::NGramTokenizer(x, Weka_control(min = tokens, max = tokens))}

# weighted tokenizer
myTokenizer = function(text){
  DocumentTermMatrix(text, control = list(weighting = function(x)
    weightTfIdf(x, normalize = FALSE), tokenize = myGramTokenizer))
}

freqTokenizer = function(text){
  DocumentTermMatrix(text, control = list(tokenize = myGramTokenizer))
}


# plotting functions
# get top n
rollData = function(meltedData, topn, metric) {
  meltedData %>% 
  group_by_(metric, 'Term') %>% 
  mutate(TFIDF = jitter(TFIDF, amount = 1.0)) %>%
  summarise(Total = sum(TFIDF)) %>%
  top_n(topn)
}



# plot rollup data
plotRollup = function(df, fillField, cols, metric){
  g =  ggplot(df, aes(reorder(Term, Total), Total, fill = fillField)) + 
  geom_col(show.legend = FALSE, fill = "blue", color = "lightblue") + 
  labs(x = NULL, y = "count") + 
  facet_wrap(~df[[metric]], ncol = cols, scales = "free") + theme_bryan() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
  return(g)
}


## @knitr tokenizeOsat
####### check OSAT
tokens <- 5
metric <- 'OSAT'

DTM = freqTokenizer(text = text)

# make it a dataframe
dtmDF <- as.data.frame(as.matrix(DTM))

# get the factor
dtmDF[[metric]] <- textDf[[metric]]

# clean up column
commentsDf <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF[[metric]]), sum)

colnames(commentsDf)[1] = metric

# use OSAT function
commentsDf[[metric]] = osat_func(commentsDf[[metric]], high = 8, middle = 5, low = 3)


# make it a factor for plotting
commentsDf[[metric]] = order_osat(commentsDf[[metric]])


# Melt the data
meltText <- melt(commentsDf, id = metric,
                 variable.name = 'Term',
                 value.name = 'TFIDF')


# roll up and plot the text
rollup = rollData(meltText, 40, 'OSAT')
plotRollup(df = rollup, fillField = rollup[[metric]],
           metric = metric, cols = 2)




## @knitr tokenizeEmotion
##### what are the comments start with emotion
textDfClean <- filter(textDf, !is.na(textDf$NeedsImprovement))

text <- VCorpus(VectorSource(textDfClean$NeedsImprovement))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(smallWds))


tokens <- 5
metric <- 'Emotion'

DTM = freqTokenizer(text = text)

# make it a dataframe
dtmDF <- as.data.frame(as.matrix(DTM))

# get the factor
dtmDF[[metric]] <- textDfClean[[metric]]

# clean up column
commentsDf <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF[[metric]]), sum)

colnames(commentsDf)[1] = metric

# use function to bucket scores
commentsDf[[metric]] = promoter_func1(commentsDf[[metric]], 5)


# make it a factor for plotting
commentsDf[[metric]] = as.factor(commentsDf[[metric]])


# Melt the data
meltText <- melt(commentsDf, id = metric,
                 variable.name = 'Term',
                 value.name = 'TFIDF')


# roll up and plot the text
rollup = rollData(meltText, 40, metric)
plotRollup(df = rollup, fillField = rollup[[metric]],
           metric = metric, cols = 2)



## @knitr tokenizeEffort
##### Effort
textDfClean <- filter(textDf, !is.na(textDf$MoreEffortless))

text <- VCorpus(VectorSource(textDfClean$MoreEffortless))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(smallWds))


tokens <- 5
metric <- 'Effort'

DTM = myTokenizer(text = text)

# make it a dataframe
dtmDF <- as.data.frame(as.matrix(DTM))

# get the factor
dtmDF[[metric]] <- textDfClean[[metric]]

# clean up column
commentsDf <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF[[metric]]), sum)

colnames(commentsDf)[1] = metric

# use function to bucket scores
commentsDf[[metric]] = promoter_func1(commentsDf[[metric]], 5)


# make it a factor for plotting
commentsDf[[metric]] = as.factor(commentsDf[[metric]])


# Melt the data
meltText <- melt(commentsDf, id = metric,
                 variable.name = 'Term',
                 value.name = 'TFIDF')


# roll up and plot the text
rollup = rollData(meltText, 40, metric)
plotRollup(df = rollup, fillField = rollup[[metric]],
           metric = metric, cols = 2)


## @knitr tokenizeEfficiency
####   Efficiency
textDfClean <- filter(textDf, !is.na(textDf$NeedsImprovement))

text <- VCorpus(VectorSource(textDfClean$NeedsImprovement))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(smallWds))


tokens <- 5
metric <- 'Efficiency'

DTM = freqTokenizer(text = text)

# make it a dataframe
dtmDF <- as.data.frame(as.matrix(DTM))

# get the factor
dtmDF[[metric]] <- textDfClean[[metric]]

# clean up column
commentsDf <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF[[metric]]), sum)

colnames(commentsDf)[1] = metric

# use function to bucket scores
commentsDf[[metric]] = promoter_func1(commentsDf[[metric]], 5)


# make it a factor for plotting
commentsDf[[metric]] = as.factor(commentsDf[[metric]])


# Melt the data
meltText <- melt(commentsDf, id = metric,
                 variable.name = 'Term',
                 value.name = 'TFIDF')


# roll up and plot the text
rollup = rollData(meltText, 40, metric)
plotRollup(df = rollup, fillField = rollup[[metric]],
           metric = metric, cols = 2)


################### TOPIC MODELING WITH LDA
## @knitr checkTopicsImprove
# tokenize

# clean and process

# remove NA from text
textDfClean <- filter(textDf, !is.na(textDf$NeedsImprovement))

text <- VCorpus(VectorSource(textDfClean$NeedsImprovement))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(smallWds))



tokens <- 4
DTM = freqTokenizer(text = text)

# use LDA, set k = 4
# Find the sum of words in each Document ensure no zeros
rowTotals <- apply(DTM , 1, sum) 
dtmClean   <- DTM[rowTotals> 0, ]
reviews_lda <- LDA(dtmClean, k = 6, control = list(seed = 1000))
# reviews_lda

# check topics
reviews_topics <- tidy(reviews_lda, matrix = "beta")
# reviews_topics

# revise palette
easternPal = c('lightsteelblue1', 'royalblue4', 'sienna1', 'skyblue', 'orange', 'darkblue')

# Make the plot
reviews_top_terms <- reviews_topics %>%
  group_by(topic) %>%
  mutate(beta = jitter(beta, amount = .0001)) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


reviews_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  scale_fill_manual(values = easternPal) + 
  geom_col(show.legend = FALSE) +
  theme_bryan() + 
  facet_wrap(~ topic, scales = "free", , ncol = 2) +
  coord_flip()


## @knitr checkTopicsEffortless
###### MORE EFFORTLESS
# clean and process
textDfClean <- filter(textDf, !is.na(textDf$MoreEffortless))

text <- VCorpus(VectorSource(textDfClean$MoreEffortless))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(smallWds))



tokens <- 4
DTM = freqTokenizer(text = text)

# use LDA, set k = 4
# Find the sum of words in each Document ensure no zeros
rowTotals <- apply(DTM , 1, sum) 
dtmClean   <- DTM[rowTotals> 0, ]
reviews_lda <- LDA(dtmClean, k = 6, control = list(seed = 1000))
# reviews_lda

# check topics
reviews_topics <- tidy(reviews_lda, matrix = "beta")
# reviews_topics

# revise palette
easternPal = c('lightsteelblue1', 'royalblue4', 'sienna1', 'skyblue', 'orange', 'darkblue')

# Make the plot
reviews_top_terms <- reviews_topics %>%
  group_by(topic) %>%
  mutate(beta = jitter(beta, amount = .0001)) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


reviews_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  scale_fill_manual(values = easternPal) + 
  geom_col(show.legend = FALSE) +
  theme_bryan() + 
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip()







##### WORD ASSOCIATIONS
## @knitr wordAssoc1
### Words with at least three mentions
textDfClean <- filter(textDf, !is.na(textDf$ExplainRating))

text <- VCorpus(VectorSource(textDfClean$ExplainRating))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(smallWds))


tokens <- 2
DTM = freqTokenizer(text = text)


# findFreqTerms(DTM, lowfreq = 3, highfreq = Inf)

### Words Correlated to "customer service" with at least .25
keyTerm <- "not easy"
wordAssoc <- findAssocs(DTM, keyTerm, .10)

# Convert to a dataframe
wordAssocDf <- data.frame(Corr=as.numeric(unlist(wordAssoc)),
                          Terms = gsub(paste(keyTerm,'.', sep = ''),'',names(unlist(wordAssoc))))

wordAssocDf <- wordAssocDf %>% 
  mutate(Corr = jitter(Corr, amount = .015))
# head(wordAssocDf)

# Plot it
wordP <- ggplot(wordAssocDf, aes(y = Corr, x = reorder(Terms, Corr))) + 
  geom_col(position = 'stack', color = 'white', fill = 'steelblue') +
  coord_flip() + 
  # shape = 23, size = 3, fill = 'white'
  ggtitle(paste("Phrases that are Correlated with", keyTerm)) + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bryan()
wordP


## @knitr wordAssoc2
tokens <- 2
DTM = freqTokenizer(text = text)

keyTerm <- "black lives"
wordAssoc <- findAssocs(DTM, keyTerm, .10)

# Convert to a dataframe
wordAssocDf <- data.frame(Corr=as.numeric(unlist(wordAssoc)),
                          Terms = gsub(paste(keyTerm,'.', sep = ''),'',names(unlist(wordAssoc))))

wordAssocDf2 <- wordAssocDf %>% 
  mutate(Corr = jitter(Corr, amount = .015))
# head(wordAssocDf)

# Plot it
wordP <- ggplot(wordAssocDf2, aes(y = Corr, x = reorder(Terms, Corr))) + 
  geom_col(position = 'stack', color = 'white', fill = 'steelblue') +
  coord_flip() + 
  # shape = 23, size = 3, fill = 'white'
  ggtitle(paste("Phrases that are Correlated with", keyTerm)) + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bryan()
wordP


## @knitr wordAssoc3
textDfClean <- filter(textDf, !is.na(textDf$ExplainRating))

text <- VCorpus(VectorSource(textDfClean$ExplainRating))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(smallWds))


tokens <- 3
DTM = freqTokenizer(text = text)

keyTerm <- "get customer service"
wordAssoc <- findAssocs(DTM, keyTerm, .10)

# Convert to a dataframe
wordAssocDf <- data.frame(Corr=as.numeric(unlist(wordAssoc)),
                          Terms = gsub(paste(keyTerm,'.', sep = ''),'',names(unlist(wordAssoc))))

wordAssocDf <- wordAssocDf %>% 
  mutate(Corr = jitter(Corr, amount = .015))
# head(wordAssocDf)

# Plot it
wordP <- ggplot(wordAssocDf, aes(y = Corr, x = reorder(Terms, Corr))) + 
  geom_col(position = 'stack', color = 'white', fill = 'steelblue') +
  coord_flip() + 
  # shape = 23, size = 3, fill = 'white'
  ggtitle(paste("Phrases that are Correlated with", keyTerm)) + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bryan()
wordP


## @knitr wordAssoc4
textDfClean <- filter(textDf, !is.na(textDf$MoreEffortless))

text <- VCorpus(VectorSource(textDfClean$MoreEffortless))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(smallWds))


tokens <- 2
DTM = freqTokenizer(text = text)

keyTerm <- "online banking"
wordAssoc <- findAssocs(DTM, keyTerm, .10)

# Convert to a dataframe
wordAssocDf <- data.frame(Corr=as.numeric(unlist(wordAssoc)),
                          Terms = gsub(paste(keyTerm,'.', sep = ''),'',names(unlist(wordAssoc))))

wordAssocDf <- wordAssocDf %>% 
  mutate(Corr = jitter(Corr, amount = .015))
# head(wordAssocDf)

# Plot it
wordP <- ggplot(wordAssocDf, aes(y = Corr, x = reorder(Terms, Corr))) + 
  geom_col(position = 'stack', color = 'white', fill = 'steelblue') +
  coord_flip() + 
  # shape = 23, size = 3, fill = 'white'
  ggtitle(paste("Phrases that are Correlated with", keyTerm)) + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bryan()
wordP


## @knitr wordAssoc5
textDfClean <- filter(textDf, !is.na(textDf$NeedsImprovement))

text <- VCorpus(VectorSource(textDfClean$NeedsImprovement))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(smallWds))

tokens <- 2
DTM = freqTokenizer(text = text)

keyTerm <- "bank statement"
wordAssoc <- findAssocs(DTM, keyTerm, .7015)

# Convert to a dataframe
wordAssocDf <- data.frame(Corr=as.numeric(unlist(wordAssoc)),
                          Terms = gsub(paste(keyTerm,'.', sep = ''),'',names(unlist(wordAssoc))))

wordAssocDf <- wordAssocDf %>% 
  mutate(Corr = jitter(Corr, amount = .015))
# head(wordAssocDf)

# Plot it
wordP <- ggplot(wordAssocDf, aes(y = Corr, x = reorder(Terms, Corr))) + 
  geom_col(position = 'stack', color = 'white', fill = 'steelblue') +
  coord_flip() + 
  # shape = 23, size = 3, fill = 'white'
  ggtitle(paste("Phrases that are Correlated with", keyTerm)) + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bryan()
wordP









## @knitr naiveBayes
##### Naive Bayes Models
# clean and process


text <- VCorpus(VectorSource(textDf$ExplainRating))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(smallWds))


tokens <- 2
metric <- 'OSAT'

DTM = freqTokenizer(text = text)

dtmRed = removeSparseTerms(DTM, 0.95)
dtmDF = as.data.frame(as.matrix(dtmRed))


# get the factor
dtmDF[[metric]] <- textDf[[metric]]

# create test and training dfs
# shuffle for good measure
dfShuffle <- dtmDF[sample(nrow(dtmDF)), ]


# get names
# get the predictor names by excluding the outcome variable
predictorNames = names(dfShuffle)[names(dfShuffle) != metric]


# make the outcome a factor
# dfShuffle[[metric]] <- as.factor(ifelse(dfShuffle[[metric]] == "Yes", 1,0))

# alternate
dfShuffle[[metric]] <- as.factor(ifelse(dfShuffle[[metric]] > 8, 1,0))


set.seed(1000)
split_ratio = .8
split = sample.split(dfShuffle[[metric]], SplitRatio = split_ratio)
trainDf = subset(dfShuffle, split == TRUE)
testDf = subset(dfShuffle, split == FALSE) 



# Specify the model using e1071 package
objModel <- e1071::naiveBayes(trainDf[[metric]] ~ ., data = trainDf, laplace = 1)


# Raw classes - actually yields conditional probabilities
predictions <- predict(object = objModel, testDf, type = 'raw')
# head(predictions)

## @knitr nbAuc
# get the AUC Use the Raw Predictions
auc <- roc(as.character(testDf[[metric]]),predictions[,2])
print(auc$auc)

## @knitr nbCM
# Confusion matrix
confusionMatrix(predict(object = objModel, testDf),
                testDf[[metric]])


# Unlist the tables
condProbs <- unlist(objModel$tables)
condProbDF <- data.frame(Prob=as.numeric(unlist(condProbs)),
                         Terms = names(unlist(condProbs)))


# head(condProbDF)

# Likelihood of the word, given they join
wordGivenJoin <- condProbDF %>% 
  dplyr::mutate(CleanTerms = sub('2','',Terms)) %>%
  dplyr::arrange(desc(Prob)) %>%
  dplyr::filter(grepl('2', Terms))

# Likelihood of the word, given they don't join
wordGivenNotJoin <- condProbDF %>% 
  dplyr::mutate(CleanTerms = sub('3','',Terms)) %>%
  dplyr::arrange(desc(Prob)) %>%
  dplyr::filter(grepl('3', Terms))


## @knitr nbPlot1
# Plots contributing to joining
wordRed <- wordGivenJoin %>% filter(Prob > .02)
wordP <- ggplot(wordRed, aes(y = Prob, x = reorder(CleanTerms, Prob))) + 
  geom_col(position = 'stack', color = 'lightblue', fill = 'steelblue') +
  coord_flip() + 
  # theme_bryan() + 
  xlab('') + 
  ggtitle(paste('Words most likely to show up in ', metric, sep = '')) + 
  ylab(paste('Conditional Prob of Word Given ', metric, sep = ''))
wordP

## @knitr nbPlot2
# Make plots contributing to not joining
wordNotRed <- wordGivenNotJoin %>% filter(Prob > .75)
wordP <- ggplot(wordNotRed, aes(y = Prob, x = reorder(CleanTerms, Prob))) + 
  geom_col(position = 'stack', color = 'lightblue', fill = 'steelblue') +
  coord_flip() + 
  ggtitle('Words most likely to show up in not Join User Community') + 
  ylab('Conditional Prob of Word Given not Join User Community') +
  xlab('') + 
  theme_bryan()
wordP









#script to analyze teller transactions and impacts

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
library(dendextend)
library(dendextendRcpp)
library(RColorBrewer)
library(htmltools)
library(plotly)

#get the data
#get the files  with location
url <- "C:/Users/n846490/Documents/DataScience/CSVs/balanceSheetAggQ2.csv"
balanceAgg <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get the attrition information
url <- "C:/Users/n846490/Documents/DataScience/CSVs/branchHeadCountsNullRem.csv"
branchHead <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)
branchHeadRed <- select(branchHead, Branch.Number, TellerAct, TotAct, TellerAttr)



#get checking production and related info
url <- "C:/Users/n846490/Documents/DataScience/CSVs/juneDataReducedClean.csv"
branchData <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get teller transaction data by branch
url <- "C:/Users/n846490/Documents/DataScience/CSVs/tellerTransactions.csv"
tellerData <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)
#tellerDataRed <- select(tellerData, Branch.Number, PercDeposits, PercCheck.Cashing, PercWithdrawal, PercLoanLine.Payment,
#                     PercCash.Advance, PercOther)

tellerDataRed <- select(tellerData, Branch.Number, Deposit, CheckCashing, Withdrawal, LoanLinePayment,
                     CashAdvance, Other)


branchData <- select(branchData, BranchNumber, Q2BookedNewChkgCnts, Q2BookedChkNewUSD, Q2TotalChkRev,
                     TotalBizChkMM, SBLoanBal)

names(branchData) <- c("Branch.Number", "Q2NewCkCounts", "Q2NewCkRevenue", "Q2CkMarginRevenue",
                       "Q2BizCkMMRevenue", "Q2SBLoanBal")


balanceAggRed <- select(balanceAgg, Branch.Number, CSat, PerfLevel, Home, BizTotal, ConsTotal, Invest)

#create the clean na function
na.zero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}




#join the data to create a master data set
#merge the data
allData <- inner_join(branchData, balanceAggRed, by = "Branch.Number")

allData <- inner_join(allData, branchHeadRed, by = "Branch.Number")

allData <- inner_join(allData, tellerDataRed, by = "Branch.Number")


#clean up CSat and convert it to a number
#clean csat and convert it to usable buckets
allData$CSat <- as.character(allData$CSat)
allData$CSat <- gsub("%", "", allData$CSat)
allData$CSat <- as.numeric(allData$CSat)

#subset the teller transactions for clustering
#tellerTrans <- select(allData, PercDeposits, PercCheck.Cashing, PercWithdrawal, PercLoanLine.Payment,
#                      PercCash.Advance, PercOther)



#subset the teller transactions for clustering
tellerTrans <- select(allData, Deposit, CheckCashing, Withdrawal, LoanLinePayment,
                      CashAdvance, Other)



#transpose the data so that it is clustering by transaction type

tellerTransT <- as.data.frame(t(tellerTrans))
colnames(tellerTransT) <- branchData$Branch.Number


#cluster the transactions
clust <- 5


par(mar=c(2,2,2,10))
d <- dist(tellerTransT, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = clust) %>%
    set("labels_cex", .75) %>%
    color_branches(k=clust) %>%
    set("branches_lwd", 3) %>%
    hang.dendrogram (hang = -1) %>%
    plot(horiz=T,main = paste("Teller Activities = ",clust, sep = ""), axes = T, xlim = c(8,0))
dend %>% rect.dendrogram(k = clust, horiz=T)


#create a function to split the data
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


#remove na
allData <- na.omit(allData)


#get the one data and merge it
#get checking production and related info
url <- "C:/Users/n846490/Documents/DataScience/CSVs/oneDataClean.csv"
oneData <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

oneDataRed <- select(oneData, Branch.Number, UniqueUsers, ExploreNotesUpdate,
                     UniqueCustomerSearches, LogIns)


allData <- inner_join(allData, oneDataRed, by = "Branch.Number")



#create high perf category
#this is for those in performance level 3 and above
allData$HighPerf <- ifelse(allData$PerfLevel>3,1,0)
#convert highperf to a factor
allData$HighPerf <- as.factor(as.character(allData$HighPerf))



#use  the function above to make the segment splits
splitData(allData,"Q2NewCkCounts", "trainDf", "testDf", .70)

#take the log of the counts
allData$LogCounts <- log(allData$Q2NewCkCounts)

outcome <- "Q2NewCkCounts"

outcome <- "LogCounts"


#use  the function above to make the segment splits
splitData(allData,"LogCounts", "trainDf", "testDf", .70)



predictorNames <- c("Deposit", "CheckCashing", "Withdrawal","CashAdvance", "ExploreNotesUpdate")


#try model
objControl <- trainControl(method = 'cv', number = 5, returnResamp = 'none')
objModel <- train(trainDf[,predictorNames], trainDf[,outcome],
                  method = 'lm',
                  metric = "RMSE",
                  trControl = objControl)

summary(objModel)


predictions <- predict(object = objModel, testDf[,predictorNames])





plot(varImp(objModel, scale = F))

objModel$finalModel
coef(objModel$finalModel, objModel$bestTune$.lambda)
coef(objModel$finalModel, 0.001746)



m1 <- lm(log(Q2NewCkCounts) ~ Deposit + 
             CheckCashing + 
             Withdrawal + 
             CashAdvance + 
             UniqueUsers + 
             ExploreNotesUpdate + 
             UniqueCustomerSearches + 
             LogIns + 
             TellerAct, data = trainDf)
summary(m1)

plot(m1)


m2 <- lm(log(Q2NewCkCounts) ~ Deposit + 
             CheckCashing + 
             Withdrawal + 
             CashAdvance + 
             ExploreNotesUpdate +
             TotAct, data = trainDf)
summary(m2)




#create a data frame for comparing
check <- data.frame(predictions,testDf[,outcome])
names(check) <- c("Predict", "Actual")
check$SqError <- (check$Actual - check$Predict)^2

#convert to real numbers vs logs
check$ActualCnts <- exp(check$Actual)
check$PredCnts <- exp(check$Predict)

check$PerError <- abs(check$ActualCnts - check$PredCnts)/check$ActualCnts


g1 <- ggplot(check, aes(x = PredCnts, y  = ActualCnts, color = PerError)) + geom_point() +
    xlim(0,300)
g1

(gg <- ggplotly(g1))



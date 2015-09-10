
#libraries for reshaping and plotting data
library(ggplot2)    #for plotting
library(reshape2)   # for making clean R panels
library(rpart)      # for decision tree packages
library(rpart.plot)

#load the file
url <- "K:/Sandbox/R/CQ 07-28.csv"
cqDataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


#change the characters of the data for the Q's this will cause a warning for the NA's
factorToNumeric <- function(f) as.numeric(levels(f))[as.integer(f)]
cols <- c(6:ncol(cqDataRaw))
cqDataRaw[cols] <- lapply(cqDataRaw[cols], factorToNumeric)

#remove NA rows from the data
cqData <- na.omit(cqDataRaw)

#copy the test data
TestData <- cqData

#apply the changes to the groupings based on the analysis
change18 <- function(x) {
  if (x>3) x = 1
  x = 0
}

TestData$Q18 <- sapply(TestData$Q18, change18)

#change the q19 values
change19 <- function(x) {
  if (x>4) x = 1
  x = 0
}
TestData$Q19 <- sapply(TestData$Q19, change19)

#Cut the data for the specific question
TestData18 <- TestData[,c(1:23, 26:30)]


#set seed and create a random number generator using the uniform dist
set.seed(8888)
TestData18$P <- runif(28652)


#create the training data
TestData18Train <- TestData18[TestData18$P < .75,]
TestData18Test <- TestData18[TestData18$P >= .75,]

#build a decision tree model
Q18Tree <- rpart(Q18 ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + 
                   Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + 
                   Q21 + Q22 + Q23 + Q24 + Q25, data = TestData18Train)

Q18X <- TestData18Train[,c(6:22)]
Q18Y <- TestData18Train[,23]

library(useful)
library(randomForest)

Q18Tree <-randomForest(x=Q18X, y=Q18Y)
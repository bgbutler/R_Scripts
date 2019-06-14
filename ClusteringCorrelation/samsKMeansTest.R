# this tests k means between R and SPSS

library(dplyr)
library(ggplot2)
library(class)  #for knn
library(rpart)  #other machine learning
library(caret)  #machine learning package
library(dendextend)
library(dendextendRcpp)
library(xgboost)
library(Metrics)
library(pROC)

#get the data
url <- "Q:/Analytics/Segmentation/Sams CQ data/samsSegmentsKmeans.csv"
dataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#create first cluster test
test3 <- na.omit(dataRaw)
test3 <- test3[,c(3:12,15:20)]

#######################
#plot the dendrogram
#check the clustering of the responses
#this tests for 4 centers, presentation had 3
par(mar=c(2,2,2,4))
d <- dist(test3, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 4) %>%
  set("labels_cex", .75) %>%
  color_branches(k=4) %>%
  set("branches_lwd", 3) %>%
  hang.dendrogram (hang = -1) %>%
  plot(horiz=F,main = "Response Clusters", axes = F, ylim = c(0,30))
axis(side = 2, col = "blue", at = seq(0,30,10), labels = FALSE)
mtext(seq(0,30,10), side = 2, at = seq(0,30,10),
      line = 1, col = "blue", las = 1)
dend %>% rect.dendrogram(k = 4, horiz=F)

#make scree plot check for centers
mydata <- test3[,predictorNames]

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:8) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


#########
#test of training and testing for k-means

outcomeName <- c("Cluster3")

predictorNames <- names(test3)[names(test3) != outcomeName]

#create testing and training data
set.seed(1234)
splitIndex <- createDataPartition(test3[,outcomeName], 
                                  p=.75, list = FALSE, times = 1)
trainDf <- test3[splitIndex,]
testDf <- test3[-splitIndex,]


prop.table(table(testDf$Cluster3))

####### DISREGARD THIS FOR THE PRESENTATION ##########
# this is the training phase that finds the seeds
seeds <- numeric()
diags <- numeric()

for (n in 1:1000 ){
  set.seed(n)
  kclust = kmeans(trainDf[,predictorNames], centers = 3, iter.max = 30, nstart = 6)
  clustTest <- table(trainDf$Cluster3, kclust$cluster)
  diag <- sum(diag(clustTest))
  
  if (diag > .90*sum(table(trainDf$Cluster3))){    
    print(sprintf("The seed is %s, the diagonal is %s", n, diag))
    clustTest
    seeds <- append(seeds, n, after = length(seeds))
    diags <- append(diags, diag, after = length(diags))
  }
}
  
newSeeds <- data.frame(seeds,diags)

unique(diags)

# using the seeds, find the best one to use in the algorithm
# this is the training phase that finds the seeds
# this was done to compare to another k-means method where the clusters were provided

seedsTest <- numeric()
diagsTest <- numeric()

for (n in 1:length(seeds) ){
  testSeed <- seeds[n]
  set.seed(testSeed)
  kclustTest = kmeans(testDf[,predictorNames], centers = 3, iter.max = 30, nstart = 6)
  clustTestTable <- table(testDf$Cluster3, kclustTest$cluster)
  diagTest <- sum(diag(clustTestTable))
  
  if (diagTest > .94*sum(table(testDf$Cluster3))){    
    print(sprintf("The seed is %s, the diagonal is %s",testSeed,diagTest))
    seedsTest <- append(seedsTest, testSeed, after = length(seedsTest))
    diagsTest <- append(diagsTest, diagTest, after = length(diagsTest))
    print(clustTestTable)
  }
}


########################
##### This handles the results from the unsupervised learning
#make dummy vars for segments
#make factors out of outcomes and clusters
test3$Cluster3 <- as.factor(as.character(test3$Cluster3))

dmy <- dummyVars("~.", data = test3)
dmyDf <- data.frame(predict(dmy, newdata = test3))
head(dmyDf,10)

#save the segments as values for other modeling
tempOutcome <- dmyDf[,c(16:18)]

table(as.factor(as.character(tempOutcome$Cluster.2)))

colnames(tempOutcome) <- c("Cluster.1", "Cluster.2", "Cluster.3")

#clean up the cluster names
names(dmyDf)[16:18] <- c("Cluster.1", "Cluster.2", "Cluster.3")

`%notin%` <- function(x,y) !(x %in% y)
allNames <- names(dmyDf)
allNames
outcomeNames <- c("Cluster.1", "Cluster.2", "Cluster.3")
predictorNames <- allNames[allNames %notin% outcomeNames]
predictorNames


#make factors from outcomes 
dmyDf$Cluster.1 <- as.character(dmyDf$Cluster.1)
dmyDf$Cluster.2 <- as.character(dmyDf$Cluster.2)
dmyDf$Cluster.3 <- as.character(dmyDf$Cluster.3)

dmyDf$Cluster.1 <- ifelse(dmyDf$Cluster.1==1, "Seg1", "Other")
dmyDf$Cluster.2 <- ifelse(dmyDf$Cluster.2==1, "Seg2", "Other")
dmyDf$Cluster.3 <- ifelse(dmyDf$Cluster.3==1, "Seg3", "Other")

dmyDf$Cluster.1 <- as.factor(dmyDf$Cluster.1)
dmyDf$Cluster.2 <- as.factor(dmyDf$Cluster.2)
dmyDf$Cluster.3 <- as.factor(dmyDf$Cluster.3)


Segment1 <- dmyDf[,c(1:16)]
Segment2 <- dmyDf[,c(1:15,17)]
Segment3 <- dmyDf[,c(1:15,18)]

# testing and training data for regression
# this is a function to split the data
# enter the dataframe to split, the outcome variable to split on, training name, testing name, split proportion
splitData <- function(df,outcomeName, tr, te, p){
  set.seed(1234)
  splitIndex <- createDataPartition(df[,outcomeName], 
                                  p=p, list = FALSE, times = 1)
  trainDf <- df[splitIndex,]
  testDf <- df[-splitIndex,]
  assign(tr, trainDf, envir = .GlobalEnv)
  assign(te, testDf, envir = .GlobalEnv)
}  


##########################################
#### SUPERVISED ML
#create the new dataset
gl1 <- cbind(dmyDf[1:15],tempOutcome[1])
gl2 <- cbind(dmyDf[1:15],tempOutcome[2])
gl3 <- cbind(dmyDf[1:15],tempOutcome[3])

#use  the function above to make the segment splits
splitData(gl1,"Cluster.1", "train1Df", "test1Df", .70)
splitData(gl2,"Cluster.2", "train2Df", "test2Df", .70)
splitData(gl3,"Cluster.3", "train3Df", "test3Df", .70)

#if using numeric values for dependent then no classProbs, RMSE can be metric
#if converting to factors then classProbs = T, use alternate metric
#experiment with glmnet for variable reduction
#set the outcome for the appropriate data set
outcome <- "Cluster.1"
outcome <- "Cluster.2"
outcome <- "Cluster.3"


### ELASTIC NET (GLMNET)
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


coef(objModel$finalModel, objModel$bestTune$lambda)

#glmnet is very good
###########################################################
# LOGISTIC REGRESSION
#use the segment df's created earlier
#then make training and testing sets

splitData(Segment1,"Cluster.1", "train1Df", "test1Df", .70)
splitData(Segment2,"Cluster.2", "train2Df", "test2Df", .70)
splitData(Segment3,"Cluster.3", "train3Df", "test3Df", .70)

#Train <- createDataPartition(Segment1$Cluster.1, p = .7, list = F)
#train1Df <- Segment1[Train, ]
#test1Df <- Segment1[-Train,]

#convert Cluster values to factors for training/testing
train1Df$Cluster.1 <- as.factor(as.character(train1Df$Cluster.1))
test1Df$Cluster.1 <- as.factor(as.character(test1Df$Cluster.1))

train2Df$Cluster.2 <- as.factor(as.character(train2Df$Cluster.2))
test2Df$Cluster.2 <- as.factor(as.character(test2Df$Cluster.2))

train3Df$Cluster.3 <- as.factor(as.character(train3Df$Cluster.3))
test3Df$Cluster.3 <- as.factor(as.character(test3Df$Cluster.3))

## THIS SETS UP THE 'VOTING'
train1Df$Cluster.1 <- ifelse(train1Df$Cluster.1 =="1", "Seg1", "other")
test1Df$Cluster.1 <- ifelse(test1Df$Cluster.1 =="1", "Seg1", "other")

train2Df$Cluster.2 <- ifelse(train2Df$Cluster.2 =="1", "Seg2", "other")
test2Df$Cluster.2 <- ifelse(test2Df$Cluster.2 =="1", "Seg2", "other")

train3Df$Cluster.3 <- ifelse(train3Df$Cluster.3 =="1", "Seg3", "other")
test3Df$Cluster.3 <- ifelse(test3Df$Cluster.3 =="1", "Seg3", "other")


str(test2Df)
str(train2Df)

######################################
#this does not use the caret package to create the logistic regressions
#make sure the dependent variables are factors
#build the scaled down models
logCluster1 <- train(Cluster.1 ~ Q16 + Q22 + Q25 + Q13, data = train1Df,
                     method = "glm")

summary(logCluster1)$coef


logCluster2 <- train(Cluster.2 ~ Q10 + Q3 + Q23 + Q9, data = train2Df,
                     method = "glm")

#alternate model for segment 2
logCluster2 <- train(Cluster.2 ~ Q10 + Q3 + Q23 + Q9 + Q11, data = train2Df,
                     method = "glm")


logCluster3 <- train(Cluster.3 ~ Q23 + Q16 + Q25 + Q10 + Q13 + Q2 + Q3,
                     data = train3Df, method = "glm")

logCluster3

exp(coef(logCluster1$finalModel))
summary(logCluster2)
coef(logCluster1$finalModel)[1]


predictions1 <- predict(object = logCluster1, newdata = test1Df, type = "prob")
predictions2 <- predict(object = logCluster2, newdata = test2Df, type = "prob")
predictions3 <- predict(object = logCluster3, newdata = test3Df, type = "prob")



varImp(logCluster1)

predictions1$seg <- ifelse(predictions1$Seg1 >.50, "Seg1", "other")
confusionMatrix(predictions1$seg, test1Df$Cluster.1)

predictions2$seg <- ifelse(predictions2$Seg2 >.50, "Seg2", "other")
confusionMatrix(predictions2$seg, test2Df$Cluster.2)

predictions3$seg <- ifelse(predictions3$Seg3 >.50, "Seg3", "other")
confusionMatrix(predictions3$seg, test3Df$Cluster.3)

######################################################
#use the caret for the logistic regression
train1Df$Cluster.1 <- as.character(train1Df$Cluster.1)
train1Df$Cluster.1 <- ifelse(train1Df$Cluster.1 =="1", "Seg1", "other")
train1Df$Cluster.1 <- as.factor(train1Df$Cluster.1)

test1Df$Cluster.1 <- as.character(test1Df$Cluster.1)
test1Df$Cluster.1 <- ifelse(test1Df$Cluster.1 =="1", "Seg1", "other")
test1Df$Cluster.1 <- as.factor(test1Df$Cluster.1)

#this method only produces classifications
objControl <- trainControl(method = 'cv', number = 10, 
                           returnResamp = 'none', twoClassSummary)
objModel <- train(train1Df[,predictorNames], train1Df[,outcome],
                  method = 'glm',
                  family = 'binomial',
                  metric = 'ROC',
                  trControl = objControl)


predictions <- predict(object = objModel, test1Df[,predictorNames])

#create a data frame for comparing
check <- data.frame(predictions,test1Df[,outcome])
check$inseg <- ifelse(check$predictions > .50,1,0)
colnames(check) <- c("Prob", "Actual","Predict")
confusionMatrix(check$Predict, check$Actual)

#get the AUC for the glm
auc <- roc(test1Df[,outcome], predictions)
print(auc$auc)


#glm method from article
objControl <- trainControl(method = 'cv', number = 10, 
                           savePredictions = TRUE)

objModel <- train(Cluster.1 ~ Q16 + Q22 + Q25 + Q13,
                  data = train1Df,
                  method = 'glm',
                  family = 'binomial',
                  trControl = objControl)

summary(objModel)$coef

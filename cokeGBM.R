#load in the libraries


library(MASS)   #for LDA
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(class)  #for knn
library(rpart)  #other machine learning
library(caret)  #machine learning package
library(dendextend)
library(dendextendRcpp)
library(xgboost)
library(Metrics)
library(pROC)

#get a list of the files
url <- "K:/Sandbox/R/Segmentation/TrainingCoke2.csv"
cokeRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

cokeRaw$Cluster <- as.factor(cokeRaw$Cluster)

#clean up the feminino in the data
cokeRaw$Gender <- as.character(cokeRaw$Gender)
cokeRaw$Gender <- ifelse(cokeRaw$Gender %in% c("Female", "Femenino"),"Female", "Male")
cokeRaw$Gender <- as.factor(cokeRaw$Gender)

#######################################################
#exploratory plot
p <- ggplot(cokeRaw, aes(x = Country, y = Cluster, color = Cluster)) +
  geom_point(size = 5)
p

cokeDist <- cokeRaw[,c(8:84)]

#########################################################
#plot tree diagram for entire matrix
d <- dist(cokeDist)
hc <- hclust(d)
op = par(bg = "white")
dend <- d %>% hclust %>% as.dendrogram
dend %>% color_branches(k=5) %>% plot(horiz=FALSE,
                                      main = "Hierchical Clusters of 7 Segments", axes = FALSE)
axis(side = 2, col = "blue", at = seq(1,25,5), labels = FALSE)
mtext(seq(1,25,5), side = 2, at = seq(1,25,5),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = 5, horiz = FALSE)

#######################################################
#get the data for the dummy vars
newDf <- cokeRaw[,c(2,8:84)]

dmy <- dummyVars("~.", data = newDf)
dmyDf <- data.frame(predict(dmy, newdata = newDf))
head(dmyDf,10)

print(names(dmyDf))

outcomeNames <- c("Cluster.1", "Cluster.2","Cluster.3","Cluster.4", 
                 "Cluster.5", "Cluster.6", "Cluster.7")

predictorNames <- names(dmyDf)[names(dmyDf) != outcomeNames]
predictorNames

for (i in 1:length(outcomeNames)){
  dmyDf[,i] <- as.factor(dmyDf[,i])
}

#create the various dataframes to model
clusterDf <- list()
for (i in 1:length(outcomeNames)){
   clusterDf[[i]] <- data.frame(dmyDf[,c(i,8:84)])
}

names(clusterDf) <- outcomeNames

list2env(clusterDf, envir = .GlobalEnv)

outcomeName <- "Segment" 

#rename the field Cluster.x
#for each of the models below change the Cluster.x

names(Cluster.7)[1] <- outcomeName

#save the segment for glmnet
tempSegment <- Cluster.7$Segment

rm(clusterDf)

Cluster.7$Segment <- ifelse(Cluster.7$Segment ==1, "S7","Other")
Cluster.7$Segment <- as.factor(Cluster.7$Segment)

prop.table(table(Cluster.7$Segment))




#create a partition of the data
set.seed(1000)
splitIndex <- createDataPartition(Cluster.7[,outcomeName], 
                                  p=.75, list = FALSE, times = 1)
trainDf <- Cluster.7[splitIndex,]
testDf <- Cluster.7[-splitIndex,]

objControl <- trainControl(method = "cv", number = 3, returnResamp = "none", 
                           summaryFunction = twoClassSummary, classProbs = TRUE)

objModel <- train(trainDf[,predictorNames], trainDf[,outcomeName],
                  method = "gbm",
                  trControl = objControl,
                  metric = "ROC")

summary(objModel)

objModel

#class prediction
predictions <- predict(object = objModel, testDf[,predictorNames], type = "raw")
head(predictions)
postResample(pred = predictions, obs = as.factor(testDf[,outcomeName]))


# probabilities
predictions <- predict(object = objModel, testDf[,predictorNames], type = "prob")
head(predictions)
postResample(pred = predictions, obs = testDf[,outcomeName])

auc <- roc(ifelse(testDf[,outcomeName]=="S7",1,0),predictions[[2]])
print(auc$auc)

plot(varImp(objModel, scale = FALSE))

############################
#use the glmnet model

#reset the variables for regression
Cluster.1$Segment <- tempSegment

head(tempSegment,20)

prop.table(table(Cluster.1$Segment))


#create a partition of the data
set.seed(1000)
splitIndex <- createDataPartition(Cluster.1[,outcomeName], 
                                  p=.75, list = FALSE, times = 1)
trainDf <- Cluster.1[splitIndex,]
testDf <- Cluster.1[-splitIndex,]

objControl <- trainControl(method = "cv", number = 3, returnResamp = "none")

getModelInfo()$glmnet$type

objModel <- train(trainDf[,predictorNames], trainDf[,outcomeName],
                  method = "glmnet",
                  metric = "RMSE")

predictions <- predict(object = objModel, testDf[,predictorNames])

library(pROC)
auc <- roc(testDf[,outcomeName], predictions)
print(auc$auc)





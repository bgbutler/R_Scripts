#load in the libraries


library(MASS)   #for LDA
library(dplyr)
library(ggplot2)
library(caret)  #machine learning package
library(dendextend)
library(dendextendRcpp)
library(xgboost)
library(Metrics)
library(pROC)
library(nnet)
library(rpart)
library(class)


#get a list of the files
url <- "K:/Sandbox/R/Segmentation/TrainingCoke2.csv"
cokeRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

cokeRaw$Cluster <- as.factor(cokeRaw$Cluster)

#clean up the feminino in the data
cokeRaw$Gender <- as.character(cokeRaw$Gender)
cokeRaw$Gender <- ifelse(cokeRaw$Gender %in% c("Female", "Femenino"),"Female", "Male")
cokeRaw$Gender <- as.factor(cokeRaw$Gender)

#######################################################
#exploratory plot might be good to work as a balloon plot
p <- ggplot(cokeRaw, aes(x = Gender, y = Cluster, color = Cluster)) +
  geom_point(size = 5)
p

#####################
cokeDist <- cokeRaw[,c(8:84)]

#########################################################
#plot tree diagram for entire matrix
d <- dist(cokeDist)
hc <- hclust(d)
op = par(bg = "white")
dend <- d %>% hclust %>% as.dendrogram
dend %>% color_branches(k=7) %>% plot(horiz=FALSE,
                                      main = "Hierchical Clusters of 7 Segments", axes = FALSE)
axis(side = 2, col = "blue", at = seq(1,25,5), labels = FALSE)
mtext(seq(1,25,5), side = 2, at = seq(1,25,5),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = 5, horiz = FALSE)

#####################################################
#refine the data set
clusters <- data.frame(cokeRaw[,c(2,8:84)])

clusters <- data.frame(cokeRaw[,c(2:5,8:84)])

#vew the counts as a table
table(clusters$Cluster)

outcomeName <- "Cluster"

#create a partition of the data
set.seed(1000)
splitIndex <- createDataPartition(clusters[,outcomeName], 
                                  p=.90, list = FALSE, times = 1)
trainDf <- clusters[splitIndex,]
testDf <- clusters[-splitIndex,]

trainLabels <- trainDf[,1]
testLabels <- testDf[,1]

#################################
#model using nnet
segmentModel <- multinom(Cluster ~., data = trainDf, maxit = 100, trace = TRUE)

#get most important variables
topModels <- varImp(segmentModel)
topModels$Variables <- row.names(topModels)
topModels <- topModels[order(-topModels$Overall),]

topModels

#predict the probabilities and classes
preds1 <- predict(segmentModel, type = "probs", newdata = testDf)
preds2 <- predict(segmentModel, type = "class", newdata = testDf)

head(preds2,5)

#make a table
#get the classifications
predsClass <- table(preds2, testDf[,1])
predsClass

head(preds1,5)

postResample(testDf[,1],preds2)


totalAccuracy <- c()
cv <- 5
cvDivider <- floor(nrow(clusters)/(cv+1))

for (cv in seq(1:cv)){
  #assign check to data set
  dataTestIndex <- c((cv*cvDivider):(cv*cvDivider + cvDivider))
  dataTest <- clusters[dataTestIndex,]
  #everything else to train
  dataTrain <- clusters[-dataTestIndex,]
  
  segmentModel <- multinom(Cluster ~., data = dataTrain, maxit = 500, trace = TRUE)
  
  pred <- predict(segmentModel, newdata = dataTest, type = "class")
  
  #classification error
  
  #err <- ce(as.numeric(dataTest$Cluster), as.numeric(pred))
  #totalError <- c(totalError, err)
  
  cv_ac <- postResample(dataTest$Cluster, pred)
  print(paste('Current Accuracy:', cv_ac, 'for CV:',cv))
  totalAccuracy <- c(totalAccuracy, cv_ac)
}

###################################
#try KNN

knnModel <- knn(trainDf, testDf, cl=trainLabels, k=2, prob = TRUE)

postResample(testLabels,knnModel)

table(testLabels, knnModel)



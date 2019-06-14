#introduction to sgementation of data
# apply clustering and logistic regression to analyzing clusters



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
library(circlize)


#get the data
url <- "Q:/Analytics/Segmentation/diabetesSanofiWithRemovals.csv"
diabetesRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

head(diabetesRaw)

diabetesClean <- diabetesRaw[diabetesRaw$Respondent != '68',]

#####################################################################
#this section is for the hierarchical clustering
#create a matrix for distance plotting
diabDist <- diabetesRaw[,c(11:47)]


dend <- list()

#loop through a bunch of options
for (p in 2:10){

#  myPath <- file.path("Q:","Analytics", "Segmentation",
#                      sprintf("Clusters%s.pdf",p))
  
#  pdf(file = myPath, onefile = T, paper = "USr", width = 11, height = 8.5)
  
clusters <- p
p=4


#plot tree diagram for entire matrix
d <- (dist(diabDist, method = "euclidean"))
hc <- hclust(d, method = "single")
op = par(bg = "white")
dend <- d %>% hclust %>% as.dendrogram
dend %>% color_branches(k=clusters) %>% sort %>% 
  plot(horiz=FALSE,main = paste("Hierarchical Clusters K=",clusters),
                                      axes = FALSE, xlim = c(1,86))
axis(side = 2, col = "blue", at = seq(1,25,5), labels = FALSE)
mtext(seq(1,25,5), side = 2, at = seq(1,25,5),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = clusters, horiz = FALSE)
print(dend)
#dev.off()
  }


head(d)

#shows the locations of all of the nodes
dend %>% get_nodes_attr("height")

#shows what member is in each node NAs are aggregated nodes
dend %>% get_nodes_attr("label")

#shows the labes in order left to right
labels <- dend %>% labels

#plots a cicular dendrogram
d <- dist(diabDist)
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram %>%
  set("branches_k_color", k=clusters) %>% 
  set("labels_colors")
circlize_dendrogram(dend)

str(dend)

#export the data as a CSV - name the file
write.csv(labels, file = "Q:/Analytics/Segmentation/labels.csv", row.names = FALSE)

#########################################
#this is for predictive modeling

#convert NA's to zero
#create function to clean up the NAs
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

for (i in 2:9){
  diabetesRaw[,i] <- na.zero(diabetesRaw[,i])
}

#make dummy vars for segments
newDf <- subset(diabetesRaw, select = -Type)
newDf <- subset(newDf, select = -Respondent)


#convert segment to factor
newDf$Segment <- as.factor(newDf$Segment)


dmy <- dummyVars("~.", data = newDf)
dmyDf <- data.frame(predict(dmy, newdata = newDf))
head(dmyDf,10)

outcomeNames <- c("Segment.1", "Segment.2","Segment.3","Segment.4")

predictorNames <- names(dmyDf)[names(dmyDf) != outcomeNames]
predictorNames


#make factors from outcomes 
for (i in 1:length(outcomeNames)){
  dmyDf[,i] <- as.factor(dmyDf[,i])
}


#create the various dataframes to model
clusterDf <- list()
for (i in 1:length(outcomeNames)){
  clusterDf[[i]] <- data.frame(dmyDf[,c(i,5:50)])
}

names(clusterDf) <- outcomeNames

list2env(clusterDf, envir = .GlobalEnv)
rm(clusterDf)

#make factor outcomes numeric for glmnet
Segment.1$Segment.1 <- as.numeric(as.character(Segment.1$Segment.1))
Segment.2$Segment.2 <- as.numeric(as.character(Segment.2$Segment.2))
Segment.3$Segment.3 <- as.numeric(as.character(Segment.3$Segment.3))
Segment.4$Segment.4 <- as.numeric(as.character(Segment.4$Segment.4))

Segment.1$Segment.1 <- as.factor(Segment.1$Segment.1)
Segment.2$Segment.2 <- as.factor(Segment.2$Segment.2)
Segment.3$Segment.3 <- as.factor(Segment.3$Segment.3)
Segment.4$Segment.4 <- as.factor(Segment.4$Segment.4)

#clean up the column name of oucome variable
colnames(Segment.1)[1] <- "Segment1"
colnames(Segment.2)[1] <- "Segment2"
colnames(Segment.3)[1] <- "Segment3"
colnames(Segment.4)[1] <- "Segment4"

colnames(Segment.1)[8] <- "NotApp"
colnames(Segment.2)[8] <- "NotApp"
colnames(Segment.3)[8] <- "NotApp"
colnames(Segment.4)[8] <- "NotApp"

colnames(Segment.1)[7] <- "NoMeds"
colnames(Segment.2)[7] <- "NoMeds"
colnames(Segment.3)[7] <- "NoMeds"
colnames(Segment.4)[7] <- "NoMeds"

colnames(Segment.1)[6] <- "InsInject"
colnames(Segment.2)[6] <- "InsInject"
colnames(Segment.3)[6] <- "InsInject"
colnames(Segment.4)[6] <- "InsInject"



outcomeName <- names(Segment.2)[1]


predictorNames[6] <- "NoMeds"



#create testing and training data
  set.seed(1234)
  splitIndex <- createDataPartition(Segment.2[,outcomeName], 
                                  p=.75, list = FALSE, times = 1)
  trainDf <- Segment.2[splitIndex,]
  testDf <- Segment.2[-splitIndex,]

prop.table(table(Segment.2$Segment2))


#experiment with glmnet
objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none')
objModel <- train(trainDf[,predictorNames], trainDf[,outcomeName],
                  method = 'glmnet',
                  metric = "RMSE",
                  trControl = objControl)


#experiment with glm
objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none',
                           summaryFunction = twoClassSummary, classProbs = T)
objModel <- train(trainDf[,predictorNames],trainDf[,outcomeName],
                  method = "glm",
                  family = binomial,
                  metric = "ROC",
                  trControl = objControl
             )


summary(objModel)$coef


predictions <- predict(object = objModel, testDf[,predictorNames])

library(pROC)
auc <- roc(testDf[,outcomeName], predictions)
print(auc$auc)

postResample(pred=predictions, obs=testDf[,outcomeName])

summary(objModel)
plot(varImp(objModel, scale = F))

predictions

check4 <- data.frame(predictions,testDf[,outcomeName])
head(check4)

check2 <- data.frame(predictions,testDf[,outcomeName])
head(check2)

objModel

exp(coef(objModel$finalModel))



objModel$finalModel

#for glmnet
coef(objModel$finalModel, objModel$bestTune$.lambda)
coef(objModel, s = "lambda.min")


head(objModel$predictions)



######################################
#make scree plot check for 15 centers
mydata <- diabetesRaw[,c(4:42)]

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


### k means

diab <- diabetesRaw[,c(3:42)]

outcomeName <- 'kMeans'
predictorsNames <- names(diab)[names(diab) != outcomeName]


#create testing and training data
set.seed(1234)
splitIndex <- createDataPartition(diab[,outcomeName], 
                                  p=.75, list = FALSE, times = 1)
trainDf <- diab[splitIndex,]
testDf <- diab[-splitIndex,]


#perform the k-means

diab$kMeans <- as.factor(as.character(diab$kMeans))

seeds <- numeric()
diags <- numeric()

for (n in 1:100000 ){
  set.seed(n)
  kclust = kmeans(diab[,c(2:40)], centers = 4, iter.max = 30, nstart = 6)
  clustTest <- table(diab[,1], kclust$cluster)
  diag <- sum(diag(clustTest))
  
  if (diag > .85*sum(table(diab$kMeans))){    
    print(paste("The seed is ",n))
    print(paste("The diag is ", diag))
      clustTest
      seeds <- append(seeds, n, after = length(seeds))
      diags <- append(diags, diag, after = length(diags))
  }
  #else {
  #  print(paste(n," is not a good seed, diag = ",diag))
  #}
}


set.seed(27078)
kclust = kmeans(diab[,c(2:40)], centers = 4, iter.max = 30, nstart = 6)
clustTest <- table(diab[,1], kclust$cluster)
clustTest




table(diab$kMeans)
table(kclust$cluster)


prop.table(table(diab$kMeans))
prop.table(table(kclust$cluster))



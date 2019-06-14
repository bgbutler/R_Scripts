
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
library(gmodels)
library(stats)

#get a list of the files
url <- "K:/Sandbox/R/Segmentation/TrainingCoke2.csv"
cokeRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

cokeRaw$Cluster <- as.factor(cokeRaw$Cluster)

#clean up the feminino in the data
cokeRaw$Gender <- as.character(cokeRaw$Gender)
cokeRaw$Gender <- ifelse(cokeRaw$Gender %in% c("Female", "Femenino"),"Female", "Male")
cokeRaw$Gender <- as.factor(cokeRaw$Gender)


#exploratory plot
p <- ggplot(cokeRaw, aes(x = Country, y = Cluster, color = Cluster)) +
  geom_point(size = 5)
p

cokeDist <- cokeRaw[,c(8:84)]


#plot tree diagram for entire matrix
par(mai=c(2.0,1.0, 1.0,1.0))
d <- dist(cokeDist)
hc <- hclust(d)
op = par(bg = "white")
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 7) %>%
  set("labels_cex", .75) %>%
  color_branches(k=7) %>%
  set("branches_lwd", 2) %>%
  hang.dendrogram (hang = -1) %>%
  plot(horiz=FALSE, main = "Hierchical Clusters of 7 Segments", axes = FALSE)
axis(side = 2, col = "blue", at = seq(1,25,5), labels = FALSE)
mtext(seq(1,25,5), side = 2, at = seq(1,25,5),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = 7, horiz = FALSE)

#set up for lda
#create subset of data

lda.data <- cokeRaw[,c(2,8:84)]

#set up the data sets training and testing
set.seed(1000)
split <- sample(nrow(lda.data), floor (0.9*nrow(lda.data)))
traindf <- lda.data[split,]
testdf <- lda.data[-split,]


#make the training model
data1.lda <- lda(Cluster ~ ., data = traindf)

#make the training model
data2.lda <- lda(Cluster ~ ., data = lda.data,CV = TRUE)

data1.lda

#predict function
data1.lda.p <- predict(data1.lda, newdata = testdf[,c(2:78)])$class


data1.lda.p

plot(data1.lda)

#get the classifications
preVal <- table(data1.lda.p, testdf[,1])
preVal

sum(diag(preVal))/length(testdf$Cluster)

# a better table for evaluating
preValX <- table(data2.lda$class, lda.data[,1])
sum(diag(preValX))/length(lda.data$Cluster)


prevalX <- CrossTable(x = data1.lda.p, y = testdf[,1], prop.chisq = FALSE)

###### Result = LDA is not a good method for this data set

### k means

set.seed(1000)

kclust = kmeans(traindf[,c(2:78)], centers = 7, nstart = 30)

kclust

clustTest <- table(traindf[,1], kclust$cluster)
clustTest

dataDummy <- dummyVars("~", data = lda.data, fullRank = F)
trsfData <- data.frame(predict(dataDummy, newdata = lda.data))



# this tests k means between R and SPSS

library(dplyr)
library(ggplot2)
library(class)  #for knn
library(rpart)  #other machine learning
library(caret)  #machine learning package
library(dendextend)
library(dendextendRcpp)
library(Metrics)
library(pROC)

#get the data
url <- "Q:/Analytics/ADA/adaSurveyClean.csv"
dataRaw <- read.csv(url, header = TRUE, sep=",", na.strings = 0,as.is = FALSE)

dataRaw[is.na(dataRaw)] <- 0

#explore a little
#start exploratory analysis on ADA
plt0 <- ggplot(dataRaw, aes(x= Age, fill = Gender))
plt0 + geom_histogram(na.rm = TRUE) + facet_wrap(~Gender, ncol = 2) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Age") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


#start exploratory analysis on ADA
plt0 <- ggplot(dataRaw, aes(x= Age, fill = Gender))
plt0 + geom_density(na.rm = TRUE, alpha = 0.8)  +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Age") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

#let's look at HHI
plt0 <- ggplot(dataRaw, aes(x= HHI,fill = Gender))
plt0 + geom_bar(alpha = 0.7, stat = "count") +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("HHI") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
plt0








adaMat <- dataRaw[,c(20:65)]

wss <- (nrow(adaMat)-1)*sum(apply(adaMat,2,var))

for (i in 2:20) wss[i] <- sum(kmeans(adaMat,centers=i)$withinss)

plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


sg <- 5

#plot the dendrogram
#check the clustering of the responses
par(mar=c(2,2,2,4))
d <- dist(adaMat, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = sg) %>%
  set("labels_cex", .75) %>%
  color_branches(k=sg) %>%
  set("branches_lwd", 3) %>%
  hang.dendrogram (hang = -1) %>%
  plot(horiz=F,main = "Response Clusters", axes = F, ylim = c(0,30))
axis(side = 2, col = "blue", at = seq(0,30,10), labels = FALSE)
mtext(seq(0,30,10), side = 2, at = seq(0,30,10),
      line = 1, col = "blue", las = 1)
dend %>% rect.dendrogram(k = sg, horiz=F)

#load in the reduced data set
#get the data
url <- "Q:/Analytics/ADA/adaSurveyRed.csv"
dataRawRed <- read.csv(url, header = TRUE, sep=",", na.strings = 0,as.is = FALSE)

dataRawRed[is.na(dataRawRed)] <- 0
adaMatRed <- dataRawRed[,c(20:34)]

wss <- (nrow(adaMatRed)-1)*sum(apply(adaMatRed,2,var))

for (i in 2:20) wss[i] <- sum(kmeans(adaMatRed,centers=i)$withinss)

plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


sg <- 5

#plot the dendrogram
#check the clustering of the responses
par(mar=c(2,2,2,4))
d <- dist(adaMatRed, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = sg) %>%
  set("labels_cex", .75) %>%
  color_branches(k=sg) %>%
  set("branches_lwd", 3) %>%
  hang.dendrogram (hang = -1) %>%
  plot(horiz=F,main = "Response Clusters", axes = F, ylim = c(0,20))
axis(side = 2, col = "blue", at = seq(0,20,10), labels = FALSE)
mtext(seq(0,20,10), side = 2, at = seq(0,20,10),
      line = 1, col = "blue", las = 1)
dend %>% rect.dendrogram(k = sg, horiz=F)


#set the directory for the output
setwd("Q:/Analytics/ADA")

sink("dendRed5.txt", type = "output")
str(dend)
sink()






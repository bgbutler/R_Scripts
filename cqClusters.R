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
library(reshape2)
library(RColorBrewer)

#get the data
url <- "Q:/Analytics/CQ/CSV Data/industryQuestions.csv"
dataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

url2 <- "Q:/Analytics/CQ/CSV Data/BusinessMetrics.csv"
dataBiz <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

clus1 <- dataRaw[,c(2,5:19)]
clus2 <- dataRaw[,c(1,5:19)]

#make scree plot check for centers
mydata <- clus1[,c(2:16)]

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


#make a dendrogram to review
#plot as a horizontal
par(mar=c(2,2,2,10))
d <- dist(clus1, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 5) %>%
  set("labels_cex", .75) %>%
  color_branches(k=5) %>%
  set("branches_lwd", 3) %>%
  
  hang.dendrogram (hang = -1) %>%
  plot(horiz=T,main = "Industry Clusters", axes = F, xlim = c(10,0))
axis(side = 1, col = "blue", at = seq(0,15,5), labels = FALSE)
mtext(seq(0, 15,5), side = 1, at = seq(0,15,5),
      line = 1, col = "blue")
dend %>% rect.dendrogram(k = 5, horiz=T)



#use mar to set bottom, left, top and right margins good vertical plot
#For k = 4, need to spread limits, when k = 5, limit is 15

myPath <- file.path("Q:","Analytics", "CQ", "PDF Exhibits","cq5Clusters.pdf") 

pdf(file = myPath, onefile = T, width = 40, height = 15)

par(mar=c(3,3,2,4))
d <- dist(clus1, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 3) %>%
  set("labels_cex", .75) %>%
  color_branches(k=3) %>%
  set("branches_lwd", 3) %>%
  hang.dendrogram (hang = -1) %>%
  plot(horiz=F,main = "Industry Clusters", axes = F, ylim = c(0,100))
axis(side = 2, col = "blue", at = seq(0,100,10), labels = FALSE)
mtext(seq(0, 100,10), side = 2, at = seq(0,100,10),
      line = 1, col = "blue", las = 1)
dev.off()


labs <- dend %>% labels

head(labs)

labs[185:325]



#uses a sum aggregation
dfInd <- aggregate(clus1[,c(2:16)], by = list(Category = clus1$Industry2), sum)
rownames(dfInd) <- dfInd[,1]

#use a mean aggregation
dfInd <- aggregate(clus1[,c(2:16)], by = list(Category = clus1$Industry2), mean)
rownames(dfInd) <- dfInd[,1]


#plot as a horizontal this plot uses the sums
par(mar=c(3,2,2,12))
d <- dist(dfInd, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 5) %>%
  set("labels_cex", .75) %>%
  color_branches(k=5) %>%
  set("branches_lwd", 3) %>%
  
  hang.dendrogram (hang = -1) %>%
  plot(horiz=T,main = "Industry Clusters", axes = F, xlim = c(120,0))
axis(side = 1, col = "blue", at = seq(0,120,10), labels = FALSE)
mtext(seq(0, 120,10), side = 1, at = seq(0,120,10),
      line = 1, col = "blue")
dend %>% rect.dendrogram(k = 5, horiz=T)


#plot as a horizontal scale for using averages
myPath <- file.path("Q:","Analytics", "CQ", "PDF Exhibits","cqIndustryClusters.pdf") 

pdf(file = myPath, onefile = T, width = 11, height = 8.5)

par(mar=c(3,2,2,12))
d <- dist(dfInd, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 5) %>%
  set("labels_cex", .75) %>%
  color_branches(k=5) %>%
  set("branches_lwd", 3) %>%
  
  hang.dendrogram (hang = -1) %>%
  plot(horiz=T,main = "Industry Clusters using Average Scores", axes = F, xlim = c(5,0))
axis(side = 1, col = "blue", at = seq(0,5,1), labels = FALSE)
mtext(seq(0, 5,1), side = 1, at = seq(0,5,1),
      line = 1, col = "blue")
dend %>% rect.dendrogram(k = 2, horiz=T)
dev.off()


#get the labels
labsdfInd <- dend %>% labels


#use the industry tag as the rowname rather than the order 1 to 325
#does not work, row names must be unique and non-repeating
clus1Ind <- clus1
rownames(clus1Ind) <- clus1Ind[,1]


#plot as a horizontal
par(mar=c(3,2,2,12))
d <- dist(clus1Ind, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 5) %>%
  set("labels_cex", .75) %>%
  color_branches(k=5) %>%
  set("branches_lwd", 3) %>%
  
  hang.dendrogram (hang = -1) %>%
  plot(horiz=T,main = "Industry Clusters", axes = F, xlim = c(10,0))
axis(side = 1, col = "blue", at = seq(0,10,5), labels = FALSE)
mtext(seq(0, 10,5), side = 1, at = seq(0,10,5),
      line = 1, col = "blue")
dend %>% rect.dendrogram(k = 5, horiz=T)

labsClus1Ind <- dend %>% labels

#the next series of steps is for making a dataframe from the clusters
# and assigning the cluster number based on the sequence
id <- seq(1:325)
clusterdf <- data.frame(id, labs)

#view the tree find the branches
str(dend)

# add the dimensions
clusterdf$Cluster <- ifelse(clusterdf$id <=  32, "Clus1",
                           ifelse(clusterdf$id > 32 & clusterdf$id <= 81 , "Clus2",
                                  ifelse(clusterdf$id  > 81 & clusterdf$id <= 176, "Clus3",
                                         ifelse(clusterdf$id > 176 & clusterdf$id <= 242, "Clus4",
                                                "Clus5"))))

clus1$labs <- seq(1:325)
clusterdf <- cbind(clusterdf,dataRaw$CQScore)
names(clusterdf)[names(clusterdf)=="dataRaw$CQScore"] <- "CQScore"

#merge the dataframes
mergeDF <- merge(clusterdf,clus1, by = "labs")
head(mergeDF)

#melt for plotting
clusterMelt <- melt(mergeDF, id=c("Industry2", "labs", "id", "Cluster", "CQScore"), 
                    variable.name = "Question",
                    value.name = "Score")

head(clusterMelt,20)
tail(clusterMelt,20)

#add the dimensions
clusterMelt$Dimension <- ifelse(clusterMelt$Question == "Q11", "Openness",
                         ifelse(clusterMelt$Question %in% c("Q2", "Q7", "Q24"), "Relevance",
                         ifelse(clusterMelt$Question  %in% c("Q3","Q9","Q10","Q16"), "CX",
                         ifelse(clusterMelt$Question %in% c("Q1","Q13","Q17"), "Empathy",
                                                "EV"))))



# add the questions
clusterMelt$Text <- ifelse (clusterMelt$Question == "Q3", "CX is everyone's job",
               ifelse (clusterMelt$Question == "Q9", "CX-Cross-channel consistency",
               ifelse (clusterMelt$Question == "Q10","CX-Good use of my time",
               ifelse (clusterMelt$Question == "Q16","CX-Appreciates my loyalty",
               ifelse (clusterMelt$Question == "Q21","EV-I feel Smart",
               ifelse (clusterMelt$Question == "Q22","EV-I feel Proud",
               ifelse (clusterMelt$Question == "Q23","EV-Not ripped off",
               ifelse (clusterMelt$Question == "Q25","EV-Sense of Belonging",
               ifelse (clusterMelt$Question == "Q11", "Open dialogue",
               ifelse (clusterMelt$Question == "Q1","Emp-Gets me",
               ifelse (clusterMelt$Question == "Q13","Emp-Use own prods/svcs",
               ifelse (clusterMelt$Question == "Q17","Emp-Better intuition than others",
               ifelse (clusterMelt$Question == "Q2","Rel-Speaks my language",
               ifelse (clusterMelt$Question == "Q7","Rel-Shares my values", "Rel-Meets needs like no other"
               ))))))))))))))

head(clusterMelt)

#make factors
clusterMelt$Text <- as.factor(clusterMelt$Text)
clusterMelt$Dimension <- as.factor(clusterMelt$Dimension)
clusterMelt$Cluster <- as.factor(clusterMelt$Cluster)



#make some boxplots
position <- "bottom"
titleText <- "CQ Clusters"
pal5 <- rainbow(26)
pal4 <- rainbow(5)
pal3 <- rainbow(15)


P1 <- ggplot(clusterMelt, aes(x = Cluster, y = Score, fill = Cluster)) + 
  geom_boxplot(notch = T,col=c("black")) + scale_fill_manual(values = pal5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size = 3, fill = "white") +  
  facet_wrap(~Text) +
  theme(legend.position = position) +
  ylab("Score") + 
  xlab("") +
  theme(plot.title = element_text(face = "bold.italic", color = "blue"),
        axis.title.x=element_text(color = "red"),
        axis.title.y=element_text(color = "red"),
        axis.text.x=element_text(color = "white"),
        axis.text.y=element_text(color = "red"),
        strip.text.x = element_text(color = "blue"))
P1


P1 <- ggplot(clusterMelt, aes(x =Industry2, y = Score, fill = Industry2)) + 
  geom_boxplot(notch = F,col=c("black")) + scale_fill_manual(values = pal5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size = 3, fill = "white") +  
  facet_wrap(~Text) +
  xlab("") +
  theme(plot.title = element_text(face = "bold.italic", color = "blue"),
        axis.title.x=element_text(color = "red"),
        axis.title.y=element_text(color = "red"),
        axis.text.x=element_text(color = "white"),
        axis.text.y=element_text(color = "red"),
        strip.text.x = element_text(color = "blue"))
P1

P1 <- ggplot(clusterMelt, aes(x =Dimension, y = Score, fill = Dimension)) + 
  geom_boxplot(notch = T,col=c("black")) + scale_fill_manual(values = pal4) +
  stat_summary(fun.y="mean", geom="point", shape=23, size = 3, fill = "white") +  
  facet_wrap(~Cluster) +
  xlab("") +
  theme(plot.title = element_text(face = "bold.italic", color = "blue"),
        axis.title.x=element_text(color = "red"),
        axis.title.y=element_text(color = "red"),
        axis.text.x=element_text(color = "white"),
        axis.text.y=element_text(color = "red"),
        strip.text.x = element_text(color = "blue"))
P1


P1 <- ggplot(clusterMelt, aes(x =Text, y = Score, fill = Text)) + 
  geom_boxplot(notch = T,col=c("black")) + scale_fill_manual(values = pal3) +
  stat_summary(fun.y="mean", geom="point", shape=23, size = 3, fill = "white") +  
  facet_wrap(~Cluster) +
  xlab("") +
  theme(plot.title = element_text(face = "bold.italic", color = "blue"),
        axis.title.x=element_text(color = "red"),
        axis.title.y=element_text(color = "red"),
        axis.text.x=element_text(color = "white"),
        axis.text.y=element_text(color = "red"),
        strip.text.x = element_text(color = "blue"))
P1

P1 <- ggplot(clusterMelt, aes(x = Dimension, y = Score, fill = Dimension)) + 
  geom_boxplot(notch = F,col=c("black")) + scale_fill_manual(values = pal4) +
  stat_summary(fun.y="mean", geom="point", shape=23, size = 3, fill = "white") +  
  facet_wrap(~Industry2) +
  xlab("") +
  theme(plot.title = element_text(face = "bold.italic", color = "blue"),
        axis.title.x=element_text(color = "red"),
        axis.title.y=element_text(color = "red"),
        axis.text.x=element_text(color = "white"),
        axis.text.y=element_text(color = "red"),
        strip.text.x = element_text(color = "blue"))
P1

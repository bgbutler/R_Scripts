# text mining of sam's club recent survey

library(tm)
library(dplyr)
library(htmltools)
library(devtools)
library(SnowballC)
library(RTextTools)
library(stringi)
library(wordcloud)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(dendextend)
library(dendextendRcpp)
library(openxlsx)
library(rJava)
library(RWeka)  #for making tokens out of the phrases



#get all data
urlAll <- "Q:/Analytics/CQ/CSV Data/Sams.csv"
rawData <- read.csv(urlAll, header = TRUE, na.strings = c('', "NA"), sep=",", as.is = FALSE)

#convert to lower
rawData$Essay <- tolower(rawData$Essay)

#remove NAs
rawData <- na.omit(rawData)

unique(rawData$Segment)

retail <- filter(rawData, Segment == "Care Oranization"|
                          Segment =="New Mom"|
                          Segment =="Social Couple"|
                          Segment =="Large Family")

prof <- filter(rawData, Segment == "Professional Services Business")

b2b <- filter(rawData, Segment == "Neighborhood Family"|
                Segment =="Foodservice/Restaurant"|
                Segment =="Convenience Store/Reseller")


#process the text
#start building the corpus

processText <- function(inputText, corpName){
  
  outputName <- Corpus(VectorSource(inputText))
  outputName <- tm_map(outputName,PlainTextDocument)
  outputName <- tm_map(outputName, removeWords, stopwords("english"))
  outputName <- tm_map(outputName, removePunctuation)
  outputName <- tm_map(outputName, removeNumbers)
  outputName <- tm_map(outputName, stripWhitespace)
  outputName <- tm_map(outputName, removeWords, c("sams", "feel", "club",
                                            "shop", "shopping"))
  assign(corpName, outputName, envir = .GlobalEnv)
  return(retailCorp)
}




#################
#for dendrograms
#make it a dataframe add in states, roll up start with Document Term Matrix

textDTM <- DocumentTermMatrix(textData)
textDF <- as.data.frame(as.matrix(textDTM))
textDF$Segment <- as.factor(rawData$Segment)
dfSeg <- aggregate(textDF[,-length(textDF)], by = list(textDF$Segment), sum)

#setting the sum equal to a frequency for the entire set
dfSegRed <- dfSeg[,colSums(dfSeg[,2:length(dfSeg)]) > 2]
rownames(dfSegRed) <- dfSeg[,1]

head(dfSegRed,8)
#################################


##########
#cluster by segment
#use mai to set bottom, left, top and right margins good horizontal plot
par(mar=c(2,2,2,10))
d <- dist(dfSegRed, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 8) %>%
  set("labels_cex", .75) %>%
  color_branches(k=8) %>%
  set("branches_lwd", 3) %>%
  
  hang.dendrogram (hang = -1) %>%
  plot(horiz=T,main = "Word Clusters by Segment", axes = F, xlim = c(60,0))
axis(side = 1, col = "blue", at = seq(0,60,20), labels = FALSE)
mtext(seq(0, 60,20), side = 1, at = seq(0,60,20),
      line = 1, col = "blue")
dend %>% rect.dendrogram(k = 8, horiz=T)



#use mai to set bottom, left, top and right margins good vertical plot
par(mar=c(3,2,2,4))
d <- dist(dfSegRed, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 10) %>%
  set("labels_cex", .75) %>%
  color_branches(k=10) %>%
  set("branches_lwd", 3) %>%
  hang.dendrogram (hang = -1) %>%
  plot(horiz=F,main = "Word Clusters by State", axes = F, ylim = c(0,60))
axis(side = 2, col = "blue", at = seq(0,60,20), labels = FALSE)
mtext(seq(0, 60,20), side = 2, at = seq(0,60,20),
      line = 1, col = "blue", las = 1)
#dend %>% rect.dendrogram(k = 8, horiz=T)


#check clusters by response
cqClusters <- rawData[,c(1,5:21)]
outCome <- cqClusters[,1]

samsDist <- cqClusters[,c(2:18)]

#check the clustering of the responses
par(mar=c(3,2,2,4))
d <- dist(samsDist, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 10) %>%
  set("labels_cex", .75) %>%
  color_branches(k=8) %>%
  set("branches_lwd", 3) %>%
  hang.dendrogram (hang = -1) %>%
  plot(horiz=F,main = "Response Clusters", axes = F, ylim = c(0,30))
axis(side = 2, col = "blue", at = seq(0,30,20), labels = FALSE)
mtext(seq(0, 30,20), side = 2, at = seq(0,30,20),
      line = 1, col = "blue", las = 1)



#this deconstructs and maps the clusters to segments
labels <- dend %>% labels
k_8 <- cutree(dend, k = 8, order_clusters_as_data = FALSE)

#confirm same number
sum(table(k_8))
sum(table(outCome))

outComeDf <- data.frame(seq(1:907),outCome)
newnames <- c("labels", "Segment")
colnames(outComeDf) <- newnames

head(outComeDf)

fullDf <- cbind(outComeDf, cqClusters)

samsDf <- data.frame(labels,k_8)

head(samsDf)

mergeDF <- merge(samsDf, fullDf, by = "labels")

head(mergeDF)

#export the data as a CSV - name the file
write.csv(mergeDF, file = "Q:/Analytics/CQ/CSV Data/samsSegments.csv", row.names = FALSE)


table(mergeDF$Segment,mergeDF$k_8)


###########################################
#start the text processing
output <- file.path("Q:","Analytics", "CQ", "PDF Exhibits")
setwd(output)



#use a function to build wordclouds
cloud <- function(inputCorpus, fileName, minFreq){
  allTDM <- TermDocumentMatrix(inputCorpus, control = list(tokenize = ngramTokenizer))
  tokenMatrix <- as.matrix(allTDM)

  #sort it most frequent to least frequent and create a table
  tokenV <- sort(rowSums(tokenMatrix), decreasing = TRUE)
  token.d <- data.frame(word = names(tokenV), freq=tokenV)
  table(token.d$freq)

  #build giant cloud
  png(paste(fileName,".png",sep = ""), width = 1280, height = 800)
  wordcloud(token.d$word, token.d$freq,scale=c(4,1), min.freq = minFreq, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off()
}

#this is the body of the program using the functions

processText(retail$Essay,"retailCorp")
processText(prof$Essay,"profCorp")
processText(b2b$Essay,"b2bCorp")


#make the ngrams using a function
ngramTokenizer <- function (x) {NGramTokenizer(x,
                                               Weka_control(min = 4, max = 4))}

cloud(retailCorp, "retail4Word", 1)
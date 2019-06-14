# text mapping for boa and Merrill

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
library(RWeka)  #for making tokens out of the phrases


#check the directory and change for this file
cwd <- getwd()
myPath <- file.path("Q:","Analytics", "BoAMerrill", "CombinedFilesForText")
output <- file.path("Q:","Analytics", "BoAMerrill", "CombinedFilesForText", "CleanRun")
setwd(myPath)

#loop through all files and combine
#start with the .xlsx files then the .xls files
conv <- data.frame()

#read the .xlsx files
url <- "Q:/Analytics/BoAMerrill/CombinedFilesForText/"
files <- list.files(url, full.names = TRUE, pattern = ".xlsx")
count <- length(files)

for (i in 1:count){
  conv <- rbind(conv, 
               read.xlsx(files[i], sheet = 1, startRow = 1, colNames = T))
  print(files[i])
}


#create some ordered factors
members$AgeRange <- factor(members$AgeRange, ordered = TRUE)



#remove the facilitator
grx <- glob2rx("*Facilitator*")
members <- with(conv, subset(conv, subset = !grepl(grx, conv$LastName), drop = TRUE))


#start building the corpus
textData <- Corpus(VectorSource(members$Post))
textData <- tm_map(textData,PlainTextDocument)
#textData <- tm_map(textData, removeWords, stopwords("english"))
#textData <- tm_map(textData, removePunctuation)
textData <- tm_map(textData, removeNumbers)
textData <- tm_map(textData, stripWhitespace)

textData <- tm_map(textData, removeWords, c("biggest", "financial", "concern",
                                            "top", "mind", "now", "my"))



output <- file.path("Q:","Analytics", "BoAMerrill", "CombinedFilesForText", "CleanRun")
setwd(output)

textTDM <- TermDocumentMatrix(textData)

textMatrix <- as.matrix(textTDM)
#sort it most frequent to least frequent and create a table
textV <- sort(rowSums(textMatrix), decreasing = TRUE)
text.d <- data.frame(word = names(textV), freq=textV)
table(text.d$freq)

#build large single word cloud
png("boaCloudAll.png", width = 1280, height = 800)
wordcloud(text.d$word, text.d$freq,scale=c(8,.2), min.freq = 3, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))

dev.off()

#make the ngrams
ngramTokenizer <- function (x) {NGramTokenizer(x,
                                               Weka_control(min = 10, max = 10))}

allTDM <- TermDocumentMatrix(textData, control = list(tokenize = ngramTokenizer))
tokenMatrix <- as.matrix(allTDM)

#sort it most frequent to least frequent and create a table
tokenV <- sort(rowSums(tokenMatrix), decreasing = TRUE)
token.d <- data.frame(word = names(tokenV), freq=tokenV)
table(token.d$freq)

#build giant cloud
png("boaAllWords10Word.png", width = 1280, height = 800)
wordcloud(token.d$word, token.d$freq,scale=c(4,1), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off()

#create one for each community
grx <- glob2rx("*CEF*")
CEF <- subset(members, subset = grepl(grx, members$Community))
cefText <- Corpus(VectorSource(CEF$Post))
cefText <- tm_map(cefText,PlainTextDocument)
cefText <- tm_map(cefText, content_transformer(tolower))
cefText <- tm_map(cefText, removeWords, stopwords("english"))
cefText <- tm_map(cefText, removePunctuation)
cefText <- tm_map(cefText, removeNumbers)
cefText <- tm_map(cefText, stripWhitespace)


grx <- glob2rx("*bef*")
bef <- subset(members, subset = grepl(grx, members$Community))
befText <- Corpus(VectorSource(bef$Post))
befText <- tm_map(befText,PlainTextDocument)
befText <- tm_map(befText, content_transformer(tolower))
befText <- tm_map(befText, removeWords, stopwords("english"))
befText <- tm_map(befText, removePunctuation)
befText <- tm_map(befText, removeNumbers)
befText <- tm_map(befText, stripWhitespace)


grx <- glob2rx("*ME*")
ME <- subset(members, subset = grepl(grx, members$Community))
meText <- Corpus(VectorSource(ME$Post))
meText <- tm_map(meText,PlainTextDocument)
meText <- tm_map(meText, content_transformer(tolower))
meText <- tm_map(meText, removeWords, stopwords("english"))
meText <- tm_map(meText, removePunctuation)
meText <- tm_map(meText, removeNumbers)
meText <- tm_map(meText, stripWhitespace)

#################    make the ngrams of each and plot them
befTDM <- TermDocumentMatrix(befText, control = list(tokenize = ngramTokenizer))
cefTDM <- TermDocumentMatrix(cefText, control = list(tokenize = ngramTokenizer))
meTDM <- TermDocumentMatrix(meText, control = list(tokenize = ngramTokenizer))

tokenMatrixbef <- as.matrix(befTDM)
tokenMatrixcef <- as.matrix(cefTDM)
tokenMatrixme <- as.matrix(meTDM)

tokenV_bef <- sort(rowSums(tokenMatrixbef), decreasing = TRUE)
token.d_bef <- data.frame(word = names(tokenV_bef), freq=tokenV_bef)
table(token.d_bef$freq)

tokenV_cef <- sort(rowSums(tokenMatrixcef), decreasing = TRUE)
token.d_cef <- data.frame(word = names(tokenV_cef), freq=tokenV_cef)
table(token.d_cef$freq)

tokenV_me <- sort(rowSums(tokenMatrixme), decreasing = TRUE)
token.d_me <- data.frame(word = names(tokenV_me), freq=tokenV_me)
table(token.d_me$freq)

#bef
png("tokenMultiCloud.png")
par(fig= c(.25,.75,.55,.9))
wordcloud(token.d_bef$word, token.d_bef$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))

#cef
par(fig= c(0,.45,0,.45), new = TRUE)
wordcloud(token.d_cef$word, token.d_cef$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))

#me
par(fig= c(.55,1,0,.45), new = TRUE)
wordcloud(token.d_me$word, token.d_me$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))

#add the titles 
mtext("bef", outer = TRUE, side=3, line = -3, col = "blue", cex = 1.5)
mtext("CEF",side=3,line= -18, at=0.20, outer = TRUE, col = "blue", cex = 1.5)
mtext("ME",side=3,line= -18, at=0.8, outer = TRUE,col = "blue", cex = 1.5)
dev.off()


###############################   Last Plot
#check conversations by age range
#make it a dataframe add in states, roll up start with Document Term Matrix

textDTM <- DocumentTermMatrix(textData)
textDF <- as.data.frame(as.matrix(textDTM))
textDF$AgeRange <- as.factor(members$AgeRange)
dfAge <- aggregate(textDF[,-length(textDF)], by = list(textDF$AgeRange), sum)

#remove sparse terms and columns by 
#setting the sum equal to a frequency for the entire set
dfAgeRed <- dfAge[,colSums(dfAge[,2:length(dfAge)]) > 5]
rownames(dfAgeRed) <- dfAge[,1]


dis <- as.dist(1 - cov2cor(cov(dfAgeRed[,c(1:710)], 
                                       method = "pearson", 
                                       use = "pairwise.complete.obs")))

par(mai=c(1.0,2.0, 1.0,.75))
d <- dist(dfAgeRed, method = "Euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 3) %>%
  set("labels_cex", .75) %>%
  color_branches(k=3) %>%
  set("branches_lwd", 3) %>%
  plot(horiz=TRUE,main = "Word Clusters by Age Groups", axes = FALSE, xlim = c(20,100))
axis(side = 1, col = "blue", at = seq(0,100,20), labels = FALSE)
mtext(seq(20, 100,20), side = 1, at = seq(20,100,20),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = 5, horiz=T)


dis <- as.dist(1 - cov2cor(cov(dfAgeRed[,c(1:710)], 
                               method = "pearson", 
                               use = "pairwise.complete.obs")))


#use mai to set bottom, left, top and right margins good vertical plot
par(mai=c(2.0,1.0, 1.0,1.0))
d <- dist(dfAgeRed, method = "Euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 5) %>%
  set("labels_cex", .75) %>%
  color_branches(k=5) %>%
  set("branches_lwd", 3) %>%
  hang.dendrogram (hang = -1) %>%
  plot(horiz=F,main = "Word Clusters by Age Groups", axes = FALSE, ylim = c(20,100))
axis(side = 2, col = "blue", at = seq(20,100,20), labels = FALSE)
mtext(seq(20, 100,20), side = 2, at = seq(20,100,20),
      line = 1, col = "blue", las = 1)
dend %>% rect.dendrogram(k = 5, horiz=T)














### remove biggest financial concern

textDataRed <- tm_map(textData, removeWords, c("biggest", "financial", "concern", "top", "mind", "now"))



#make the ngrams
ngramTokenizer <- function (x) {NGramTokenizer(x,
                                               Weka_control(min = 5, max = 5))}

allTDMRed <- TermDocumentMatrix(textDataRed, control = list(tokenize = ngramTokenizer))
tokenMatrix <- as.matrix(allTDMRed)

#sort it most frequent to least frequent and create a table
tokenV <- sort(rowSums(tokenMatrix), decreasing = TRUE)
token.d <- data.frame(word = names(tokenV), freq=tokenV)
table(token.d$freq)

#build giant cloud
png("boaAllWords5Red.png", width = 1280, height = 800)
wordcloud(token.d$word, token.d$freq,scale=c(2,1), min.freq = 2, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off()



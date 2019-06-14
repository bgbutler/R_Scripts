#This is a text mining script to do word phrases
#This uses the tokenization from RWeka to make n-grams

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
library(RWeka)  #for making tokens out of the phrases


url <- "K:/Sandbox/R/HealthNet/HNandCSCombined.csv"
dataRaw <- read.csv(url, header = TRUE, sep=",", as.is = TRUE)

#get the clean text
dataRaw$TEXT <- iconv(dataRaw$TEXT, "latin1", "ASCII", sub="")

#remove the date field and create factors
dataRaw$DATE <- NULL
dataRaw$State <- as.factor(dataRaw$State)
dataRaw$Activity <- as.factor(dataRaw$Activity)
dataRaw$OnEx <- as.factor(dataRaw$OnEx)
dataRaw$Provider <- as.factor(dataRaw$Provider)
dataRaw$TITLE <- as.factor(dataRaw$TITLE)


dataRaw$stateProvider <- as.factor(
                    paste(dataRaw$State, dataRaw$Provider, sep = "-"))

newNames <- c("Title", "Text", "State","Activity", "Exchange", "Provider",
              "Length", "StateProvider")

colnames(dataRaw) <- newNames

#create revised stopwords list
newWords <- stopwords("english")
keep <- c("no", "more", "not")
newWords <- newWords [! newWords %in% keep]

#start building the master corpus
textData <- Corpus(VectorSource(dataRaw$Text))
textData <- tm_map(textData,PlainTextDocument)
textData <- tm_map(textData, removeWords, newWords)
textData <- tm_map(textData, removePunctuation)
textData <- tm_map(textData, removeNumbers)
textData <- tm_map(textData, stripWhitespace)
textData <- tm_map(textData, removeWords, c("net"))

# see if there are clusters by the state provider
textDTM <- DocumentTermMatrix(textData)
textDF <- as.data.frame(as.matrix(textDTM))
textDF$stateProvider <- as.factor(dataRaw$StateProvider)
dfstateProv <- aggregate(textDF[,-length(textDF)], by = list(textDF$stateProvider), sum)

#remove sparse terms and columns by 
#setting the sum equal to a frequency for the entire set
dfstateProvRed <- dfstateProv[,colSums(dfstateProv[,2:length(dfstateProv)]) > 2]
rownames(dfstateProvRed) <- dfstateProv[,1]

par(mai=c(1.0,2.0, 1.0,.75))
d <- dist(dfstateProvRed)
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 10) %>%
  set("labels_cex", .75) %>%
  set("branches_k_color", k=10) %>%
  set("branches_lwd", 3) %>%
  hang.dendrogram(hang = -1) %>%
  plot(horiz=TRUE,main = "Word Clusters for State & Provider", axes = FALSE, xlim = c(20,100))
axis(side = 1, col = "blue", at = seq(20,100,20), labels = FALSE)
mtext(seq(20, 100,20), side = 1, at = seq(20,100,20),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = 7, horiz = TRUE)

#split by relevent factor
#split function might be better alternative
textFactors <- split(dataRaw, dataRaw$StateProvider)

# loop through list to create corpi
count = length(textFactors)
names <- names(textFactors)
names <- c("No-State", names[2:10])
names(textFactors) <- names


#create a list of corpi based on the state provider
cp <- list()
for (i in 1:9){
  z <- Corpus(VectorSource(textFactors[[i]][[2]]))
  z <- tm_map(z,PlainTextDocument)
  z <- tm_map(z, content_transformer(tolower))
  z <- tm_map(z, removeWords, newWords)
  z <- tm_map(z, removePunctuation)
  z <- tm_map(z, removeNumbers)
  z <- tm_map(z, stripWhitespace)
  cp[[i]] <- z
}


#set the output file
output <- file.path("K:","Sandbox", "R", "HealthNet")
setwd(output)

#find some of the lengths of the data



caHMO <- textFactors[[7]]
grx <- glob2rx("*On*")
caON <- subset(caHMO, subset = grepl(grx, caHMO$Exchange))

grx <- glob2rx("*C Space*")
caTest <- subset(caON, subset = grepl(grx, caON$Activity))


#set number of words to parse
t = 4

#Do some more text work with tokens - cut into sentence fragments make a reduced set of observations
ngramTokenizer <- function (x) {NGramTokenizer(x,
                                               Weka_control(min = t, max = t))}

#create a list of TDMS etc to process each file
tokenTDM <- list()
for (i in 1:9){
tokenTDM[[i]] <- TermDocumentMatrix(cp[[i]], control = list(tokenize = ngramTokenizer))
}


#do a one-off
tokenTDM[[7]] <- TermDocumentMatrix(cp[[7]], control = list(tokenize = ngramTokenizer))
inspect(tokenTDM[[7]])

#change i to make the plot
i <- 8
tokenMatrix <- as.matrix(tokenTDM[[i]])

#sort it most frequent to least frequent and create a table
tokenV <- sort(rowSums(tokenMatrix), decreasing = TRUE)
token.d <- data.frame(word = names(tokenV), freq=tokenV)
table(token.d$freq)

pngName = names[i]

png(paste(pngName, t,"Words",".png",sep = ""), width = 1280, height = 800)
wordcloud(token.d$word, token.d$freq,scale=c(6,.5), min.freq = 1, max.words = 400,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = pngName, col.main = "blue", cex.main = 2, font.main = 4)
dev.off()


##### Try some dendrograms

tdm <- removeSparseTerms(tokenTDM[[i]], sparse=.999)
a.tdm.df <- as.data.frame(inspect(tokenTDM[[i]]))
a.tdm.red <- head(a.tdm.df,10)
a.tdm.df.scale <- scale(a.tdm.red)
d <- dist(a.tdm.df.scale, method = "euclidean") 
fit <- hclust(d, method="ward")
plot(fit)




a.tdm.red <- head(token.d,10)
a.tdm.df.scale <- scale(a.tdm.red)
d <- dist(a.tdm.df.scale, method = "euclidean") 
fit <- hclust(d, method="ward")
plot(fit)


###### plot with good dendrogram color
d <- dist(a.tdm.df.scale, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 10) %>%
  set("labels_cex", .75) %>%
  set("branches_k_color", k=10) %>%
  set("branches_lwd", 3) %>%
  hang.dendrogram(hang = -1) %>%
  plot(horiz=TRUE,main = "Word Clusters", axes = FALSE, xlim = c(0,8))
axis(side = 1, col = "blue", at = seq(0,8,4), labels = FALSE)
mtext(seq(0, 8,4), side = 1, at = seq(0,8,4),
      line = 1, col = "blue", las = 1)
dend

d <- dist(a.tdm.df.scale, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = 10) %>%
  set("labels_cex", .75) %>%
  set("branches_k_color", k=10) %>%
  set("branches_lwd", 3) %>%
  hang.dendrogram(hang = -1) %>%
  plot(horiz=FALSE,main = "Word Clusters", axes = FALSE, xlim = c(0,10))
axis(side = 2, col = "blue", at = seq(0,10,5), labels = FALSE)
mtext(seq(0, 10,5), side = 2, at = seq(0,10,5),
      line = 1, col = "blue", las = 2)
dend

head(a.tdm.df)

####### one last test plot
topCaHMO <- head(token.d,40)

caHMOP <- ggplot(topCaHMO, aes(x = freq, y = reorder(word, freq))) + 
  geom_point(size = 2) + scale_x_continuous(breaks = seq(1,5,1)) +
  ggtitle("Top CA HMO Phrases") + 
  xlab("Frequency of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
caHMOP


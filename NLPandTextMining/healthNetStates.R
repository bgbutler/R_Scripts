#### healthnet working around the State

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

#CA HMO is the largest group split it out
#split function might be better alternative
textFactors <- split(dataRaw, dataRaw$StateProvider)

caHMO <- textFactors[[7]]
grx <- glob2rx("*On*")
caON <- subset(caHMO, subset = grepl(grx, caHMO$Exchange))

#plot the distribution of length
plt0 <- ggplot(caON, aes(x= Length)) + geom_histogram(binwidth = 20, fill = "tomato") +
  xlim(0,1500) +
  theme(
    legend.position="right",
    axis.title.x = element_text(colour = "red"),
    axis.title.y = element_text(colour = "red"),
    legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Distribution of Length of Text") +
  theme(plot.title = element_text(size = rel(1.5), 
                                  face = "bold.italic", color = "blue"))
plt0

#split the data by the length of characters at 250
caShort <- caON[caON$Length <= 100,]
caLong <- caON[caON$Length > 100,]


#create revised stopwords list
newWords <- stopwords("english")
keep <- c("no", "more", "not")
newWords <- newWords [! newWords %in% keep]


#perform text processing to make the matrices
  cShort <- Corpus(VectorSource(caShort))
  cShort <- tm_map(cShort,PlainTextDocument)
  cShort <- tm_map(cShort, content_transformer(tolower))
  cShort <- tm_map(cShort, removeWords, newWords)
  cShort <- tm_map(cShort, removePunctuation)
  cShort <- tm_map(cShort, removeNumbers)
  cShort <- tm_map(cShort, stripWhitespace)

  cLong <- Corpus(VectorSource(caLong))
  cLong <- tm_map(cLong,PlainTextDocument)
  cLong <- tm_map(cLong, content_transformer(tolower))
  cLong <- tm_map(cLong, removeWords, newWords)
  cLong <- tm_map(cLong, removePunctuation)
  cLong <- tm_map(cLong, removeNumbers)
  cLong <- tm_map(cLong, stripWhitespace)

  #set number of words to parse
  t = 3
  #Do some more text work with tokens - cut into sentence fragments make a reduced set of observations
  ngramTokenizer <- function (x) {NGramTokenizer(x,
                                                 Weka_control(min = t, max = t))} 
  
  shortTDM <- TermDocumentMatrix(cShort, control = list(tokenize = ngramTokenizer))
  longTDM <- TermDocumentMatrix(cLong, control = list(tokenize = ngramTokenizer))
  
  
  shortMatrix <- as.matrix(shortTDM)
  longMatrix <- as.matrix(longTDM)
  
  
############################
# Set the output
  
#set the output file
output <- file.path("K:","Sandbox", "R", "HealthNet")
setwd(output)  
  
  
  
  
################################################################################
#work with the short set
  
  #sort it most frequent to least frequent and create a table
  shortV <- sort(rowSums(shortMatrix), decreasing = TRUE)
  short.d <- data.frame(word = names(shortV), freq=shortV)
  table(short.d$freq)
  
  pngName = "CaHMOShortEssay"
  
  png(paste(pngName, t,"Words",".png",sep = ""), width = 1280, height = 800)
  wordcloud(short.d$word, short.d$freq,scale=c(6,.5), min.freq = 1, max.words = 400,
            random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
            colors = brewer.pal(8,"Dark2"))
  title(main = pngName, col.main = "blue", cex.main = 2, font.main = 4)
  dev.off()
  
  
#################### 
# make the long ones
  #sort it most frequent to least frequent and create a table
  longV <- sort(rowSums(longMatrix), decreasing = TRUE)
  long.d <- data.frame(word = names(longV), freq=longV)
  table(long.d$freq)
  
  pngName = "CaHMOlongEssay"
  
  png(paste(pngName, t,"Words",".png",sep = ""), width = 1280, height = 800)
  wordcloud(long.d$word, long.d$freq,scale=c(6,.5), min.freq = 1, max.words = 400,
            random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
            colors = brewer.pal(8,"Dark2"))
  title(main = pngName, col.main = "blue", cex.main = 2, font.main = 4)
  dev.off()
  
  
  ###########################################
  #find themes in data
  
  #theme1 = coverage
  grx <- glob2rx("*coverage*")
  theme1 <- subset(members, subset = grepl(grx, members$Post))
  #start building the corpus
  theme1 <- Corpus(VectorSource(theme1$Post))
  theme1 <- tm_map(theme1,PlainTextDocument)
  theme1 <- tm_map(theme1, removeWords, stopwords("english"))
  theme1 <- tm_map(theme1, removePunctuation)
  theme1 <- tm_map(theme1, removeNumbers)
  theme1 <- tm_map(theme1, stripWhitespace)
  
  
  
  
  
  
  

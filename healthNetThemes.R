####### Healthnet themes
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

#create revised stopwords list
newWords <- stopwords("english")
keep <- c("no", "more", "not")
healthNet <- c("health", "net", "nets")
newWords <- newWords [! newWords %in% keep]


dataRaw$Text <- tolower(dataRaw$Text)


###########################################
#find themes in data

themes <- c("cover", "website", "service", "pay", "care", "not", "difficult")

#theme1 = coverage
grx <- glob2rx("*cover*")
theme1 <- subset(dataRaw, subset = grepl(grx, dataRaw$Text))
coverage <- NROW(theme1)
#start building the corpus
theme1 <- Corpus(VectorSource(theme1$Text))
theme1 <- tm_map(theme1,PlainTextDocument)
theme1 <- tm_map(theme1, removeWords, newWords)
theme1 <- tm_map(theme1, removeWords, healthNet)
theme1 <- tm_map(theme1, removePunctuation)
theme1 <- tm_map(theme1, removeNumbers)
theme1 <- tm_map(theme1, stripWhitespace)


#theme2 = website
grx <- glob2rx("*website*")
theme2 <- subset(dataRaw, subset = grepl(grx, dataRaw$Text))
website <- NROW(theme2)
#start building the corpus
theme2 <- Corpus(VectorSource(theme2$Text))
theme2 <- tm_map(theme2,PlainTextDocument)
theme2 <- tm_map(theme2, removeWords, newWords)
theme2 <- tm_map(theme2, removeWords, healthNet)
theme2 <- tm_map(theme2, removePunctuation)
theme2 <- tm_map(theme2, removeNumbers)
theme2 <- tm_map(theme2, stripWhitespace)

#theme3 = website
grx <- glob2rx("*service*")
theme3 <- subset(dataRaw, subset = grepl(grx, dataRaw$Text))
service <- NROW(theme3)
#start building the corpus
theme3 <- Corpus(VectorSource(theme3$Text))
theme3 <- tm_map(theme3,PlainTextDocument)
theme3 <- tm_map(theme3, removeWords, newWords)
theme3 <- tm_map(theme3, removeWords, healthNet)
theme3 <- tm_map(theme3, removePunctuation)
theme3 <- tm_map(theme3, removeNumbers)
theme3 <- tm_map(theme3, stripWhitespace)

grx <- glob2rx("*pay*|*bill*")
theme4 <- subset(dataRaw, subset = grepl(grx, dataRaw$Text))
pay <- NROW(theme4)
#start building the corpus
theme4 <- Corpus(VectorSource(theme4$Text))
theme4 <- tm_map(theme4,PlainTextDocument)
theme4 <- tm_map(theme4, removeWords, newWords)
theme4 <- tm_map(theme4, removeWords, healthNet)
theme4 <- tm_map(theme4, removePunctuation)
theme4 <- tm_map(theme4, removeNumbers)
theme4 <- tm_map(theme4, stripWhitespace)

grx <- glob2rx("*primary care*")
theme5 <- subset(dataRaw, subset = grepl(grx, dataRaw$Text))
care <- NROW(theme5)
#start building the corpus
theme5 <- Corpus(VectorSource(theme5$Text))
theme5 <- tm_map(theme5,PlainTextDocument)
theme5 <- tm_map(theme5, removeWords, newWords)
theme5 <- tm_map(theme5, removeWords, healthNet)
theme5 <- tm_map(theme5, removePunctuation)
theme5 <- tm_map(theme5, removeNumbers)
theme5 <- tm_map(theme5, stripWhitespace)


grx <- glob2rx("*not*")
theme6 <- subset(dataRaw, subset = grepl(grx, dataRaw$Text))
not <- NROW(theme6)
#start building the corpus
theme6 <- Corpus(VectorSource(theme6$Text))
theme6 <- tm_map(theme6,PlainTextDocument)
theme6 <- tm_map(theme6, removeWords, newWords)
theme6 <- tm_map(theme6, removeWords, healthNet)
theme6 <- tm_map(theme6, removePunctuation)
theme6 <- tm_map(theme6, removeNumbers)
theme6 <- tm_map(theme6, stripWhitespace)


grx <- glob2rx("*difficult*")
theme7 <- subset(dataRaw, subset = grepl(grx, dataRaw$Text))
not <- NROW(theme7)
#start building the corpus
theme7 <- Corpus(VectorSource(theme7$Text))
theme7 <- tm_map(theme7,PlainTextDocument)
theme7 <- tm_map(theme7, removeWords, newWords)
theme7 <- tm_map(theme7, removeWords, healthNet)
theme7 <- tm_map(theme7, removePunctuation)
theme7 <- tm_map(theme7, removeNumbers)
theme7 <- tm_map(theme7, stripWhitespace)



#set number of words to parse
t = 5

#Do some more text work with tokens - cut into sentence fragments make a reduced set of observations
ngramTokenizer <- function (x) {NGramTokenizer(x,
                                               Weka_control(min = t, max = t))}

#create a list of TDMS etc to process each file
tokenTDM <- list()
tokenTDM[[1]] <- TermDocumentMatrix(theme1,control = list(tokenize = ngramTokenizer))
tokenTDM[[2]] <- TermDocumentMatrix(theme2,control = list(tokenize = ngramTokenizer))
tokenTDM[[3]] <- TermDocumentMatrix(theme3,control = list(tokenize = ngramTokenizer))
tokenTDM[[4]] <- TermDocumentMatrix(theme4,control = list(tokenize = ngramTokenizer))
tokenTDM[[5]] <- TermDocumentMatrix(theme5,control = list(tokenize = ngramTokenizer))
tokenTDM[[6]] <- TermDocumentMatrix(theme6,control = list(tokenize = ngramTokenizer))
tokenTDM[[7]] <- TermDocumentMatrix(theme7,control = list(tokenize = ngramTokenizer))



#set the output file
output <- file.path("K:","Sandbox", "R", "HealthNet", "Themes")
setwd(output)



i <- 7
#change i to make the plot
for (i in 1:length(themes)){
tokenMatrix <- as.matrix(tokenTDM[[i]])

#sort it most frequent to least frequent and create a table
tokenV <- sort(rowSums(tokenMatrix), decreasing = TRUE)
token.d <- data.frame(word = names(tokenV), freq=tokenV)
table(token.d$freq)

pngName = themes[i]

png(paste(pngName, t,"Words",".png",sep = ""), width = 1280, height = 800)
wordcloud(token.d$word, token.d$freq,scale=c(6,.5), min.freq = 1, max.words = 400,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = pngName, col.main = "blue", cex.main = 2, font.main = 4)
dev.off()
}


############################################
#### make the correlation phrase plots

top <- list()
for (i in 1:length(themes)){
  tokenMatrix <- as.matrix(tokenTDM[[i]])
  
  #sort it most frequent to least frequent and create a table
  tokenV <- sort(rowSums(tokenMatrix), decreasing = TRUE)
  token.d <- data.frame(word = names(tokenV), freq=tokenV)

topData <- head(token.d,40)

#set the name of the pdf file
pdfName <- paste("Top ", names[i], " Phrases.pdf", sep = "")
pdf(file = pdfName, onefile = F, paper = "USr", width = 8.5, height = 11)

top[[i]] <- ggplot(topData, aes(x = freq, y = reorder(word, freq))) + 
  geom_point(size = 2) +
  ggtitle(sprintf("Top Phrases Associated with %s",themes[i])) + 
  xlab("Frequency of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
print(top[[i]])
dev.off()
}


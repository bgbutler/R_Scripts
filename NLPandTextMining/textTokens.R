#This is a text mining script to do word phrases
#This uses the tokenization from RWeka to make n-grams

library(stringi)
library(RColorBrewer)
library(ggplot2)
library(tm)
library(wordcloud)
library(RWeka)  #for making tokens out of the phrases
library(maps)
library(dplyr)
library(htmltools)
library(devtools)
library(leaflet)
library(SnowballC)
library(RTextTools)
library(ggthemes)
library(zipcode)

#get the zipcode data
data(zipcode)

url <- "K:/Sandbox/R/Data IO/WalMartv2.csv"
dataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get the clean text - drops 15 observations
dataRaw <- dataRaw[stri_enc_isutf8(dataRaw$Essay)== T,]

#pick up state, zip and essay
dataState <- dataRaw[,c(13:18)]

#remove the NA
dataState <- na.omit(dataState)

#merge with the zipcode data to get lat and lon
#convert the zipcodes to char
dataState$Zip <-as.character(dataState$Zip)
dataState$Zip <- clean.zipcodes(dataState$Zip)
dataZip <- merge(dataState, zipcode, by.x = 'Zip', by.y = 'zip')

#remove the raw data
rm(dataRaw)
rm(zipcode)
rm(dataState)

#clean up column names
newNames <- c("Zip", "Income", "Status", "Household", "State", "Essay",
              "City", "Abbrev", "latitide", "longitude")
colnames(dataZip) <- newNames

dataZip$Essay <- tolower(dataZip$Essay)
dataZip$Essay <- as.character(dataZip$Essay)

#potential service issues
#full dataset is too large, need to create a subset
grx <- glob2rx("*long*|*service*|*line*|*cashier*")
serviceText <- subset(dataZip, subset = grepl(grx, dataZip$Essay))





#start building the corpus
textData <- Corpus(VectorSource(dataZip$Essay))

textData <- tm_map(textData,PlainTextDocument)
textData <- tm_map(textData, removeWords, stopwords("english"))
textData <- tm_map(textData, removePunctuation)
textData <- tm_map(textData, removeNumbers)
textData <- tm_map(textData, stripWhitespace)

#walmart <- tm_map(walmart, removeWords, c("club","shop", "store", "please", "shopping",
#                                          "also", "however", "shopped", "else", "clubs",
#                                          "walmart", "sams", "wal", "mart"))



#Do some more text work with tokens - cut into words of 3 make a reduced set of observations
trigramTokenizer <- function (x) {NGramTokenizer(x,
                                                 Weka_control(min = 4, max = 4))}

tokenTDM <- TermDocumentMatrix(textData, control = list(tokenize = trigramTokenizer))

dim(tokenTDM)


tokenMatrix <- as.matrix(tokenTDM)
#sort it most frequent to least frequent and create a table
tokenV <- sort(rowSums(tokenMatrix), decreasing = TRUE)
token.d <- data.frame(word = names(tokenV), freq=tokenV)
table(token.d$freq)

#from the r-page build giant cloud for walmart
png("serviceCloud.png", width = 1280, height = 800)
wordcloud(token.d$word, token.d$freq,scale=c(8,.5), min.freq = 3, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))

dev.off()





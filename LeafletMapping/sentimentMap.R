# Script to plot sentiment on map

library(tm)
library(plyr)
library(stringr)
#install packages for manipulating and plotting data
library(zipcode)
library(ggplot2)
library(maps)
library(htmltools)
library(devtools)
library(leaflet)
library(stringi)

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

dataZip <- na.omit(dataZip)

#remove the raw data
rm(dataRaw)
rm(zipcode)
rm(dataState)

dataZip$Essay <- as.character(dataZip$Essay)
dataZip$Essay <- tolower(dataZip$Essay)



############################ sentiment analysis
#get the text files

pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")

score.sentiment = function(sentences, pos.words, neg.words, .progress = 'none')
{
  #parameters
  #sentences: vector of text to score
  #pos.words: vector of words of positve sentiment
  #neg words: pvector of words of negative sentiment
  #.progress: passed to laply() to control of of progress bar
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   #remove the punctuation
                   sentence = gsub("[[:punct:]]","",sentence)
                   #remove control characters
                   sentence = gsub("[[:cntrl:]]","",sentence)
                   #remove digits
                   sentence = gsub('\\d+',"",sentence)
                   
                   #define error handling function when trying to lower
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y= tolower(x)
                     return(y)
                   }
                   
                   
                   sentence = sapply(sentence, tryTolower)
                   
                   #split the sentences into words
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   #make the comparison
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress = .progress) 
  
  scores.df = data.frame(text=sentences, score = scores)
  return(scores.df)
}  

Sentiment = score.sentiment(dataZip$Essay, pos, neg)

dataZip$SentimentScore <- Sentiment$score

# create the sentiment factor
sentiment <- function(score){
  if (score >= 2) {sentiment = "Very Positive"
  } else if (score == 1) {
    sentiment = "Positive"
  } else if (score == 0){
    sentiment = "Neutral"
  } else if (score == -1){
    sentiment = "Negative"
    } else
      sentiment = "Very Negative"
    return(sentiment)
}
      
dataZip$Sentiment <- sapply(dataZip$SentimentScore, sentiment)      
dataZip$Sentiment <- as.factor(dataZip$Sentiment)  

      
#mapping of the sentiment

unique(dataZip$Sentiment)

sentimentFactor <- c("Very Negative", "Negative", "Neutral", "Positive", "Very Positive")
dataZip$Sentiment <- factor(dataZip$Sentiment, levels = sentimentFactor, ordered = TRUE)



binColors <- colorFactor("RdYlGn", dataZip$Sentiment, ordered = F)
      
m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stamen.Toner") %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addCircles(data = dataZip, lat = ~ latitude, lng = ~ longitude, 
                   color = ~binColors(dataZip$Sentiment), radius = 3) %>%
  addLegend("bottomright",pal = binColors, values = dataZip$Sentiment,
            title = "Sentiment",
            opacity = 1)
m      
      
write.csv(dataZip, file = "K:/Sandbox/R/Data IO/sentiment.csv", row.names = F)  
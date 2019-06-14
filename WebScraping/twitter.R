#load the required pacakges
library(twitteR)
library(ROAuth)
library(RCurl)
library(httr)
library(streamR)
library(wordcloud)
library(tm)
library(plyr)
library(stringr)

key = "AS9oq1tqMgBFhmRM2rPQBOUNT"
secret = "8mPBNtKMiwKp7hjPSnGMwbiruXjSgTD5xc7syq6lOa8Q8k2h8k"

setwd("K:\Sandbox\R\twitteR")


### process for authenticating this is best method

key <- "AS9oq1tqMgBFhmRM2rPQBOUNT"
secret <- "8mPBNtKMiwKp7hjPSnGMwbiruXjSgTD5xc7syq6lOa8Q8k2h8k"

secrettk <- "pM6S9SDTxO3vHtS34jfP0EpJHu2l4EJBCuOacQZNKvhbb"
myToken <- "3675069928-q4xfgDZb1TXeR6nbxmAkIRH0pj2o9YudvkJxFdn"

setup_twitter_oauth(consumer_key = key, consumer_secret = secret, access_token = myToken,
                    access_secret = secrettk)


#get last n tweets from a user
udemyTweets <- userTimeline("Udemy")


searchTerm <- "tom brady"
searchResults <- searchTwitter(searchTerm, n = 1000)
tweetFrame <- twListToDF(searchResults)


#for streaming use the filterStream command


searchList <- sapply(searchResults, function(x) x$getText())

searchCorpus <- Corpus(VectorSource(searchList))

#run the standard text mining cleaning routine
searchCorpus <- tm_map(searchCorpus,PlainTextDocument)
searchCorpuss <- tm_map(searchCorpus, content_transformer(tolower))
searchCorpus <- tm_map(searchCorpus, removeWords, stopwords("english"))
searchCorpus <- tm_map(searchCorpus, removePunctuation)
searchCorpus <- tm_map(searchCorpus, removeNumbers)
searchCorpus <- tm_map(searchCorpus, stripWhitespace)

wordcloud(searchCorpus, min.freq=2, scale=c(5,1), random.color = TRUE,
          max.words = Inf, random.order = F, colors = brewer.pal(8,"Dark2"))


#create term document matrix -- each row is a document


#find frequent terms


#associations

#remove sparse terms
search2tdm <- removeSparseTerms(searchtdm, sparse = .9)


#scale the data
search2tdmscale <- scale(search2tdm)

#distance matrix

searchdist <- dist(search2tdmscale, method = "euclidean")

#cluster
searchfit <- hclust(searchdist)
plot(searchfit)
cutree(searchfit, k= 6)
rect.hclust(searchfit, k=6, border = "red")




#### sentiment analysis


#get the text files

pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")

mytest = c("great you re here", "awesome experience", "You had a bad night", "she loves candy")

testsentiment = score.sentiment(mytest, pos, neg)

class(testsentiment)


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
  
  
  
  
  
  
  




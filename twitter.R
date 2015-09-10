#load the required pacakges
library(twitteR)


searchTerm <- "communispace"
searchResults <- searchTwitter(searchTerm, n = 1000)
tweetFrame <- twListToDF(searchResults)
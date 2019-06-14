#new procedure to do text mining


library(tm)
library(maps)
library(dplyr)
library(htmltools)
library(devtools)
library(leaflet)
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


fileName <- "J&JOpenEnds.csv"

#get a list of the files
url <- sprintf("K:/Sandbox/R/Data IO/%s",fileName)
dataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#rename column
newNames <- c("Member", "Country", "Text")
colnames(dataRaw) <- newNames

#ensure text is character
dataRaw$Text <- as.character(dataRaw$Text)


#make sure clean coding
Encoding(dataRaw$Text) <- "latin1"
dataRaw$Text <- iconv(dataRaw$Text, "latin1", "ASCII", sub="")

#split by relevent factor
#split function might be better alternative
textRegions <- split(dataRaw, dataRaw$Country)

# loop through list to create corpi
count = length(textRegions)
names <- names(textRegions)

franceCorpus <- Corpus(VectorSource(textRegions[[1]][[3]]))
franceCorpus <- tm_map(franceCorpus,PlainTextDocument)
franceCorpus <- tm_map(franceCorpus, content_transformer(tolower))
franceCorpus <- tm_map(franceCorpus, removeWords, stopwords("english"))
franceCorpus <- tm_map(franceCorpus, removePunctuation)
franceCorpus <- tm_map(franceCorpus, removeNumbers)
franceCorpus <- tm_map(franceCorpus, stripWhitespace)

taiwanCorpus <- Corpus(VectorSource(textRegions[[2]][[3]]))
taiwanCorpus <- tm_map(taiwanCorpus,PlainTextDocument)
taiwanCorpus <- tm_map(taiwanCorpus, content_transformer(tolower))
taiwanCorpus <- tm_map(taiwanCorpus, removeWords, stopwords("english"))
taiwanCorpus <- tm_map(taiwanCorpus, removePunctuation)
taiwanCorpus <- tm_map(taiwanCorpus, removeNumbers)
taiwanCorpus <- tm_map(taiwanCorpus, stripWhitespace)

usCorpus <- Corpus(VectorSource(textRegions[[3]][[3]]))
usCorpus <- tm_map(usCorpus,PlainTextDocument)
usCorpus <- tm_map(usCorpus, content_transformer(tolower))
usCorpus <- tm_map(usCorpus, removeWords, stopwords("english"))
usCorpus <- tm_map(usCorpus, removePunctuation)
usCorpus <- tm_map(usCorpus, removeNumbers)
usCorpus <- tm_map(usCorpus, stripWhitespace)

franceDTM <- DocumentTermMatrix(franceCorpus)
taiwanDTM <- DocumentTermMatrix(taiwanCorpus)
usDTM <- DocumentTermMatrix(usCorpus)

png("testMultiCloud.png")
par(mfrow=c(2,2), oma = c(0,0,3,0), mar = c(0,0,1,0))
#make a word cloud
#png("france1wordCloud.png", width = 1280, height = 800)
wordcloud(franceCorpus,scale=c(5,1), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.3, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = "France")
#dev.off()

#png("taiwan1wordCloud.png", width = 1280, height = 800)
text("Taiwan")
wordcloud(taiwanCorpus,scale=c(5,1), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.3, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = "Taiwan")
#dev.off()

#png("us1wordCloud.png", width = 1280, height = 800)
text("US")
wordcloud(usCorpus,scale=c(5,1), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.3, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = "US")
mtext("Responses of Doctors by Country", outer = TRUE, cex = 1.5)
dev.off()

#alternative options using par and fig
#this allowsfor finer control and the dimensions are 0,0 to 1,1


png("testMultiCloud.png")
par(fig= c(.25,.75,.55,.9), new = TRUE)
wordcloud(franceCorpus,scale=c(5,1), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.3, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))


par(fig= c(0,.45,0,.45), new = TRUE)
wordcloud(taiwanCorpus,scale=c(5,1), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.3, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))



par(fig= c(.55,1,0,.45), new = TRUE)
wordcloud(usCorpus,scale=c(5,1), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.3, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))

mtext("France", outer = TRUE, side=3, line = -3, col = "blue", cex = 1.5)
#mtext("Taiwan                                                           US", 
#      outer = TRUE, side=3, line = -18)
mtext("Taiwan",side=3,line= -18, at=0.20, outer = TRUE, col = "blue", cex = 1.5)
mtext("US",side=3,line= -18, at=0.8, outer = TRUE,col = "blue", cex = 1.5)
dev.off()










#make the phrase clouds
ngramTokenizer <- function (x) {NGramTokenizer(x,
                                                 Weka_control(min = 2, max = 2))}

franceTDM <- TermDocumentMatrix(franceCorpus)
taiwanTDM <- TermDocumentMatrix(taiwanCorpus)
usTDM <- TermDocumentMatrix(usCorpus)


franceTDM <- TermDocumentMatrix(franceCorpus, control = list(tokenize = ngramTokenizer))
taiwanTDM <- TermDocumentMatrix(taiwanCorpus, control = list(tokenize = ngramTokenizer))
usTDM <- TermDocumentMatrix(usCorpus, control = list(tokenize = ngramTokenizer))

tokenMatrix <- as.matrix(franceTDM)
tokenMatrix <- as.matrix(taiwanTDM)
tokenMatrix <- as.matrix(usTDM)


#sort it most frequent to least frequent and create a table
tokenV <- sort(rowSums(tokenMatrix), decreasing = TRUE)
token.d <- data.frame(word = names(tokenV), freq=tokenV)
table(token.d$freq)

palNew = c("red", "blue", "green", "orange")

#from the r-page build giant cloud
png("us3WordColorTest.png", width = 1280, height = 800)
wordcloud(token.d$word, token.d$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))

dev.off()

#test colors and scales
palNew = c("green", "blue", "red", "orange")

png("us3WordColorTest.png", width = 1280, height = 800)
wordcloud(token.d$word, token.d$freq,scale=c(8,.2), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0, use.r.layout=FALSE,
          colors = palNew)

dev.off()


# look into cluster dendrogram
# carry on with typical things that can now be done, ie. cluster analysis
# newstopwords <- findFreqTerms(usTDM, lowfreq=10) # get most frequent words
# remove most frequent words for this corpus
#don't use this function unless you have lots of text
#usTDM2 <- usTDM[!(usTDM$dimnames$Terms) %in% newstopwords,] 
inspect(usTDM2)


tdm <- removeSparseTerms(usTDM, sparse=0.98)
inspect(tdm)

a.tdm.df <- as.data.frame(inspect(tdm))
a.tdm.df.scale <- scale(a.tdm.df)
d <- dist(a.tdm.df.scale, method = "euclidean") 
fit <- hclust(d, method="ward")
plot(fit)








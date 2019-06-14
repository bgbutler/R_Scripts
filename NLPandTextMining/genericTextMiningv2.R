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


fileName <- "PhilipsSNM.csv"

#get a list of the files
url <- sprintf("K:/Sandbox/R/Data IO/%s",fileName)
dataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#rename column
newNames <- c("Status", "Text")
colnames(dataRaw) <- newNames

#ensure text is character
dataRaw$Text <- as.character(dataRaw$Text)


#make sure clean coding
Encoding(dataRaw$Text) <- "latin1"
dataRaw$Text <- iconv(dataRaw$Text, "latin1", "ASCII", sub="")

#split by relevent factor
#split function might be better alternative
textFactors <- split(dataRaw, dataRaw$Status)

# loop through list to create corpi
count = length(textFactors)
names <- names(textFactors)

partnerCorpus <- Corpus(VectorSource(textFactors[[1]][[2]]))
partnerCorpus <- tm_map(partnerCorpus,PlainTextDocument)
partnerCorpus <- tm_map(partnerCorpus, content_transformer(tolower))
#partnerCorpus <- tm_map(partnerCorpus, removeWords, stopwords("english"))
partnerCorpus <- tm_map(partnerCorpus, removePunctuation)
partnerCorpus <- tm_map(partnerCorpus, removeNumbers)
partnerCorpus <- tm_map(partnerCorpus, stripWhitespace)

snorerCorpus <- Corpus(VectorSource(textFactors[[2]][[2]]))
snorerCorpus <- tm_map(snorerCorpus,PlainTextDocument)
snorerCorpus <- tm_map(snorerCorpus, content_transformer(tolower))
#snorerCorpus <- tm_map(snorerCorpus, removeWords, stopwords("english"))
snorerCorpus <- tm_map(snorerCorpus, removePunctuation)
snorerCorpus <- tm_map(snorerCorpus, removeNumbers)
snorerCorpus <- tm_map(snorerCorpus, stripWhitespace)


partnerDTM <- DocumentTermMatrix(partnerCorpus)
snorerDTM <- DocumentTermMatrix(snorerCorpus)





#make the phrase clouds
ngramTokenizer <- function (x) {NGramTokenizer(x,
                                                 Weka_control(min = 4, max = 5))}

partnerTDM <- TermDocumentMatrix(partnerCorpus, control = list(tokenize = ngramTokenizer))
snorerTDM <- TermDocumentMatrix(snorerCorpus, control = list(tokenize = ngramTokenizer))


partnertokenMatrix <- as.matrix(partnerTDM)
snorertokenMatrix <- as.matrix(snorerTDM)


#sort it most frequent to least frequent and create a table
partnertokenV <- sort(rowSums(partnertokenMatrix), decreasing = TRUE)
partnertoken.d <- data.frame(word = names(partnertokenV), freq=partnertokenV)
table(partnertoken.d$freq)

#sort it most frequent to least frequent and create a table
snorertokenV <- sort(rowSums(snorertokenMatrix), decreasing = TRUE)
snorertoken.d <- data.frame(word = names(snorertokenV), freq=snorertokenV)
table(snorertoken.d$freq)

output <- file.path("K:","Sandbox", "R")
setwd(output)


#from the r-page build giant cloud

#png("philips4Word.png")
#par(mfrow=c(1,2))
png("partnerNoStopWord.png", width = 1280, height = 800)
wordcloud(partnertoken.d$word, partnertoken.d$freq,scale=c(8,.2), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=.20, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = "Partner", col.main = "blue", cex.main = 2, font.main = 4)
dev.off()

png("snorerNoStopWord.png", width = 1280, height = 800)
wordcloud(snorertoken.d$word, snorertoken.d$freq,scale=c(8,.2), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=.20, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = "Snorer")
dev.off()





######################################################
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








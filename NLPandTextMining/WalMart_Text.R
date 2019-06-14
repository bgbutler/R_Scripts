#new procedure to do text mining
#load in the Excel files
library(readxl)
library(tm)
#library(qdap)
library(SnowballC)
library(RTextTools)
library(stringi)
library(wordcloud)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(dplyr)

#get a list of the files
url <- "K:/Sandbox/R/Data/WalMart.csv"
wmRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#remove the bad characters
#get data without special chars
wmRaw <- wmRaw[stri_enc_isutf8(wmRaw$Essay)== T,]

#remove factors from Essay
wmRaw$Essay <- as.character(wmRaw$Essay)

#start building the corpus
walmart <- Corpus(VectorSource(wmRaw$Essay))

#view some of the content
walmart[[5000]]$content[1]
walmart[[155]][1]

#lets do some cleaning of the text
#first create cleanup function

walmart <- tm_map(walmart,PlainTextDocument)
walmart <- tm_map(walmart, content_transformer(tolower))
walmart <- tm_map(walmart, removePunctuation)
walmart <- tm_map(walmart, removeNumbers)
walmart <- tm_map(walmart, removeWords, stopwords("english"))
#walmart <- tm_map(walmart, stemDocument)
walmart <- tm_map(walmart, stripWhitespace)

#remove wal mart and sams from the text
walmart <- tm_map(walmart, removeWords, c("wal mart", "walmart", "sams", "shop", "store"))

#count and view the stopwords
length(stopwords("english"))
stopwords()


#look at content after cleanup
walmart[[155]][1]

#create a corpus for stemming
wmStem <- walmart
wmStem <- tm_map(wmStem, stemDocument)

#look at some content after stemming if done
wmStem[[155]][1]

#develop term document matrix
walmartTDM <- TermDocumentMatrix(walmart)
dim(walmartTDM)  #output is rows and columns
walmartMatrix <- as.matrix(walmartTDM)

#find the frequent terms in the TDM
findFreqTerms(walmartTDM, 500)

#word associations and correlations
findAssocs(walmartTDM, "employee", 0.10)

findAssocs(walmartTDM, "customer", 0.10)

findAssocs(walmartTDM, "walmart", 0.10)

findAssocs(walmartTDM, "employer", 0.10)

findAssocs(walmartTDM, "service", 0.10)

findAssocs(walmartTDM, "target", 0.10)

#sort it most frequent to least frequent and create a table
wmV <- sort(rowSums(walmartMatrix), decreasing = TRUE)
wm.d <- data.frame(word = names(wmV), freq=wmV)
table(wm.d$freq)


#from the r-page build giant cloud
png("walmartCloud.png", width = 1280, height = 800)
wordcloud(wm.d$word, wm.d$freq,scale=c(8,.2), min.freq = 3, max.words = Inf,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off()

#from the main corpus
set.seed(100)
wordcloud(walmart,scale=c(3,0.1), max.words = 200,
          random.order = FALSE,rot.per=0.35, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))


#from the stemmed corpus
set.seed(100)
wordcloud(wmStem,scale=c(3,0.1), max.words = 200,
          random.order = FALSE,rot.per=0.25, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))


#remove sparse terms higher number retains more
tdm.S <- removeSparseTerms(walmartTDM, sparse = .99)

sparseMatrix <- as.matrix(tdm.S)

#make a dataframe from the cleaner matrix
sparseDf <- data.frame(sparseMatrix)
sparseDf$keywords = rownames(sparseDf)

#melt and add column names
matrix.melted <- melt(sparseDf)
colnames(matrix.melted) = c("Keyword", "Post","Freq")


#test a heatmap
hm = ggplot(matrix.melted, aes(x=Post, y=Keyword)) + 
  geom_tile(aes(fill=Freq), colour = "white") + 
  scale_fill_gradient(low="black", high= "darkorange") + 
  labs(title = "Keywords in Walmart Essay") + 
  theme_few() + 
  theme(axis.text.x = element_text(size=6))
hm


tf = rowSums(as.matrix(tdm.S))
tf.reduced = subset(tf,tf>=1000)

bf = qplot(names(tf.reduced), tf.reduced, geom="bar", stat = "identity") + 
  coord_flip() + 
  labs(title="Keywords in Walmart Essay", x="Keyword",y="Frequency") + 
  theme_few()
bf




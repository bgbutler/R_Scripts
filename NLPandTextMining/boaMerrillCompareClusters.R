# text mapping for boa and Merrill

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
library(openxlsx)
library(RWeka)  #for making tokens out of the phrases


#check the directory and change for this file
cwd <- getwd()
myPath <- file.path("Q:","Analytics", "BoAMerrill", "CombinedFilesForText")
output <- file.path("Q:","Analytics", "BoAMerrill", "CombinedFilesForText", "CleanRun")
setwd(myPath)

#loop through all files and combine
#start with the .xlsx files then the .xls files
conv <- data.frame()

#read the .xlsx files
url <- "Q:/Analytics/BoAMerrill/CombinedFilesForText/"
files <- list.files(url, full.names = TRUE, pattern = ".xlsx")
count <- length(files)

for (i in 1:count){
  conv <- rbind(conv, 
                read.xlsx(files[i], sheet = 1, startRow = 1, colNames = T))
  print(files[i])
}


#remove the facilitator
grx <- glob2rx("*Facilitator*")
members <- with(conv, subset(conv, subset = !grepl(grx, conv$LastName), drop = TRUE))

#create some ordered factors
members$AgeRange <- factor(members$AgeRange, ordered = TRUE)


###################    separate the 21 - 24 and 65/66

twenties <- members[members$AgeRange == "21 to 24",]
sixties <- members[members$AgeRange %in% c("65+", "66 +"),]

#start building the corpus for 20's and 60's
twentiesText <- Corpus(VectorSource(twenties$Post))
twentiesText <- tm_map(twentiesText,PlainTextDocument)
twentiesText <- tm_map(twentiesText, removeWords, stopwords("english"))
twentiesText <- tm_map(twentiesText, removePunctuation)
twentiesText <- tm_map(twentiesText, removeNumbers)
twentiesText <- tm_map(twentiesText, stripWhitespace)

twentiesText <- tm_map(twentiesText, removeWords, c("biggest", "financial", "concern",
                                                    "top", "mind", "now", "my"))



sixtiesText <- Corpus(VectorSource(sixties$Post))
sixtiesText <- tm_map(sixtiesText,PlainTextDocument)
sixtiesText <- tm_map(sixtiesText, removeWords, stopwords("english"))
sixtiesText <- tm_map(sixtiesText, removePunctuation)
sixtiesText <- tm_map(sixtiesText, removeNumbers)
sixtiesText <- tm_map(sixtiesText, stripWhitespace)

sixtiesText <- tm_map(sixtiesText, removeWords, c("biggest", "financial", "concern",
                                                  "top", "mind", "now", "my"))

#make the ngrams
ngramTokenizer <- function (x) {NGramTokenizer(x,
                                               Weka_control(min = 2, max = 2))}

#################    make the ngrams of each and plot them
sixtiesTDM <- TermDocumentMatrix(sixtiesText, control = list(tokenize = ngramTokenizer))
twentiesTDM <- TermDocumentMatrix(twentiesText, control = list(tokenize = ngramTokenizer))


tokenMatrixsixties <- as.matrix(sixtiesTDM)
tokenMatrixtwenties <- as.matrix(twentiesTDM)


tokenV_sixties <- sort(rowSums(tokenMatrixsixties), decreasing = TRUE)
token.d_sixties <- data.frame(word = names(tokenV_sixties), freq=tokenV_sixties)
table(token.d_sixties$freq)

tokenV_twenties <- sort(rowSums(tokenMatrixtwenties), decreasing = TRUE)
token.d_twenties <- data.frame(word = names(tokenV_twenties), freq=tokenV_twenties)
table(token.d_twenties$freq)




output <- file.path("Q:","Analytics", "BoAMerrill", "CombinedFilesForText", "CleanRun")
setwd(output)

png("compareClusters2Wordv2.png")
#png("twenties5Word.png",width = 1280, height = 800)
par(fig= c(0,1,.50,.90))
wordcloud(token.d_twenties$word, token.d_twenties$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
#dev.off()



#sixties
par(fig= c(0,1,0,.48), new = TRUE)
#png("sixties5Word.png",width = 1280, height = 800)
wordcloud(token.d_sixties$word, token.d_sixties$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))

mtext("21 to 24",side=3,line= -3, outer = TRUE, col = "blue", cex = 1.5)
mtext("65 +",side=3,line= -18, outer = TRUE,col = "blue", cex = 1.5)
dev.off()



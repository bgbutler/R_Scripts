# part 2 of the BOA Merrill data
#focusing on life events


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
setwd(myPath)

#loop through all files and combine
conv <- data.frame()
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

#convert to lower
members$Post <- tolower(members$Post)


#check for life events and create cuts
#life1 = health and healthcare costs
grx <- glob2rx("*health*")
life1 <- subset(members, subset = grepl(grx, members$Post))
#start building the corpus
life1 <- Corpus(VectorSource(life1$Post))
life1 <- tm_map(life1,PlainTextDocument)
life1 <- tm_map(life1, removeWords, stopwords("english"))
life1 <- tm_map(life1, removePunctuation)
life1 <- tm_map(life1, removeNumbers)
life1 <- tm_map(life1, stripWhitespace)


#life2 = job change move/loss
grx <- glob2rx("*lose*|*job*")
life2 <- subset(members, subset = grepl(grx, members$Post))
#start building the corpus
life2 <- Corpus(VectorSource(life2$Post))
life2 <- tm_map(life2,PlainTextDocument)
life2 <- tm_map(life2, removeWords, stopwords("english"))
life2 <- tm_map(life2, removeWords, c("biggest", "financial", "concern",
                                            "top", "mind", "now"))
life2 <- tm_map(life2, removePunctuation)
life2 <- tm_map(life2, removeNumbers)
#life2 <- tm_map(life2, stripWhitespace)



#life3 = retirement
grx <- glob2rx("*retire*|*retirement*")
life3 <- subset(members, subset = grepl(grx, members$Post))
#start building the corpus
life3 <- Corpus(VectorSource(life3$Post))
life3 <- tm_map(life3,PlainTextDocument)
#life3 <- tm_map(life3, removeWords, stopwords("english"))
life3 <- tm_map(life3, removeWords, c("biggest", "financial", "concern",
                                      "top", "mind", "now", "puts"))
life3 <- tm_map(life3, removePunctuation)
life3 <- tm_map(life3, removeNumbers)
#life3 <- tm_map(life3, stripWhitespace)

#life4 = moving
grx <- glob2rx("*house*|*home*")
life4 <- subset(members, subset = grepl(grx, members$Post))
#start building the corpus
life4 <- Corpus(VectorSource(life4$Post))
life4 <- tm_map(life4,PlainTextDocument)
life4 <- tm_map(life4, removeWords, stopwords("english"))
life4 <- tm_map(life4, removePunctuation)
life4 <- tm_map(life4, removeNumbers)
#life4 <- tm_map(life4, stripWhitespace)


#life5 = busines
grx <- glob2rx("*start*|*business*")
life5 <- subset(members, subset = grepl(grx, members$Post))
#start building the corpus
life5 <- Corpus(VectorSource(life5$Post))
life5 <- tm_map(life5,PlainTextDocument)
life5 <- tm_map(life5, removeWords, stopwords("english"))
life5 <- tm_map(life5, removePunctuation)
life5 <- tm_map(life5, removeNumbers)
#life5 <- tm_map(life5, stripWhitespace)


#life6 = stock market
grx <- glob2rx("*college*")
life6 <- subset(members, subset = grepl(grx, members$Post))
#start building the corpus
life6 <- Corpus(VectorSource(life6$Post))
life6 <- tm_map(life6,PlainTextDocument)
life6 <- tm_map(life6, removeWords, stopwords("english"))
life6 <- tm_map(life6, removePunctuation)
life6 <- tm_map(life6, removeNumbers)
#life6 <- tm_map(life6, stripWhitespace)


#life7 = stock market
grx <- glob2rx("*stock*")
life7 <- subset(members, subset = grepl(grx, members$Post))
#start building the corpus
life7 <- Corpus(VectorSource(life7$Post))
life7 <- tm_map(life7,PlainTextDocument)
life7 <- tm_map(life7, removeWords, stopwords("english"))
life7 <- tm_map(life7, removePunctuation)
life7 <- tm_map(life7, removeNumbers)
#life7 <- tm_map(life7, stripWhitespace)


#remove some other common words
life1 <- tm_map(life1, removeWords, c("biggest", "financial", "concern", "top", "mind", "now"))
life2 <- tm_map(life2, removeWords, c("biggest", "financial", "concern", "top", "mind", "now"))
life3 <- tm_map(life3, removeWords, c("biggest", "financial", "concern", "top", "mind", "now"))
life4 <- tm_map(life4, removeWords, c("biggest", "financial", "concern", "top", "mind", "now"))
life5 <- tm_map(life5, removeWords, c("biggest", "financial", "concern", "top", "mind", "now"))
life6 <- tm_map(life6, removeWords, c("biggest", "financial", "concern", "top", "mind", "now"))
life7 <- tm_map(life7, removeWords, c("biggest", "financial", "concern", "top", "mind", "now"))

#make the n-grams for the life events
#make the ngrams
ngramTokenizer <- function (x) {NGramTokenizer(x,
                                               Weka_control(min = 10, max = 10))}

healthTDM <- TermDocumentMatrix(life1, control = list(tokenize = ngramTokenizer))
jobTDM <- TermDocumentMatrix(life2, control = list(tokenize = ngramTokenizer))
retireTDM <- TermDocumentMatrix(life3, control = list(tokenize = ngramTokenizer))
houseTDM <- TermDocumentMatrix(life4, control = list(tokenize = ngramTokenizer))
businessTDM <- TermDocumentMatrix(life5, control = list(tokenize = ngramTokenizer))
collegeTDM <- TermDocumentMatrix(life6, control = list(tokenize = ngramTokenizer))
stocksTDM <- TermDocumentMatrix(life7, control = list(tokenize = ngramTokenizer))


tokenMatrixhealth <- as.matrix(healthTDM)
tokenMatrixjob <- as.matrix(jobTDM)
tokenMatrixretire <- as.matrix(retireTDM)
tokenMatrixhouse <- as.matrix(houseTDM)
tokenMatrixbusiness <- as.matrix(businessTDM)
tokenMatrixcollege <- as.matrix(collegeTDM)
tokenMatrixstocks <- as.matrix(stocksTDM)


tokenV_health <- sort(rowSums(tokenMatrixhealth), decreasing = TRUE)
token.d_health <- data.frame(word = names(tokenV_health), freq=tokenV_health)
table(token.d_health$freq)

tokenV_job <- sort(rowSums(tokenMatrixjob), decreasing = TRUE)
token.d_job <- data.frame(word = names(tokenV_job), freq=tokenV_job)
table(token.d_job$freq)

tokenV_retire <- sort(rowSums(tokenMatrixretire), decreasing = TRUE)
token.d_retire <- data.frame(word = names(tokenV_retire), freq=tokenV_retire)
table(token.d_retire$freq)

tokenV_house <- sort(rowSums(tokenMatrixhouse), decreasing = TRUE)
token.d_house <- data.frame(word = names(tokenV_house), freq=tokenV_house)
table(token.d_house$freq)

tokenV_business <- sort(rowSums(tokenMatrixbusiness), decreasing = TRUE)
token.d_business <- data.frame(word = names(tokenV_business), freq=tokenV_business)
table(token.d_business$freq)

tokenV_college <- sort(rowSums(tokenMatrixcollege), decreasing = TRUE)
token.d_college <- data.frame(word = names(tokenV_college), freq=tokenV_college)
table(token.d_college$freq)

tokenV_stocks <- sort(rowSums(tokenMatrixstocks), decreasing = TRUE)
token.d_stocks <- data.frame(word = names(tokenV_stocks), freq=tokenV_stocks)
table(token.d_stocks$freq)


output <- file.path("Q:","Analytics", "BoAMerrill", "CombinedFilesForText", "CleanRun")
setwd(output)


#make the clouds by theme
png("testLifeEventCloud4Word.png")
png("healthcare.png", width = 1280, height = 800)
#par(mfrow=c(3,2), oma = c(0,0,3,0), mar = c(0,0,1,0))
wordcloud(token.d_health$word, token.d_health$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = "Health and Healthcare")
dev.off()

png("job4word.png", width = 1280, height = 800)
wordcloud(token.d_job$word, token.d_job$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = "Job")
dev.off()

png("retire10word.png", width = 1280, height = 800)
wordcloud(token.d_retire$word, token.d_retire$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = "Retirement")
dev.off()

wordcloud(token.d_house$word, token.d_house$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = "House and Moving")

wordcloud(token.d_business$word, token.d_business$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = "Business")

png("college.png", width = 1280, height = 800)
wordcloud(token.d_college$word, token.d_college$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
title(main = "College")
dev.off()




png("stockMarket.png", width = 1280, height = 800)
wordcloud(token.d_stocks$word, token.d_stocks$freq,scale=c(4,.5), min.freq = 1, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off()

######## word associations

life6DTM <- DocumentTermMatrix(life6)

life6Assoc <- findAssocs(life6DTM, "college", 0.25)


life6Assocdf <- data.frame(Corr=as.numeric(unlist(life6Assoc)),
                   Terms = gsub("College.","",names(unlist(life6Assoc))))

life6Assocdf$Terms <- factor(life6Assocdf$Terms, levels = life6Assocdf$Terms)

#plot it
collP <- ggplot(life6Assocdf, aes(x = Corr, y = reorder(Terms, Corr))) + 
  geom_point(size = 2) +
  ggtitle("Words that are Correlated with 'College'") + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
collP



life1DTM <- DocumentTermMatrix(life1)

life1Assoc <- findAssocs(life1DTM, "healthcare", 0.20)


life1Assocdf <- data.frame(Corr=as.numeric(unlist(life1Assoc)),
                           Terms = gsub("Healthcare.","",names(unlist(life1Assoc))))

life1Assocdf$Terms <- factor(life1Assocdf$Terms, levels = life1Assocdf$Terms)

#plot it
healthP <- ggplot(life1Assocdf, aes(x = Corr, y = reorder(Terms, Corr))) + 
  geom_point(size = 2) +
  ggtitle("Words that are Correlated with 'Health'") + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
healthP


life2DTM <- DocumentTermMatrix(life2)

life2Assoc <- findAssocs(life2DTM, "job", 0.20)


life2Assocdf <- data.frame(Corr=as.numeric(unlist(life2Assoc)),
                           Terms = gsub("job.","",names(unlist(life2Assoc))))

life2Assocdf$Terms <- factor(life2Assocdf$Terms, levels = life2Assocdf$Terms)

#plot it
jobP <- ggplot(life2Assocdf, aes(x = Corr, y = reorder(Terms, Corr))) + 
  geom_point(size = 2) +
  ggtitle("Words that are Correlated with 'Job'") + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
jobP


life3DTM <- DocumentTermMatrix(life3)

life3Assoc <- findAssocs(life3DTM, "retirement", 0.20)


life3Assocdf <- data.frame(Corr=as.numeric(unlist(life3Assoc)),
                           Terms = gsub("retirement.","",names(unlist(life3Assoc))))

life3Assocdf$Terms <- factor(life3Assocdf$Terms, levels = life3Assocdf$Terms)

#plot it
retP <- ggplot(life3Assocdf, aes(x = Corr, y = reorder(Terms, Corr))) + 
  geom_point(size = 2) +
  ggtitle("Words that are Correlated with 'Retirement'") + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
retP








####### one last test plot
topRetire <- head(token.d_retire,40)

retireP <- ggplot(topRetire, aes(x = freq, y = reorder(word, freq))) + 
  geom_point(size = 2) + scale_x_continuous(breaks = seq(1,3,1)) +
  ggtitle("Top Retirement Phrases") + 
  xlab("Frequency of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
retireP
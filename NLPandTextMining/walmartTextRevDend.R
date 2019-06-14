#new procedure to do text mining


library(tm)
#library(qdap)
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
library(zipcode)
library(cluster)
library(dendextend)
library(dendextendRcpp)

#get the zipcode data
data(zipcode)

#get a list of the files
url <- "K:/Sandbox/R/Data/WalMartv2.csv"
wmRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get the clean text - drops 15 observations
wmRaw <- wmRaw[stri_enc_isutf8(wmRaw$Essay)== T,]

#pick up state, zip and essay
wmState <- wmRaw[,c(13:22)]

#remove the NA
wmState <- na.omit(wmState)

#merge with the zipcode data to get lat and lon
#convert the zipcodes to char
wmState$Zip <-as.character(wmState$Zip)
wmState$Zip <- clean.zipcodes(wmState$Zip)
wmZip <- merge(wmState, zipcode, by.x = 'Zip', by.y = 'zip')

#remove the raw data
rm(wmRaw)
rm(zipcode)
rm(wmState)

#remove extraneous columns
wmZip$LengthEssay <- NULL
wmZip$Essay1 <- NULL
wmZip$Essay2 <- NULL
wmZip$Essay3 <- NULL

#clean up column names
newNames <- c("Zip", "Income", "Status", "Household", "State", "Essay","City", "Abbrev", "latitide", "longitude")
colnames(wmZip) <- newNames

#Append state to the Essay and convert it all to lower for ease or processing later
#wmZip$StateComment <- paste(wmZip$State, wmZip$Essay, sep = " ")
wmZip$Essay <- tolower(wmZip$Essay)

#start building the corpus
walmart <- Corpus(VectorSource(wmZip$Essay))
#walmartDict <- Corpus(VectorSource(wmZip$StateComment))

#view some of the content
walmart[[5000]]$content[1]
walmart[[146]][1]


#lets do some cleaning of the text
#first create cleanup function

walmart <- tm_map(walmart,PlainTextDocument)
walmart <- tm_map(walmart, content_transformer(tolower))
walmart <- tm_map(walmart, removeWords, stopwords("english"))
walmart <- tm_map(walmart, removePunctuation)
walmart <- tm_map(walmart, removeNumbers)
walmart <- tm_map(walmart, stripWhitespace)
#walmart <- tm_map(walmart, stemDocument)
#walmart <- tm_map(walmart, stemCompletion, walmartDict)

#look at the stopwords
stopwords("english")

walmart <- tm_map(walmart, removeWords, c("club","shop", "store", "please", "shopping",
                                          "also", "however", "shopped", "else", "clubs"))

#create document term matrices - preserves the order of the documents
walmartDTM <- DocumentTermMatrix(walmart)



#######      This Section is for Wal Mart Only       ##############
# do word associations with walmart and sams before removing
wmAssoc <- findAssocs(walmartDTM, "walmart", 0.10)
wmAssoc

wmdf <- data.frame(Corr=as.numeric(unlist(wmAssoc)),
                        Terms = gsub("walmart.","",names(unlist(wmAssoc))))

wmdf$Terms <- factor(wmdf$Terms, levels = wmdf$Terms)

#plot it
wmP <- ggplot(wmdf, aes(x = Corr, y = reorder(Terms, Corr))) + 
  geom_point(size = 2) +
  ggtitle("Words that are Correlated with 'Wal Mart'") + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
wmP



##############    This Section is for SAMs ################################
# do word associations with walmart and sams before removing
samAssoc <- findAssocs(walmartDTM, "sams", 0.10)
samAssoc

samdf <- data.frame(Corr=as.numeric(unlist(samAssoc)),
                   Terms = gsub("sams.","",names(unlist(samAssoc))))

samdf$Terms <- factor(samdf$Terms, levels = samdf$Terms)

#plot it
samP <- ggplot(samdf, aes(x = Corr, y = reorder(Terms, Corr))) + 
  geom_point(size = 2) +
  ggtitle("Words that are Correlated with 'Sams'") + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  xlim(0.1,.3) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
samP



####   Word Cloud Time #####
#remove wal mart and sams from the text
walmart <- tm_map(walmart, removeWords, c("wal mart", "walmart", "wal", "mart", "club",
                                          "sams", "shop", "store", "please", "sam"))

walmartTDM <- TermDocumentMatrix(walmart)

walmartMatrix <- as.matrix(walmartTDM)
#sort it most frequent to least frequent and create a table
wmV <- sort(rowSums(walmartMatrix), decreasing = TRUE)
wm.d <- data.frame(word = names(wmV), freq=wmV)
table(wm.d$freq)

#from the r-page build giant cloud for walmart
png("walmartCloud5.png", width = 1280, height = 800)
wordcloud(wm.d$word, wm.d$freq,scale=c(8,.2), min.freq = 3, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))

dev.off()




############## End of Walmart matrix use





##############      check for themes, meaningful number of obs


#process the documents as corpi
#love the prices
grx <- glob2rx("*price*")
prices <- subset(wmZip, subset = grepl(grx, wmZip$Essay))
wmprices <- Corpus(VectorSource(prices$Essay))
wmprices <- tm_map(wmprices,PlainTextDocument)
wmprices <- tm_map(wmprices, content_transformer(tolower))
wmprices <- tm_map(wmprices, removeWords, stopwords("english"))
wmprices <- tm_map(wmprices, removePunctuation)
wmprices <- tm_map(wmprices, removeNumbers)
wmprices <- tm_map(wmprices, stripWhitespace)

#potential service issues
grx <- glob2rx("*long*|*line*|*cashier*|*service*")
service <- subset(wmZip, subset = grepl(grx, wmZip$Essay))
wmservice <- Corpus(VectorSource(service$Essay))
wmservice <- tm_map(wmservice,PlainTextDocument)
wmservice <- tm_map(wmservice, content_transformer(tolower))
wmservice <- tm_map(wmservice, removeWords, stopwords("english"))
wmservice <- tm_map(wmservice, removePunctuation)
wmservice <- tm_map(wmservice, removeNumbers)
wmservice <- tm_map(wmservice, stripWhitespace)

#inventory comments
grx <- glob2rx("*stock*|*invent*")
stock <- subset(wmZip, subset = grepl(grx, wmZip$Essay))
wmstock <- Corpus(VectorSource(stock$Essay))
wmstock <- tm_map(wmstock,PlainTextDocument)
wmstock <- tm_map(wmstock, content_transformer(tolower))
wmstock <- tm_map(wmstock, removeWords, stopwords("english"))
wmstock <- tm_map(wmstock, removePunctuation)
wmstock <- tm_map(wmstock , removeNumbers)
wmstock <- tm_map(wmstock, stripWhitespace)

#grocery comments
grx <- glob2rx("*groc*|*produce*")
prod <- subset(wmZip, subset = grepl(grx, wmZip$Essay))
wmprod <- Corpus(VectorSource(prod$Essay))
wmprod <- tm_map(wmprod,PlainTextDocument)
wmprod <- tm_map(wmprod, content_transformer(tolower))
wmprod <- tm_map(wmprod, removeWords, stopwords("english"))
wmprod <- tm_map(wmprod, removeWords, c("wal mart", "walmart", "wal", "mart"))
wmprod <- tm_map(wmprod, removePunctuation)
wmprod <- tm_map(wmprod, removeNumbers)
wmprod <- tm_map(wmprod, stripWhitespace)


#create document term matrices - preserves the order of the documents
produceDTM <- DocumentTermMatrix(wmprod)
priceDTM <- DocumentTermMatrix(wmprices)
serviceDTM <- DocumentTermMatrix(wmservice)
stockDTM <- DocumentTermMatrix(wmstock)

#get dimensions of matrices
dim(produceDTM)
dim(priceDTM)
dim(serviceDTM)
dim(stockDTM)


#create term document matrices
produceTDM <- TermDocumentMatrix(wmprod)
priceTDM <- TermDocumentMatrix(wmprices)
serviceTDM <- TermDocumentMatrix(wmservice)
stockTDM <- TermDocumentMatrix(wmstock)

dim(produceTDM)
dim(priceTDM)
dim(serviceTDM)
dim(stockTDM)

#make data frames from associated words

#service
serviceAssoc <- findAssocs(serviceTDM, "service", 0.10)
servicedf <- data.frame(Corr=as.numeric(unlist(serviceAssoc)),
                        Terms = gsub("service.","",names(unlist(serviceAssoc))))
servicedf$Terms <- factor(servicedf$Terms, levels = servicedf$Terms)

#produce and groceries
produceAssoc <- findAssocs(produceTDM, "groceries", 0.10)
producedf <- data.frame(Corr=as.numeric(unlist(produceAssoc)),
                        Terms = gsub("groceries.","",names(unlist(produceAssoc))))
producedf$Terms <- factor(producedf$Terms, levels = producedf$Terms)

#stock and inventory
stockAssoc <- findAssocs(stockTDM, "stock", 0.10)
stockdf <- data.frame(Corr=as.numeric(unlist(stockAssoc)),
                        Terms = gsub("stock.","",names(unlist(stockAssoc))))
stockdf$Terms <- factor(stockdf$Terms, levels = stockdf$Terms)

#prices
priceAssoc <- findAssocs(priceTDM, "price", 0.10)
pricedf <- data.frame(Corr=as.numeric(unlist(priceAssoc)),
                        Terms = gsub("price.","",names(unlist(priceAssoc))))
pricedf$Terms <- factor(pricedf$Terms, levels = pricedf$Terms)


#plot them
servP <- ggplot(servicedf, aes(x = Corr, y = reorder(Terms, Corr))) + 
  geom_point(size = 2) +
  ggtitle("Words that are Correlated with Service") + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
servP

prodP <- ggplot(producedf[1:50,], aes(x = Corr, y = reorder(Terms, Corr))) + 
  geom_point(size = 2) +
  ggtitle("Words that are Correlated with Produce") + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
prodP

stockP <- ggplot(stockdf, aes(x = Corr, y = reorder(Terms, Corr))) + 
  geom_point(size = 2) +
  ggtitle("Words that are Correlated with Stock") + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
stockP


priceP <- ggplot(pricedf[1:50,], aes(x = Corr, y = reorder(Terms, Corr))) + 
  geom_point(size = 2) +
  ggtitle("Words that are Correlated with Price") + 
  xlab("Correlation of Terms") +
  ylab("Terms (Most Frequent at Top)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="light blue", linetype = "dashed")
  )
priceP





######      This section is for making the cluster matrix by state

#make it a dataframe add in states, roll up
walmartDF <- as.data.frame(as.matrix(walmartDTM))
walmartDF$States <- as.factor(wmZip$State)
dfState <- aggregate(walmartDF[,-length(walmartDF)], by = list(walmartDF$States), sum)

#remove sparse terms and columns by setting the sum equal to a frequency for the entire set
dfStateRed <- dfState[,colSums(dfState[,2:length(dfState)]) > 100]
rownames(dfStateRed) <- dfState[,1]

#plot tree diagram for entire matrix
d <- dist(dfStateRed)
hc <- hclust(d)
op = par(bg = "white")
dend <- d %>% hclust %>% as.dendrogram
dend %>% color_branches(k=6) %>% plot(horiz=FALSE,
                                      main = "Word Clusters in Essay by State", axes = FALSE)
axis(side = 2, col = "blue", at = seq(0,300,50), labels = FALSE)
mtext(seq(0, 300,50), side = 2, at = seq(0,300,50),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = 6, horiz = FALSE)

#do by income level
#make it a dataframe add in states, roll up
walmartDF <- as.data.frame(as.matrix(walmartDTM))
walmartDF$Income <- as.factor(wmZip$Income)
dfIncome <- aggregate(walmartDF[,-length(walmartDF)], by = list(walmartDF$Income), sum)

#remove sparse terms and columns by setting the sum equal to a frequency for the entire set
dfIncomeRed <- dfIncome[,colSums(dfIncome[,2:length(dfIncome)]) > 100]
rownames(dfIncomeRed) <- dfIncome[,1]

labels <- gsub(" to less than ", "-", rownames(dfIncomeRed))
rownames(dfIncomeRed) <- labels

d <- dist(dfIncomeRed)
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", "blue") %>%
  set("labels_cex", .75) %>%
color_branches(k=6) %>%
plot(horiz=FALSE,main = "Word Clusters in Essay by Income", axes = FALSE, ylim = c(0,100))
axis(side = 2, col = "blue", at = seq(0,100,50), labels = FALSE)
mtext(seq(0, 100,50), side = 2, at = seq(0,100,50),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = 6, horiz = FALSE)


#repeat for service
serviceDF <- as.data.frame(as.matrix(serviceDTM))
serviceDF$States <- as.factor(service$State)
dfState <- aggregate(serviceDF[,-length(serviceDF)], by = list(serviceDF$States), sum)

dfServiceRed <- dfState[,colSums(dfState[,2:length(dfState)]) > 250]
rownames(dfServiceRed) <- dfState[,1]

#plot tree diagram for service
d <- dist(dfserviceRed)
hc <- hclust(d)
op = par(bg = "white")
dend <- d %>% hclust %>% as.dendrogram
dend %>% color_branches(k=6) %>% plot(horiz=FALSE,
                                      main = "Clusters in Essay Words Related to Service By State", axes = FALSE)
axis(side = 2, col = "blue", at = seq(0,300,50), labels = FALSE)
mtext(seq(0, 300,50), side = 2, at = seq(0,300,50),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = 6, horiz = FALSE)


#repeat for prices
priceDF <- as.data.frame(as.matrix(priceDTM))
priceDF$States <- as.factor(prices$State)
dfState <- aggregate(priceDF[,-length(priceDF)], by = list(priceDF$States), sum)

dfpriceRed <- dfState[,colSums(dfState[,2:length(dfState)]) > 100]
rownames(dfpriceRed) <- dfState[,1]

#plot tree diagram for prices
d <- dist(dfpriceRed)
hc <- hclust(d)
op = par(bg = "white")
dend <- d %>% hclust %>% as.dendrogram
dend %>% color_branches(k=6) %>% plot(horiz=FALSE,
                                      main = "Clusters in Essay Words Related to Price By State", axes = FALSE)
axis(side = 2, col = "blue", at = seq(0,50,10), labels = FALSE)
mtext(seq(0, 50,10), side = 2, at = seq(0,50,10),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = 6, horiz = FALSE)


#repeat for stock
stockDF <- as.data.frame(as.matrix(stockDTM))
stockDF$States <- as.factor(stock$State)
dfState <- aggregate(stockDF[,-length(stockDF)], by = list(stockDF$States), sum)

dfstockRed <- dfState[,colSums(dfState[,2:length(dfState)]) > 100]
rownames(dfstockRed) <- dfState[,1]

#plot tree diagram for stock
d <- dist(dfstockRed)
hc <- hclust(d)
op = par(bg = "white")
dend <- d %>% hclust %>% as.dendrogram
dend %>% color_branches(k=6) %>% plot(horiz=FALSE,
                                      main = "Clusters in Essay Words Related to Stock By State", axes = FALSE)
axis(side = 2, col = "blue", at = seq(0,10,2), labels = FALSE)
mtext(seq(0, 10,2), side = 2, at = seq(0,10,2),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = 6, horiz = FALSE)

#repeat for produce
produceDF <- as.data.frame(as.matrix(produceDTM))
produceDF$States <- as.factor(prod$State)
dfState <- aggregate(produceDF[,-length(produceDF)], by = list(produceDF$States), sum)

dfproduceRed <- dfState[,colSums(dfState[,2:length(dfState)]) > 100]
rownames(dfproduceRed) <- dfState[,1]

#plot tree diagram for produce
d <- dist(dfproduceRed)
hc <- hclust(d)
op = par(bg = "white")
dend <- d %>% hclust %>% as.dendrogram
dend %>% color_branches(k=6) %>% plot(horiz=FALSE,
                                      main = "Clusters in Essay Words Related to Produce By State", axes = FALSE)
axis(side = 2, col = "blue", at = seq(0,10,2), labels = FALSE)
mtext(seq(0, 10,2), side = 2, at = seq(0,10,2),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = 6, horiz = FALSE)



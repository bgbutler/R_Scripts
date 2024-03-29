# This batch of code is for extracting reviews from Amazon
# Requires XML package to be installed

#1. Load Packages
#2. Get the URL and review counts for working with the data
#3. Create the dates, reviews and stars vectors

library(XML)
library(RCurl)
library(stringi)
library(RColorBrewer)
library(ggplot2)
library(tm)
library(wordcloud)
library(RWeka)  #for making tokens out of the phrases

#fileURL <- "http://www.amazon.com/Herbalife-Cookies-Cream-Shake-ounces/product-reviews/B005DN1RVA?pageNumber=1"
fileURL <- "http://www.amazon.com/VonHaus-Upright-Handheld-Cleaner-Filtration/product-reviews/B00OHTKVM2/ref=cm_cr_pr_btm_link_1?ie=UTF8&showViewpoints=1&sortBy=helpful&reviewerType=all_reviews&filterByStar=all_stars&pageNumber=1"
#fileURL <- "http://www.amazon.com/VonHaus-Upright-Handheld-Cleaner-Filtration/dp/B00OHTKVM2/ref=cm_cr_pr_product_top?ie=UTF8"
doc <- htmlTreeParse(fileURL, useInternal=TRUE)
URL <- fileURL

#get the text into numeric format to be supplied later for loops and counts from review page
reviewCounts <- as.numeric(xpathSApply(doc, "//span[@class='a-size-medium a-text-beside-button totalReviewCount']", xmlValue))
pages <- as.numeric(xpathSApply(doc, "//li[@class='page-button']", xmlValue))
maxPages <- max(pages)

#get the text from the main product review page
reviewCounts <- xpathSApply(doc, "//span[@id='acrCustomerReviewText']", xmlValue)
count <- as.numeric(sub(" customer reviews","",count))



################# Skip this section for a major run

#main url just grab the first [1], depends on page you are on
mainURL = xpathSApply(doc, "//a[@class='a-link-normal']", xmlValue)


#if you land on the main page this is the link to the reviews

mainURL = xpathSApply(doc, "//a/@href")
mainURL = xpathSApply(doc, "//a[@class='a-link-emphasis a-text-bold']/", xmlValue)

best <- mainURL[grep("showViewpoints=1&sortBy=bySubmissionDateDescending", mainURL)]

#this collects a lot of odd stuff, but useful to find the best and worst
firstReviews <- xpathSApply(doc, "//span[@class='a-size-base']", xmlValue)

#nodes in this case shows that each review is a node
nodes = getNodeSet(doc, "//span[@class='a-size-base a-color-secondary review-date']")

#these next two steps get the key elements in the form of vectors
review <- xpathSApply(doc, "//span[@class='a-size-base review-text']", xmlValue)

reviewURL <- xpathSApply(doc, "//span[@class='asinReviewsSummary acr-popover']", xmlValue)


#each page has extra reviews because it shows postive and negative reviews on each page
#therefore you need to drop the first two
date <- xpathSApply(doc, "//span[@class='a-size-base a-color-secondary review-date']", xmlValue)
date <- date[3:length(date)]

# there are several extra ratings on a page 2 at begining and one at end
rating <- xpathSApply(doc, "//span[@class='a-icon-alt']", xmlValue)
rating <- rating[3:(length(review)+2)]


############### end of skip section for production run


#set up the dataframe and looping
dates <- character()
reviews <- character()
stars <-character()

for (i in 1:maxPages){
  fileURL <- sprintf("http://www.amazon.com/VonHaus-Upright-Handheld-Cleaner-Filtration/product-reviews/B00OHTKVM2/ref=cm_cr_pr_btm_link_1?ie=UTF8&showViewpoints=1&sortBy=helpful&reviewerType=all_reviews&filterByStar=all_stars&pageNumber=%s", i)
  print(fileURL)
  doc <- htmlTreeParse(fileURL, useInternal=TRUE)
  
  #first get the dates
  date <- xpathSApply(doc, "//span[@class='a-size-base a-color-secondary review-date']", xmlValue)
  date <- date[3:length(date)]
  dates <- append(dates, date, after = length(dates))
  
  #then get reviews
  review <- xpathSApply(doc, "//span[@class='a-size-base review-text']", xmlValue)
  reviews <- append(reviews, review, after = length(reviews))
  
  #then get the stars
  rating <- xpathSApply(doc, "//span[@class='a-icon-alt']", xmlValue)
  rating <- rating[3:(length(review)+2)]
  stars <- append(stars, rating, after =length(stars))
}

#combine into a dataframe
reviewData <- data.frame(dates, reviews, stars)

#do some clean up
rm(date)
rm(review)
#rm(firstReviews)
rm(rating)


#clean up date field
reviewData$dates <- gsub("on ", "", reviewData$dates)
#reviewData$dates <- gsub(",", "", reviewData$dates)
#you can leave the comma in and just add it after the d in the format
reviewData$dates <- as.Date(reviewData$dates, format = '%B %d, %Y')

#clean up the stars field
reviewData$stars <- gsub(" out of 5 stars", "", reviewData$stars)
reviewData$stars <- as.factor(reviewData$stars)

#clean up the review field
reviewData$reviews <- as.character(reviewData$reviews)


#############################start the text mining algorithms

#make sure clean coding
Encoding(reviewData$reviews) <- "latin1"
reviewData$reviews <- iconv(reviewData$reviews, "latin1", "ASCII", sub="")

reviewCorpus <- Corpus(VectorSource(reviewData$reviews))
reviewCorpus <- tm_map(reviewCorpus,PlainTextDocument)
reviewCorpus <- tm_map(reviewCorpus, content_transformer(tolower))
reviewCorpus <- tm_map(reviewCorpus, removeWords, stopwords("english"))
reviewCorpus <- tm_map(reviewCorpus, removePunctuation)
reviewCorpus <- tm_map(reviewCorpus, removeNumbers)
reviewCorpus <- tm_map(reviewCorpus, stripWhitespace)

# make document term matrix
reviewDTM <- DocumentTermMatrix(reviewCorpus)

#make a word cloud

png("reviewCloudVac.png", width = 1280, height = 800)
wordcloud(reviewCorpus,scale=c(5,1), min.freq = 3, max.words = 500,
          random.order = FALSE,rot.per=0.3, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off()

#Do some more text work with tokens - cut into words of 3
trigramTokenizer <- function (x) {NGramTokenizer(x,
                                  Weka_control(min = 3, max = 3))}

tokenTDM <- TermDocumentMatrix(reviewCorpus, control = list(tokenize = trigramTokenizer))

dim(tokenDTM)

inspect(tokenDTM)

tokenMatrix <- as.matrix(tokenTDM)
#sort it most frequent to least frequent and create a table
tokenV <- sort(rowSums(tokenMatrix), decreasing = TRUE)
token.d <- data.frame(word = names(tokenV), freq=tokenV)
table(token.d$freq)

#from the r-page build giant cloud for walmart
png("tokenCloud.png", width = 1280, height = 800)
wordcloud(token.d$word, token.d$freq,scale=c(8,.2), min.freq = 3, max.words = 500,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))

dev.off()






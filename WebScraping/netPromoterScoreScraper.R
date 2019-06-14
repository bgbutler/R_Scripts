#Web Scraper for the Net Promoter Score


library(XML)
library(httr)
library(RCurl)
library(stringi)
library(RColorBrewer)
library(ggplot2)


#load a file to get the companies
#load the file
url <- "K:/Sandbox/Bryan Projects/CQ/NPS_baseline01262016.csv"
cqDataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

companyList <- cqDataRaw$newName

companyList <- as.character(companyList)

#get the clean text
Encoding(companyList) <- "latin1"
companyList <- iconv(companyList, "latin1", "ASCII", sub="")

#format for making a url
companyListClean <- tolower(gsub(" ","_", companyList))
companyListClean

###########################################
#for testing
#names <- NULL
#companyName <- NULL
#companyListClean <- c("hilton", "trader_joe's", "365", "3M", "apple")
#companyListClean <- c("hilton","trader_joe's")
#companyListClean <- c("hollywood_casinos")
#i = 1
########################



#set up the dataframe and looping
scores <- character()
names <- character()
url <- character()

for (i in 251:325){

  fileURL <- sprintf("http://www.npsbenchmarks.com/%s",companyListClean[i])
  print(fileURL)
  doc <- htmlTreeParse(fileURL, useInternal=TRUE)
  
  #track urls
  url <- append(url, fileURL, after = length(url))
  
  
  #check to see if web page actually exists by checking for a back button
  
  getError <- try(xpathSApply(doc,"//h2",xmlValue))
  if(inherits(getError,"character")) {
    companyName <- companyListClean[i]
    names <- append(names, companyName, after = length(names))
    lastScore <- 0
    scores <- append(scores, lastScore, after = length(scores))
    next
  }
    
  
  #scrap the name
  clean <- "\\n|\\t"
  
  
  
  companyName <- xpathSApply(doc, "//td[@class='kv-value-big']",
                             function(x) gsub(clean,"",xmlValue(x)))

  #start the cleanup of the text
  #remove leading and traing spaces
  companyName <- gsub("^\\s+|\\s+$", "", companyName)
  
  lenName <- nchar(companyListClean[i])
  
  #get the company name
  companyName <- substr(companyName,1,lenName)
  companyName <- as.character(companyName)
  names <- append(names, companyName, after = length(names))
  
  
  #then get the score
  lastScore <- xpathSApply(doc, "//i[@class='fa kv-icon kv-icon-secondary kv-big']", 
                               xmlValue)
  lastScore <- as.numeric(lastScore)
  scores <- append(scores, lastScore, after = length(scores))
  
}

#combine into a dataframe
npsData1 <- data.frame(names, scores)
npsData1

npsData2 <- data.frame(names, scores)
npsData2

npsData3 <- data.frame(names, scores)
npsData3

dfAll <- rbind(npsData1, npsData2, npsData3)

#write to .CSV
write.csv(dfAll, file = "K:/Sandbox/Bryan Projects/CQ/NPS_baseline01262016v2.csv", 
          row.names = FALSE)



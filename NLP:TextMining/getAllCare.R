

# script to analyze care data
# load in a bunch of files and merge them


.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

# these packages are more data cleaning and tefor text analytics
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(readxl)
library(SnowballC)
library(tm)
library(RTextTools)
library(stringi)
library(wordcloud)
library(RWeka)
library(digest)
library(readxl)
library(data.table)
library(qdap)

url <- "C:/Users/n846490/Documents/DataScience/XLSX/CareFiles2/"

files <- list.files(url, full.names = T)
fileNames <- list.files(url, full.names = F)

count <- length(files)

# start with the all other types
# create an empty data frame

otherCase <- data.frame()

# loop through the 'all other case types' and append
for (i in 1:count){
  print(paste('File: ',i, fileNames[i], sep = ' '))
  otherCase <- rbind(otherCase,
                     read_excel(files[i], sheet = 'All Other Case Types',
                                col_names = T, col_types = NULL,
                                skip = 1))}

# then get the Record Only files
recordOnly <- data.frame()

for (i in 1:count){
  print(paste('File: ',i, fileNames[i], sep = ' '))
  recordOnly <- rbind(recordOnly,
                     read_excel(files[i], sheet = 'Record Only',
                                col_names = T, col_types = NULL,
                                skip = 1))}


# get some select fields from the otherCase
# filter out 'Positive Feedback' and 'Service Requests'

colnames(otherCase)

oC <- otherCase %>% 
  filter(`Type of Cases` != 'Positive Feedback' & `Type of Cases` != 'Service Request') %>%
  select(`Registered Date`,
         `Type of Cases`,
         `Family`,
         `Sub-family`,
         `Type ID`,
         `Customer ID`,
         Details,
         Comments,
         `Final resolution`)

unique(oC$`Type of Cases`)


rO <- recordOnly %>% 
#  filter(`Type of Cases` != 'Positive Feedback' & `Type of Cases` != 'Service Request') %>%
  select(`Registered Date`,
         `Type of Cases`,
         `Issue Category`,
         `Issue Sub-category 1`,
         `Type ID`,
         `Customer ID`,
         Details,
         Comments,
         `Final resolution`)

unique(rO$`Type of Cases`)


# standardize the names
newNames <- c('Date', 'Type', 'Category', 'SubCategory', 'CustType',
             'CustNumber', 'Details', 'Comments', 'FinalResolution')


# apply the names
colnames(oC) <- newNames
colnames(rO) <- newNames

# clean the dates
oC$Date <- as.Date(oC$Date)
rO$Date <- as.Date(rO$Date)

# create a month column
# used for aggregating by month
oC$Month <- format(oC$Date, format = "%b-%y")
oC$Month <- as.factor(oC$Month)

# get the unique values
unique(oC$Month)

oC$Month <- ordered(oC$Month, levels = c('Feb-16', 'May-16', 'Jun-16',
                                         'Jul-16', 'Aug-16', 'Sep-16',
                                         'Oct-16', 'Nov-16', 'Dec-16', 'Jan-17', 'Feb-17'))

# month columns of rO
rO$Month <- format(rO$Date, format = "%b-%y")
rO$Month <- as.factor(rO$Month)

unique(rO$Month)

rO$Month <- ordered(rO$Month, levels = c('Sep-16', 'Oct-16', 'Nov-16',
                                         'Dec-16', 'Jan-17', 'Feb-17'))


# remove the excess files
rm(otherCase); rm(recordOnly); gc()

################################################

# prepare the text columns by cleaning
# create revised stopwords list
newWords <- stopwords("english")
keep <- c("no", "more", "not", "can't", "cannot", "isn't", "aren't", "wasn't",
          "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't",
          "")

newWords <- newWords [! newWords %in% keep]   

# make dataframe for details
# get a count of na

sum(is.na(oC$Month))

# first is for the other comments

# create a grob for statement fees
grx <- glob2rx('*65*|*statement*|*fee*|*waive*')
stateFee <- with(oC,
                 subset(oC,
                        subset = grepl(grx, oC$Details)))

# make a plot over time

#get a plot of month of the statement fee
g <- ggplot(stateFee, aes(x = Month)) + ggtitle('Paper Statement Fee Issues') + 
  geom_bar(stat = 'count', fill = 'red') + 
  theme(axis.text.x = element_text(size = 8, color = 'red', angle = 0),
        legend.position = 'bottom',
        axis.text.y = element_text(color = 'red'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = 'red'),
        strip.text.x = element_text(size = 10, face = 'bold'),
        strip.background = element_rect(fill = 'red')) +
  theme(plot.title = element_text(color = 'red'))
g 


# all other comments
aoC <- with(oC,
            subset(oC,
                   subset = !grepl(grx, oC$Details)))

# make a function for unique words
uniqueWords = function(d) {
  return(paste(unique(strsplit(d, " ")[[1]]), collapse = ' '))
}

# make a special reader
myReader <- readTabular(mapping=list(content='Details', id='Month'))
# this implements the reader to keep our ids
ocDet <- VCorpus(DataframeSource(aoC), readerControl=list(reader=myReader))
# ocDet <- Corpus(VectorSource(aoC$Details))
ocDet <- tm_map(ocDet,PlainTextDocument)
ocDet<-tm_map(ocDet, content_transformer(tolower))
#ocDet <- tm_map(ocDet, removeWords, newWords)
#ocDet <- tm_map(ocDet, removePunctuation)
# ocDet <- tm_map(ocDet, removeNumbers)
# apply the unique counter
ocDet = tm_map(ocDet, content_transformer(uniqueWords))

# ocDet <- tm_map(ocDet, stripWhitespace)


# make the tokenizer
m = 5
ngramTokenizer <- function (x)
                    {NGramTokenizer(x,Weka_control(min = m, max = m))}

# make a DTM
ocDetDTM <- DocumentTermMatrix(ocDet,
                               control = 
                                 list(tokenize = ngramTokenizer))
# clean up
gc()

# reduce sparsity in matrix due to size
# use the DTM
ocDetMat <- as.matrix(removeSparseTerms
                      (x = ocDetDTM, sparse = .9992))

# make a df of the document term matrix
ocDetDF <- data.frame(ocDetMat)

# now that the dimensions are preserved
ocDetDF$Month <- as.factor(aoC$Month)

# get the counts
dfOc <- aggregate(ocDetDF[,-length(ocDetDF)], by = list(ocDetDF$Month), sum)

# change the row names
rownames(dfOc) <- dfOc[,1]

colnames(dfOc)[1] <- 'Month'


meltOc <- melt(dfOc, id = 'Month',
               variable.name = "Term",
               value.name = "Count")

head(meltOc,10)

# clean up the periods
meltOc$Term <- gsub('\\.', ' ', meltOc$Term)

rollup <- meltOc %>% group_by(Month, Term) %>% 
  summarise(
    Total = sum(Count))

head(rollup)

# set the treshold - want to end up with about 45 terms
rollupRed <- filter(rollup, Total > 130)


#get a plot of month
g <- ggplot(rollupRed, aes(x = reorder(Term, Total), y = Total)) + coord_flip() + 
  geom_bar(stat = 'identity', fill = 'red') + 
  facet_wrap(~Month) + 
  theme(axis.text.x = element_text(size = 8, color = 'red', angle = 0),
#        legend.position = 'bottom',
        axis.text.y = element_text(color = 'red'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 10, face = 'bold'),
        strip.background = element_rect(fill = 'red')) +
  theme(plot.title = element_text(color = 'red'))
g 

# get a zoom of last 3 months
rollup3Month <- meltOc %>% group_by(Month, Term) %>% 
  filter(Month == 'Nov-16' | Month == 'Dec-16' | Month == 'Jan-17') %>%
  summarise(
    Total = sum(Count))

head(rollup3Month)

# set the treshold - want to end up with about 45 terms
rollup3Red <- filter(rollup3Month, Total > 85)


#get a plot of month
g <- ggplot(rollup3Red, aes(x = reorder(Term, Total), y = Total)) + coord_flip() + 
  geom_bar(stat = 'identity', fill = 'red') + 
  facet_wrap(~Month) + 
  theme(axis.text.x = element_text(size = 8, color = 'red', angle = 0),
        #        legend.position = 'bottom',
        axis.text.y = element_text(color = 'red'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 10, face = 'bold'),
        strip.background = element_rect(fill = 'red')) +
  theme(plot.title = element_text(color = 'red'))
g 



####################################
#####   THIS IS FOR THE MINING OF RECORD ONLY

# then for the details in the record only
# get a count of na

sum(is.na(rO$Details))

# remove the NA and blanks

rODetailsClean <- filter(rO, Details != "" | Details != "NA")

roDet <- Corpus(VectorSource(rODetailsClean$Details))
roDet <- tm_map(roDet,PlainTextDocument)
roDet <- tm_map(roDet, removeWords, newWords)
roDet <- tm_map(roDet, removePunctuation)
roDet <- tm_map(roDet, removeNumbers)
roDet <- tm_map(roDet, stripWhitespace)
roDet = tm_map(roDet, content_transformer(uniqueWords))

# make a DTM
roDetDTM <- DocumentTermMatrix(roDet,
                               control = 
                                 list(tokenize = ngramTokenizer))
# clean up
gc()

# reduce sparsity in matrix due to size
# use the DTM
roDetMat <- as.matrix(removeSparseTerms
                      (x = roDetDTM, sparse = .9992))

# make a df of the document term matrix
roDetDF <- data.frame(roDetMat)

# now that the dimensions are preserved
roDetDF$Month <- as.factor(rODetailsClean$Month)

# get the counts
dfRo <- aggregate(roDetDF[,-length(roDetDF)], by = list(roDetDF$Month), sum)

# change the row names
rownames(dfRo) <- dfRo[,1]

colnames(dfRo)[1] <- 'Month'


meltRo <- melt(dfRo, id = 'Month',
               variable.name = "Term",
               value.name = "Count")

head(meltRo,10)

# clean up the periods
meltRo$Term <- gsub('\\.', ' ', meltRo$Term)

rollup <- meltRo %>% group_by(Month, Term) %>% 
  summarise(
    Total = sum(Count))

head(rollup)

# set the treshold - want to end up with about 45 terms
rollupRed <- filter(rollup, Total > 9)


#get a plot of month
g <- ggplot(rollupRed, aes(x = reorder(Term, Total), y = Total)) + coord_flip() + 
  geom_bar(stat = 'identity', fill = 'red') + 
  facet_wrap(~Month) + 
  theme(axis.text.x = element_text(size = 8, color = 'red', angle = 0),
        #        legend.position = 'bottom',
        axis.text.y = element_text(color = 'red'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 10, face = 'bold'),
        strip.background = element_rect(fill = 'red')) +
  theme(plot.title = element_text(color = 'red'))
g 







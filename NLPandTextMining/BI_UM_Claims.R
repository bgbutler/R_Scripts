



# load libraries for text mining

library(ggplot2)
library(reshape2)
library(scales)
library(SnowballC)          # Porter's word stemming
library(tm)                 # Text Cleaning and Processing
library(RTextTools)         # Automatic Text Classification via Supervised Learning
library(stringi)            # For processing strings
library(wordcloud)          # Plot word clouds
library(RWeka)              # Machine Learning Java Library for Tokenization
library(tidytext)           # Additional Text Processing Library (new from RStudio)
library(tidyr)              
library(knitr)
library(dendextend)         # Making Dendograms
library(dendextendRcpp)     # Supporting package for dendextend
library(qdapRegex)          # function to remove words less than 3 letters
library(readr)
library(stringr)
library(textclean)


# Modeling Libraries
library(caret)
library(Metrics)
library(pROC)
library(naivebayes)         # Naive Bayes package
library(klaR)               # Alternative NB package
library(e1071)              # Windows specific modeling package best NB package
library(caTools)            # Utility function to help modeling split
library(randomForest)

# Manipulating Data
library(dplyr)


# use this theme for any ggplots
theme_bryan <- function () { 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 90),
        legend.position = 'right',
        axis.text.y = element_text(color = 'blue'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 10, face = 'bold'),
        strip.background = element_rect(fill = 'light blue'))
}


setwd('N:/Projects/Text-Mining/BI_UM_Text')
dataURL <- 'BI_UM_Claims_Text.csv'
textDf <- read.csv(dataURL, stringsAsFactors = F)


## @knitr names
# get the names data to remove from text
# read in txt file delimited by comma
names_1 <- read_delim("all_names.txt", ',', col_names = FALSE)

# grab first column and filter for unique names
unique_names <- names_1$X1 %>% unique() %>% tolower()

# place in dataframe
names_df <- as.data.frame(unique_names)

names_df$unique_names <- as.character(names_df$unique_names)

head(names_df)



# original words for replacement
orig_words <- c('insd','insrd', 'ins', 'agt', 'agnt', 'cancellation',
                'prem', 'pmt', 'cov', 'req', 'attn', 'pymt', 'rcvd',
                'recd', 'toyt', 'doesn', 'acct', 'cancelled', 'pymnt',
                'pmyt', 'pmts', 'wasn', 'agcy', 'mortg', 'eff', 'adv',
                'advsd', 'advd', 'uw', 'veh', 'didn', 'recieved', 'recvd',
                'amt', 'xfer', 'dr', 'insureds', 'liab', 'xfered', 'xferred',
                'trsf', 'insure', 'restricti', 'restric', 'restrictio', 'restrict',
                'restri', 'dodg', 'niss', 'suba', 'wouldn', 'couldn', 'inq',
                'chvy', 'covg', 'dept', 'recv', 'hond', 'isnd', 'lexs', 'chng',
                'deduc', 'amnt', 'cst', 'insds', 'premi', 'pre', 'premiu', 'addr', 
                'buic', 'fro', 'hadn', 'addl', 'cance', 'clmt', 'cmlt','atty', 'attny')


# corresponding replacement words 
repl_words <- c('insured', 'insured', 'insurance', 'agent', 'agent', 'cancel', 
                'premium', 'payment', 'coverage', 'required', 'attention', 
                'payment', 'recieved', 'recieved', 'toyota', 'doesnt', 'account', 
                'cancel', 'payment', 'payment', 'payment', 'wasnt', 'agency', 
                'mortgage', 'effective', 'advise', 'advise', 'advise', 
                'underwriter', 'vehicle', 'didnt', 'received', 'received', 
                'amount', 'transfer', 'doctor', 'insured', 'liability', 'transfer', 
                'transfer', 'transfer', 'insured', 'restriction', 'restriction', 
                'restriction', 'restriction', 'restriction', 'dodge', 'nissan', 
                'subaru', 'wouldnt', 'couldnt', 'inquire', 'chevy', 'coverage', 
                'department', 'received', 'honda', 'insured', 'lexus', 'change', 
                'deduction', 'amount', 'cost', 'insured', 'premium', 'premium', 
                'premium', 'address', 'buick', 'from', 'hadnt', 'additional', 
                'cancel', 'claimant', 'claimant', 'attorney', 'attorney')


# check to make sure original and repl words match up
length(orig_words) == length(repl_words)

# add a space to the end of the words
# using qdap will replace these later on
orig_words <- orig_words %>% paste0(" ")
repl_words <- repl_words %>% paste0(" ")

## @knitr cleanData
# clean up the dates types
textDf$note_date <- as.Date(textDf$authoringdate, format = '%d/%M/%Y')
textDf$loss_date <- as.Date(textDf$dt_of_loss, format = '%d/%m/%Y')
textDf$note_year <- lubridate::year(textDf$note_date)
textDf$loss_year <- lubridate::year(textDf$loss_date)

# make factors
textDf$topic <- as.factor(textDf$topic)
textDf$subject <- as.factor(textDf$subject)


# make um bi numeric
textDf$bi_il <- as.numeric(textDf$bi_il)
textDf$um_il <- as.numeric(textDf$um_il)


# add identifier for um/bi
textDf$coverage <- ifelse(textDf$bi_il == 0, "um", "bi")

# remove extra columns
# test_df[, !(colnames(test_df) %in% rm_col), drop = FALSE]
rm_col <- c('year', 'year.1', 'authoringdate', 'dt_of_loss')
textDf <- textDf[, !(colnames(textDf) %in% rm_col), drop = FALSE]

# cut the incurred loss into bins
# first take the max, they are mutually exclusive
textDf$loss <- pmax(textDf$bi_il, textDf$um_il)

textDf$loss_bins <-  cut(textDf$loss,
                         c(0, 25000,50000,125000, 250000, 10000000), include.lowest = T,
                         labels = c('0-25K', '025-50K', '050-125K','125-250K','250K+'))

# remove NA from coverage
textDf <- textDf %>% filter(!is.na(coverage))


# make the final identifier
textDf$claim_bin <- paste(textDf$coverage, textDf$loss_bins, sep = ": ")

# make loss year a character
textDf$loss_year_char <- as.character(textDf$loss_year)

textDf <- textDf %>% filter(loss_year != 'NA')

textDf$claim_yr_bin <- paste(textDf$loss_year_char, textDf$coverage, textDf$loss_bins, sep = ": ")


## @knitr cleanText
######### THIS IS THE TEXT CLEANING AND PROCESSING ####

# process the text to clean
# First step in Processing
textDf$masked_notetext <- tolower(textDf$masked_notetext)

stop_words <- stopwords("english")
keep <- c("no", "more", "not", "can't", "cannot", "isn't", "aren't", "wasn't",
          "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't")
newWords <- stop_words [! stop_words %in% keep]
words_names <- c(newWords, unique_names)


# replace double '' with single
textDf$text_clean <- gsub("''", "'", textDf$masked_notetext)

# expand the contractions
textDf$text_clean <- qdap::replace_contraction(textDf$text_clean)


## BUILD FUNCTIONS FOR CLEANING ###
# clean the format remove oddball chars leaves a\
non_ascii <- function (x) {replace_non_ascii(x, remove.nonconverted = TRUE)}
clean_non_ascii <- function (x) {gsub(" a\"", " ", x)}

# apply functions
textDf$text_clean <- non_ascii(textDf$text_clean)
textDf$text_clean <- clean_non_ascii(textDf$text_clean)

# remove masking combination of six x's()
pattern <- c("xxx*[a-z]")
repl_masking <- function (x) {gsub(pattern, " ",x)}
textDf$text_clean <- repl_masking(textDf$text_clean)


# clean possessives
possessive <- function (x) {gsub("'s", " ", x)}
textDf$text_clean <- possessive(textDf$text_clean)

# remove URLs from http to first space
url_repl <- function (x) {gsub("(http)(.*?)[[:space:]]+", " ", x)}
textDf$text_clean <- url_repl(textDf$text_clean)
# clean up residuals
pat_resid <- c("&([a-z]|[0-9]|=|_)*")
repl_resid <- function (x) {gsub(pat_resid, " ",x)}
textDf$text_clean <- repl_resid(textDf$text_clean)


# clean punctuation
punct_replace <- function (x) {str_replace_all(x, '[[:punct:]]', ' ')}
textDf$text_clean <- punct_replace(textDf$text_clean)

# replace words
use_dict <- function (x) {qdap::mgsub(orig_words, repl_words,x)}
textDf$text_clean <- use_dict(textDf$text_clean)

# replace the names
textDf$text_clean <- textclean::replace_names(textDf$text_clean, 
                                              names = unique_names,
                                              replacement = " ")


smallWds <- function(x) {rm_nchar_words(x, "1,2")}
wordsNumbers <- function(x) {gsub("[^[:alnum:][:blank:]']", " ", x)}

# write clean csv
write.csv(textDf, file='bi_um_clean_text.csv', row.names=FALSE)

# clean and process
text <- VCorpus(VectorSource(textDf$text_clean))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)

text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(wordsNumbers))
text <- tm_map(text, content_transformer(smallWds))

text <- tm_map(text, stripWhitespace)

## @knitr clusterClaimBin
# start cluster analysis
# requires DTM
# make the DTM
DTM <- DocumentTermMatrix(text)


# save for later
DTM <- DocumentTermMatrix(text, 
                          control = list(weighting = 
                                           function(x) weightTfIdf(x, normalize = FALSE), 
                                         tokenize = MygramTokenizer))

# Convert it to a data frame via matrix
# Collapse it down to state
dtmDF <- as.data.frame(as.matrix(removeSparseTerms(x = DTM, sparse = .9995)))

# need to be careful of the column name
dtmDF$claim_bins <- as.factor(textDf$claim_bin)
df_claim <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF$claim_bins), sum)

# Remove sparse terms and columns by picking columns that sum to 5 or more
# Setting the sum equal to a frequency for the entire set
dfClaimRed <- df_claim[,colSums(df_claim[,2:length(df_claim)]) > 5]
rownames(dfClaimRed) <- df_claim[,1]

# Create a correlation matrix and make it a distance matrix
words <- length(dfClaimRed) 
dis <- as.dist(1 - cov2cor(cov(dfClaimRed[,c(2:words)], 
                               method = "pearson", 
                               use = "pairwise.complete.obs")))

# remove scientific notation
options(scipen = 999)

par(mar = c(2.5, 0.5, 1.0, 7))
d <- dist(dfClaimRed, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
labels_cex(dend) <- 1.25
dend %>% 
  color_branches(k=5) %>%
  color_labels() %>%
  highlight_branches_lwd(4) %>% 
  plot(horiz=TRUE, main = "Claim Word Clusters by Claim Bin", axes = T, xlim = c(40000,0))


## @knitr clusterYearBins
#################   LOSS YEAR BINS
# cluster on loss year bins
# need to be careful of the column name
dtmDF$claim_yr_bins <- as.factor(textDf$claim_yr_bin)

# drop claim bins first
dtmDF$claim_bins <- NULL

# run to ensure last column
grep("claim_yr_bins", colnames(dtmDF))

# aggregate it
df_claim <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF$claim_yr_bins), sum)

# reduce the data set

# Remove sparse terms and columns by picking columns that sum to 5 or more
# Setting the sum equal to a frequency for the entire set
# remove NA
dfClaimRed <- df_claim[,colSums(df_claim[,2:length(df_claim)]) > 5]
rownames(dfClaimRed) <- df_claim[,1]

# Create a correlation matrix and make it a distance matrix
words <- length(dfClaimRed) 
dis <- as.dist(1 - cov2cor(cov(dfClaimRed[,c(2:words)], 
                               method = "pearson", 
                               use = "pairwise.complete.obs")))

# remove scientific notation
options(scipen = 999)

par(mar = c(2.5, 0.5, 1.0, 7))
d <- dist(dfClaimRed, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
labels_cex(dend) <- 0.85
dend %>% 
  color_branches(k=8) %>%
  color_labels() %>%
  highlight_branches_lwd(4) %>% 
  plot(horiz=TRUE, main = "Claim Word Clusters by Claim Yr Bin", axes = T, xlim = c(15000,0))



# groups to investigate
# 2018 UM :250K+
#2016 UM 025 - 
# 2017 bi 125K

# 2018 UM 025 - 50
# 2013 UM 025 - 50
# 2018 BI 250K +

# 2017 UM 250K
# 2016 125 - 250K
# 2016 um 250K

# one last clustering



## @knitr clusterTopicBins
###### Clustering by topic

# need to be careful of the column name
textDf$topic_claim_bin <- paste(textDf$topic, textDf$coverage, textDf$loss_bins, sep = ": ")
dtmDF$topic_claim_bins <- as.factor(textDf$topic_claim_bin)

# remove the 0 - 25K bins
unique_bins <- unique(dtmDF$topic_claim_bins)
unique_bins

dtmDF <- dtmDF %>% dplyr::filter(!str_detect(topic_claim_bins, '0-25K'))
dtmDF <- dtmDF %>% dplyr::filter(!str_detect(topic_claim_bins, '025-50K'))

# drop claim bins first
dtmDF$claim_yr_bins <- NULL

# run to ensure last column
grep("topic_claim_bins", colnames(dtmDF))

# aggregate it
df_claim <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF$topic_claim_bins), sum)

# reduce the data set

# Remove sparse terms and columns by picking columns that sum to 5 or more
# Setting the sum equal to a frequency for the entire set
# remove NA
dfClaimRed <- df_claim[,colSums(df_claim[,2:length(df_claim)]) > 5]
rownames(dfClaimRed) <- df_claim[,1]

# Create a correlation matrix and make it a distance matrix
words <- length(dfClaimRed) 
dis <- as.dist(1 - cov2cor(cov(dfClaimRed[,c(2:words)], 
                               method = "pearson", 
                               use = "pairwise.complete.obs")))

# remove scientific notation
options(scipen = 999)

par(mar = c(2.5, 0.5, 1.0, 8))
d <- dist(dfClaimRed, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
labels_cex(dend) <- 0.70
dend %>% 
  color_branches(k=8) %>%
  color_labels() %>%
  highlight_branches_lwd(4) %>% 
  plot(horiz=TRUE, main = "Claim Word Clusters by Topic Claim Bin", axes = T, xlim = c(5000,0))


## @knitr largeLosses
# start with the large losses
large <- textDf %>% filter(loss > 50000)

# clean and process
text <- VCorpus(VectorSource(large$text_clean))
text <- tm_map(text,PlainTextDocument)
text <- tm_map(text, removeWords, newWords)
text <- tm_map(text, removeWords, "gonzalez")
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, content_transformer(wordsNumbers))
text <- tm_map(text, content_transformer(smallWds))

text <- tm_map(text, stripWhitespace)

# tokenize
tokens <- 3
MygramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = tokens, max = tokens))}



llDTM <- DocumentTermMatrix(text, control = list(weighting = 
                                                        function(x)
                                                        weightTfIdf(x, normalize = FALSE), 
                                                        tokenize = MygramTokenizer))

# Collapse it down
llTextDF <- as.data.frame(as.matrix(removeSparseTerms(x = llDTMCommon, sparse = .9999)))
llTextDF$yr_claim <- as.factor(large$claim_yr_bin)
lldf <- aggregate(llTextDF[,-length(llTextDF)], by = list(llTextDF$yr_claim), sum)
colnames(lldf)[colnames(lldf) == 'Group.1'] <- 'YearClaim'

# Melt the data
melttext <- melt(lldf, id = 'YearClaim',
                  variable.name = 'Term',
                  value.name = 'TFIDF')

## @knitr UMlargeLosses_weighted
# alternate plot for um
rollup <- melttext %>% 
  group_by(YearClaim, Term) %>% 
  summarise(Total = sum(TFIDF)) %>%
  filter(str_detect(YearClaim, 'um: 250K+')) %>% 
  top_n(25)

g <-  ggplot(rollup, aes(reorder(Term, Total), Total, fill = YearClaim)) + 
  geom_col(show.legend = FALSE, fill = "#7FD4BE", color = "#6EB7A9") + 
  labs(x = NULL, y = "count") + 
  facet_wrap(~YearClaim, ncol = 3, scales = "free") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
g


## @knitr BIlargeLosses_weighted
# alternative plot for bi
rollupbi <- melttext %>% 
  group_by(YearClaim, Term) %>% 
  summarise(Total = sum(TFIDF)) %>%
  filter(str_detect(YearClaim, 'bi: 250K+')) %>% 
  top_n(30)



g2 <-  ggplot(rollupbi, aes(reorder(Term, Total), Total, fill = YearClaim)) + 
  geom_col(show.legend = FALSE, fill = "#7FD4BE", color = "#6EB7A9") + 
  labs(x = NULL, y = "count") + 
  facet_wrap(~YearClaim, ncol = 3, scales = "free") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
g2



## @knitr largeLossesCommon
llDTMCommon <- DocumentTermMatrix(text, control = list(tokenize = MygramTokenizer))
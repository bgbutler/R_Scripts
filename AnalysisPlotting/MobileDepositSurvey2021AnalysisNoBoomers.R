

## @knitr loadLibraries
# analyze survey



library(ggplot2)
library(reshape2)
library(scales)
library(SnowballC)          # Porter's word stemming
library(tm)                 # Text Cleaning and Processing
library(RTextTools)         # Automatic Text Classification via Supervised Learning
library(stringi)            # For processing strings
library(wordcloud)          # Plot word clouds

library(tidytext)           # Additional Text Processing Library (new from RStudio)
library(tidyr)              
library(knitr)
library(dendextend)         # Making Dendograms
library(plotly)


library(qdapRegex)          # function to remove words less than 3 letters
library(readr)
library(stringr)
library(textclean)
library(forcats)
library(topicmodels)

library(RColorBrewer)
library(GGally)
library(dplyr)


## @knitr getData
# set the wd
setwd('C:/Users/bbutler/MyStuff/DataScience/NLP')


# get the data
# list.files()
allData = read_csv('MobileDepositSurvey2021.csv')

# get the original list to check ages
data_list = read_csv('C:/Users/bbutler/MyStuff/TransactionAnalysis/CustomerSurveyListCleanRev1.csv')


# get the old survey data
oldList = read_csv('C:/Users/bbutler/MyStuff/TransactionAnalysis/Mobile Deposit in Branch List - March 2019 Clean.csv')


# old survey key questions
oldSurvey = read_csv('C:/Users/bbutler/MyStuff/TransactionAnalysis/OldSurveyKeyQs.csv')



colnames(allData)

# get critical data
data = allData %>% dplyr::select(CompletedDate,
                                 Age,
                                 BranchName,
                                 DepositAmount,
                                 RecallDeposit,
                                 KnowMobile,
                                 UsedMobile,
                                 PrimaryNotUseMobile,
                                 PrimaryNotMobileComments,
                                 DescribeChallenges,
                                 SmartPhone,
                                 GetSmartPhoneUseApp,
                                 UseAppInFuture,
                                 MobileReluctance,
                                 ReluctanceMobileComments,
                                 NowMobileAwareLikelyUse,
                                 ReluctanceReason,
                                 ReluctanceOther)


# clean the date
data$CompletedDate = as.Date(data$CompletedDate, format = '%m/%d/%Y')



# function that orders likely
order_likely <- function(x) {
  ordered(x, levels = c(NA, 'Very unlikely', 'Somewhat unlikely', 'Note sure', 'Somewhat likely', 'Very likely'))
  
}

# use for use app in future
order_likely_future <- function(x) {
  ordered(x, levels = c(NA, 'Very unlikely', 'Somewhat unlikely', 'Unsure', 'Somewhat likely', 'Very likely'))
  
}




# order the likely answers
data$GetSmartPhoneUseApp = order_likely(data$GetSmartPhoneUseApp)

data$UseAppInFuture = order_likely_future(data$UseAppInFuture)

data$NowMobileAwareLikelyUse = order_likely(data$NowMobileAwareLikelyUse)


# handle the age issue
data$Age = ifelse(data$Age > 100, mean(data$Age), data$Age)

# clean up the branch names to proper first make all lower
data$BranchName = tools::toTitleCase(tolower(data$BranchName))

# make generation function
apply_gen = function(x){
  ifelse(x < 25, 'Gen Z',
         ifelse(x > 24 & x < 41, 'Millennials',
                ifelse(x > 40 & x < 57, 'Gen X',
                       ifelse(x > 56, 'Boomers+',
                              NA))))
}


order_gen = function(x){
  ordered(x, levels = c(NA,
                        'Gen Z',
                        'Millennials',
                        'Gen X',
                        'Boomers+'
  ))
  }


# make the generation factor for the original set
data$Generation = apply_gen(data$Age)
data$Generation = order_gen(data$Generation)

# original data set
data_list$Generation = apply_gen(data_list$Customer_Age)
data_list$Generation = order_gen(data_list$Generation)

# old data set
oldList$Generation = apply_gen(oldList$Age)
oldList$Generation = order_gen(oldList$Generation)

# old survey
oldSurvey$Generation = apply_gen(oldSurvey$Age)
oldSurvey$Generation = order_gen(oldSurvey$Generation)


## @knitr plotFunctions
######   FUNCTIONS TO HANDLE DATA AND PLOTTING
# select a palette
easternPal = c('darkblue', 'sienna1', 'lightsteelblue1', 'royalblue4', 'skyblue', 'darkgreen', 'orange')


# use this theme
# use this theme
theme_bryan <- function () { 
  theme(axis.text.x = element_text(size = 10, color = 'blue', angle = 0, face ='bold'),
        legend.position = 'bottom',
        axis.text.y = element_text(size = 12, color = 'blue', angle = 0, face ='bold'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = 'darkblue', size = 10, face = 'bold'),
        strip.background = element_rect(fill = 'light blue'))
}


# barplot function for generations
plot_bar_generation = function(df, x, y, fill, n, title){
  g = ggplot(df, aes(y = y, x=x, fill = Generation)) +
    geom_bar(position = 'dodge', stat = 'identity') + 
    
    geom_text(position = position_dodge(width= 1),
              size = 4, color = 'blue',
              aes(x=Percent+1, label=paste0(format(Percent, digits = 0),"%"), hjust=0)) +
    
    
    scale_fill_manual(values = easternPal) +
    theme_bryan() + 
    theme(legend.position = '') + 
    theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
    ggtitle(paste(title," (n= ", n, ")"))
  return(g)
}


# code reasons
data$NotUseMobile = ifelse(data$PrimaryNotUseMobile == 'My deposit included cash/money order that an app could not accommodate',
                           'Had cash/money',
                           ifelse(data$PrimaryNotUseMobile=='I  was near branch/had other banking business to do',
                                  'Other business nearby',
                                  ifelse(data$PrimaryNotUseMobile=='I had multiple checks to deposit so more convenient to do it at branch',
                                         'Multiple Checks',
                                         ifelse(data$PrimaryNotUseMobile=='I believed my funds would be available sooner by depositing the check at a branch',
                                                'Thought funds avail sooner',
                                                ifelse(data$PrimaryNotUseMobile=='Check amount exceeded deposit limit',
                                                       'Exceeded Limit',
                                                       ifelse(data$PrimaryNotUseMobile=='I do not trust Mobile Deposit',
                                                              "Bad experience/don't Trust",
                                                              ifelse(data$PrimaryNotUseMobile=='I did not have a good experience using app/did not work for me',
                                                                     "Bad experience/don't Trust",
                                                                     data$PrimaryNotUseMobile)))))))


# shorten reasons
data$AwareMobileReluctance = ifelse(data$MobileReluctance == 'I am concerned about security making a deposit by phone',
                                    'Security',
                                    ifelse(data$MobileReluctance =='I am not all that comfortable using technology/apps',
                                           'Tech Skills',
                                           ifelse(data$MobileReluctance =='I do not like to use online or mobile services to do my banking',
                                                  "Don't Like",
                                                  ifelse(data$MobileReluctance =='I like banking in person with the local branch staff',
                                                         'Like Branch Staff',
                                                         data$MobileReluctance))))





################################# GET SURVEY AND MAIL DEMOGRAPHICS
# survey generations
surveyGen = data %>% dplyr::select(Generation) %>% 
  group_by(Generation) %>% 
  summarise(Count = n()) %>% 
  filter(!is.na(Generation))

# get the total for percentage
survey_gen = sum(surveyGen$Count)

# get percentage
surveyGen$Percent = surveyGen$Count/survey_gen * 100


# plot for current survey
plot_bar_generation(df=surveyGen, x=surveyGen$Percent, y=surveyGen$Generation, n=survey_gen,
                    title = 'Survey Participation by Generation')


# current survey mailing list
# survey generations
listGen = data_list %>% dplyr::select(Generation) %>% 
  group_by(Generation) %>% 
  summarise(Count = n()) %>% 
  filter(!is.na(Generation))

# get the total for percentage
list_gen = sum(listGen$Count)

# get percentage
listGen$Percent = listGen$Count/list_gen * 100


# plot for current survey
plot_bar_generation(df=listGen, x=listGen$Percent, y=listGen$Generation, n=list_gen,
                    title = 'Email List by Generation')



# get original counts
originalGen = oldList %>% dplyr::select(Generation) %>% 
  group_by(Generation) %>% 
  summarise(Count = n()) %>% 
  filter(!is.na(Generation))

# get the total for percentage
original_gen = sum(originalGen$Count)

# get percentage
originalGen$Percent = originalGen$Count/original_gen * 100


plot_bar_generation(df=originalGen, x=originalGen$Percent, y=originalGen$Generation, n=original_gen,
                    title = 'Original List by Generation')


# get original counts
oldGen = oldSurvey %>% dplyr::select(Generation) %>% 
  group_by(Generation) %>% 
  summarise(Count = n()) %>% 
  filter(!is.na(Generation))

# get the total for percentage
old_gen = sum(oldGen$Count)

# get percentage
oldGen$Percent = oldGen$Count/old_gen * 100


plot_bar_generation(df=oldGen, x=oldGen$Percent, y=oldGen$Generation, n=old_gen,
                    title = 'Original Survey by Generation')



plot_box = function(df, x, y, fill, title){
  g = ggplot(data=df, aes(x = x, y = y, fill = fill)) + 
    geom_boxplot(notch=TRUE) + 
    scale_fill_manual(values = easternPal, na.value='royalblue') +
    stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    theme_bryan() + 
    theme(legend.position = '') +
    theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
    ggtitle(title)
  return(g)
}



# used mobile
plot_box(df = data, x=data$Age, y=data$UsedMobile, fill=data$UsedMobile, title = 'Distribution of Mobile Use by Age')

# mobile awareness
plot_box(df = oldSurvey, x=oldSurvey$Age, y=oldSurvey$MobileAware, fill=oldSurvey$MobileAware, title = 'Mobile Awareness Ages (Orig Survey)')

# used mobile
plot_box(df = data, x=data$Age, y=data$KnowMobile, fill=data$KnowMobile, title = 'Distribution of Mobile Awareness by Age')

# mobile awareness
plot_box(df = oldSurvey, x=oldSurvey$Age, y=oldSurvey$AwareUsedService, fill=oldSurvey$AwareUsedService, 
         title = 'Aware and Used Service (Original Survey)')


# function for histograms with facet wrap
facet_hist = function(data=data, metric, fill, bins = 100, title, facet, position, lab) {
  df <- data.frame(x = metric, fill = fill, fwrap = facet)
  g = ggplot(df, aes(x = metric, fill = fill)) + 
    geom_histogram(binwidth =bins, color='white') + 
    scale_fill_manual(values = easternPal) +
    facet_wrap(~fwrap)+
    theme_bryan() + 
    theme(legend.position = position) +
    theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
    ggtitle(title)+ labs(fill=lab)
  return(g)
}



# facet hist
facet_hist(data = data, metric=data$DepositAmount, fill=data$KnowMobile, bins=100,
           title='Distribution of Deposit Amt by Mobile Awareness', position='bottom', lab='Know Mobile', facet=data$KnowMobile)



facet_hist(data = oldSurvey, metric=oldSurvey$DepositAmount, fill=oldSurvey$Generation, bins=100,
           title='Deposit Distribution for Original Survey', position='bottom', lab='Generation', facet=oldSurvey$Generation)


facet_hist(data = oldSurvey, metric=oldSurvey$DepositAmount, fill=oldSurvey$Generation, bins=100,
           title='Deposit Distribution for Original Survey', position='bottom', lab='Generation', facet=oldSurvey$Generation)


facet_hist(data = oldList, metric=oldList$DepositAmount, fill=oldList$Generation, bins=100,
           title='Deposit Distribution for Original List', position='bottom', lab='Generation', facet=oldList$Generation)



# make a generic rollup function and plotting function
rollup_fun = function(df, ...){
  df %>% dplyr::select(...) %>% 
    group_by(...) %>% 
    summarise(Count = n())
}

# barchart of counts
rollup_bar = function(df, x, y, fill, title){
  g = ggplot(df, aes(y = y, x=x, fill = fill)) +
    geom_bar(position = 'dodge', stat = 'identity') + 
    
    geom_text(position = position_dodge(width= 1),
              size = 4, color = 'blue',
              aes(x=Count+1, label=Count, hjust=0)) +
    
    
    scale_fill_manual(values = easternPal) +
    theme_bryan() + 
    theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
    ggtitle(title) +
    theme(legend.position = 'bottom') + 
    theme(axis.title.x = element_text(color = 'blue'))
  return(g)
  
}

# for percentages
bar_percent = function(df, x, y, xmax=40, title, n, fill='darkblue'){
  g = ggplot(df, aes(y = reorder(y, Percent), x=Percent, fill = fill)) +
    geom_bar(position = 'dodge', stat = 'identity') + 
    
    geom_text(position = position_dodge(width= 1),
              size = 4, color = 'blue',
              aes(x=Percent+1, label=paste0(format(Percent, digits = 0),"%"), hjust=0)) +
    
    
    scale_fill_manual(values = easternPal) + 
    xlim(0,xmax) + 
    theme_bryan() + 
    theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
    ggtitle(paste(title, " (n= ", n, ")")) +
    theme(legend.position = '') + 
    theme(axis.title.x = element_text(color = 'blue'))
  return(g)
}


# for percentages without ordering
bar_percent_no_order = function(df, x, y, xmax=40, title, n, fill='darkblue'){
  g = ggplot(df, aes(y = y, x=Percent, fill = fill)) +
    geom_bar(position = 'dodge', stat = 'identity') + 
    
    geom_text(position = position_dodge(width= 1),
              size = 4, color = 'blue',
              aes(x=Percent+1, label=paste0(format(Percent, digits = 0),"%"), hjust=0)) +
    
    
    scale_fill_manual(values = easternPal) + 
    xlim(0,xmax) + 
    theme_bryan() + 
    theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
    ggtitle(paste(title, " (n= ", n, ")")) +
    theme(legend.position = '') + 
    theme(axis.title.x = element_text(color = 'blue'))
  return(g)
}


# remove the Boomers+ from the data
data = data %>% filter(Generation != 'Boomers+')


# know mobile
knowMobile = rollup_fun(df=data, KnowMobile, Generation)
rollup_bar(df=knowMobile, x=knowMobile$Count, y=knowMobile$KnowMobile, fill=knowMobile$Generation,
           title="Customers Who Know About Mobile App")

# used mobile
usedMobile = rollup_fun(data, UsedMobile, Generation)
rollup_bar(df=usedMobile, x=usedMobile$Count, y=usedMobile$UsedMobile, fill=usedMobile$Generation,
           title = "Customers Who used Mobile App")

# why not
whyNot = rollup_fun(data, NotUseMobile)
whyNot = whyNot %>% filter(!is.na(NotUseMobile))
n_total = sum(whyNot$Count)
whyNot$Percent = whyNot$Count/n_total * 100

bar_percent(df=whyNot, x=whyNot$Percent, y=whyNot$NotUseMobile, n=n_total,
            title='Why Not Use App')





# use app in future
futureUse = rollup_fun(data, UseAppInFuture) %>% 
  filter(!is.na(UseAppInFuture))
use_app_total = sum(futureUse$Count)
futureUse$Percent = futureUse$Count/use_app_total * 100


bar_percent_no_order(df=futureUse, x=futureUse$Percent, y=futureUse$UseAppInFuture, n=use_app_total,
            fill=futureUse$UseAppInFuture ,title='Likely to Use in Future', xmax=30)



# reluctance to use mobile
mobileReluct = rollup_fun(data, AwareMobileReluctance) %>% 
  filter(!is.na(AwareMobileReluctance))
mobile_reluct_total = sum(mobileReluct$Count)
mobileReluct$Percent = mobileReluct$Count/ mobile_reluct_total * 100


bar_percent(df=mobileReluct, x=mobileReluct$Percent, y=mobileReluct$AwareMobileReluctance,
            n=mobile_reluct_total, fill = mobileReluct$AwareMobileReluctance,
            title='Mobile Reluctance', xmax=55)

# smartphone
smartPhone = rollup_fun(data, SmartPhone, Generation) %>% 
  filter(!is.na(SmartPhone))


rollup_bar(smartPhone, x=smartPhone$Count, y=smartPhone$SmartPhone, fill=smartPhone$Generation,
           title = 'Smartphone Counts')



# likelihood given smartphone
useApp = rollup_fun(data, GetSmartPhoneUseApp) %>% 
  filter(!is.na(GetSmartPhoneUseApp))
get_use_total = sum(useApp$Count)
useApp$Percent = useApp$Count/get_use_total * 100

bar_percent_no_order(df=useApp, x=useApp$Percent, y=useApp$GetSmartPhoneUseApp,
            n=get_use_total, fill = useApp$GetSmartPhoneUseApp,
            title='Use App if Get Smartphone', xmax=45)


# Mobile aware likely use
awareUse = rollup_fun(data, NowMobileAwareLikelyUse) %>% 
  filter(!is.na(NowMobileAwareLikelyUse))

aware_use_total = sum(awareUse$Count)
awareUse$Percent = awareUse$Count/aware_use_total * 100


# likelihood to use app now that they know
# use single plot to preserve order
g = ggplot(awareUse, aes(y = NowMobileAwareLikelyUse, x=Percent, fill = NowMobileAwareLikelyUse)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=Percent+1, label=paste0(format(Percent, digits = 0),"%"), hjust=0)) +
  
  
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(legend.position = '') + 
  # theme(axis.title.x = element_text(color = 'blue')) + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle(paste("Likelihood of Using App now Aware of App (n= ", aware_use_total, ")"))
g


#  follow up reluctance reason
reluctUse = rollup_fun(data, ReluctanceReason) %>% 
  filter(!is.na(ReluctanceReason))

reluct_use_total = sum(reluctUse$Count)
reluctUse$Percent = reluctUse$Count/reluct_use_total * 100


# clean reasons
reluctUse$Reason = ifelse(reluctUse$ReluctanceReason == 'I like banking with the local branch staff',
                          "Like Branch Staff",
                          ifelse(reluctUse$ReluctanceReason == "I do not like to use online or mobile services to do my banking",
                                 "Don't Like",
                                 ifelse(reluctUse$ReluctanceReason =="I do not have or plan to get a smart phone",
                                        "No Smartphone",
                                        ifelse(reluctUse$ReluctanceReason =="I am concerned about security making a deposit by phone",
                                               "Security",
                                               reluctUse$ReluctanceReason))))
                                        
                          







# reluctance to use app
g = ggplot(reluctUse, aes(y = reorder(Reason,Percent), x=Percent, fill = Reason)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=Percent+1, label=paste0(format(Percent, digits = 0),"%"), hjust=0)) +
  
  
  scale_fill_manual(values = easternPal) +
  xlim(0,50) + 
  theme_bryan() + 
  theme(legend.position = '') + 
  # theme(axis.title.x = element_text(color = 'blue')) + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle(paste("Reluctance (n= ", reluct_use_total, ")"))
g





## look at some text mining
textDf = data %>% dplyr::select(Generation,
                         PrimaryNotMobileComments,
                         ReluctanceMobileComments,
                         ReluctanceOther)


# to lower
textDf$PrimaryNotMobileComments = tolower(textDf$PrimaryNotMobileComments)
textDf$ReluctanceMobileComments = tolower(textDf$ReluctanceMobileComments)
textDf$ReluctanceOther = tolower(textDf$ReluctanceOther)

# make separate dataframes to combine the two columns into one text field
df1 = textDf %>% select(Generation, ReluctanceMobileComments) 
df2 = textDf %>% select(Generation, ReluctanceOther)%>% 
  rename(ReluctanceMobileComments = ReluctanceOther)

reluctance = rbind(df1,df2)


reluctance = reluctance %>% filter(!is.na(ReluctanceMobileComments))



# get and modify stopwords
stop_words <- stopwords("english")
keep <- c("no", "more", "not", "can't", "cannot", "isn't", "aren't", "wasn't",
          "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't")
newWords <- stop_words [! stop_words %in% keep]


# clean possessives
possessive <- function (x) {gsub("'s", " ", x)}

# clean and process
process_text = function(x){
  text <- VCorpus(VectorSource(x))
  text <- tm_map(text,PlainTextDocument)
  text <- tm_map(text, removeWords, newWords)
  text <- tm_map(text, removePunctuation)
  text <- tm_map(text, removeNumbers)
  return(text)
}


# make the clustering functions
create_distance = function(df){
  as.dist(1 - cov2cor(cov(df[,c(2:words)], 
                          method = "pearson", 
                          use = "pairwise.complete.obs")))
}


# make clustering function
create_cluster = function(df, k, title, max){
  options(scipen = 999)
  par(mar = c(2.5, 0.5, 1.0, 7))
  d <- dist(df, method = "euclidean")
  hc <- hclust(d)
  dend <- d %>% hclust %>% as.dendrogram
  labels_cex(dend) <- 1.25
  dend %>% 
    color_branches(k=k) %>%
    color_labels() %>%
    highlight_branches_lwd(4) %>% 
    plot(horiz=TRUE, main = title, axes = T, xlim = c(max,0))
}

# get the first text
clean_text = textDf %>% dplyr::select(Generation, PrimaryNotMobileComments) %>% 
  filter(!is.na(PrimaryNotMobileComments))


# Primary not mobile
text = process_text(clean_text$PrimaryNotMobileComments)


# get the second text reluctance
text = process_text(reluctance$ReluctanceMobileComments)


# make the DTM
DTM <- DocumentTermMatrix(text)
dtmDF <- as.data.frame(as.matrix(DTM))

# need to be careful of the column name
dtmDF$Generation <- as.factor(as.character(clean_text$Generation))


# clean up generation column
clusDf <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF$Generation), sum)


colnames(clusDf)[1] = 'Generation'


# clustering works off row names
rownames(clusDf) <- clusDf[,1]

# Create a correlation matrix and make it a distance matrix
words <- length(clusDf) 


create_cluster(clusDf, k = 3, "Word Clusters by Generation", max = 50)


#### Tokenizing and Plotting Functions
tokens <- 2

NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), tokens:tokens), paste, collapse = " "), use.names = FALSE)
}



# weighted tokenizer
myTokenizer = function(text){
  DocumentTermMatrix(text, control = list(weighting = function(x)
    weightTfIdf(x, normalize = FALSE), tokenize = myGramTokenizer))
}

freqTokenizer = function(text){
  DocumentTermMatrix(text, control = list(tokenize = NLP_tokenizer))
}

# plotting functions
# get top n
rollData = function(meltedData, topn, metric) {
  meltedData %>% 
    group_by(metric, 'Term') %>% 
    mutate(TFIDF = jitter(TFIDF, amount = 0.5)) %>%
    summarise(Total = sum(TFIDF)) %>%
    top_n(topn)
}


# plot rollup data
plotRollup = function(df, fillField, cols, metric){
  g =  ggplot(df, aes(reorder(Term, Total), Total, fill = fillField)) + 
    geom_col(show.legend = FALSE, fill = "blue", color = "lightblue") + 
    labs(x = NULL, y = "count") + 
    facet_wrap(~df[[metric]], ncol = cols, scales = "free") + theme_bryan() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
  return(g)
}



metric <- 'Generation'

DTM = freqTokenizer(text = text)

# make it a dataframe
dtmDF <- as.data.frame(as.matrix(DTM))

# get the factor
dtmDF[[metric]] <- clean_text[[metric]]

# clean up column
commentsDf <- aggregate(dtmDF[,-length(dtmDF)], by = list(dtmDF[[metric]]), sum)

colnames(commentsDf)[1] = metric


# Melt the data
meltText <- melt(commentsDf, id = metric,
                 variable.name = 'Term',
                 value.name = 'TFIDF')


# roll up and plot the text
# rollup = rollData(meltText, 40, 'Generation')


rollup = meltText %>% group_by(Generation, Term) %>% 
  mutate(TFIDF = jitter(TFIDF, amount = 0.5)) %>%
  summarise(Total = sum(TFIDF)) %>% 
  arrange(desc(Total)) %>% 
  top_n(40)



plotRollup(df = rollup, fillField = rollup[[metric]],
           metric = metric, cols = 3)




####### Perform LDA
# use LDA, set k = 4
# Find the sum of words in each Document ensure no zeros
rowTotals <- apply(DTM , 1, sum) 
dtmClean   <- DTM[rowTotals> 0, ]
reasons_lda <- LDA(dtmClean, k = 2, control = list(seed = 1000))


# check topics
reasons_topics <- tidy(reasons_lda, matrix = "beta")
# reasons_topics

# revise palette
# easternPal = c('lightsteelblue1', 'royalblue4', 'sienna1', 'skyblue', 'orange', 'darkblue')

# Make the plot
reasons_top_terms <- reasons_topics %>%
  group_by(topic) %>%
  mutate(beta = jitter(beta, amount = .0001)) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


reasons_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  scale_fill_manual(values = easternPal) + 
  geom_col(show.legend = FALSE) +
  theme_bryan() + 
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip()


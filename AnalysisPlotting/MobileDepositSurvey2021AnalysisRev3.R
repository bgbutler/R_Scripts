

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

# get the old survey data
# oldList = read_csv('C:/Users/bbutler/MyStuff/TransactionAnalysis/Mobile Deposit in Branch List - March 2019 Clean.csv')


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
  ordered(x, levels = c(NA, 'Very unlikely', 'Somewhat unlikely', 'Not sure', 'Somewhat likely', 'Very likely'))
  
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
  ifelse(x < 25, 'Millennials & Z',
         ifelse(x > 24 & x < 41, 'Millennials & Z',
                ifelse(x > 40 & x < 57, 'Gen X',
                       ifelse(x > 56, 'Boomers+',
                              NA))))
}


order_gen = function(x){
  ordered(x, levels = c(NA,
                        'Millennials & Z',
                        'Gen X',
                        'Boomers+'
  ))
  }



reason_notuse = function(x){
  ifelse(x == "My deposit included cash/money order that an app could not accommodate", "Branch More Convenient",
         ifelse(x == "I  was near branch/had other banking business to do", "Branch More Convenient",
                ifelse(x == "I had multiple checks to deposit so more convenient to do it at branch", "Branch More Convenient",
                       ifelse(x == "I forgot to use it", "Branch More Convenient",
                              ifelse(x == "Check amount exceeded deposit limit", "Exceeded Limit",
                                     ifelse(x == "I believed my funds would be available sooner by depositing the check at a branch", "Funds available sooner",
                                            ifelse(x== "I do not trust Mobile Deposit", "Don't like",
                                                   ifelse(x == "I did not have a good experience using app/did not work for me", "Don't like",
                                                          ifelse(x == "Other", "Branch More Convenient", NA
                                                          )))))))))
}





# make a function for the first survey
old_reason_notuse = function(x){
  ifelse(x == "I  had other business to do at the branch", "Branch More Convenient",
         ifelse(x == "I  was near branch/had other banking business to do", "Branch More Convenient",
                ifelse(x == "I forgot to use it", "Branch More Convenient",
                       ifelse(x == "I believed my funds would be available sooner by depositing the check at a branch", "Funds available sooner",
                              ifelse(x== "I do not trust Mobile Deposit", "Don't like",
                                     ifelse(x == "I did not have a good experience using Mobile Check Deposit previously", "Don't like",
                                            ifelse(x == "Other", "Branch More Convenient", NA
                                                   )))))))
}




# apply the function
data$ReasonNotUse = reason_notuse(data$PrimaryNotUseMobile)
oldSurvey$ReasonNotUse = old_reason_notuse(oldSurvey$PrimaryNotUseMobile)



# make the generation factor for the original set
data$Generation = apply_gen(data$Age)
data$Generation = order_gen(data$Generation)


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


theme_big_white <- function () { 
  theme(axis.text.x = element_text(size = 10, color = 'blue', angle = 0, face ='bold'),
        legend.position = 'bottom',
        axis.text.y = element_text(size = 14, color = 'blue', angle = 0, face ='bold'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill='white'),
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
# plot_box(df = data, x=data$Age, y=data$UsedMobile, fill=data$UsedMobile, title = 'Distribution of Mobile Use by Age')

# mobile awareness
# plot_box(df = oldSurvey, x=oldSurvey$Age, y=oldSurvey$MobileAware, fill=oldSurvey$MobileAware, title = 'Mobile Awareness Ages (Orig Survey)')

# used mobile
# plot_box(df = data, x=data$Age, y=data$KnowMobile, fill=data$KnowMobile, title = 'Distribution of Mobile Awareness by Age')

# mobile awareness
# plot_box(df = oldSurvey, x=oldSurvey$Age, y=oldSurvey$AwareUsedService, fill=oldSurvey$AwareUsedService, 
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
bar_percent = function(df, x, y, xmax=40, title, n, fill='darkblue', lab="", theme='theme_big_white'){
  g = ggplot(df, aes(y = reorder(y, Percent), x=Percent, fill = fill)) +
    geom_bar(position = 'dodge', stat = 'identity') + 
    
    geom_text(position = position_dodge(width= 1),
              size = 4, color = 'blue',
              aes(x=Percent+1, label=paste0(format(Percent, digits = 0),"%"), hjust=0)) +
    
    
    scale_fill_manual(values = easternPal) + 
    xlim(0,xmax) + 
    get(theme)() + 
    theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
    ggtitle(paste(title, " (n= ", n, ")")) +
    theme(legend.position = '') + 
    labs(fill=lab)
    theme(axis.title.x = element_text(color = 'blue'))
  return(g)
}


# for percentages stacked
bar_percent_stacked = function(df, x, y, xmax=100, title, n, fill='darkblue', theme='theme_big_white', lab=""){
  g = ggplot(df, aes(y = y, x=x, fill = fill)) +
    geom_bar(stat = 'identity') + 
    
    geom_text(position = position_stack(0.5),
              size = 5, color = 'mediumvioletred',
              aes(label=paste0(format(Percent, digits = 0),"%"))) +
    
    
    scale_fill_manual(values = easternPal) + 
    xlim(0,xmax) + 
    get(theme)() + 
    labs(fill=lab) + 
    theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
    ggtitle(paste(title, " (n= ", n, ")")) +
    theme(legend.position = 'bottom')
  return(g)
}



# for percentages stacked
bar_percent_stacked_reorder = function(df, x, y, xmax=100, title, n, fill='darkblue', theme='theme_big_white', lab="", order='Count'){
  g = ggplot(df, aes(y = reorder(y, order), x=x, fill = fill)) +
    geom_bar(stat = 'identity') + 
    
    geom_text(position = position_stack(0.5),
              size = 5, color = 'mediumvioletred',
              aes(label=paste0(format(Percent, digits = 0),"%"))) +
    
    
    scale_fill_manual(values = easternPal) + 
    xlim(0,xmax) + 
    get(theme)() + 
    labs(fill=lab) + 
    theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
    ggtitle(paste(title, " (n= ", n, ")")) +
    theme(legend.position = 'bottom')
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
    theme_big_white() + 
    theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
    ggtitle(paste(title, " (n= ", n, ")")) +
    theme(legend.position = '') + 
    theme(axis.title.x = element_text(color = 'blue'))
  return(g)
}


# remove the Boomers+ from the data
# data = data %>% filter(Generation != 'Boomers+')


#################################    POST PANDEMIC

# know mobile
knowMobile = rollup_fun(df=data, KnowMobile, Generation)
# rollup_bar(df=knowMobile, x=knowMobile$Count, y=knowMobile$KnowMobile, fill=knowMobile$Generation,
           title="Customers Who Know About Mobile App")
# know mobile percentages
n_total = sum(knowMobile$Count)
knowMobile$Percent = knowMobile$Count/n_total * 100
bar_percent_stacked(df=knowMobile, x = knowMobile$Count, y=knowMobile$KnowMobile, xmax=1500,
                    title="Customers Who Know About Mobile App", n=n_total, fill=knowMobile$Generation)



# used mobile
usedMobile = rollup_fun(data, UsedMobile, Generation)
usedMobile = usedMobile %>% filter(!is.na(UsedMobile))
n_total = sum(usedMobile$Count)
usedMobile$Percent = usedMobile$Count/n_total * 100
bar_percent_stacked(df=usedMobile, x = usedMobile$Count, y=usedMobile$UsedMobile, xmax=850,
                    title="Mobile Usage by Customers who are Aware", n=n_total, fill=usedMobile$Generation)





# used mobile
usedMobile = rollup_fun(data, UsedMobile, Generation)
rollup_bar(df=usedMobile, x=usedMobile$Count, y=usedMobile$UsedMobile, fill=usedMobile$Generation,
           title = "Customers Who used Mobile App")

# why not
whyNot = rollup_fun(data, ReasonNotUse, Generation)
whyNot = whyNot %>% filter(!is.na(ReasonNotUse))
n_total = sum(whyNot$Count)
whyNot$Percent = whyNot$Count/n_total * 100

bar_percent_stacked_reorder(df=whyNot, x=whyNot$Count, y=whyNot$ReasonNotUse, n=n_total, title='Why Not Use App This Time', fill = whyNot$Generation, xmax = 425)


#######   END POST PANDEMIC

###### START PRE PANDEMIC

# change the column names for reuse
newNames = c('Age', 'DepositAmount', 'KnowMobile', 'UsedMobile', 'PrimaryNotuse', 'ReasonNotUse', 'Generation')

colnames(oldSurvey) = newNames

# know mobile
knowMobile = rollup_fun(df=oldSurvey, KnowMobile, Generation)
rollup_bar(df=knowMobile, x=knowMobile$Count, y=knowMobile$KnowMobile, fill=knowMobile$Generation,
           title="Customers Who Know About Mobile App")
# know mobile percentages
n_total = sum(knowMobile$Count)
knowMobile$Percent = knowMobile$Count/n_total * 100
bar_percent_stacked(df=knowMobile, x = knowMobile$Count, y=knowMobile$KnowMobile, xmax=1100,
                    title="Customers Who Know About Mobile App", n=n_total, fill=knowMobile$Generation)



# used mobile
usedMobile = rollup_fun(oldSurvey, UsedMobile, Generation)
usedMobile = usedMobile %>% filter(!is.na(UsedMobile))
n_total = sum(usedMobile$Count)
usedMobile$Percent = usedMobile$Count/n_total * 100
bar_percent_stacked(df=usedMobile, x = usedMobile$Count, y=usedMobile$UsedMobile, xmax=750,
                    title="Customers Who used Mobile App", n=n_total, fill=usedMobile$Generation)



# why not
whyNot = rollup_fun(oldSurvey, ReasonNotUse, Generation)
whyNot = whyNot %>% filter(!is.na(ReasonNotUse))
n_total = sum(whyNot$Count)
whyNot$Percent = whyNot$Count/n_total * 100

bar_percent_stacked_reorder(df=whyNot, x=whyNot$Count, y=whyNot$ReasonNotUse, n=n_total, title='Why Not Use App This Time',
                            fill = whyNot$Generation, xmax = 275)




#########################   END PRE PANDEMIC


# use app in future
# futureUse = rollup_fun(data, UseAppInFuture, Generation) %>% 
futureUse = rollup_fun(data, UseAppInFuture) %>%
  filter(!is.na(UseAppInFuture))
use_app_total = sum(futureUse$Count)
print(use_app_total)
futureUse$Percent = futureUse$Count/use_app_total * 100

# stacked bar by generation
bar_percent_stacked(df=futureUse, x = futureUse$Count, y=futureUse$UseAppInFuture, xmax=250,
                    title="Likelihood to Use App in Future", n=use_app_total, fill=futureUse$Generation)

# regular bar without generation
bar_percent_no_order(df=futureUse, x=futureUse$Percent, y=futureUse$UseAppInFuture, xmax=40, n=use_app_total,
                    fill=futureUse$UseAppInFuture, title='Likelihood to Use App in Future')


# shorten reasons
data$AwareMobileReluctance = ifelse(data$MobileReluctance == 'I am concerned about security making a deposit by phone',
                                    "Don't Like/Security/Not Comfortable",
                                    ifelse(data$MobileReluctance =='I am not all that comfortable using technology/apps',
                                           "Don't Like/Security/Not Comfortable",
                                           ifelse(data$MobileReluctance =='I do not like to use online or mobile services to do my banking',
                                                  "Don't Like/Security/Not Comfortable",
                                                  ifelse(data$MobileReluctance =='I like banking in person with the local branch staff',
                                                         'Like Branch Staff',
                                                         data$MobileReluctance))))


# reluctance to use mobile
mobileReluct = rollup_fun(data, AwareMobileReluctance, Generation) %>% 
  filter(!is.na(AwareMobileReluctance))
mobile_reluct_total = sum(mobileReluct$Count)
# make a total column
reluct = mobileReluct  %>% group_by(AwareMobileReluctance) %>% 
  summarize(Totals = sum(Count))

# join on to mobileReluct
mobileReluct = left_join(mobileReluct, reluct, by = 'AwareMobileReluctance')

# make rest
mobileReluct$Percent = mobileReluct$Count/ mobile_reluct_total * 100


bar_percent_stacked_reorder(df=mobileReluct, x=mobileReluct$Count, y=mobileReluct$AwareMobileReluctance,
            n=mobile_reluct_total, fill = mobileReluct$Generation,
            title='Mobile Reluctance', xmax=275, order=mobileReluct$Totals)


# now aware how likely to use
# use app in future
nowAwareUse = rollup_fun(data, NowMobileAwareLikelyUse, Generation) %>% 
  filter(!is.na(NowMobileAwareLikelyUse))
use_now_total = sum(nowAwareUse$Count)
print(use_now_total)
nowAwareUse$Percent = nowAwareUse$Count/use_now_total * 100


bar_percent_stacked(df=nowAwareUse, x = nowAwareUse$Count, y=nowAwareUse$NowMobileAwareLikelyUse, xmax=250,
                    title="Likelihood to Use App Now Aware", n=use_now_total, fill=nowAwareUse$Generation)






#########   FIND THE NEVERS
notAwareNo = rollup_fun(data, ReluctanceReason, NowMobileAwareLikelyUse, Generation) %>% 
  filter(!is.na(ReluctanceReason),
                NowMobileAwareLikelyUse %in% c('Very unlikely', 'Somewhat unlikely', 'Not sure'))
# drop the likely use column
notAwareNo = notAwareNo %>% group_by(ReluctanceReason, Generation) %>% 
  summarise(Count = sum(Count))


# why not
awareButNo = rollup_fun(data, UseAppInFuture, MobileReluctance, Generation)
awareButNo = awareButNo %>% filter(!is.na(MobileReluctance),
                           UseAppInFuture %in% c('Very unlikely', 'Somewhat unlikely', 'Unsure'))

awareButNo = awareButNo %>%  group_by(MobileReluctance, Generation) %>% 
  summarise(Count = sum(Count))

# match column names
namesMatch = colnames(awareButNo)
colnames(notAwareNo) = namesMatch

# stack them
reasonsNever = rbind(awareButNo, notAwareNo)


# shorten reasons
reasonsNever$MobileReluctance = ifelse(reasonsNever$MobileReluctance == 'I am concerned about security making a deposit by phone',
                                    "Don't Like/Don't Trust/Not Comfortable",
                                    ifelse(reasonsNever$MobileReluctance =='I am not all that comfortable using technology/apps',
                                           "Don't Like/Don't Trust/Not Comfortable",
                                           ifelse(reasonsNever$MobileReluctance =='I do not like to use online or mobile services to do my banking',
                                                  "Don't Like/Don't Trust/Not Comfortable",
                                                  ifelse(reasonsNever$MobileReluctance =='I like banking in person with the local branch staff',
                                                         'Like Branch Staff',
                                                         ifelse(reasonsNever$MobileReluctance == 'I like banking with the local branch staff',
                                                                'Like Branch Staff',
                                                                ifelse(reasonsNever$MobileReluctance == "I do not have or plan to get a smart phone",
                                                                       "Don't Like/Don't Trust/Not Comfortable",
                                                                       'Other'
                                                                ))))))


reasonsNever = reasonsNever %>% group_by(MobileReluctance, Generation) %>% 
  summarise(Count = sum(Count))
n_nevers = sum(reasonsNever$Count)
reasonsNever$Percent = reasonsNever$Count/n_nevers * 100


bar_percent_stacked_reorder(df=reasonsNever, x=reasonsNever$Count, y=reasonsNever$MobileReluctance,
                            n=n_nevers, fill = reasonsNever$Generation,
                            title='Reasons Not Likely to Use Mobile', xmax=425, order=reasonsNever$Percent)


























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
awareUse = rollup_fun(data, NowMobileAwareLikelyUse, Generation) %>% 
  filter(!is.na(NowMobileAwareLikelyUse))

aware_use_total = sum(awareUse$Count)
awareUse$Percent = awareUse$Count/aware_use_total * 100



bar_percent_stacked(df=awareUse, x = awareUse$Count, y=awareUse$NowMobileAwareLikelyUse, xmax=150,
                    title="Newly Aware Customers Likely Mobile Usage", n=aware_use_total, fill=awareUse$Generation)






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







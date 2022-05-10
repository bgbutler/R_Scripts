
## @knitr loadlibs

library(tidyverse)
library(DT)


library(reshape2)
library(RColorBrewer)

library(ggplot2)
library(scales)

library(gridExtra)
library(tidytext)

library(plotly)

library(dplyr)
library(htmltools)

library(markovchain)


library(kableExtra)

library(knitr)

library(tidyr)

library(data.table)

# disable scientific notation
options(scipen=999)

# set wd
setwd('C:/Users/bbutler/Documents/HELOC')


## @knitr loadData
# load file
# get the final data file
heloc <- readr::read_csv('helocMarkovData.csv')

# check month
# unique(heloc$UtilMonth)


## @knitr cleanData
# make different periods
periods2020 <- c('Month1', 'Month2', 'Month3', 'Month6')
periods <- c('Month1', 'Month2', 'Month3', 'Month6', 'Month9', '1Yr')


# make the bins
heloc$OrigBins <- cut(heloc$OrigUtil, c(0,.2, .4, .6, .8, 1), include.lowest = T,
                      labels = c('0-20%', 
                                 '20-40%', 
                                 '40-60%', 
                                 '60-80%',
                                 '80-100%'
                                 ))


heloc$UtilBins <- cut(heloc$Utilization, c(0,.2, .4, .6, .8, 1), include.lowest = T,
                      labels = c('0-20%', 
                                 '20-40%', 
                                 '40-60%', 
                                 '60-80%',
                                 '80-100%'
                                 ))
# head(heloc)

## @knitr sliceData
# slice into campaigns and bau
camp2019 <- heloc %>% filter(Issue_Date > '2019-03-03' & Issue_Date < '2019-08-01' & UtilMonth %in% periods)

camp2020 <- heloc %>% filter(Issue_Date > '2020-03-14' & Issue_Date < '2020-07-30'& UtilMonth %in% periods2020)

bau <- heloc %>% filter(Issue_Date > '2019-07-31' & Issue_Date < '2020-03-01'& UtilMonth %in% periods)


## @knitr plotData
# order the factors for plotting
camp2019$UtilMonth <- ordered(camp2019$UtilMonth, levels = c('Month1',
                                                        'Month2',
                                                        'Month3',
                                                        'Month6',
                                                        'Month9',
                                                        '1Yr'))


bau$UtilMonth <- ordered(bau$UtilMonth, levels = c('Month1',
                                                        'Month2',
                                                        'Month3',
                                                        'Month6',
                                                        'Month9',
                                                        '1Yr'))



# make plots of utilization by Util month

# make some plots
# use this theme
theme_bryan <- function () { 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 0),
        legend.position = 'bottom',
        axis.text.y = element_text(color = 'blue'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = 'darkblue', size = 10, face = 'bold'),
        strip.background = element_rect(fill = 'light blue'))
}

# palette
myPal <- c('royalblue4', 'darkorchid1')

## @knitr origUtil
# original utilization
g <- ggplot(camp2019, aes(x = OrigUtil, fill = Note_Type_Descr)) + 
  geom_histogram(bins=10) +  
  theme_bryan() + theme(axis.text.y = element_text(size = 8, color = 'blue', angle = 0)) + 
  ggtitle("Intial Utilization By Month for 2019 Campaign") + 
  scale_fill_manual(values = myPal) +
  theme(plot.title = element_text(color = 'blue', size = 12, face = 'bold'))
g


g <- ggplot(camp2020, aes(x = OrigUtil, fill = Note_Type_Descr)) + 
  geom_histogram(bins = 10) +  
  theme_bryan() + theme(axis.text.y = element_text(size = 8, color = 'blue', angle = 0)) + 
  ggtitle("Initial Utilization By Month for 2020 Campaign") + 
  scale_fill_manual(values = myPal) +
  theme(plot.title = element_text(color = 'blue', size = 12, face = 'bold'))
g



g <- ggplot(bau, aes(x = OrigUtil, fill = Note_Type_Descr)) + 
  geom_histogram(bins = 10) +  
  theme_bryan() + theme(axis.text.y = element_text(size = 8, color = 'blue', angle = 0)) + 
  ggtitle("Initial Utilization By Month for BAU") + 
  scale_fill_manual(values = myPal) +
  theme(plot.title = element_text(color = 'blue', size = 12, face = 'bold'))
g







## @knitr utilizationPlots
g <- ggplot(camp2019, aes(x = Utilization, fill = Note_Type_Descr)) + 
  geom_histogram(bins = 25) +  
  theme_bryan() + theme(axis.text.y = element_text(size = 8, color = 'blue', angle = 0)) + 
  ggtitle("Utilization By Month for 2019 Campaign") + facet_wrap(~UtilMonth) +
  scale_fill_manual(values = myPal) +
  theme(plot.title = element_text(color = 'blue', size = 12, face = 'bold'))
g



g <- ggplot(camp2020, aes(x = Utilization, fill = Note_Type_Descr)) + 
  geom_histogram(bins = 25) +  
  theme_bryan() + theme(axis.text.y = element_text(size = 8, color = 'blue', angle = 0)) + 
  ggtitle("Utilization By Month for 2020 Campaign") + facet_wrap(~UtilMonth) +
  scale_fill_manual(values = myPal) +
  theme(plot.title = element_text(color = 'blue', size = 12, face = 'bold'))
g



g <- ggplot(bau, aes(x = Utilization, fill = Note_Type_Descr)) + 
  geom_histogram(bins = 25) +  
  theme_bryan() + theme(axis.text.y = element_text(size = 8, color = 'blue', angle = 0)) + 
  ggtitle("Utilization By Month for BAU") + facet_wrap(~UtilMonth) +
  scale_fill_manual(values = myPal) +
  theme(plot.title = element_text(color = 'blue', size = 12, face = 'bold'))
g


## @knitr prepMarkov
# prep data 
prep_data <- function(df){
  reduced <- df %>% select('Loan_Account_Number', 'OrigBins', 'UtilBins', 'UtilMonth')
  
  reduced$OrigBins <- as.character(reduced$OrigBins)
  reduced$UtilBins <- as.character(reduced$UtilBins)
  reduced$UtilMonth <- as.character(reduced$UtilMonth)
  return(reduced)
}

###### CAMPAIGN 2019
## @knitr camp2019MC
reduced <- prep_data(camp2019)

# check for duplicates
# df[!duplicated(df), ]

 reduced$Dupes <- duplicated(reduced)

 dupes <- reduced %>% filter(Dupes == TRUE)

reduced <- reduced[!duplicated(reduced),]

# pivot_wider(tmp, names_from = y, values_from = z)
# markovData2 <- pivot_wider(reduced, names_from = UtilMonth, values_from = UtilBins)
# markovData3 <- pivot_wider(reduced, names_from = UtilMonth, values_from = c(UtilBins, OrigBins))

# reduced <- na.omit(reduced)

markovData <- reshape2::dcast(reduced, Loan_Account_Number + OrigBins ~ UtilMonth, value.var = 'UtilBins', fun.aggregate = max)


# clean up the NA

markovData$Month1 <- ifelse(is.na(markovData$Month1),markovData$OrigBins, markovData$Month1)
markovData$Month2 <- ifelse(is.na(markovData$Month2),markovData$OrigBins, markovData$Month2)
markovData$Month3 <- ifelse(is.na(markovData$Month3),markovData$OrigBins, markovData$Month3)
markovData$Month6 <- ifelse(is.na(markovData$Month6),markovData$OrigBins, markovData$Month6)
markovData$`1Yr` <- ifelse(is.na(markovData$`1Yr`),markovData$OrigBins, markovData$`1Yr`)




# head(markovData)

# for markov chains
options(digits=2)


# 1 month
md = markovData[,c(2,4)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "1 Mo Utilization Transitions")


helocUtil$estimate


# for markdown
# df <- as.data.frame(helocUtil$estimate@transitionMatrix)

# kable(df)

# 2 month
md = markovData[,c(2,5)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "2 Mo Utilization Transitions")


helocUtil$estimate


# 3 month
md = markovData[,c(2,6)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "3 Mo Utilization Transitions")


helocUtil$estimate


# 6 month
md = markovData[,c(2,7)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "6 Mo Utilization Transitions")


helocUtil$estimate



# 1 Yr
md = markovData[,c(2,3)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "1 Yr Utilization Transitions")


helocUtil$estimate


########   CAMPAIGN 2020
## @knitr camp2020MC

reduced <- prep_data(camp2020)

# check for duplicates
# df[!duplicated(df), ]

# reduced$Dupes <- duplicated(reduced)

# dupes <- reduced %>% filter(Dupes == TRUE)

# reduced <- reduced[!duplicated(reduced),]


# reduced <- na.omit(reduced)

markovData <- reshape2::dcast(reduced, Loan_Account_Number + OrigBins ~ UtilMonth, value.var = 'UtilBins', fun.aggregate = max)

# head(markovData)

# clean up the NA

markovData$Month1 <- ifelse(is.na(markovData$Month1),markovData$OrigBins, markovData$Month1)
markovData$Month2 <- ifelse(is.na(markovData$Month2),markovData$OrigBins, markovData$Month2)
markovData$Month3 <- ifelse(is.na(markovData$Month3),markovData$OrigBins, markovData$Month3)
markovData$Month6 <- ifelse(is.na(markovData$Month6),markovData$OrigBins, markovData$Month6)

# for markov chains
options(digits=2)


# 1 month
md = markovData[,c(2,3)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "1 Mo Utilization Transitions")


helocUtil$estimate


# 2 month
md = markovData[,c(2,4)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "2 Mo Utilization Transitions")


helocUtil$estimate


# 3 month
md = markovData[,c(2,5)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "3 Mo Utilization Transitions")


helocUtil$estimate


# 6 month
md = markovData[,c(2,6)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "6 Mo Utilization Transitions")


helocUtil$estimate



####### BAU
## @knitr bauMC


reduced <- prep_data(bau)

# check for duplicates
# df[!duplicated(df), ]

reduced$Dupes <- duplicated(reduced)

dupes <- reduced %>% filter(Dupes == TRUE)

reduced <- reduced[!duplicated(reduced),]


reduced <- na.omit(reduced)

markovData <- reshape2::dcast(reduced, Loan_Account_Number + OrigBins ~ UtilMonth, value.var = 'UtilBins', fun.aggregate = max)

# head(markovData)
markovData$Month1 <- ifelse(is.na(markovData$Month1),markovData$OrigBins, markovData$Month1)
markovData$Month2 <- ifelse(is.na(markovData$Month2),markovData$OrigBins, markovData$Month2)
markovData$Month3 <- ifelse(is.na(markovData$Month3),markovData$OrigBins, markovData$Month3)
markovData$Month6 <- ifelse(is.na(markovData$Month6),markovData$OrigBins, markovData$Month6)
markovData$`1Yr` <- ifelse(is.na(markovData$`1Yr`),markovData$OrigBins, markovData$`1Yr`)

# for markov chains
options(digits=2)


# 1 month
md = markovData[,c(2,4)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "1 Mo Utilization Transitions")


helocUtil$estimate


# for markdown
# df <- as.data.frame(helocUtil$estimate@transitionMatrix)

# kable(df)

# 2 month
md = markovData[,c(2,5)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "2 Mo Utilization Transitions")


helocUtil$estimate


# 3 month
md = markovData[,c(2,6)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "3 Mo Utilization Transitions")


helocUtil$estimate


# 6 month
md = markovData[,c(2,7)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "6 Mo Utilization Transitions")


helocUtil$estimate



# 1 Yr
md = markovData[,c(2,3)]

# make the markov chain
helocUtil <- markovchainFit(data = md, name = "1 Yr Utilization Transitions")


helocUtil$estimate






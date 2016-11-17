

# this is for the kaggle competition 

library(dplyr)
library(data.table)
library(ggplot2)
library(markovchain)


#get the data use fread and data table to control input
url <- "~/Desktop/RDataFiles/CSVs/train_ver2.csv"
data <- read.csv(url, header = TRUE, sep=",", na.strings = 0,as.is = FALSE)

set.seed(1000)

df <- fread("~/Desktop/RDataFiles/CSVs/train_ver2.csv", nrows = -1)

#clean the date field
df$fecha_dato <- as.Date(df$fecha_dato, format = "%Y-%m-%d")

#create a month field
df$Month <- as.character(df$fecha_dato, format = "%b")

#check for missing columns
sapply(df, function(x)any(is.na(x)))

#find the number of unique users
#there are 956645 unique users
length(unique(df$ncodpers))


#check the format of the data
test <- filter(df, ncodpers == 1050611)

#set up some common plotting themes
my_theme <- theme_bw() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))

my_theme_dark <- theme_dark() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))

#plot the age distribution
ggplot(data=df,aes(x=age)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") +
  ggtitle("Age Distribution") + 
  my_theme





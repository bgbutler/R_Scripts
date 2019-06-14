
#new procedure to do text mining
#load in the Excel files
library(readxl)
library(tm)
library(qdap)
library(SnowballC)
library(RTextTools)



#get a list of the files
url <- "K:/Sandbox/Bryan Projects/Activity Reports/"

files <- list.files(url, full.names = TRUE)
count <- length(files)

#loop through all files and combine
Act <- data.frame()

for (i in 1:count){
  Act <- rbind(Act, 
               read_excel(files[i], sheet = 2, col_names = TRUE,
                          col_types = NULL, na = "", skip = 1))
}


#change column names to be cleaner for processing
newNames <- c("Level", "Title", "InvitedGroups", "StartDate", "EndDate", "AuthFirstName",
              "AuthLastName", "AuthRole", "ActivityType", "FirstName", "LastName", 
              "ContributorRole", "Contributions", "TotContributors")

#change the names of the columns
colnames(Act) <- newNames

#set up factor variables
Act$Level <- as.factor(Act$Level)
Act$Title <- as.factor(Act$Title)
Act$InvitedGroups <- as.factor(Act$InvitedGroups)
Act$ActivityType <- as.factor(Act$ActivityType)
Act$AuthRole <- as.factor(Act$AuthRole)
Act$ContributorRole <- as.factor(Act$ContributorRole)

#set up dates and create duration
Act$StartDate <- as.Date(Act$StartDate, format="%m/%d/%Y")
Act$EndDate <- as.Date(Act$EndDate, format="%m/%d/%Y")
Act$Duration <- Act$EndDate - Act$StartDate
Act$Duration <- as.numeric(Act$Duration)


#separate the summary from the data and remove it
actSum <- subset(Act, TotContributors != 'NA')
Act <- Act[Act$Level != 'Activity',]

#clean up and remove Act since it does not contain more information
rm(Act)

#load the text into a vector corpus for analysis
docs <- actSum$Title
vSource <- Corpus(VectorSource(docs))


inspect(vSource)

#view some of the items

vSource[[204]][[1]]


#create transformation and cleaning functions
wordMatrix <- create_matrix(docs, stemWords = TRUE, removeStopwords = FALSE,
                            minWordLength = 2)



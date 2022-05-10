
# heloc files
# this is the first version

library(tidyverse)
library(DT)

library(zipcode)
library(reshape2)

library(dplyr)



# set wd
setwd('C:/Users/bbutler/Documents/HELOC/2020Campaign/V2')


# load in the necessary files
list.files()


# get the files
apps <- readr::read_csv('Eastern_ConsumerLoanApps.csv')

property <- readr::read_csv('Eastern_ProcessingChecklistFlexEqV6.csv')

data <- readr::read_csv('Eastern_QcRBPReviewCSV.csv')

branches <- readr::read_csv('BranchLocs.csv')


# get the date component
property$AppDate <-  gsub( " .*$", "", property$AppCreationDate)
property$AppDate


# make AppCreationDate a date
property$AppCreationDateClean <- as.Date(property$AppDate, format = '%m/%d/%Y')
property$AppCreationDateClean

# make a function to get last 5 chars for zip
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# get the zip code from the address
property$propertyZip <- substrRight(property$PropertyAdd, 5)
  

# write to csv
write.csv(property, file = "C:/Users/bbutler/OneDrive - Eastern Bank/HELOC/V2/helocProperties.csv",  row.names = FALSE)

write.csv(apps, file = "C:/Users/bbutler/OneDrive - Eastern Bank/HELOC/V2/helocApps.csv",  row.names = FALSE)

write.csv(data, file = "C:/Users/bbutler/OneDrive - Eastern Bank/HELOC/V2/PLData.csv",  row.names = FALSE)

write.csv(branches, file = "C:/Users/bbutler/OneDrive - Eastern Bank/HELOC/V2/BranchData.csv",  row.names = FALSE)


# make EntryCompleteDate a date
apps$AppDate <- as.Date(apps$AppEntryCompletedDate, format = '%m/%d/%Y')

# get the campaign data within the date range
campaign <-  apps %>% filter(AppDate > '2020-03-14' & AppDate < '2020-07-30')

# filter for heloc
heloc <-  campaign %>% filter(product == 'Flex Equity Line of Credit') %>% 
  select(AppDate,
         CustomerNumber,
         appid,
         product,
         testfield,
         Score,
         contractAmt,
         InitialDecision,
         LastDecision,
         BookedYN,
         OriginationChannel,
         OriginationChannelCode,
         DistributionChannelCode,
         DistributionChannel,
         ApplicationDaystoDecision,
         LoanOfficer,
         Underwriter
  )


head(heloc)

# check na
sum(is.na(heloc$AppDate))

# join property information
appAddress <- property %>% select(
  AppID,
  ProductID,
  APR,
  LoanToValue,
  PropertyAdd,
  InitialEstimatedAmt,
  FinalEstimatedAmt,
  MortBal,
  DTI,
  PropertyType,
  propertyZip,
  AnalystFirstName,
  AnalystLastName
)


head(appAddress)

# make the analyst name
appAddress$Analyst <- paste(appAddress$AnalystFirstName,appAddress$AnalystLastName, sep = ' ')

# drop analyst first and last name
drops <- c('AnalystFirstName', 'AnalystLastName')
address <- appAddress[ , !(names(appAddress) %in% drops)]

# get the duplicates
dupes <- address[duplicated(address$AppID),]

# sort by appid
dupes <- dupes[order(dupes$AppID)]

# remove duplicates
# # Remove duplicates based on Sepal.Width columns
# my_data[!duplicated(my_data$Sepal.Width), ]
address <- address[!duplicated(address$AppID), ]

# join files
helocAddress <-  left_join(heloc, address, by = c("appid" = "AppID") )

# sort by appID
helocAddress <- helocAddress[order(helocAddress$appid),]


# join branch information
helocFile <-left_join(helocAddress, branches, by = c("OriginationChannelCode" = "Branch"))



# write the csv
write.csv(helocFile, file = "Z:/Client Analytics and Product Discovery/ANALYTICS_PROJECTS/ZDriveHELOC/helocCampaignV2.csv",  row.names = FALSE)







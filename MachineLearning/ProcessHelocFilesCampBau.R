
# heloc files
# this is the latest version

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
# get apps
apps <- readr::read_csv('Eastern_ConsumerLoanApps.csv')

# get property data
property <- readr::read_csv('Eastern_ProcessingChecklistFlexEqV6.csv')

# misc data
data <- readr::read_csv('Eastern_QcRBPReviewCSV.csv')

# branch data
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


# get the length of time in the range
date_strings <- c('2020-03-14' , '2020-07-30')
dates <-  as.Date(date_strings, format = "%Y-%m-%d")

# get the difference
diff_in_days = difftime(dates[2], dates[1], units = "days")
paste('The campaign has a ', diff_in_days, ' day duration.', sep = '')

# get the date that is 138 days prior to 2020-03-14
bau <- dates[1] - diff_in_days
bau


# get the campaign data within the date range
campaign <-  apps %>% filter(AppDate > '2020-03-14' & AppDate < '2020-07-30')

# get bau
diff_in_days = difftime('2020-02-15', '2019-10-01', units = "days")
paste('The BAU has a ', diff_in_days, ' day duration.', sep = '')

bau <-  apps %>% filter(AppDate > '2019-10-01' & AppDate < '2020-02-15')




# get a slice with BAU data
#campaignBAU <-  apps %>% filter(AppDate > '2019-10-15' & AppDate < '2020-07-30')


# make it a column
campaign$Campaign <- c('Campaign')

# make it a column
bau$Campaign <- c('BAU')


# restack them
campaignBAU <- rbind(bau, campaign)




# filter for heloc
heloc <-  campaignBAU %>% filter(product == 'Flex Equity Line of Credit') %>% 
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
         Underwriter,
         Campaign
  )


head(heloc)

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
head(dupes)

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
write.csv(helocFile, file = "Z:/Client Analytics and Product Discovery/ANALYTICS_PROJECTS/ZDriveHELOC/helocCampaignBAUV2.csv",  row.names = FALSE)


# write locally too
write.csv(helocFile, file ='helocCampaignBAU.csv', row.names = FALSE)



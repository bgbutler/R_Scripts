# mapping agency performance

# make sure that the directories are set
getwd()
setwd('N:/Bryan/Mapping')

library(RODBC)
library(dplyr)
library(DT)
library(leaflet)
library(zipcode)
library(reshape2)
library(RColorBrewer)

# import the CSVs
auto <- read.csv('AutoProduction.csv', stringsAsFactors = F, header = T, sep = ',')
home <- read.csv('HomeProduction.csv', stringsAsFactors = F, header = T, sep = ',')

auto$Product <- rep('Auto', length(auto$Date))
home$Product <- rep('Home', length(home$Date))

colnames(auto)[5] <- 'Count'
colnames(home)[5] <- 'Count'

combined <- rbind(auto, home)

rm(auto); rm(home)

# create a month factor for aggregating
# clean up the data formats
combined$Date <- as.Date(combined$Date)

combined$ProducerZip <- as.character(combined$ProducerZip)

combined$Month <- format(combined$Date, format = "%b")
combined$Month <- ordered(combined$Month, levels = c("Jan",
                                                     "Feb",
                                                     "Mar",
                                                     "Apr",
                                                     "May",
                                                     "Jun",
                                                     "Jul",
                                                     "Aug"))

# get the master agent names
conStr <- 'driver={SQL Server};server=HOMSQLPR09;database=COMP_RATER;trusted_connection=true'
channel <- odbcDriverConnect(conStr)
masterAgent <- sqlQuery(channel,"SELECT distinct [MSTR_AGNT_CD], 
                        [MSTR_AGNT_FULL_NM] 
                        FROM [COMP_RATER].[dbo].[ref_PRDCR_TSM]")
odbcClose(channel)


# clean the column names
colnames(masterAgent) <- c('AgentCode', 'AgentName')


# join the names
masterAgent$AgentCode <- as.character(masterAgent$AgentCode)
masterAgent$AgentName <- as.character(masterAgent$AgentName)
dataAgent <- left_join(combined, masterAgent, by = c('MasterAgent' = 'AgentCode'))

## merge zipcode data
data(zipcode)
dataAgent$ProducerZip <- clean.zipcodes(dataAgent$ProducerZip)
#  append the data to the zipcode
zipData <- merge(dataAgent, zipcode, by.x = 'ProducerZip', by.y = 'zip')
zipData$state <- as.factor(zipData$state)

# clean up
rm(combined); rm(masterAgent); rm(zipcode); rm(dataAgent)


# write csv
# write tsv
write.table(zipData, file = 'AllZipData.tsv', quote = T, sep = '\t',row.names = F)

# write.csv(zipData, file = 'AllZipData.csv', row.names = F)



Totals <- zipData %>% select(Producer, Product, Month, Count)

# roll it up
TotalsAgg <- aggregate(Totals[,1:3], Totals[,4], FUN = sum)

TotalAgg <- aggregate(. ~ Producer + 
                        Product + 
                        Month,
                      data = Totals,
                      FUN = sum)

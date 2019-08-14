
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







colnames(masterAgent) <- c('AgentCode', 'AgentName')
# 
# 
# colnames(home) <- c('Date', 'Producer', 'MasterAgent', 'ProducerZip', 'Count')
# 
# clean up the data formats
auto$Date <- as.Date(auto$Date)
home$Date <- as.Date(home$Date)
# 
# auto$Producer <- as.character(auto$Producer)
# home$Producer <- as.character(home$Producer)
# 
# auto$MasterAgent <- as.character(auto$MasterAgent)
# home$MasterAgent <- as.character(home$MasterAgent)
# 
# auto$ProducerZip <- as.character(auto$ProducerZip)
# home$ProducerZip <- as.character(home$ProducerZip)

# write to csv
# write.csv(auto, file = 'AutoProduction.csv', row.names = F)
# write.csv(home, file = 'HomeProduction.csv', row.names = F)


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
combined$Month <- format(combined$Date, format = "%b")
combined$Month <- ordered(combined$Month, levels = c("Jan",
                                                     "Feb",
                                                     "Mar",
                                                     "Apr",
                                                     "May",
                                                     "Jun",
                                                     "Jul",
                                                     "Aug"))


combProd <- dcast(combined, Date + Producer + MasterAgent + ProducerZip + Month ~ Product, value.var='Count', fun.aggregate = length)

# make a total column
combProd$Total <- combProd$Auto + combProd$Home

# drop date
combProd$Date <- NULL

combProd <- combProd %>% arrange(Month)

# create factors
combProd$MasterAgent <- as.factor(combProd$MasterAgent)

combProdAgg <- combProd[,2:7]

# roll it up
data <- aggregate(combProdAgg[,4:6], combProdAgg[,1:3], FUN = sum)

# use numbers to get last x months and select by code
# lvls <- levels(data$Month)
# lvl <- test[6:8]
# newDf <- data %>% filter(Month %in% lvl)

# join MasterAgent on the data
data$MasterAgent <- as.character(data$MasterAgent)
masterAgent$AgentCode <- as.character(masterAgent$AgentCode)
masterAgent$AgentName <- as.character(masterAgent$AgentName)
dataAgent <- left_join(data, masterAgent, by = c('MasterAgent' = 'AgentCode'))


# add zip data
## merge zipcode data
data(zipcode)
dataAgent$ProducerZip <- clean.zipcodes(dataAgent$ProducerZip)
#  append the data to the zipcode
zipData <- merge(dataAgent, zipcode, by.x = 'ProducerZip', by.y = 'zip')



zipData$AgentName <- as.factor(zipData$AgentName)
zipData$state <- as.factor(zipData$state)

write.csv(zipData, file = 'AgentZipData.csv', row.names = F)

# clean up
rm(combined); rm(combProd); rm(combProdAgg); rm(data); rm(dataRed); rm(masterAgent);
rm(zipcode); rm(dataAgent)

# get last three months
currentMonth <- as.numeric(format(Sys.Date(), format = "%m"))
lvls <- levels(zipData$Month)
lastThree <- tail(lvls, 3)
lastThreeMonths <- zipData %>% filter(Month %in% lvl)

lastThreeMonths <- na.omit(lastThreeMonths)

# national average
natAve <- mean(lastThreeMonths$Total)

lastThreeMonths$BeatAvg <- lastThreeMonths$Total/natAve


lastThreeMonths$BeatAvgLvl <- cut(lastThreeMonths$BeatAvg, 
                                  c(0,.5,1,2,3,5,100), include.lowest = T,
                                  labels = c('<.5x', '.5-1x', '1-2x', '2-3x', '3-5x','5x+'))


# Add Jittering to the Zips so that they don't stack
l <- nrow(lastThreeMonth)
jitterFactor <- function(l){
  jF <- runif(l, min = -1, max = 1)
  jF <- jF/100
  
  return(jF)
}

# use randomness so that 0's don't overlap
zipData$JitterLat <- jitterFactor()
zipData$JitterLon <- jitterFactor()

# apply the jitter
zipData$Lat <- zipData$latitude + zipData$JitterLat
zipData$Lon <- zipData$longitude + zipData$JitterLon

# Create a Popup
lastThreeMonths$popUp <- paste('<strong>',AgentName, '</strong><br>',
                       'Paid = $',formatC(lastThreeMonths$FinalPaid,0, format = "f", big.mark = ','), '<br>',
                       'Gen = ', lastThreeMonths$Generation, '<br>',
                       'Fault = ', lastThreeMonths$FaultLvl, '<br>',
                       'Attny = ', lastThreeMonths$AttyRepresent, '<br>')








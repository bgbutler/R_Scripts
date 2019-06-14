#script to analyze CMG portfolio data

.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

library(dplyr)
library(plyr)
library(ggplot2)
library(plotly)
library(reshape2)

#get the files  with location
url <- "//STATESFPSOP1/apps/RASHD/Projects/RAnalyses/CMG_Premier_Checking.csv"
cmgChecking <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


#get the month data and make a table
months <- select(cmgChecking, HHID, Month)

months$Jan <- ifelse(months$Month == '31JAN2016', 1, 0)
months$Feb <- ifelse(months$Month == '29FEB2016', 1, 0)
months$Mar <- ifelse(months$Month == '31MAR2016', 1, 0)
months$Apr <- ifelse(months$Month == '30APR2016', 1, 0)
months$May <- ifelse(months$Month == '31MAY2016', 1, 0)
months$Jun <- ifelse(months$Month == '30JUN2016', 1, 0)
months$Jul <- ifelse(months$Month == '31JUL2016', 1, 0)

monthsAll <- select(months, HHID, Jan, Feb, Mar, Apr, May, Jun, Jul)
monthsAll$HHID <- as.factor(as.character(monthsAll$HHID))

#get the max value for each column by HHID
monthsAllSum <- aggregate(. ~ HHID, data = monthsAll, FUN = max)

#sum across to see how mnay months each HHID is in the portfolio
monthsAllSum$MonthsIn <- apply(monthsAllSum[,-1], 1, sum)

# Write the data to clean .CSV file
write.csv(monthsAllSum, file="//STATESFPSOP1/apps/RASHD/Projects/RAnalyses/HHIDbyMonth.csv", row.names=FALSE)

#get only the HHIDs that last all 7 months
HHIDAllMonths <- filter(monthsAllSum, MonthsIn == 7)


# Write the data to clean .CSV file
write.csv(HHIDAllMonths, file="//STATESFPSOP1/apps/RASHD/Projects/RAnalyses/HHIDallMonths.csv", row.names=FALSE)


#get a histogram of the months
g <- ggplot(monthsAllSum, aes(x = MonthsIn)) + geom_histogram(binwidth = 3)
g






#  Convert the data to a monthly time series

library(reshape2)
library(plyr)
library(strngr)

#Get data
ENSOindex <- read.table(file="~/Documents/RFiles/enso1950.clean.csv", header=TRUE, sep=",")
head(ENSOindex)

meltENSO <- melt(ENSOindex, id=c("Year"))
head(meltENSO)
  
#rename columns after melting
  
colnames(meltENSO) <- c("year", "month", "enso")
head(meltENSO)

#re-order the data by year
meltENSO <- meltENSO[order(meltENSO$year),]
head(meltENSO)

#plot as time series
colors <- ifelse(meltENSO$enso >=0, "red", "blue")

with(meltENSO, {
  plot(year, enso, type = 'h', col = colors, lwd=1)
  })

#convert to uniform months and dates
ss <- meltENSO$month
getmonth <- substr(ss,4,6)
meltENSO$month <- getmonth
fulldate = cbind(meltENSO, paste(meltENSO$month,"01",meltENSO$year))
colnames(fulldate) <- c("year", "month", "enso", "date")
fulldate$date <- as.character(fulldate$date)
fulldate$date <- as.Date(fulldate$date, "%B%d%Y")
cleanENSO <- data.frame(fulldate$date, fulldate$enso)
colnames(cleanENSO) <- c("date", "enso")


#make zoo for ts work

zooENSO <- zoo(fulldate$enso, fulldate$date)

adf.test(meltENSO$enso, alternative = "stationary")

# create moving average of series and plot 
movenso <- rollmean(zooENSO, 10, align = "right")
zooEnsoMov <- merge(zooENSO, movenso)


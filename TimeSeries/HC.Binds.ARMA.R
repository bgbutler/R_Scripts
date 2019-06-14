#activate appropriate libraries
library(zoo)
library(xts)
library(vars)
library(aod)
library(forecast)
library(tseries)
library(plyr)

#get the data file
dataURL <- "~/Desktop/RDataFiles/CSVs/JulDecTSAllDays.csv"

#dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/CSVs/JulDecTSAllDays.csv"
tsData <- read.csv(dataURL, header = TRUE, as.is = FALSE, sep = "," )

#turn the month into a factor
tsData$Month <- as.factor(tsData$Month)
tsData$Date <- as.Date(tsData$Date, format = "%m/%d/%Y")

#create clean full data set
tsAll <- tsData[tsData$Month !='7',]
tsAll <- tsAll[tsAll$Month != '8',]
tsAll <- tsAll[tsAll$Month != '6',]

#remove Saturday and Sunday from all data to reduce the noise
tsxSS <- tsAll[tsAll$Day != 'Sat',]
tsxSS <- tsxSS[tsxSS$Day != 'Sun',]

#remove the holidays
tsxSS <- tsxSS[tsxSS$Binds > 0,]

#create clean training and testing data sets
#create training data exlcude Jan
tsTrain <- tsxSS[tsxSS$Train != 'Test',]
tsTest <- tsxSS[tsxSS$Train == 'Test',]

#get key data series and create zoo objects from the reduced series
Binds <-zoo(tsTrain$Binds, tsTrain$Date)
Leads <-zoo(tsTrain$Leads, tsTrain$Date)
Day <-zoo(tsTrain$Day, tsTrain$Date)
HC <-zoo(tsTrain$DialerHC, tsTrain$Date)

#merge the series to create usable time series objects
#merge zoo objects
zooBindsHC <- merge(Binds,HC)
zooBinds <- merge(Binds, Leads)

#check autocorrelations and partials
par(mfrow = c(1,2))
acf(tsTrain$Binds)
pacf(tsTrain$Binds)

#build some ARIMA models around Binds
ts1 <- arima(tsTrain$Binds, order = c(2,0,5), xreg = tsTrain$DialerHC)
tsdisplay(tsTrain$Binds)
summary(ts1)
tsdiag(ts1)

#take the difference of the series at lag 5
diffts <- diff(tsTrain$Binds,5)
tsdisplay(diffts)
ts2 <- arima(diffts, order = c(5,0,2))
summary(ts2)
tsdiag(ts2)




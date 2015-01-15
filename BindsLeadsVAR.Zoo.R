#activate appropriate libraries
library(zoo)
library(xts)
library(vars)
library(aod)
library(forecast)
library(tseries)


#get the data file
dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/CSVs/JulyDecTS.csv"
tsData <- read.csv(dataURL, header = TRUE, as.is = FALSE, sep = "," )

#turn the month into a factor
tsData$Month <- as.factor(tsData$Month)
tsData$Date <- as.Date(tsData$Date, format = "%m/%d/%Y")

#create new dataset and exclude Saturday to reduce the noise
tsDataxSat <- tsData[tsData$Day != 'Sat',]

#remove July data to reduce noise
tsDataCut <- tsDataxSat[tsDataxSat$Month != '7',]
tsDataCut <- tsDataCut[tsDataCut$Month != '8',]

#check autocorrelations
acf(tsDataxSat$Binds)
pacf(tsDataxSat$Binds)



#create zoo objects from the reduced series
Binds <-zoo(tsDataCut$Binds, tsDataCut$Date)
Leads <-zoo(tsDataCut$OutBoundLeads, tsDataCut$Date)
Day <-zoo(tsDataCut$Day, tsDataCut$Date)
HC <-zoo(tsDataCut$DialerHC, tsDataCut$Date)


#create zoo objects using the full data set
Binds <-zoo(tsDataxSat$Binds, tsDataxSat$Date)
Leads <-zoo(tsDataxSat$OutBoundLeads, tsDataxSat$Date)
Day <-zoo(tsDataxSat$Day, tsDataxSat$Date)
HC <-zoo(tsDataxSat$DialerHC, tsDataxSat$Date)


#merge zoo objects
zooTS <- merge(Binds,Leads,HC)
zooTSBinds <- merge(Binds, Leads)
ts <- as.data.frame(zooTSBinds)


#create time series objects



plot(zooTSBinds, screens = c(1,2), main = "Time Series of Leads and Binds")
ccf(tsDataxSat$Leads,tsDataxSat$Binds,main= "Leads vs Binds")


#test for unit roots k is lags to test
adf.test(ts$Binds, alternative = "stationary", k= 2)

#need a ts dataset for VAR
VARselect(zooTSBinds, lag.max = 5, type = "const")$selection

#to see combinations of VAR use the following
VARselect(zooTSBinds, lag.max = 5, type = "const")

var1 <- VAR(zooTSBinds, p = 3, type = "const")
serial.test(var1, lags.pt = 10, type ="PT.asymptotic")

#serial test fails to reject null of no serial correlation
var2 <- VAR(zooTSBinds, p = 5, type ="none", exog = HC)
serial.test(var2, lags.pt = 10, type ="PT.asymptotic")

summary(var2)

tsdisplay(residuals(var2))
fcastV2 <- forecast(var2, find.frequency = TRUE)

pred2 <- predict(var2, n.ahead = 10)
plot(pred2,xlab="Date", xlim = c(100,140))





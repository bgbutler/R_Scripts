#activate appropriate libraries
library(zoo)
library(xts)
library(vars)
library(aod)
library(forecast)
library(tseries)

#get the data file
dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/CSVs/JulDecTSAllDays.csv"
tsData <- read.csv(dataURL, header = TRUE, as.is = FALSE, sep = "," )

#turn the month into a factor
tsData$Month <- as.factor(tsData$Month)
tsData$Date <- as.Date(tsData$Date, format = "%m/%d/%Y")

#create training data exlcude Jan
tsTrain <- tsData[tsData$Train != 'Test',]
tsTest <- tsData[tsData$Train == 'Test',]

#check autocorrelations
acf(tsTrain$Binds)
pacf(tsTrain$Binds)

#create zoo objects using the full data set
Binds <-zoo(tsTrain$Binds, tsTrain$Date)
Leads <-zoo(tsTrain$Leads, tsTrain$Date)
Day <-zoo(tsTrain$Day, tsTrain$Date)
HC <-zoo(tsTrain$DialerHC, tsTrain$Date)
zooTest <- zoo(tsData$Binds, tsData$Date)

#merge zoo objects
zooBLH <- merge(Binds,Leads,HC)
zooBL <- merge(Binds, Leads)


#plot the series
plot(zooBL, screens = c(1,2), main = "Time Series of Leads and Binds")
ccf(tsTrain$Leads,tsTrain$Binds,main= "Leads vs Binds")

#serial test fails to reject null of no serial correlation
VARselect(zooBL, lag.max = 5, type = "none")$selection

var1 <- VAR(zooBL, p = 5, type ="none", season = 7)
var2 <- VAR(zooBLH, p = 5, type ="none", exog = HC)
serial.test(var1, lags.pt = 10, type ="PT.asymptotic")

summary(var1)

tsdisplay(residuals(var1))
fcastV1 <- forecast(var1, find.frequency = TRUE)

pred1 <- predict(var1, n.ahead = 11)
plot(pred2,xlab="Date", xlim = c(100,140))

plot(pred1, xlab = "Date")
lines(tsData$Date, tsData$Binds, lty = "dashed)

actx <- tsData$Date
acty <- tsData$Binds
plot(actx, acty, type = "l", col = "Red")
lines(pred1, col = "Blue")

p <- pred1$Binds[,1]

f1 <- forecast(var1)

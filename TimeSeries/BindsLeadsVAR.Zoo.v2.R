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

dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/CSVs/JulDecTSAllDays.csv"
tsData <- read.csv(dataURL, header = TRUE, as.is = FALSE, sep = "," )

#turn the month into a factor
tsData$Month <- as.factor(tsData$Month)
tsData$Date <- as.Date(tsData$Date, format = "%m/%d/%Y")

#create clean full data set
tsAll <- tsData[tsData$Month !='7',]
tsAll <- tsAll[tsAll$Month != '8',]
tsAll <- tsAll[tsAll$Month != '6',]

#create training data exlcude Jan
tsTrain <- tsData[tsData$Train != 'Test',]
tsTest <- tsData[tsData$Train == 'Test',]

#create new dataset and exclude Saturday and Sunday to reduce the noise
tsxSS <- tsTrain[tsTrain$Day != 'Sat',]
tsxSS <- tsxSS[tsxSS$Day != 'Sun',]


testxSS <- tsTest[tsTest$Day != 'Sat',]
testxSS <- testxSS[testxSS$Day != 'Sun',]


#remove holidays which are rows with zero
tsxHol <- tsxSS[tsxSS$Binds > 0,]


#remove July, Aug, June data to reduce noise
tsCut <- tsTrain[tsTrain$Month != '7',]
tsCut <- tsCut[tsCut$Month != '8',]
tsCut <- tsCut[tsCut$Month != '6',]

#check autocorrelations
acf(tsCut$Binds)
pacf(tsCut$Binds)

#create zoo objects from the reduced series
Binds <-zoo(tsxHol$Binds, tsxHol$Date)
Leads <-zoo(tsxHol$Leads, tsxHol$Date)
Day <-zoo(tsxHol$Day, tsxHol$Date)
HC <-zoo(tsxHol$DialerHC, tsxHol$Date)


#create zoo objects using the full data set
tsBinds <-zoo(tsAll$Binds, order.by = tsAll$Date, frequency = 7)
tsLeads <-zoo(tsAll$Leads, order.by = tsAll$Date, frequency = 7)
tsDay <-zoo(tsAll$Day, order.by = tsAll$Date, frequency = 7)
tsHC <-zoo(tsAll$DialerHC, order.by = tsAll$Date, frequency = 7)

#merge zoo objects
zooBindsHC <- merge(Binds,HC)
zooBinds <- merge(Binds, Leads)
ts <- as.data.frame(zooTSBinds)

#make some plots
#ccf (x,y) when x are predictors of y and lag is positive, x lags y
plot(zooBinds, screens = c(1,2), main = "Time Series of Leads and Binds")
plot(zooBindsHC, screens = c(1,2), main = "Time Series of HC and Binds")
ccf(tsxHol$Binds, tsxHol$Leads,main= "Binds (x) Lags Outbound.Leads (y) for (+)")
ccf(tsxHol$Leads, tsxHol$Binds, main = "Outbound.Leads (x) Lags Binds (y) for (+)")
ccfvalues

#need a ts dataset for VAR
VARselect(zooBinds, lag.max = 10, type = "none")$selection

#create time series models and diagnostics
#serial test fails to reject null of no serial correlation
var1 <- VAR(zooBinds, p = 5, type ="none")
serial.test(var1, lags.pt = 7, type ="PT.asymptotic")

summary(var1)

#review model performance
tsdisplay(residuals(var1))

pred1 <- predict(var1, n.ahead = 7)
plot(pred1,xlab="Date")
plot(var1)



#create second model with headcount - Does not work
VARselect(zooBindsHC, lag.max = 5, type = "none", exogen = HC)$selection
var2 <- VAR(zooBindsHC, p = 5, type ="none", exogen = HC)
serial.test(var2,lags.pt = 10, type = "PT.asymptotic")

#begin to extract values from the lists
pred.Binds <- unlist(pred1[[1]][[1]])
pred.Binds <- pred.Binds[1:7,1]

pred.Leads <- unlist(pred1[[1]][[2]])
pred.Leads <- pred.Leads[1:7,1]

Actual.Binds <- zoo(testxSS$Binds, testxSS$Date)
Actual.Leads <- zoo(testxSS$Leads, testxSS$Date)
Predicted.Binds <- zoo(pred.Binds, testxSS$Date)
Predicted.Leads <- zoo(pred.Leads, testxSS$Date)

zooPredBinds <- merge(Actual.Binds, Predicted.Binds)
zooPredLeads <- merge(Actual.Leads, Predicted.Leads)


#comparison plots
lty = c("dotted", "solid")
plot(zooPredBinds, screens = c(1), lty = lty,
     main = "Compare Forecast to Actual Binds", ylab = "Binds")
legend(as.Date("2015-01-12"), 320, c("Pred","Actual"), lty=lty)


plot(zooPredLeads, screens = c(1), lty = lty, main = "Compare Forecast to Actual Leads")
legend(as.Date("2015-01-12"), 320, c("Pred","Actual"), lty=lty)

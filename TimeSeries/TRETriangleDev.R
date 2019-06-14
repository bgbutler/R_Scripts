# loss dev forecasting

# load the key libraries
library(ggplot2)
library(zoo)
library(vars)
library(aod)
library(xts)
library(tseries)
library(dplyr)
library(readxl)
library(forecast)

# look at the avg of UW Year Reported
# this is going down the rows
# prevent viewing dplyr tibbles
options(dplyr.print_max = 1e9)


# set the working directory
setwd('~/Documents/URSProjects/DataSetsforDS/')
dataURL <- 'TREDevTriangles.xlsx'

# read the files from excel
paid <- read_excel(dataURL, sheet = 'Paid', col_names = TRUE,
                   col_types = NULL, na = '', skip = 0)

# read the files from excel
reported <- read_excel(dataURL, sheet = 'Reported', col_names = TRUE,
                       col_types = NULL, na = '', skip = 0)


# make a function to convert NAs to 0
naToZero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}

paidClean <- naToZero(paid)
reportedClean <- naToZero(reported)

# this is the development 

avgReported <- reportedClean %>% 
    select(DevYr,Average)

sumReported <- reportedClean %>% 
    select(DevYr, Sum)


# make it a time series zoo object
avgReportedZoo <- zoo(avgReported$Average, avgReported$DevYr)

# plot it
plot(avgReportedZoo, screens = 1)

# take a first difference since not stationary, too much trend
plot(diff(avgReportedZoo,1), screens = 1)

# get the pac and acf

ggAcf(avgReportedZoo)
ggPacf(avgReportedZoo)

# check the differences
ggAcf(diff(avgReportedZoo,1))
ggPacf(diff(avgReportedZoo,1))

# check and auto arima on the series
fitRepSum <- auto.arima(avgReportedZoo)

summary(fitRepSum)

coeftest(fitRepSum)

# look at acf of residuals
ggAcf(residuals(fitRepSum))

# try a model use test set of yr 2000
# training set of 2005

ts <- reportedClean %>% 
    select(DevYr,Y2000)

tsTrain <- ts[c(1:15),]

tsTest <- ts[c(16:17),]

trainZoo <- zoo(tsTrain$Y2000, tsTrain$DevYr)

ggAcf(trainZoo)
ggPacf(trainZoo)

m1 <- arima(trainZoo,order = c(1,0,0), include.mean = F)

m2 <- auto.arima(trainZoo)

summary(m1)

coeftest(m1)

summary(m2)

coeftest(m2)

pred1 <- predict(m2, n.ahead = 2)

tsTest




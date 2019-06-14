# file for analyzing triangles

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
library(knitr)




# prevent viewing dplyr tibbles
options(dplyr.print_max = 1e9)


# set the working directory
setwd('~/Documents/URSProjects/DataSetsforDS/')
dataURL <- 'TRELossTriangles.xlsx'

# read the files from excel
paid <- read_excel(dataURL, sheet = 1, col_names = TRUE,
                        col_types = NULL, na = "", skip = 0)

# read the files from excel
reported <- read_excel(dataURL, sheet = 2, col_names = TRUE,
                       col_types = NULL, na = "", skip = 0)

# make a function to convert NAs to 0
naToZero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}

# run the function to clean all of the data
# call the function on the whole dataframe
paidClean <- naToZero(paid)
reportedClean <- naToZero(reported)

# look at the avg of UW Year Reported
# this is going down the rows

avgReported <- reportedClean %>% 
    select(UwrtYr,Avg)

sumReported <- reportedClean %>% 
    select(UwrtYr, Sum)

# make it a time series zoo object
avgReportedZoo <- zoo(avgReported$Avg, avgReported$UwrtYr)

sumReportedZoo <- zoo(sumReported$Sum, sumReported$UwrtYr)

# get a plot of the series

plot(avgReportedZoo, screens = 1)

plot(sumReportedZoo, screens = 1)

# merge for a combined plot

combinedReported <- merge(avgReportedZoo,sumReportedZoo)

plot(combinedReported, screens = c(1,2))


# take a first difference since not stationary, too much trend
plot(diff(combinedReported,1), screens = c(1,2))

# check the autocorrelations
# this is the lag correlations including all other lags up to it

ggAcf(avgReportedZoo)
ggAcf(sumReportedZoo)

# check the acf of the differences

ggAcf(diff(avgReportedZoo,1))
ggAcf(diff(sumReportedZoo,1))

# check the partial autocorrelations
# these are the lag correlations after controlling the effects of the intermediary lags

ggPacf(avgReportedZoo)
ggPacf(sumReportedZoo)

# check the differences
ggPacf(diff(avgReportedZoo,1))
ggPacf(diff(sumReportedZoo,1))


# check and auto arima on the series
fitRepSum <- auto.arima(sumReportedZoo)

summary(fitRepSum)

coeftest(fitRepSum)

# try a better fit
fitRepSum1 <- arima(sumReportedZoo, order=c(1,1,0))

summary(fitRepSum1)
    
# look at acf of residuals
ggAcf(residuals(fitRepSum1))

# try a simple ar model
# try another fit check the 4th lag
fitRepSum2 <- arima(sumReportedZoo, order=c(4,0,0))

summary(fitRepSum2)

ggAcf(residuals(fitRepSum2))


#########################################################
# REDUCE TO A WINDOW OF 1997 TO 2014

sumRepRed <- window(sumReportedZoo,start = 1997, end = 2014)

plot(sumRepRed, screens = 1)

ggAcf(sumRepRed)

ggPacf(sumRepRed)


################################################################
# Move to Paid Losses

sumPaid <- paidClean %>% 
    select(UwrtYr, Sum)

sumPaidZoo <- zoo(sumPaid$Sum, sumPaid$UwrtYr)

plot(sumPaidZoo, screens = 1)

ggAcf(sumPaidZoo)

ggPacf(sumPaidZoo)

# try the window
sumPaidRed <- window(sumPaidZoo,start = 1997, end = 2014)

ggAcf(sumPaidRed)

ggPacf(sumPaidRed)

# check and auto arima on the series
fitPaidSum <- auto.arima(sumPaidZoo)

summary(fitPaidSum)

fitPaidSum1 <- arima(sumPaidZoo, order = c(1,1,0))

summary(fitPaidSum1)

fitPaidSum2 <- arima(sumPaidZoo, order = c(1,0,0), include.mean = T)

summary(fitPaidSum2)





#Get data
ENSOindex <- read.table(file="~/Documents/RFiles/enso1950.clean.csv", header=TRUE, sep=",")
head(ENSOindex)

#make zoo object of time series
ENSOws <- zoo(ws, ENSOindex$YEAR)


#plot single series
plot(ENSOws, xlab="Year", type="l", main = "ENSO Index for Jul Aug", ylab = "ENSO Index")

adf.test(ENSOws, alternative = "stationary")

par(mfrow=c(1,2))
acf(ENSOws)
pacf(ENSOws)

ENSOdiff <- diff(ENSOws, lag = 12)

ENSOmodel <- Arima(ENSOws, order = c(1,0,12))







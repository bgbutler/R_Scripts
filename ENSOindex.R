## Load ENSO Index data and Do quick Analysis

ENSOindex <- read.table(file="~/Documents/RFiles/enso1950.clean.csv", header=TRUE, sep=",")
head(ENSOindex)


#Checking how well data correlates
par(mfrow = c(1,3))
  with(ENSOindex, {
    plot(DECJAN, JANFEB, main = "End of Year")
    abline(lm(JANFEB~DECJAN, data = ENSOindex), lwd = 2, col='red')
    plot(JUNJUL, JULAUG, main = "Wind Season")
    abline(lm(JULAUG~JUNJUL, data = ENSOindex), lwd = 2, col='blue')
    plot(DECJAN, APRMAY, main = "Pre Wind Season")
    abline(lm(APRMAY~DECJAN, data = ENSOindex), lwd = 2, col='black')
    
})





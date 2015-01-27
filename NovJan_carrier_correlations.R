
#get the data
dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/CSVs/NovJanCorr.csv"
carrierData <- read.csv(dataURL, header = TRUE, as.is = TRUE, sep = "," )

#turn the month into a factor
carrierData$Date <- as.factor(carrierData$Date)
carrierData$Month <- as.factor(carrierData$Month)


#create some subsets of data by month
novData <- carrierData[carrierData$Month == "11",]
decData <- carrierData[carrierData$Month == "12",]
janData <- carrierData[carrierData$Month == "1",]

#remove unessesary columns
novM <- novData[,c(3:10,12:16)]
decM <- decData[,c(3:10,12:16)]
janM <- janData[,c(3:9,12:16)]
allM <- carrierData[,c(3:10,12:16)]

#create the covariance matrices
novCov <- cov(novM, method = "pearson", use = "pairwise.complete.obs")
decCov <- cov(decM, method = "pearson", use = "pairwise.complete.obs")
janCov <- cov(janM, method = "pearson", use = "pairwise.complete.obs")
allCov <- cov(allM, method = "pearson", use = "pairwise.complete.obs")

#convert to correlation matrices
novCorr <- cov2cor(novCov)
decCorr <- cov2cor(decCov)
janCorr <- cov2cor(janCov)
allCorr <- cov2cor(allCov)

#write matrices to .CSV files
write.csv(novCorr, file = "~/RDataFiles/nov_carrier_cor.csv", row.names = FALSE)
write.csv(decCorr, file = "~/RDataFiles/dec_carrier_cor.csv", row.names = FALSE)
write.csv(janCorr, file = "~/RDataFiles/jan_carrier_cor.csv", row.names = FALSE)
write.csv(allCorr, file = "~/RDataFiles/novjan_carrier_cor.csv", row.names = FALSE)




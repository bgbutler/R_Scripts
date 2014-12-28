dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/Carrier Mix.csv"
stateData <- read.csv(dataURL, header = TRUE, as.is = TRUE, sep = "," )

head(stateData)

#get the headers as column names
carriers <-colnames(stateData[2:15])

octData <- stateData[1:26,2:15]
novData <- stateData[27:50,2:15]
carrierAllMonths <- carrierData[,2:15]

#assign the headers to the two dataframes
colnames(octData) <-carriers
colnames(novData) <- carriers



#create the covariance matrices
octCov <- cov(octData, method = "pearson", use = "pairwise.complete.obs")
novCov <- cov(novData, method = "pearson", use = "pairwise.complete.obs")
allCov <- cov(carrierAllMonths, method = "pearson", use = "pairwise.complete.obs")

#convert to correlation matrices
octCorr <- cov2cor(octCov)
novCorr <- cov2cor(novCov)
allCorr <- cov2cor(allCov)

#write matrices to .CSV files
write.csv(octCorr, file = "~/RDataFiles/oct_correlations.csv", row.names = FALSE)
write.csv(novCorr, file = "~/RDataFiles/nov_correlations.csv", row.names = FALSE)
write.csv(allCorr, file = "~/RDataFiles/all_correlations.csv", row.names = FALSE)





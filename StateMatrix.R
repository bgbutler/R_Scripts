dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/State Data.csv"
stateData <- read.csv(dataURL, header = TRUE, as.is = TRUE, sep = "," )

octState <- stateData[1:26,2:40]
novState <- stateData[27:50,2:40]
states <-colnames(stateData[2:40])
stateAllMonths <- stateData[1:50,3:41]


#assign the headers to the two dataframes
colnames(octState) <-states
colnames(novState) <- states



#create the covariance matrices
octStateCov <- cov(octState, method = "pearson", use = "pairwise.complete.obs")
novStateCov <- cov(novState, method = "pearson", use = "pairwise.complete.obs")
allStateCov <- cov(stateAllMonths, method = "pearson", use = "pairwise.complete.obs")

#convert to correlation matrices
octCorr <- cov2cor(octStateCov)
novCorr <- cov2cor(novStateCov)
allCorr <- cov2cor(allStateCov)

#write matrices to .CSV files
write.csv(allCorr, file = "~/RDataFiles/state_correlations.csv", row.names = FALSE)

# add teh library to make a correlation heat map
library(spatstat) # "im" function 
plot(im(allCorr[nrow(allCorr):1,]), main="Correlation Matrix Map")


#convert to dissimilarity matrix
dissimilarity <- 1 - cor(allCorr)
distance <- as.dist(dissimilarity)

plot(hclust(distance), 
     main="Dissimilarity = 1 - Correlation", xlab="", hang = -1)

plot(hclust(distance), 
     main="State Clusters", xlab="", hang = -1)

dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/CSVs/JulyDecTS.csv"
tsData <- read.csv(dataURL, header = TRUE, as.is = FALSE, sep = "," )

#turn the month into a factor
tsData$Month <- as.factor(tsData$Month)

#create linear multivariate models for binds
modelBinds <- lm(OutBinds ~ OutBoundLeads + DialerHC + Age + 0, data = tsDataxSat)
modelBinds2 <- lm(OutBinds ~ OutBoundLeads + TotalHC + Age + 0, data = tsDataxSat)
modelBinds3 <- lm(OutBinds ~ OutBoundLeads + DialerHC + Day + 0, data = tsDataxSat)
modelBinds4 <- lm(OutBinds ~ OutBoundLeads + Day + 0, data = tsDataxSat)

#get output
summary(modelBinds)
summary(modelBinds2)
summary(modelBinds3)
summary(modelBinds4)

#create multivariate linear models for conversion
modelConvert <- lm(Conversion ~ OutBoundLeads + TotalHC + Age + 0, data = tsData)
modelConvert2 <- lm(Conversion ~ OutBoundLeads + DialerHC + Age + Day + 0, data = tsData)
modelConvert3 <- lm(Conversion ~ OutBoundLeads + Day + 0, data = tsData)

#output the conversion models
summary(modelConvert)
summary(modelConvert2)
summary(modelConvert3)

#create new dataset and exclude Saturday to reduce the noise
tsDataxSat <- tsData[tsData$Day != 'Sat',]

#cut the dataset to Sept - Dec
tsDataCut <- tsDataxSat[tsDataxSat$Month != c("7", "8"),]

modelHC <- lm(DialerHC/TotalHC ~ Day + 0, data = tsDataxSat)
summary(modelHC)


#create linear multivariate models for binds
modelBinds <- lm(Binds ~ OutBoundLeads + InCalls +  DialerHC + Age + 0, data = tsDataxSat)
modelBinds2 <- lm(Binds ~ OutBoundLeads + InCalls + TotalHC + Age + 0, data = tsDataxSat)
modelBinds3 <- lm(Binds ~ OutBoundLeads + InCalls + DialerHC + Day + 0, data = tsDataxSat)
modelBinds4 <- lm(Binds ~ OutBoundLeads + InCalls + Day + 0, data = tsDataxSat)

#get output
summary(modelBinds)
summary(modelBinds2)
summary(modelBinds3)
summary(modelBinds4)

library(ggplot2)
facets <- c("9", "10", "11", "12")
g <- ggplot(tsDataCut[tsDataCut$Month %in% facets,], aes(y = Binds, x = Day)) + geom_boxplot()
g + scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thur", "Fri")) + 
    facet_wrap(~ Month,)


#create linear multivariate models for binds with sept - dec data
modelBinds <- lm(Binds ~ OutBoundLeads + InCalls +  DialerHC + Age + 0, data = tsDataCut)
modelBinds2 <- lm(Binds ~ OutBoundLeads + InCalls + TotalHC + Age + 0, data = tsDataCut)
modelBinds3 <- lm(Binds ~ OutBoundLeads + InCalls + DialerHC + Day + 0, data = tsDataCut)
modelBinds4 <- lm(Binds ~ OutBoundLeads + Day + 0, data = tsDataCut)

#get output
summary(modelBinds)
summary(modelBinds2)
summary(modelBinds3)
summary(modelBinds4)

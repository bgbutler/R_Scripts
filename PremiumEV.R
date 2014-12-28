dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/OctDecData.csv"
evData <- read.csv(dataURL, header = TRUE, as.is = TRUE, sep = "," )


#clean up the column names
colnames(evData) = c("Carrier", "PayDate", "Premium", "Term", "State", "EV", "CommType")

#two linear models
modelEV <- lm(ev ~ premium, data = evData)
modelEV2 <- lm(ev~ premium + 0, data = evData)

#plot some of the data in a histogram
library(ggplot2)
h <- ggplot(evData, aes(x = Premium))
h + geom_histogram(binwidth = 100, fill = "blue", colour = "white") + xlim(0,5000) 


#make a scatterplot varied by commission
sp <- ggplot(evData, aes(x = Premium, y = EV))
sp + geom_point(aes(color = CommType)) + xlim(0,10000) + ylim(0,1000) + facet_wrap("Carrier")
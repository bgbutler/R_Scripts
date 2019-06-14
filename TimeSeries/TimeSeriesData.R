dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/TimeSeriesData.csv"
tsData <- read.csv(dataURL, header = TRUE, as.is = FALSE, sep = "," )

tsData$Month <- as.factor(tsData$Month)
tsData$Day <- as.factor(tsData$Day)
tsData$DayofWeek <- as.factor(tsData$DayofWeek)

library(ggplot2)

g <- ggplot(tsData, aes(x = Day, y  = Binds.Volume)) + 
g + geom_bar(stat = "identity") + facet_wrap( ~ Month, ncol=3,nrow=2) + 
    theme(legend.position="right") + 
    theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
    theme(legend.position="right")

g <- ggplot(tsData, aes(x = Lookup, y  = Binds.Volume)) 
g + geom_bar(stat = "identity") + 
    theme(legend.position="right") + 
    theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
    theme(legend.position="right")
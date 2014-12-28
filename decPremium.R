dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/DecPremiumMix.csv"
premData <- read.csv(dataURL, header = TRUE, as.is = TRUE, sep = "," )


#convert carrier to factor
premData$carrier <- as.factor(premData$carrier)
premData$date <- as.factor(premData$date)


#aggregate data by carrier and date
avgPrem <- aggregate(premium ~ carrier + date, data = premData, FUN = "mean")


#make facet plot with ggplot
g <- ggplot(avgPrem, aes(x =  date, y = premium, color = carrier))
g + ylim(0,1500) + facet_wrap("carrier") + geom_point() + theme(axis.text.x=element_text(angle=90))
+ labels(format(avgPrem$date, format = "%d"))



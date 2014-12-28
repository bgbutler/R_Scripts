library(ggplot2)
dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/DecPremiumMix.csv"
premData <- read.csv(dataURL, header = TRUE, as.is = TRUE, sep = "," )


#convert carrier to factor
premData$carrier <- as.factor(premData$carrier)
premData$date <- as.factor(premData$date)


#aggregate data by carrier and date
avgPrem <- aggregate(premium ~ carrier + date, data = premData, FUN = "mean")

avgPrem<- avgPrem[avgPrem$carrier != 'ASI',]

#make facet plot with ggplot
g <- ggplot(avgPrem, aes(x =  date, y = premium, color = carrier))
g + ylim(0,1500) + facet_wrap(~carrier, ncol=4,nrow=3) + geom_line(aes(group=carrier))+ 
    theme(axis.text.x=element_text(angle=90)) + 
    theme(strip.text.x = element_text(size=8, angle=0), 
          strip.text.y = element_text(size=12, face="bold"), 
          strip.background = element_rect(colour="red", fill="#CCCCFF")) + 
    theme(legend.position="right") + 
    theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
    theme(legend.position="right") + 
    theme(legend.background=element_rect(fill="gray90", size=.5, linetype="dotted"))


dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/DecPremiumMix.csv"
premData <- read.csv(dataURL, header = TRUE, as.is = TRUE, sep = "," )

#convert the dates to new format m/d
premData$newdate <- strptime(as.character(premData$date), "%m/%d")
premData$txtdate <-format(premData$newdate,"%m/%d")

#convert carrier to factor
premData$carrier <- as.factor(premData$carrier)
premData$date <- as.factor(premData$date)
premData$txtdate <- as.factor(premData$txtdate)

datebreaks <-seq(as.Date("2014-12-01"),as.Date("2104-12-16"), by = "2 days")


#aggregate data by carrier and date
avgPrem <- aggregate(premium ~ carrier + date + txtdate, data = premData, FUN = "mean")

avgPrem<- avgPrem[avgPrem$carrier != 'ASI',]

#make facet plot with ggplot
g <- ggplot(avgPrem, aes(x =  txtdate, y = premium, color = carrier))
g + ylim(0,1500) + facet_wrap(~carrier, ncol=4,nrow=3) + 
    geom_line(aes(group=carrier))+  
    theme(axis.text.x=element_text(angle=90, hjust = 1, vjust = .5)) + 
    theme(strip.text.x = element_text(size=8, angle=0), 
          strip.text.y = element_text(size=12, face="bold"), 
          strip.background = element_rect(colour="red", fill="#CCCCFF")) + 
    theme(legend.position="right") + 
    theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
    theme(legend.position="right") + 
    theme(legend.background=element_rect(fill="gray90", size=.5, linetype="dotted"))


#this is for an all community analysis of ramp-up
#load up a bunch of libraries

library(ggplot2)
library(plyr)
library(RODBC)
library(RColorBrewer)
library(knitr)


#set up the SQL connection
ch <- odbcConnect("ApprBudget")
dataRaw <- sqlQuery(ch, "SELECT Period, CommunityName, Active, Moderate, Low, Lurkers, DidNotLogin,
                    UniqueLogins, TotalLogins, TotalContributions, TotalUniqueContributors,
                    ContractSize, Branded, MonthsActive,
                    GenSeg, Industry, ParticipationPercentageActual, PeriodID
                    FROM BenchmarkingHistory
                    WHERE Solomon_Office = 'Boston'
                    ORDER by Period ASC")


dataSeries <- sqlQuery(ch, "SELECT Industry, Avg(ParticipationPercentageActual) as AvgPart,
               MonthsActive, Avg(TotalUniqueContributors) as Contributors, 
               sum(TotalContributions) as Vibrancy
               FROM BenchmarkingHistory
               GROUP BY MonthsActive, Industry
               ORDER BY MonthsActive Asc")

odbcClose(ch)


#add total members column
dataRaw$TotalMembers <- dataRaw$Active + dataRaw$Moderate +dataRaw$Low + dataRaw$Lurkers

#clean up the period
#create dates from period
dataRaw$Period <- as.character(dataRaw$Period)
dataRaw$Period <- as.Date(paste(dataRaw$Period,"-01", sep ="")) 

#make the PeriodID a factor
#dataRaw$PeriodID <- factor(dataRaw$PeriodID)
#remove na
na.omit(dataRaw)
                                                
# the next batch of plots is used to look at time trends

PDFplots <- "Q:/DataScience/ParticipationPlots/AvgParticipation.pdf"

#use "USr" or "A4r" for landscape
pdf(file = PDFplots, onefile = TRUE, paper = "USr", width = 11, height = 8.5)

#average participation
g <- ggplot(dataSeries, aes(x =  MonthsActive, y = AvgPart, colour = Industry))
g + ylim(0.20,1) + xlim(1,36) + facet_wrap(~Industry, ncol=4) + geom_line(aes(group=Industry), size = 1.05)+ 
    ggtitle("Average Participation Rate By Industry for First 36 Months") +
    scale_color_hue(l = 40) + 
    theme(plot.title = element_text(color = "blue", face = "bold")) + 
    theme(axis.title.x=element_text(color = "blue")) +
    theme(axis.title.y=element_text(angle=90, color = "blue")) + 
    theme(axis.text.x=element_text(angle=90, color = "red")) + 
    theme(axis.text.y=element_text(angle=0, color = "red")) +
    theme(strip.text.x = element_text(size=10, angle=0), 
            strip.text.y = element_text(size=12, face="bold"), 
            strip.background = element_rect(colour="red", fill="#CCCCFF"))

#unique contributors
g <- ggplot(dataSeries, aes(x =  MonthsActive, y = Contributors, color = Industry))
g + xlim(1,36) + ylim(100,400) + facet_wrap(~Industry, ncol=4) + geom_line(aes(group=Industry), size = 1.05)+ 
  ggtitle("Average Number of Unique Contributors By Industry for First 36 Months") +
  scale_color_hue(l = 40) + 
  theme(plot.title = element_text(color = "blue", face = "bold")) + 
  theme(axis.title.x=element_text(color = "blue")) +
  theme(axis.title.y=element_text(angle=90, color = "blue")) + 
  theme(axis.text.x=element_text(angle=90, color = "red")) + 
  theme(axis.text.y=element_text(angle=0, color = "red")) +
  theme(strip.text.x = element_text(size=10, angle=0), 
        strip.text.y = element_text(size=12, face="bold", angle=45), 
        strip.background = element_rect(colour="red", fill="#CCCCFF"))

#vibrancy as total contributions
g <- ggplot(dataSeries, aes(x =  MonthsActive, y = Vibrancy, color = Industry))
g + xlim(1,36) + facet_wrap(~Industry, ncol=4) + geom_line(aes(group=Industry), size = 1.05)+ 
  ggtitle("Total Contributions By Industry for First 36 Months") +
  scale_color_hue(l = 40) + 
  theme(plot.title = element_text(color = "blue", face = "bold")) + 
  theme(axis.title.x=element_text(color = "blue")) +
  theme(axis.title.y=element_text(angle=90, color = "blue")) + 
  theme(axis.text.x=element_text(angle=90, color = "red")) + 
  theme(axis.text.y=element_text(angle=0, color = "red")) +
  theme(strip.text.x = element_text(size=10, angle=0), 
        strip.text.y = element_text(size=12, face="bold"), 
        strip.background = element_rect(colour="red", fill="#CCCCFF"))
dev.off()





#### stop here #####

#new method to split it all out
# put in number of plots here
noPlots <- 4
# save all variables in a seperate vector to select in for-loop
allVars <- unique(dataSeries2$Industry)
noVars <- length(allVars)

# indices for plotting variables
plotSequence <- c(seq(0, noVars-1, by = noPlots), noVars)

# pdf("plotpath.pdf") # uncomment to save the resulting plots in a pdf file
# loop over the variables to plot
for(ii in 2:length(plotSequence)){
  # select start and end of variables to plot
  start <- plotSequence[ii-1] + 1
  end <- plotSequence[ii]
  
  # subset the variables and save new temporary data.frame
  tmp <- subset(dataSeries2, variable %in% allVars[start:end])
  cat(unique(tmp$Industry), "\n")
  
  # generate plot
  p <- ggplot(tmp,  aes(x = MonthsActive, y = Participation, Colour = ClientName))+
  p + ylim(0.20,1) + xlim(1,36) + facet_wrap(~Industry, ncol=5, nrow = 2) + geom_line(aes(group = ClientName)) + 
    ggtitle("Participation Rate By Industry for First 36 Months") +
    theme(axis.text.x=element_text(angle=90)) + 
    theme(strip.text.x = element_text(size=8, angle=0), 
          strip.text.y = element_text(size=12, face="bold"), 
          strip.background = element_rect(colour="red", fill="#CCCCFF"))
    
    
 #old code from example   
#    geom_point(colour = "dark green", size = 1.5)+
    geom_point(aes(y = value, color = variable))+
    geom_smooth(aes(y = value, fill = variable), 
                method = loess, size = 1, linetype = 1, se = T)+
#    facet_wrap(~variable, ncol = 2, nrow = 1000)+
    ylim(0, 1)+
    labs(x = "Months Active",  y = "Participation")
  print(p)
}
# dev.off()


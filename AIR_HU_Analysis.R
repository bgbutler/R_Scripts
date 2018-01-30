

#use ggplot to plot a variety of the hurricane data
library(ggplot2)


#set up the data by importing the .CSV files
dataURL <- "~/Documents/Sirius SII/SII Data Work/US HU Data Only for R.csv"
huDataAll <- read.csv(dataURL, header = TRUE, as.is = TRUE, sep = "," )


#make factors out of region and SSI
huDataAll$Primary.Region <- as.factor(huDataAll$Primary.Region)
huDataAll$Sec.Region <- as.factor(huDataAll$Sec.Region)
huDataAll$SSI <- as.factor(huDataAll$SSI)
huDataAll$area <- as.factor(huDataAll$area)


#remove the Virgin Islands and the N/A
#removes the data from the Sec Region
huData <- huDataAll[huDataAll$Primary.Region != 'N/A',]
huData <- huData[huData$Primary.Region != 'Virgin Islands (US)',]
huData <- huData[huData$Sec.Region != 'N/A',]
huData <- huData[huData$Sec.Region != 'Virgin Islands (US)',]



#make density plots of central pressure - these are not as preferred
g <- ggplot(huData, aes(x= CP, fill = SSI))
g + geom_density() + facet_wrap(~Sec.Region, ncol = 3, nrow = 2) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Central Pressure by SSI and Region") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


#make  histogram plots using MW and CP
g1 <- ggplot(huData, aes(x= MW))
g1 + geom_histogram(fill = "blue", color = "black") + facet_wrap(~Sec.Region, ncol = 3, nrow = 2) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Distribution of Max Wind Speed and Region") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

g2 <- ggplot(huData, aes(x= CP))
g2 + geom_histogram(fill = "red", color = "black") + facet_wrap(~Sec.Region, ncol = 3, nrow = 2) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Distribution of Central Pressure by Region") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


#makes a line version of a histogram; not as good resolution
g3 <- ggplot(huData, aes(x= CP))
g3 + geom_freqpoly(binwidth = 2, color = "blue") + facet_wrap(~Sec.Region, ncol = 3, nrow = 2) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Distribution of Central Pressure by Region") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


#the next series of plots make empirical cum distribution functions

#This Facets by SSI and colors by sec.region
g4 <-ggplot(data =huData) + stat_ecdf(aes(x = CP, color = Sec.Region), lwd = 1, linetype = "solid")
g4 + facet_wrap(~SSI, ncol = 3) +
  ggtitle("Distribution of Central Pressure by Region and SSI") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


#this is the same but uses Max Wind not really useful
g5 <-ggplot(data =huData) + stat_ecdf(aes(x = MW, color = Sec.Region), lwd = 1, linetype = "solid")
g5 + facet_wrap(~SSI, ncol = 3) +
  ggtitle("Distribution of Max Wind by Region and SSI") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


#plots max windspeed on a single plot
g6 <- ggplot(huData, aes(MW, colour = Sec.Region)) + stat_ecdf()
g6 + ggtitle("Distribution of Max Wind by Region") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

#plots CP on a single plot
g7 <- ggplot(huData, aes(CP, colour = Sec.Region)) + stat_ecdf()
g7 + ggtitle("Distribution of Central Pressure by Region") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

library(dplyr)
library(ggplot2)
library(reshape2)
#library(grid)
#library(gridExtra)
#library(cowplot)

#get the files  with location
url <- "C:/Users/n846490/Documents/DataScience/csat2016.csv"
branchCsat <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

for (i in 4:10){
    branchCsat[,i] <- as.numeric(as.character(branchCsat[,i]))
}


branchCsat <- na.omit(branchCsat)

legendPos <- "bottom"


#make histogram
g1 <- ggplot(branchCsat, aes(x= Apr, fill = Region)) + geom_histogram(alpha = 1) + 
    facet_wrap(~Region) + 
    theme(legend.position = legendPos) + 
    theme(legend.title = element_text(colour="Red", size=16, face="bold")) + 
    ggtitle("Distribution of CSat by Region in April 2016") + 
    theme(plot.title = element_text(size = rel(1.1), face = "bold.italic", color = "Red"),
          axis.title.y = element_text()) +
    theme(strip.background = element_rect(fill="red")) +
    theme(strip.text.x = element_text(size = 10, face = "bold"))

g1

#the  June data
#make histogram
g2 <- ggplot(branchCsat, aes(x= Jun, fill = Region)) + geom_histogram(alpha = 1) + 
    facet_wrap(~Region) + 
    theme(legend.position=legendPos) + 
    theme(legend.title = element_text(colour="Red", size=16, face="bold")) + 
    ggtitle("Distribution of CSat by Region in June 2016") + 
    theme(plot.title = element_text(size = rel(1.1), face = "bold.italic", color = "Red"),
          axis.title.y = element_text()) +
    theme(strip.background = element_rect(fill="red")) +
    theme(strip.text.x = element_text(size = 10, face = "bold"))
g2





#density plots
g3 <- ggplot(branchCsat, aes(x= Apr, fill = Region)) + geom_density(alpha = .5) + 
    facet_wrap(~Region) + 
    theme(legend.position=legendPos) + 
    theme(legend.title = element_text(colour="Red", size=16, face="bold")) + 
    ggtitle("Distribution of CSat by Region in April 2016") + 
    theme(plot.title = element_text(size = rel(1.1), face = "bold.italic", color = "Red"),
          axis.title.y = element_text()) +
    theme(strip.background = element_rect(fill="red")) +
    theme(strip.text.x = element_text(size = 10, face = "bold"))
g3






#density plot
g4 <- ggplot(branchCsat, aes(x= Jun, fill = Region)) + geom_density(alpha = .5) + 
    facet_wrap(~Region) + 
    theme(legend.position=legendPos) + 
    theme(legend.title = element_text(colour="Red", size=16, face="bold")) + 
    ggtitle("Distribution of CSat by Region in June 2016") + 
    theme(plot.title = element_text(size = rel(1.1), face = "bold.italic", color = "Red"),
          axis.title.y = element_text()) +
    theme(strip.background = element_rect(fill="red")) +
    theme(strip.text.x = element_text(size = 10, face = "bold"))
g4


#for using cowplot
#plot_grid(g1, g2, labels=c("A", "B"), ncol = 2, nrow = 1)


#reshape the data
csatClean <- select(branchCsat, Region, BranchNumber, Name, Apr, Jun)

csatMelt <- melt(csatClean, id = c("Region", "BranchNumber", "Name"),
                 variable.name = "Month",
                 value.name = "Score")

#make histogram
g5 <- ggplot(csatMelt, aes(x= Score, fill = Month)) + geom_histogram(alpha = 1) + 
    facet_wrap(~Region) + 
    theme(legend.position = legendPos) + 
    theme(legend.title = element_text(colour="Red", size=16, face="bold")) + 
    ggtitle("Comparison of CSat") + 
    theme(plot.title = element_text(size = rel(1.1), face = "bold.italic", color = "Red"),
          axis.title.y = element_text()) +
    theme(strip.background = element_rect(fill="red")) +
    theme(strip.text.x = element_text(size = 10, face = "bold"))

g5

#make histogram
g6 <- ggplot(csatMelt, aes(x= Score, fill = Month)) + geom_density(alpha = .5) + 
    facet_wrap(~Region) + 
    theme(legend.position = legendPos) + 
    theme(legend.title = element_text(colour="Red", size=16, face="bold")) + 
    ggtitle("Comparison of CSat") + 
    theme(plot.title = element_text(size = rel(1.1), face = "bold.italic", color = "Red"),
          axis.title.y = element_text()) +
    theme(strip.background = element_rect(fill="red")) +
    theme(strip.text.x = element_text(size = 10, face = "bold"))

g6





#make histogram
g7 <- ggplot(csatMelt, aes(x= Score, fill = Month)) + geom_histogram(alpha = 1) + 
    theme(legend.position = legendPos) + facet_wrap(~Month) +
    theme(legend.title = element_text(colour="Red", size=16, face="bold")) + 
    ggtitle("Comparison of CSat") + 
    theme(plot.title = element_text(size = rel(1.1), face = "bold.italic", color = "Red"),
          axis.title.y = element_text()) +
    theme(strip.background = element_rect(fill="red")) +
    theme(strip.text.x = element_text(size = 10, face = "bold"))

g7
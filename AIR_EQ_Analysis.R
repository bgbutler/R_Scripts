#load in the plotting package

library(ggplot2)


#load in the CSV data
dataURL <- "~/Desktop/SII Data Work/US EQ Data for R.csv"
eqDataAll <- read.csv(dataURL, header = TRUE, as.is = TRUE, sep = "," )


#make factors out of region and SSI
eqDataAll$Region <- as.factor(eqDataAll$Region)
eqDataAll$SeismicRegion <- as.factor(eqDataAll$SeismicRegion)
eqDataAll$EQ_Source_Type <- as.factor(eqDataAll$EQ_Source_Type)

#due to differences in data set size need to make two groups
largeGroup <- subset(eqDataAll, SeismicRegion %in% c("CA", "Canada", "Intermountain", "PAC NW"))
smallGroup <- subset(eqDataAll, SeismicRegion %in% c("New Madrid", "Midwest", "Southeast", "Northeast"))


#make historgram but due to unequal bins, not a clean usable plot
g <- ggplot(smallGroup, aes(x= Magnitude, fill = EQ_Source_Type))
g + geom_histogram(binwidth = .05) + facet_wrap(~SeismicRegion, ncol = 2) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Distribution of Magnitude by Source and Region") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


#make hisogram for the larger group
g5 <- ggplot(largeGroup, aes(x= Magnitude, fill = EQ_Source_Type))
g5 + geom_histogram(binwidth = 0.05) + facet_wrap(~SeismicRegion, ncol = 2) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Distribution of Magnitudes by Source and Region") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))




#make more plots using magnitude and the ECDF function
g1 <- ggplot(eqDataAll, aes(Magnitude, colour = EQ_Source_Type)) + stat_ecdf()+ xlim(5,10) + ylab("Cum Probability")
g1 + facet_wrap(~SeismicRegion, ncol = 3) + 
  ggtitle("Empirical Cumulative Distribution of Magnitude by Source Type") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


g2 <- ggplot(eqDataAll, aes(Magnitude)) + stat_ecdf()+ xlim(5,10) + ylab("Cum Probability")
g2 + facet_wrap(~SeismicRegion, ncol = 3) + 
  ggtitle("Empirical Cumulative Distribution of Magnitude") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

g6 <- ggplot(eqDataAll, aes(Magnitude, color = SeismicRegion)) + stat_ecdf() + xlim(5,10) + ylab("Cum Probability")
g6 + ggtitle("Empirical Cumulative Distribution of Magnitude") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

g7 <- ggplot(eqDataAll, aes(Magnitude)) + stat_ecdf()
g7 + ggtitle("Empirical Cumulative Distribution of Magnitude of Entire Catalog") + xlim(5,10) + ylab("Cum Probability") +
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))



#make density plots of magnitude by seismic region
g3 <- ggplot(eqDataAll, aes(x= Magnitude, fill = EQ_Source_Type))
g3 + geom_density() + facet_wrap(~SeismicRegion, ncol = 2, nrow = 4) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Magnitude by Region") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

#make histogram of magnitude by region
g4 <- ggplot(eqDataAll, aes(x= Magnitude, fill = SeismicRegion))
g4 + geom_histogram(alpha = 0.4) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Distribution of Magnitude by Region") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))



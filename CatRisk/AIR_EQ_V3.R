#load in the plotting package

library(ggplot2)


#load in the CSV data
dataURL <- "~/Desktop/SII Data Work/US EQ Data for R.csv"
eqDataAll <- read.csv(dataURL, header = TRUE, as.is = TRUE, sep = "," )


#make factors out of region and seismic information
eqDataAll$Region <- as.factor(eqDataAll$Region)
eqDataAll$SeismicRegion <- as.factor(eqDataAll$SeismicRegion)
eqDataAll$EQ_Source_Type <- as.factor(eqDataAll$EQ_Source_Type)

#create subsets of the data based on region
x <- unique(eqDataAll$SeismicRegion)


#split function might be better alternative
eqRegions <- split(eqDataAll, eqDataAll$SeismicRegion)


#this method breaks up the list and creates the dfs in the global environment
lapply(names(eqRegions), function(x) assign(x,eqRegions[[x]], envir = .GlobalEnv))

#this is an alternative method, must faster
list2env(eqRegions, envir=.GlobalEnv)

#for processing within the list use the following code
eqRegionsProcessed <- lapply(eqRegions, function(df) {
  ## do domething here such as plot
  ## returns a list of values
})

#make more plots using magnitude and the ECDF function
g1 <- ggplot(eqRegions$CA, aes(Magnitude, colour = EQ_Source_Type)) + stat_ecdf()+ xlim(5,10) + ylim(.90,1.0) + ylab("Cum Probability")
g1 + 
  ggtitle("Empirical Cumulative Distribution of Magnitude by Source Type") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


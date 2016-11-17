#script to convert SPSS file to R

.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(scales)
library(gridExtra)
library(foreign)
library(Hmisc)
library(memisc)


#this used the foreign package but creates a massive file
dataset = read.spss("C:/Users/n846490/Documents/DataScience/CSVs/US_R6_2016.sav", to.data.frame=TRUE)

write.csv(dataset, file="//STATESFPSOP1/apps/RASHD/Projects/RAnalyses/HHIDbyMonth.csv", row.names=FALSE)

write.csv(dataset, file="C:/Users/n846490/Documents/DataScience/CSVs/mbData2016.csv", row.names = FALSE)

#install packages for manipulating and plotting data
library(zipcode)
data(zipcode)
library(ggplot2)
library(maps)
library(plyr)
library(htmltools)

#get the requirements for leaflet and leaflet
if(!require(devtools)) install.packages("devtools")
if(!require(leaflet)) install_github("rstudio/leaflet")

#get zipcode data and cleanse
url <- "K:/Sandbox/R/Data/citiespop1000.csv"
cityData <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#to match countries use ISO-3166 2-letter country code
#source file for the data is the following URL
#http://download.geonames.org/export/dump/readme.txt
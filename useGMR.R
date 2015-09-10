#use the clean Globale Member Report

#set the URL
url <- "K:/Sandbox/Global Report Dashboard/gmrClean_08201015.csv"
gmrData <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)
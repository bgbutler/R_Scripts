


library(tidyverse)

# set wd
setwd('C:/Users/bbutler/Documents/DigitalAnalytics')


ads <- readr::read_csv('GAdsWeek1Rev2.csv')

y <- readr::read_csv('CheckingAdsClean.csv')


head(ads)


ads$AdText <- paste(ads$`Line 1`, ads$`Line 2`, sep = " ")

ads$`Ad ID` <- as.character(ads$`Ad ID`)

# write locally too
write.csv(ads, file ='CheckingAdsClean.csv', row.names = FALSE, fileEncoding = "UTF-8")

# test the COCC file
cocc <- readr::read_csv('eOpen Summary_20200601-20201015.csv')

#Production Version of Wal Mar Mapping
#Map the WalMart Comments

#install packages for manipulating and plotting data
library(zipcode)
library(ggplot2)
library(maps)
library(plyr)
library(htmltools)
library(devtools)
library(leaflet)
library(stringi)
library(tm.plugin.sentiment)


#get the zipcode data
data(zipcode)


#get the requirements for leaflet and leaflet
if(!require(devtools)) install.packages("devtools")
if(!require(leaflet)) install_github("rstudio/leaflet")

#get zipcode data and cleanse
url <- "K:/Sandbox/R/Data/WalMartv2.csv"
walmartRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#sort the levels of income for better plotting
incFactor <- c("Prefer not to say", "Less than 7500", "7500 to less than 22500", "22500 to less than 40000", 
               "40000 to less than 50000",
               "50000 to less than 60000", "60000 to less than 70000","70000 to less than 80000", 
               "80000 to less than 90000", "90000 to less than 100000",
               "100000 to less than 125000", "125000 to less than 150000", "150000 to less than 175000",
               "175000 to less than 200000", "200000 or more")

walmartRaw$Income <- factor(walmartRaw$Income, levels = incFactor, ordered = TRUE)


#convert the zipcodes to char
walmartRaw$Zip <-as.character(walmartRaw$Zip)

#clean up zipcodes leading zeros etc
walmartRaw$Zip <- clean.zipcodes(walmartRaw$Zip)

#append the city, state and lat and lon of zip to the data
zipData <- merge(walmartRaw, zipcode, by.x = 'Zip', by.y = 'zip')

#remove the na's
wmZip <- zipData
wmZip <- na.omit(wmZip)

#remove the extra data
rm(zipcode)
rm(zipData)
rm(walmartRaw)

#get data without special chars
wmZip <- wmZip[stri_enc_isutf8(wmZip$Essay1)== T,]

#cut the groups by grocery shopping percentage q13 from screener
wmZip$Shopper <- cut(wmZip$PercentShop, 
                     c(0,25,50,75,100), include.lowest = T,
                     labels = c("ShopLess25", "Shop25to50", "Shop50to75","Shop75to100"))



competitor <- "Costco|costco|target|Target|BJ"
rows.to.keep <- grep(competitor, wmZip$Essay1)
competitor <- wmZip[rows.to.keep,]

#base plot of location of shoppers - by split, colors shoppers by percentage
#set colors to a factor
binColorsS <- colorFactor(palette = "Blues", wmZip$Shopper)
binColorsInc <- colorFactor(rainbow(15), wmZip$Income, ordered = TRUE)


#create the popup from the first 255 chars
wmZip$Popup <- paste(wmZip$Gender, wmZip$Age, wmZip$Essay1, sep = "-")


#make the plots of competitor chatter
#make the comment plot for less than 2

binColorsG <- colorFactor(rainbow(2), competitor$Gender)
m <- leaflet() %>%
  addProviderTiles("Stamen.Toner")  %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addPopups(data = competitor, lat = ~ latitude, lng = ~ longitude, ~Popup, 
            options = popupOptions(closeButton = T)) %>%
  addCircleMarkers(data = competitor, lat = ~ latitude, lng = ~ longitude,
                   color = ~binColorsG(Gender), radius = 3) %>%
  addLegend("bottomleft",pal = binColorsG, values = competitor$Gender,
            title = "Mentions of Competitors",
            opacity = 1)
m

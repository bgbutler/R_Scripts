#This is the development module for Wal Mart
#Map the WalMart Comments

#install packages for manipulating and plotting data
library(zipcode)
library(ggplot2)
library(maps)
library(plyr)
library(htmltools)
library(devtools)
library(leaflet)
library(readxl)
library(stringi)
library(knitr)

#get the zipcode data
data(zipcode)


#get the requirements for leaflet and leaflet
if(!require(devtools)) install.packages("devtools")
if(!require(leaflet)) install_github("rstudio/leaflet")

#get zipcode data and cleanse
url <- "K:/Sandbox/R/WalMartv2.csv"
walmartRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


#convert the zipcodes to char
walmartRaw$Zip <-as.character(walmartRaw$Zip)

#clean up zipcodes leading zeros etc
walmartRaw$Zip <- clean.zipcodes(walmartRaw$Zip)

#append the city, state and lat and lon of zip to the data
zipData <- merge(walmartRaw, zipcode, by.x = 'Zip', by.y = 'zip')

#remove the column clean only if included
zipData$Clean <- NULL

#remove the na's
wmZip <- zipData
wmZip <- na.omit(wmZip)



#convert the state abbreviation to state name
#zipData$state <- state.name[match(zipData$state, state.abb)]
#zipData <- as.data.frame(sapply(zipData,tolower))
rm(zipcode)
rm(zipData)
rm(walmartRaw)

#create the necessary factors
#wmZip$Gender <- as.factor(wmZip$Gender)
#wmZip$Age <- as.factor(wmZip$Age)
#wmZip$Education <- as.factor(wmZip$Education)

#ensure UTF coding for all text
all(stri_enc_isutf8(wmZip$Essay1))

#this is key to cleaning up data
wmZip <- wmZip[stri_enc_isutf8(wmZip$Essay1)== T,]


iconv(wmZip$Essay1,"latin1", "ASCII", sub = "")

#ensure UTF coding for all text
all(stri_enc_isutf8(wmZip$Essay1))

wmZip$Shopper <- cut(wmZip$PercentShop, c(0,50,100), include.lowest = T,labels = c("ShopLess50", "ShopMore50"))

#partition data to use
shop50 <- wmZip[wmZip$Shopper == 'ShopLess50',]
shop50More <- wmZip[wmZip$Shopper == 'ShopMore50',]

shop50More <- shop50More[1:3000,]

#create popup text as combined text

shop50More$Essay <- as.character(shop50More$Essay)

shop50More$Popup <- paste(shop50More$Gender, shop50More$Age, shop50More$Essay1, sep = "-")
shop50More$PopupInc <- paste(shop50More$Gender, shop50More$Age, shop50More$Income, sep = "-")

shop50$Popup <- paste(shop50$Gender, shop50$Age, shop50$Essay1, sep = "-")
shop50$PopupInc <- paste(shop50$Gender, shop50$Age, shop50$Income, sep = "-")


iconv(shop50More$Essay1,"latin1", "ASCII", sub = "")



#set up the map
(m <- leaflet() %>% addTiles())
m %>% setView(lng = -71.13, lat = 42.3, zoom = 10) # set centre and extent of map

#set colors to a factor
binColorsS <- colorFactor(rainbow(2), wmZip$Shopper)

#more than 50% Group
binColorsM <- colorFactor(rainbow(7), shop50More$Education)
binColors2M <- colorFactor(rainbow(15), shop50More$Income)
binColorsGM <- colorFactor(rainbow(2), shop50More$Gender)

#Less than 50% Group
binColorsL <- colorFactor(rainbow(7), shop50$Education)
binColors2L <- colorFactor(rainbow(15), shop50$Income)
binColorsGL <- colorFactor(rainbow(2), shop50$Gender)





#this is the final popups - two plots this one works with smaller data
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addMarkers(data = shop50More, lat = ~latitude, lng = ~longitude, 
             popup = shop50More$Popup) %>%
  addCircleMarkers(data = shop50More, lat = ~ latitude, lng = ~ longitude, 
                   color = ~binColorsGM(Gender), radius = 1) %>%
  addLegend("topright",pal = binColorsGM, values = shop50More$Gender,
            title = "Wal Mart Refresh",
            opacity = 1)
m

#this is the popups without the  blue markers.
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addPopups(data = shop50More, lat = ~ latitude, lng = ~ longitude, ~Popup, 
            options = popupOptions(closeButton = FALSE) )%>%
  addCircleMarkers(data = shop50More, lat = ~ latitude, lng = ~ longitude, 
                   color = ~binColorsGM(Gender), radius = 1) %>%
  addLegend("topright",pal = binColorsGM, values = shop50More$Gender,
            title = "Wal Mart Refresh",
            opacity = 1)
m



#this is the final popups - two plots
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addMarkers(data = shop50, lat = ~latitude, lng = ~longitude, 
             popup = shop50$Popup) %>%
  addCircleMarkers(data = shop50, lat = ~ latitude, lng = ~ longitude, 
                   color = ~binColorsGL(Gender), radius = 1) %>%
  addLegend("topright",pal = binColorsGL, values = shop50$Gender,
            title = "Wal Mart Refresh",
            opacity = 1)
m





#base plot of location of shoppers - by split
#model without text - works
m <- leaflet() %>%
#  addTiles() %>%
  addProviderTiles("Stamen.Toner")  %>%
#  addProviderTiles("CartoDB.Positron") %>%
  fitBounds(-65,0, - 100, 65) %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addCircles(data = wmZip, lat = ~ latitude, lng = ~ longitude, 
                   color = ~binColorsS(Shopper), radius = 1) %>%
  addLegend("bottomright",pal = binColorsS, values = wmZip$Shopper,
            title = "Wal Mart Refresh", opacity = 1)
m

#model without text - works for all data sets
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addCircleMarkers(data = newZip, lat = ~ latitude, lng = ~ longitude, 
             color = ~binColors2(Income), radius = 1) %>%
  addLegend("bottomright",pal = binColors2, values =newZip$Income,
            title = "Wal Mart Refresh", opacity = 1)
m




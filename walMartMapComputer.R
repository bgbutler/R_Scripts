#Production Version of Wal Mart Mapping
#Map the WalMart Comments

#install packages for manipulating and plotting data
library(zipcode)
library(ggplot2)
library(maps)
library(dplyr)
library(htmltools)
library(devtools)
library(leaflet)
library(stringi)


#get the zipcode data
data(zipcode)


#get the requirements for leaflet and leaflet
if(!require(devtools)) install.packages("devtools")
if(!require(leaflet)) install_github("rstudio/leaflet")

#get zipcode data and cleanse
url <- "K:/Sandbox/R/Data IO/wmDropOut.csv"
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

#create the popup from the first 255 chars
wmZip$Popup <- paste(wmZip$Gender, wmZip$Age, wmZip$Essay1, sep = "-")
wmZip$PopupInc <- paste(wmZip$Gender, wmZip$Age, wmZip$Income, sep = "-")

#partition data to use
shop25Less <- wmZip[wmZip$Shopper == 'ShopLess25',]
shop25to50 <- wmZip[wmZip$Shopper == 'Shop25to50',]
shop50to75 <- wmZip[wmZip$Shopper == 'Shop50to75',]
shop75to100 <- wmZip[wmZip$Shopper == 'Shop75to100',]

#base plot of location of shoppers - by split, colors shoppers by percentage
#set colors to a factor
binColorsS <- colorFactor(palette = "Blues", wmZip$Shopper)
binColorsInc <- colorFactor(rainbow(15), wmZip$Income, ordered = TRUE)
binColorsComp <- colorFactor(rainbow(15), wmZip$Computer, ordered = TRUE)

#use length essay to compare
#start exploratory analysis
plt0 <- ggplot(wmZip, aes(x= LengthEssay, fill = Gender))
plt0 + geom_histogram(binwidth = 10) + facet_wrap(~Computer, ncol = 3) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Histogram of Essay Length by Character") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

touch <- wmZip[wmZip$Computer == 'touch',]


mobilePC <- wmZip[wmZip$Computer != 'generic',]
mobilePC$wordCount <- sapply(gregexpr("[[:alpha:]]+", mobilePC$Essay), function(x) sum(x > 0))

by_Computer <- group_by(mobilePC, Computer)
avgWord <- summarise(by_Computer,
                     avg = mean(wordCount, na.rm = T),
                     stdev = sd(wordCount, na.rm = T))
avgWord

plt0 <- ggplot(mobilePC, aes(x= LengthEssay, fill = Gender))
plt0 + geom_histogram(binwidth = 10) + facet_wrap(~Computer, ncol = 3) + xlim(0,1200) + 
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Histogram of Essay Length by Character") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

plt0 <- ggplot(mobilePC, aes(x= wordCount, fill = Computer)) + xlim(0,200)
plt0 + geom_density(alpha = 0.3) + xlab("Count of Essay Words") +ylab("Proportion") + 
  theme(
    legend.position="right",
    axis.title.x = element_text(colour = "red"),
    axis.title.y = element_text(colour = "red"),
    legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Histogram of Essay Length by Character") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))




m <- leaflet() %>%
  addProviderTiles("Stamen.Toner", group = "Tonor")  %>%
  fitBounds(-65,0, - 100, 65) %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  #overlay groups
  addCircleMarkers(data = wmZip, lat = ~ latitude, lng = ~ longitude, 
             color = ~binColorsS(Shopper), radius = 3, group = "Shopper") %>%
  addCircleMarkers(data = wmZip, lat = ~ latitude, lng = ~ longitude, 
             color = ~binColorsInc(Income), radius = 2, group = "Income") %>%
  #layers control
  addLayersControl(
  baseGroups = "Tonor",
  overlayGroups = c("Income", "Shopper"),
  options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend("bottomright",pal = binColorsS, values = wmZip$Shopper,
            title = "Percent Shopping", opacity = 1) %>%
  addLegend("bottomleft",pal = binColorsInc, values = wmZip$Income,
            title = "Income", opacity = 1)
m 

#show mobile distribution

binColorsComp <- colorFactor(rainbow(3), wmZip$Computer, ordered = TRUE)
m <- leaflet() %>%
  addProviderTiles("Stamen.Toner")  %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
#  addMarkers(data = wmZip, lat = ~latitude, lng = ~longitude) %>%
  addCircleMarkers(data = wmZip, lat = ~ latitude, lng = ~ longitude, 
                   color = ~binColorsComp(Computer), radius = 3) %>%
  addLegend("bottomleft",pal = binColorsComp, values = wmZip$Computer,
            title = "Computer",
            opacity = 1)
m







#make the comment plot for less than 25

binColorsG <- colorFactor(rainbow(2), shop25Less$Gender)
m <- leaflet() %>%
  addProviderTiles("Stamen.Toner")  %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addMarkers(data = shop25Less, lat = ~latitude, lng = ~longitude, 
             popup = shop25Less$Popup) %>%
  addCircleMarkers(data = shop25Less, lat = ~ latitude, lng = ~ longitude, 
                   color = ~binColorsG(Gender), radius = 2) %>%
  addLegend("bottomleft",pal = binColorsG, values = shop25Less$Gender,
            title = "Shop Less than 25%",
            opacity = 1)
m


#make the comment plot for 25 to 50

binColorsG <- colorFactor(rainbow(2), shop25to50$Gender)
m <- leaflet() %>%
  addProviderTiles("Stamen.Toner")  %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addMarkers(data = shop25to50, lat = ~latitude, lng = ~longitude, 
             popup = shop25to50$Popup) %>%
  addCircleMarkers(data = shop25to50, lat = ~ latitude, lng = ~ longitude, 
                   color = ~binColorsG(Gender), radius = 2) %>%
  addLegend("bottomleft",pal = binColorsG, values = shop25to50$Gender,
            title = "Shop 25 -50%",
            opacity = 1)
m

#make the comment plot for 50 to 75

binColorsG <- colorFactor(rainbow(2), shop50to75$Gender)
m <- leaflet() %>%
  addProviderTiles("Stamen.Toner")  %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addMarkers(data = shop50to75, lat = ~latitude, lng = ~longitude, 
             popup = shop50to75$Popup) %>%
  addCircleMarkers(data = shop50to75, lat = ~ latitude, lng = ~ longitude, 
                   color = ~binColorsG(Gender), radius = 2) %>%
  addLegend("bottomleft",pal = binColorsG, values = shop50to75$Gender,
            title = "Shop 50 -75%",
            opacity = 1)
m

#make the comment plot for 75 to 100

binColorsG <- colorFactor(rainbow(2), shop75to100$Gender)
m <- leaflet() %>%
  addProviderTiles("Stamen.Toner")  %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addMarkers(data = shop75to100, lat = ~latitude, lng = ~longitude, 
             popup = shop75to100$Popup) %>%
  addCircleMarkers(data = shop75to100, lat = ~ latitude, lng = ~ longitude, 
                   color = ~binColorsG(Gender), radius = 2) %>%
  addLegend("bottomleft",pal = binColorsG, values = shop75to100$Gender,
            title = "Shop 75-100%",
            opacity = 1)
m


#make the cloud plot for 75 to 100

binColorsG <- colorFactor(rainbow(2), shop75to100$Gender)
m <- leaflet() %>%
  addProviderTiles("Stamen.Toner")  %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addPopups(data = shop75to100, lat = ~ latitude, lng = ~ longitude, ~Popup, 
            options = popupOptions(closeButton = FALSE) )%>%
  addCircleMarkers(data = shop75to100, lat = ~ latitude, lng = ~ longitude, 
                   color = ~binColorsG(Gender), radius = 2) %>%
  addLegend("bottomleft",pal = binColorsG, values = shop75to100$Gender,
            title = "Shop 75-100%",
            opacity = 1)
m

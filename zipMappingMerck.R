

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
url <- "K:/Merck/Women's Health/Summary/Zip code cluster_9.11.15.csv"
#url <- "K:/Sandbox/R/Data IO/Zip code cluster_9.10.15.csv"
merckZips <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#convert the zipcodes to char
merckZips$zipcode <-as.character(merckZips$zipcode)

#clean up zipcodes in 
merckZips$zip <- clean.zipcodes(merckZips$zipcode)

#append the city, state and lat and lon of zip to the data
zipData <- merge(merckZips, zipcode, by.x = 'zip', by.y = 'zip')

#convert the state abbreviation to state name
zipData$state <- state.name[match(zipData$state, state.abb)]
zipData <- as.data.frame(sapply(zipData,tolower))
rm(zipcode)


newNames <- c("zip", "guid", "therapy", 
              "oldZip", "city", "state", "lat", "long")

colnames(zipData) <- newNames

#(m <- leaflet() %>% addTiles())

#m %>% setView(lng = -71.13, lat = 42.3, zoom = 10) # set centre and extent of map


binColors2 <- colorFactor(rainbow(3), zipData$therapy)



m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stamen.Toner")  %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addCircleMarkers(data = zipData, lat = ~ lat, lng = ~ long, 
                   color = ~binColors2(therapy), radius = 3) %>%
  addLegend("bottomright",pal = binColors2, values = zipData$therapy,
            title = "Therapy",
            opacity = 1)
m



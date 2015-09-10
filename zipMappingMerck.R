

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
url <- "K:/Sandbox/R/Zip code cluster_8.26.15.csv"
merckZips <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#convert the zipcodes to char
merckZips$Zipcode <-as.character(merckZips$Zipcode)

#clean up zipcodes in 
merckZips$zip <- clean.zipcodes(merckZips$Zipcode)

#append the city, state and lat and lon of zip to the data
zipData <- merge(merckZips, zipcode, by.x = 'zip', by.y = 'zip')

#convert the state abbreviation to state name
zipData$state <- state.name[match(zipData$state, state.abb)]
zipData <- as.data.frame(sapply(zipData,tolower))
rm(zipcode)


newNames <- c("zip", "guid", "login","status", 
              "oldZip", "city", "state", "lat", "long")

colnames(zipData) <- newNames

(m <- leaflet() %>% addTiles())

m %>% setView(lng = -71.13, lat = 42.3, zoom = 10) # set centre and extent of map


binColors2 <- colorFactor(rainbow(3), zipData$login)



m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addCircleMarkers(data = zipData, lat = ~ lat, lng = ~ long, 
                   color = ~binColors2(login), radius = 3) %>%
  addLegend("bottomright",pal = binColors2, values = zipData$login,
            title = "Login Status Merck",
            opacity = 1)
m



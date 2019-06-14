

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
url <- "K:/Sandbox/R/Zip code cluster.csv"
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

binColors <- colorFactor("Blues", zipData$login, levels = 3, na.color = "Red")
binColors2 <- colorFactor(rainbow(3), zipData$login)

m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addCircles(data = zipData, lat = ~ lat, lng = ~ long, fillColor = colors)
m

m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addCircleMarkers(data = zipData, lat = ~ lat, lng = ~ long, color = ~binColors2(login)) %>%
  addPopups(data = zipData, lat = ~ lat, lng = ~ long, ~login, 
            options = popupOptions(closeButton = FALSE))
m

m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addCircleMarkers(data = zipData, lat = ~ lat, lng = ~ long, color = ~binColors2(login)) %>%
  addMarkers(data = zipData, lat = ~lat, lng = ~long, 
             popup = ~htmlEscape(login))
m

m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addCircleMarkers(data = zipData, lat = ~ lat, lng = ~ long, 
                   color = ~binColors2(login), radius = 6) %>%
  addLegend("bottomright",pal = binColors2, values = zipData$login,
            title = "Login Status Merck",
            opacity = 1)
m


newIcon <- makeIcon(icon)
  

pop <- "<a href='http://rstudio.github.io/leaflet/'>Leaflet</a>"



m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addMarkers(data = zipData, lat = ~lat, lng = ~long, 
             popup = "<a href='http://rstudio.github.io/leaflet/'>Leaflet</a>") %>%
  addCircleMarkers(data = zipData, lat = ~ lat, lng = ~ long, 
                   color = ~binColors2(login), radius = 3) %>%
  addLegend("bottomright",pal = binColors2, values = zipData$login,
            title = "Login Status Merck",
            opacity = 1)
m

m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -71.13, lat = 42.3, zoom = 1) %>%
  addMarkers(data = zipData, lat = ~lat, lng = ~long, 
             popup = pop) %>%
  addCircleMarkers(data = zipData, lat = ~ lat, lng = ~ long, 
                   color = ~binColors2(login), radius = 3) %>%
  addLegend("bottomright",pal = binColors2, values = zipData$login,
            title = "Login Status Merck",
            opacity = 1)
m


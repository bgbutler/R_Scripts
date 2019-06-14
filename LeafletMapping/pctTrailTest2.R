#testing alternative mapping with leaflet

library(rgdal)
library(maps)
library(htmltools)
library(devtools)
library(leaflet)
library(sp)
library(htmlwidgets)
library(plotKML)
library(maptools)
library(XML)

#get the GPS data
pct <- readOGR("pct.gpx", layer = "tracks")

head(pct[,c("lat", "lon")])

#unpack the file
pfile <- htmlTreeParse("pct.gpx",
                       error = function (...) {}, useInternalNodes = T)
# Get all elevations, times and coordinates via the respective xpath
#elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
#times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
# Extract latitude and longitude from the coordinates
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])
# Put everything in a dataframe and get rid of old variables
geodf <- data.frame(lat = lats, lon = lons)
rm(list=c("lats", "lons", "coords", "pfile"))
head(geodf)


#export the data as a CSV - name the file
write.csv(geodf, file = "K:/Sandbox/R/pctGEO.csv", row.names = FALSE)



#now make the map
your.map <- leaflet() %>%
  
  # Add layer
  addTiles(urlTemplate = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
  addMarkers(-116.4697, 32.60758, popup = "Campo") %>%
  addMarkers(-120.7816, 49.06465, popup = "Manning Park, Canada") %>%
  addCircleMarkers(data = geodf, lat = ~ lat, lng = ~ lon, 
                   color = "blue", radius = 1) %>%
  addPolygons(data=mapStates, fillColor = heat.colors(3, alpha = NULL), stroke = FALSE) %>%
  
  # Add legend
  addLegend(position = 'topright', colors = "red", labels = "PCT", opacity = 0.4,
            title = 'Legend')
your.map


#now make the standard map
your.map <- leaflet(pct) %>%
  
  # Add layer
  addTiles(urlTemplate = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
  addPolylines(color="red", popup="PCT")  %>%
  addMarkers(-116.4697, 32.60758, popup = "Campo") %>%
  addMarkers(-120.7816, 49.06465, popup = "Manning Park, Canada") %>%
  addPolygons(data=mapStates, fillColor = heat.colors(3, alpha = NULL), stroke = FALSE) %>%
  
  # Add legend
  addLegend(position = 'topright', colors = "red", labels = "PCT", opacity = 0.4,
            title = 'Legend')
your.map


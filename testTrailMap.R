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




#use the following to save a map
#saveWidget(widget = your.map, file="your_map.html", selfcontained = FALSE)

# Fetch the route of the PCT and convert it into a SpatialLine object
url <- "http://hiking.waymarkedtrails.org/en/routebrowser/1225378/gpx"
download.file(url, destfile = "pct.gpx", method = "wininet")

pct <- readOGR("pct.gpx", layer = "tracks")

app <- readOGR("appTrail.gpx", layer = "tracks", verbose=FALSE)

app <- readOGR("va.gpx", layer = "tracks", verbose=FALSE)

# Import list with shapefiles of the three states the PCT is crossing
mapStates <- map("state", fill = TRUE,
                 plot = FALSE,
                 region = c('california', 'oregon', 'washington:main'))


na.omit(geodf)

your.map <- leaflet() %>%
  
  # Add layer
  addTiles(urlTemplate = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
  addPolylines(data = geodf, lat = ~ lat, lng = ~ lon, color = "red")  %>%
  addMarkers(-116.4697, 32.60758, popup = "Campo") %>%
  addMarkers(-120.7816, 49.06465, popup = "Manning Park, Canada") %>%
  addPolygons(data=mapStates, fillColor = heat.colors(3, alpha = NULL), stroke = FALSE) %>%
  
  # Add legend
  addLegend(position = 'topright', colors = "red", labels = "PCT", opacity = 0.4,
            title = 'Legend')
your.map

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









your.map <- leaflet(pct) %>%

# Add tiles as baseGroup
addProviderTiles("OpenTopoMap", group = "MapQuestOpen.Aerial") %>%
  addProviderTiles("MapQuestOpen.Aerial", group = "MapQuestOpen.Aerial") %>%
  addProviderTiles("OpenMapSurfer.Roads", group = "OpenMapSurfer.Roads") %>%
  
  # Add layers as overlayGroup
  addPolylines(color="red", weight = 4,  popup="PCT", , group = "PCT")  %>%
  addMarkers(-116.4697, 32.60758, popup = "Campo", group="Southern Terminus") %>%
  addMarkers(-120.7816, 49.06465, popup = "Manning Park, Canada", group="Northern Terminus") %>%
  hideGroup("Southern Terminus") %>%
  hideGroup("Northern Terminus") %>%
  addPolygons(data=mapStates, fillColor = heat.colors(3, alpha = NULL), stroke = FALSE,
              group = "States")  %>%
  
  # Layers control
  addLayersControl(
    baseGroups = c("MapQuestOpen.Aerial", "OpenTopoMap", "OpenMapSurfer.Roads"),
    overlayGroups = c("PCT", "Southern Terminus", "Northern Terminus", "States"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

your.map

app <- readOGR("pa.gpx", layer = "tracks", verbose=FALSE)

lines <- app@lines

data <- readGPS("gpx","pa.gpx","w")

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


your.map <- leaflet() %>%
  
  # Add layer
  addProviderTiles("Stamen.Toner", group = "Tonor") %>% 
  addPolylines(data = geodf, lat = ~ lat, lng = ~ lon, color = "red")
                                                                                       
your.map









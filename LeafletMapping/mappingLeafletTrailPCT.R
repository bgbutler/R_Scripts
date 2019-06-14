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
library(RCurl)
library(stringi)
library(mapview)

install_github("environmentalinformatics-marburg/mapview", ref = "develop")

remove.packages("leaflet")

install_github('rstudio/leaflet')

pct <- readOGR("pct.gpx", layer = "tracks")


mapStates <- map("state", fill = TRUE,
                 plot = FALSE,
                 region = c('california', 'oregon', 'washington:main'))

your.map <- mapview(pct, map.types = "CartoDB.Positron")@map %>% 
  addMarkers(-116.4697, 32.60758, popup = "Campo") %>%
  addMarkers(-120.7816, 49.06465, popup = "Manning Park, Canada") %>%
  addPolygons(data=mapStates, fillColor = heat.colors(3, alpha = NULL), stroke = FALSE) %>%
  
  # Add legend
  addLegend(position = 'topright', colors = "red", labels = "PCT", opacity = 0.4,
            title = 'Legend')

your.map

x = as(pct, "SpatialLinesDataFrame")
mapView(x, burst = TRUE)

leaflet() %>% addTiles() %>% addPolylines(data = x)

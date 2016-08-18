#basic map of Disney Shanghai

#install packages for manipulating and plotting data
library(ggplot2)
library(maps)
library(plyr)
library(htmltools)
library(devtools)
library(leaflet)
library(htmlwidgets)


#put a point on the map
m <- leaflet() %>%
  addTiles() %>%
  addTiles()  %>%
  setView(lng = 121.6552, lat = 31.1456, zoom = 16)
m

setwd("Q:/Analytics/DisneyMap")
saveWidget(m,file = "disneyShanghai.html")

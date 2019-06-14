
#plot zipcode clusters in R

library(zipcode)
data(zipcode)
library(ggplot2)
library(maps)
library(plyr)


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

states_map <- map_data("state")

map <- ggplot(states_map, aes(x=long, y = lat, group = group))
map + geom_polygon(fill = "white", color = "black")

map <- ggplot(states_map, aes(x=long, y = lat, group = group))
map + geom_path() + coord_map("mercator")

newNames <- c("zip", "guid", "login","status", 
              "oldZip", "city", "state", "lat", "long")

colnames(zipData) <- newNames

mapData <- merge(states_map, zipData, by.x ="region", by.y="state")

mapData <- arrange(mapData, group, order)

map <- ggplot(mapData, aes(x = long.x, y = lat.x, fill = login))
map +   geom_polygon(color = "black") + 
  coord_map("mercator")




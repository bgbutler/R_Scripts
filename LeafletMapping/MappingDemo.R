# this is a mapping demo

# install packages for manipulating and plotting data
library(zipcode)
library(ggplot2)
library(maps)
library(dplyr)
library(htmltools)
library(devtools)
library(leaflet)
library(stringi)
library(RColorBrewer)

# the first part of the this script is for cleaning the data
# and appending lat and long to a zip code
# get the zipcode data
data(zipcode)

# get zipcode data and cleanse
url <- '~/Documents/RDatafiles/CSVs/PolicyListing.csv'
policyData <- read.csv(url, header = TRUE, sep=',', as.is = FALSE)

# convert the zipcodes to char
policyData$PostalCode <-as.character(policyData$PostalCode)

# apply the cleanup function
policyData$PostalCode <- clean.zipcodes(policyData$PostalCode)

# append the city, state and lat and lon of zip to the data
zipData <- merge(policyData, zipcode, by.x = 'PostalCode', by.y = 'zip')

# remove the extra columns City, Area, CountryISO
# we picked up clean city in zip code data
zipData <- subset(zipData, select = -c(City, Area, CountryISO))

# check for NA in zipcode
missing <- sum(is.na(zipData$PostalCode))
paste(missing, ' Zipcodes are Missing')

# now take a look at our data
head(zipData)

###############################################
# to prevent overstacking, need to jitter the data a little

# use the uniform from -1 to 1 and divide by 100
# create a function to do this

jitterFactor <- function(l){
    
    # l = 1
    l <- nrow(zipData)
    
    jF <- runif(l, min = -1, max = 1)
    jF <- jF/100
    
    return(jF)
}

# check the jitter function
# plot a histogram has zeros so change function

# now apply it to get new columns
# use randomness so that 0's don't overlap
zipData$JitterLat <- jitterFactor()
zipData$JitterLon <- jitterFactor()

# check the overlap
# should create a blank table
checkZeros <- zipData %>% 
    filter(JitterLat == 0 & JitterLon ==0) %>%
    select(JitterLat, JitterLon)

# apply the jitter
zipData$Lat <- zipData$latitude + zipData$JitterLat
zipData$Lon <- zipData$longitude + zipData$JitterLon

################################ 

# make a popup

# create a full popup
zipData$popUp <- paste('<strong>',zipData$Street, '</strong><br>',
                              'City: ', zipData$city, '<br>',
                              'YrBuilt = ', zipData$YearBuilt, '<br>',
                              'Construction = ', zipData$ConstructionCode, '<br>',
                              'Occupancy = ', zipData$OccupancyCode, '<br>',
                              'Premium = $' , formatC(zipData$Premium,4) , '<br>',
                              'GrossArea = ', formatC(zipData$GrossArea,0), '<br>', 
                              'RoofYr = ', formatC(zipData$RoofYearBuilt,0), '<br>')

# set some color scales
colorsConst <- colorFactor(palette = 'RdYlBu', zipData$ConstructionCode)
colorsOcc <- colorFactor(palette = 'Blues', zipData$OccupancyCode)
colorsLOB <- colorFactor(rainbow(7), zipData$LOB)


# create the map opbject

m <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$OpenStreetMap, group = 'Open SM')  %>%
    addProviderTiles(providers$Stamen.Toner, group = 'Toner')  %>%
    addProviderTiles(providers$CartoDB.Positron, group = 'CartoDB')  %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap, group = 'NG World') %>%
    setView(lng = -90, lat = 30, zoom = 10) %>%
    
    
    # construction
    addCircleMarkers(data = zipData, lat = ~Lat, lng = ~Lon,
                     color = ~colorsConst(ConstructionCode), popup = zipData$popUp,
                     radius = 3, group = 'Construction') %>%
    # occupancy
    addCircleMarkers(data = zipData, lat = ~Lat, lng = ~Lon, 
                     color = ~colorsOcc(OccupancyCode), popup = zipData$popUp,
                     radius = 3, group = 'Occupancy') %>%
    
    # LOB  
    addCircleMarkers(data = zipData, lat = ~Lat, lng = ~Lon, 
                     color = ~colorsLOB(LOB), popup = zipData$popUp,
                     radius = 3, group = 'LOB') %>%
    
    # layer control
    addLayersControl(
        baseGroups = c('Open SM', 'Toner', 'Carto DB', 'NG World'),

        overlayGroups = c('Construction',
                          'Occupancy',
                          'LOB'
        ),
        options = layersControlOptions(collapsed = F)
    ) %>%
    
    
    # construction        
    addLegend('bottomright', pal = colorsConst, values = zipData$ConstructionCode,
              title = 'Construction Code',
              opacity = 1) %>%
    # occupancy   
    addLegend('bottomleft', pal = colorsOcc, values = zipData$OccupancyCode,
              title = 'Occupancy',
              opacity = 1) %>%
    
    # LOB
    addLegend('topleft', pal = colorsLOB, values = zipData$LOB,
              title = 'LOB',
              opacity = 1)
    

m  # print the map



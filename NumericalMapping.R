# this is a mapping demo
# this will display numerical data

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
    
    l <- nrow(zipData)
    
    jF <- runif(l, min = -1, max = 1)
    jF <- jF/100
    
    return(jF)
}

# check the jitter function
# plot a histogram has zeros so change function
# do this with scripting

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

########################################
# cut some of the numerical data to get bins

# start with building value

# create tiv
zipData$tiv <- rowSums(zipData[,c('BuildingValue', 'ContentsValue',
                                  'OtherValue', 'TimeElementValue')])

zipData$tivLvl <- cut(zipData$tiv, 
    c(0,100000,200000,300000,400000, 10000000), include.lowest = T,
    labels = c('<$100K', '$100-200K', '$200-300K', '$300K-400K', '$400K+'))


# yr built taken from AIR cuts
zipData$yrBuiltLvl <- cut(zipData$YearBuilt, 
                            c(0,1994,2004,2018), include.lowest = F,
                            labels = c('Pre 1994', '1994 - 2004', '2004 - 2019'))

# roof yr built taken from AIR cuts
zipData$roofYrBuiltLvl <- cut(zipData$RoofYearBuilt, 
                          c(0,1994,2004,2018), include.lowest = F,
                          labels = c('Pre 1994', '1994 - 2004', '2004 - 2019'))


# create a full popup
zipData$popUp <- paste('<strong>',zipData$Street, '</strong><br>',
                       'TIV = $',prettyNum(zipData$tiv, big.mark = ',',preserve.width = 'none'), '<br>',
                       'City: ', zipData$city, '<br>',
                       'YrBuilt = ', zipData$YearBuilt, '<br>',
                       'Construction = ', zipData$ConstructionCode, '<br>',
                       'Occupancy = ', zipData$OccupancyCode, '<br>',
                       'Premium = $' , prettyNum(zipData$Premium, big.mark = ',',preserve.width = 'none') , '<br>',
                       'GrossArea = ', prettyNum(zipData$GrossArea, big.mark = ',', preserve.width = 'none'), '<br>', 
                       'RoofYr = ', zipData$RoofYearBuilt, '<br>')

# set some color scales for factors
colorsConst <- colorFactor(rainbow(4), zipData$ConstructionCode)
colorsOcc <- colorFactor(palette = 'Blues', zipData$OccupancyCode)
colorsLOB <- colorFactor(rainbow(7), zipData$LOB)

# color scales for numerical bins
colorstivValue <- colorFactor(palette = 'Accent', zipData$tivValueLvl)
colorsYrBuilt <- colorFactor(palette = 'Spectral', zipData$yrBuiltLvl)
colorsRoofYrBuilt <- colorFactor(palette = "YlOrRd", zipData$roofYrBuiltLvl)








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
                     radius = 5, group = 'Construction') %>%
    # tiv
    addCircleMarkers(data = zipData, lat = ~Lat, lng = ~Lon, 
                     color = ~colorstivValue(tivLvl), popup = zipData$popUp,
                     radius = ~tiv/20000, group = 'Bldg Value') %>%
    
    # year built  
    addCircleMarkers(data = zipData, lat = ~Lat, lng = ~Lon, 
                     color = ~colorsYrBuilt(yrBuiltLvl), popup = zipData$popUp,
                     radius = ~YearBuilt/250, group = 'Yr Built') %>%
    
    #layer control
    addLayersControl(
        baseGroups = c('Open SM', 'Toner', 'Carto DB', 'NG World'),
        
        overlayGroups = c('Construction',
                          'TIV',
                          'Yr Built'
        ),
        options = layersControlOptions(collapsed = F)
    ) %>%
    
    
    # construction        
    addLegend('bottomright', pal = colorsConst, values = zipData$ConstructionCode,
              title = 'Construction Code',
              opacity = 1) %>%
   
     # tiv 
    addLegend('bottomleft', pal = colorstivValue, values = zipData$tivLvl,
              title = 'TIV',
              opacity = 1) %>%
    
    # year built
    addLegend('topleft', pal = colorsYrBuilt, values = zipData$yrBuiltLvl,
              title = 'Yr Built',
              opacity = 1)


m  # Print the map





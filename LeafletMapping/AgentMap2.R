# mapping agency performance

# make sure that the directories are set
getwd()
setwd('N:/Bryan/Mapping')

library(RODBC)
library(DT)
library(leaflet)
library(zipcode)
library(reshape2)
library(RColorBrewer)
library(mapview)
library(dplyr)
library(randomcoloR)

zipData <- read.delim('AllZipData.tsv', 
                      stringsAsFactors = F, 
                      header = T, 
                      sep = '\t', 
                      as.is = T,
                      colClasses = c('character',
                                     'Date',
                                     'character',
                                     'character',
                                     'integer',
                                     'character',
                                     'character',
                                     'character',
                                     'character',
                                     'character',
                                     'numeric',
                                     'numeric'))



# make month and state factors
zipData$Month <- ordered(zipData$Month, levels = c("Jan",
                                                   "Feb",
                                                   "Mar",
                                                   "Apr",
                                                   "May",
                                                   "Jun",
                                                   "Jul",
                                                   "Aug"))
zipData$state <- as.factor(zipData$state)

# remove unneeded columns
zipDataRed <- zipData[,-c(2,4)]

# widen the data
data <- dcast(zipDataRed, ProducerZip + 
                          Producer + 
                          Month +
                          AgentName + 
                          city + 
                          state + 
                          latitude + 
                          longitude ~ 
                          Product, value.var='Count', fun.aggregate = length)

# resort it for viewing
data <- data %>% arrange(ProducerZip, Producer, Month)

# create the total column
data$Total <- data$Auto + data$Home

# remove the na
data <- na.omit(data)


# get distinct agents in the state
agentColor <- data %>% group_by(state, AgentName) %>% 
                       summarise(Count = n()) %>%
                       arrange(state, desc(Count))

agentCounts <- data %>% group_by(state) %>%
                        summarise(ColorCount = n_distinct(AgentName)) %>%
                        arrange(desc(ColorCount))


# merge the agents and number of needed colors
agentColorCounts <- left_join(agentColor, agentCounts, by = 'state')


# This makes a series of color palettes as character vectors
end <- nrow(agentCounts)
states <- as.character(agentCounts$state)
counts <- agentCounts$ColorCount
for (i in 1:end){
  colorVec = distinctColorPalette(k = counts[i], altCol = FALSE, runTsne = FALSE)
  assign(states[i], colorVec, envir = .GlobalEnv)
}


# make ci color index a function
colorInd <-  function(maxNum){
  ci <- as.integer(runif(n = 1, min = 1, max = maxNum))
  return(ci)}

maxColors <- agentColorCounts$ColorCount
agentColorCounts$ColorIndex <- NA
for (i in 1:nrow(agentColorCounts)){
agentColorCounts$ColorIndex[i] <- colorInd(agentColorCounts$ColorCount[i])
}
                                     
# pick the color from the appropriate index
agentColorCounts$state <- as.character(agentColorCounts$state)
agentColorCounts$Color <- NA
for (i in 1:nrow(agentColorCounts)){
  agentColorCounts$Color[i] <- get(agentColorCounts$state[i])[agentColorCounts$ColorIndex[i]]
}

# left join the colors
data$state <- as.character(data$state)
dataColor <- left_join(data, agentColorCounts, by = c('AgentName' = 'AgentName', 
                                                      'state' = 'state'))



# get last three months
currentMonth <- as.numeric(format(Sys.Date(), format = "%m"))
lvls <- levels(zipData$Month)
last3 <- tail(lvls, 3)
last <- dataColor %>% filter(Month %in% last3) %>% droplevels()
last <- na.omit(last)

# Add Jittering to the Zips so that they don't stack
l <- nrow(last)
jitterFactor <- function(l){
  jF <- runif(l, min = -1, max = 1)
  jF <- jF/100
  
  return(jF)
}

# use randomness so that 0's don't overlap
last$JitterLat <- jitterFactor(l)
last$JitterLon <- jitterFactor(l)

# apply the jitter
last$Lat <- last$latitude + last$JitterLat
last$Lon <- last$longitude + last$JitterLon


##################
# Create the metrics
# get the three month averages
MonthAvg <- last %>% dplyr::group_by(state) %>% 
                     dplyr::summarise(StateAvg = mean(Total),
                                      AutoAvg = mean(Auto),
                                      HomeAvg = mean(Home))

# join it by state
last <- left_join(last, MonthAvg, by = c('state' = 'state'))

##############
# create the metric factor bins
# create a factor for total
last$BeatTotal <- last$Total/last$StateAvg
last$BeatTotalLvl <- cut(last$BeatTotal, 
                       c(0,.5,1,2,3,5,100), include.lowest = T,
                       labels = c('<.5x', '.5-1x', '1-2x', '2-3x', '3-5x','5x+'))


last$BeatHome <- last$Home/last$HomeAvg
last$BeatHomeLvl <- cut(last$BeatHome, 
                         c(0,.5,1,2,3,5,100), include.lowest = T,
                         labels = c('<.5x', '.5-1x', '1-2x', '2-3x', '3-5x','5x+'))


last$BeatAuto <- last$Auto/last$AutoAvg
last$BeatAutoLvl <- cut(last$BeatAuto, 
                        c(0,.5,1,2,3,5,100), include.lowest = T,
                        labels = c('<.5x', '.5-1x', '1-2x', '2-3x', '3-5x','5x+'))




last$Popup <- paste('<strong>', last$AgentName,'</strong><br>',
                    'City: ', last$city,'<br>',
                    'Month: ', last$Month,'<br>',
                    'Home: ', last$Home, '<br>',
                    'Auto: ', last$Auto, '<br>',
                    'Total: ', last$Total, '<br>',
                    '<strong>','3 Mos State Avg','</strong><br>',
                    'Home Avg: ', formatC(last$HomeAvg,zero.print = T, format = 'd', digits = 2), '<br>',
                    'Auto Avg: ', formatC(last$AutoAvg,zero.print = T, format = 'd', digits = 2), '<br>',
                    'Total Avg: ', formatC(last$StateAvg,zero.print = T, format = 'd', digits = 2), '<br>',
                    '<strong>','Compare to State Avg','</strong><br>',
                    'Home : ', formatC(last$BeatHome,2), '<br>',
                    'Auto : ', formatC(last$BeatAuto,2), '<br>',
                    'Total : ', formatC(last$BeatTotal,2), '<br>')

# make different palettes
# main one
beatCol <- colorFactor(palette = 'RdYlGn', last$BeatTotalLvl)
last <- filter(last, state %in% c("CT","MA", "ME", "VT", "NH", "NY"))


# midAtlantic <- filter(last, state %in% c("NY", "NJ"))


Months <- split(last, last$Month)
list2env(Months, envir=.GlobalEnv)


######### MAKES A SINGLE MAP #############

m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM')  %>%
  addProviderTiles(providers$Stamen.Toner, group = 'Toner')  %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'NG World') %>%
  setView(lng = -72, lat = 41, zoom = 8) %>%
  
  # ORIGINAL WAY TO MAP THE AGENCY TOTALS
  # june - auto
  # addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
  #                  color = ~Jun$Color, popup = Jun$Popup,
  #                  radius = ~sqrt(Auto), group = 'Jun - Auto') %>%
  
  ######### AUTO
  # june - auto - compare
  addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatAutoLvl), popup = Jun$Popup,
                   radius = ~sqrt(BeatAuto*50), group = 'Auto - Jun') %>%
  
  
  # july - auto - compare
  addCircleMarkers(data = Jul, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatAutoLvl), popup = Jul$Popup,
                   radius = ~sqrt(BeatAuto*50), group = 'Auto - Jul') %>% 
  
  
  # aug - auto - compare
  addCircleMarkers(data = Aug, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatAutoLvl), popup = Aug$Popup,
                   radius = ~sqrt(BeatAuto*50), group = 'Auto - Aug') %>% 
  
  ########### HOME
  
  # june - Home - compare
  addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatHomeLvl), popup = Jun$Popup,
                   radius = ~sqrt(BeatHome*50), group = 'Home - Jun') %>%
  
  
  # july - Home - compare
  addCircleMarkers(data = Jul, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatHomeLvl), popup = Jul$Popup,
                   radius = ~sqrt(BeatHome*50), group = 'Home - Jul') %>% 
  
  
  # aug - Home - compare
  addCircleMarkers(data = Aug, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatHomeLvl), popup = Aug$Popup,
                   radius = ~sqrt(BeatHome*50), group = 'Home - Aug') %>% 
  
  
  ######### Total
  
  # june - Total - compare
  addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatTotalLvl), popup = Jun$Popup,
                   radius = ~sqrt(BeatTotal*50), group = 'Total - Jun') %>%
  
  
  # july - Total - compare
  addCircleMarkers(data = Jul, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatTotalLvl), popup = Jul$Popup,
                   radius = ~sqrt(BeatTotal*50), group = 'Total - Jul') %>% 
  
  
  # aug - Total - compare
  addCircleMarkers(data = Aug, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatTotalLvl), popup = Aug$Popup,
                   radius = ~sqrt(BeatTotal*50), group = 'Total - Aug') %>% 
  
  
  
  
  # layer control
  addLayersControl(
    baseGroups = c('Open SM', 'Toner', 'NG World'),
    
    overlayGroups = c('Auto - Jun',
                      'Auto - Jul',
                      'Auto - Aug',
                      'Home - Jun',
                      'Home - Jul',
                      'Home - Aug',
                      'Total - Jun', 
                      'Total - Jul',
                      'Total - Aug'),
    options = layersControlOptions(collapsed = F)
  ) %>%
  
  
  # set defaults to show by hiding groups
  hideGroup(c('Home - Jun',
              'Home - Jul',
              'Home - Aug',
              'Total - Jun', 
              'Total - Jul',
              'Total - Aug')) %>%
  
  


# legend for compare to average

addLegend('bottomright', pal = beatCol, values = last$BeatTotalLvl,
          title = 'Compare<br>Quote Count to<br>3Mos State Avg',
          opacity = 1)
m




############# MAKE TWO MAPS ONE FOR AUTO AND ONE FOR HOME ############


############
# Home

m1 <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM')  %>%
  addProviderTiles(providers$Stamen.Toner, group = 'Toner')  %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'NG World') %>%
  setView(lng = -72, lat = 41, zoom = 8) %>%
  

  # june - Home - compare
  addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatHomeLvl), popup = Jun$Popup,
                   radius = ~sqrt(BeatHome*50), group = 'Home - Jun') %>%
  
  
  # july - Home - compare
  addCircleMarkers(data = Jul, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatHomeLvl), popup = Jul$Popup,
                   radius = ~sqrt(BeatHome*50), group = 'Home - Jul') %>% 
  
  
  # aug - Home - compare
  addCircleMarkers(data = Aug, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatHomeLvl), popup = Aug$Popup,
                   radius = ~sqrt(BeatHome*50), group = 'Home - Aug') %>%   
  
  
  
  # layer control
  addLayersControl(
    baseGroups = c('Open SM', 'Toner', 'NG World'),
    
    overlayGroups = c('Home - Jun',
                      'Home - Jul',
                      'Home - Aug'
                      
    ),
    options = layersControlOptions(collapsed = F)
  ) %>%
  
  
  
  # legend for compare to average
  
  addLegend('bottomright', pal = beatCol, values = last$BeatTotalLvl,
            title = 'Compare Home<br>Quote Count to<br>3Mos State Avg',
            opacity = 1)

###############
# auto

m2 <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM')  %>%
  addProviderTiles(providers$Stamen.Toner, group = 'Toner')  %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'NG World') %>%
  setView(lng = -72, lat = 41, zoom = 8) %>%
  
  
  # june - auto - compare
  addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatAutoLvl), popup = Jun$Popup,
                   radius = ~sqrt(BeatAuto*50), group = 'Auto - Jun') %>%
  
  
  # july - auto - compare
  addCircleMarkers(data = Jul, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatAutoLvl), popup = Jul$Popup,
                   radius = ~sqrt(BeatAuto*50), group = 'Auto - Jul') %>% 
  
  
  # aug - auto - compare
  addCircleMarkers(data = Aug, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatAutoLvl), popup = Aug$Popup,
                   radius = ~sqrt(BeatAuto*50), group = 'Auto - Aug') %>% 
  
  
  
  
  # layer control
  addLayersControl(
    baseGroups = c('Open SM', 'Toner', 'NG World'),
    
    overlayGroups = c('Auto - Jun',
                      'Auto - Jul',
                      'Auto - Aug'

                      
    ),
    options = layersControlOptions(collapsed = F)
  ) %>%
  
  
  
  # legend for compare to average
  
  addLegend('bottomright', pal = beatCol, values = last$BeatTotalLvl,
            title = 'Compare Auto<br>Quote Count to<br>3Mos State Avg',
            opacity = 1)


mapview::latticeView(m1, m2, ncol = 2, 
                     sync = list(c(1, 2)), 
                     sync.cursor = FALSE, 
                     no.initial.sync = FALSE)

  


###################  SINGLE MAP ALTERNATE COLORS ######
###### USE ALTERNATE COLOR SCALES FOR HOME AND AUTO

# home
beatHomeCol <- colorFactor(palette = 'Blues', last$BeatHomeLvl)

# auto
beatAutoCol <- colorFactor(palette = 'Greens', last$BeatAutoLvl)



m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM')  %>%
  addProviderTiles(providers$Stamen.Toner, group = 'Toner')  %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'NG World') %>%
  setView(lng = -72, lat = 41, zoom = 8) %>%
  
  # ORIGINAL WAY TO MAP THE AGENCY TOTALS
  # june - auto
  # addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
  #                  color = ~Jun$Color, popup = Jun$Popup,
  #                  radius = ~sqrt(Auto), group = 'Jun - Auto') %>%
  
  ######### AUTO
  # june - auto - compare
  addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
                   color = ~beatAutoCol(BeatAutoLvl), popup = Jun$Popup,
                   radius = ~sqrt(BeatAuto*50), group = 'Auto - Jun') %>%
  
  
  # july - auto - compare
  addCircleMarkers(data = Jul, lat = ~Lat, lng = ~Lon,
                   color = ~beatAutoCol(BeatAutoLvl), popup = Jul$Popup,
                   radius = ~sqrt(BeatAuto*50), group = 'Auto - Jul') %>% 
  
  
  # aug - auto - compare
  addCircleMarkers(data = Aug, lat = ~Lat, lng = ~Lon,
                   color = ~beatAutoCol(BeatAutoLvl), popup = Aug$Popup,
                   radius = ~sqrt(BeatAuto*50), group = 'Auto - Aug') %>% 
  
  ########### HOME
  
  # june - Home - compare
  addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
                   color = ~beatHomeCol(BeatHomeLvl), popup = Jun$Popup,
                   radius = ~sqrt(BeatHome*50), group = 'Home - Jun') %>%
  
  
  # july - Home - compare
  addCircleMarkers(data = Jul, lat = ~Lat, lng = ~Lon,
                   color = ~beatHomeCol(BeatHomeLvl), popup = Jul$Popup,
                   radius = ~sqrt(BeatHome*50), group = 'Home - Jul') %>% 
  
  
  # aug - Home - compare
  addCircleMarkers(data = Aug, lat = ~Lat, lng = ~Lon,
                   color = ~beatHomeCol(BeatHomeLvl), popup = Aug$Popup,
                   radius = ~sqrt(BeatHome*50), group = 'Home - Aug') %>% 
  
  
  ######### Total
  
  # june - Total - compare
  addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatTotalLvl), popup = Jun$Popup,
                   radius = ~sqrt(BeatTotal*50), group = 'Total - Jun') %>%
  
  
  # july - Total - compare
  addCircleMarkers(data = Jul, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatTotalLvl), popup = Jul$Popup,
                   radius = ~sqrt(BeatTotal*50), group = 'Total - Jul') %>% 
  
  
  # aug - Total - compare
  addCircleMarkers(data = Aug, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatTotalLvl), popup = Aug$Popup,
                   radius = ~sqrt(BeatTotal*50), group = 'Total - Aug') %>% 
  
  
  
  
  # layer control
  addLayersControl(
    baseGroups = c('Open SM', 'Toner', 'NG World'),
    
    overlayGroups = c('Auto - Jun',
                      'Auto - Jul',
                      'Auto - Aug',
                      'Home - Jun',
                      'Home - Jul',
                      'Home - Aug',
                      'Total - Jun', 
                      'Total - Jul',
                      'Total - Aug'),
    options = layersControlOptions(collapsed = F)
  ) %>%
  
  
  # set defaults to show by hiding groups
  hideGroup(c('Home - Jun',
              'Home - Jul',
              'Home - Aug',
              'Total - Jun', 
              'Total - Jul',
              'Total - Aug')) %>%
  
  
  
  
  # legend for compare to average
  
addLegend('bottomright', pal = beatCol, values = last$BeatTotalLvl,
            title = 'Compare<br>Tot Quote Count to<br>3Mos State Avg',
            opacity = 1) %>%


addLegend('bottomleft', pal = beatHomeCol, values = last$BeatTotalLvl,
          title = 'Compare Home<br>Quote Count to<br>3Mos State Avg',
          opacity = 1) %>%


addLegend('bottomleft', pal = beatAutoCol, values = last$BeatTotalLvl,
          title = 'Compare Auto<br>Quote Count to<br>3Mos State Avg',
          opacity = 1)

m  











######## ALTERNATE COLOR SCALES FOR MONTHS
############# MAKE TWO MAPS USING DIFFERENT SCALES FOR MONTHS ############


# MONTH 1
# month1Col <- colorFactor(palette = 'Oranges', last$BeatHomeLvl)
month1Col <- colorFactor(palette = 'Blues', last$BeatHomeLvl)

# MONTH 2
month2Col <- colorFactor(palette = 'Greens', last$BeatAutoLvl)

# MONTH 3
month3Col <- colorFactor(palette = 'Reds', last$BeatAutoLvl)

# Hanover palette
# month3Col <- colorFactor(palette = rev(gray.colors(6)), last$BeatAutoLvl)


# Home

m1 <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM')  %>%
  addProviderTiles(providers$Stamen.Toner, group = 'Toner')  %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'NG World') %>%
  setView(lng = -72, lat = 41, zoom = 8) %>%
  
  
  # june - Home - compare
  addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
                   color = ~month1Col(BeatHomeLvl), popup = Jun$Popup,
                   radius = ~sqrt(BeatHome*50), group = 'Home - Jun') %>%
  
  
  # july - Home - compare
  addCircleMarkers(data = Jul, lat = ~Lat, lng = ~Lon,
                   color = ~month2Col(BeatHomeLvl), popup = Jul$Popup,
                   radius = ~sqrt(BeatHome*50), group = 'Home - Jul') %>% 
  
  
  # aug - Home - compare
  addCircleMarkers(data = Aug, lat = ~Lat, lng = ~Lon,
                   color = ~month3Col(BeatHomeLvl), popup = Aug$Popup,
                   radius = ~sqrt(BeatHome*50), group = 'Home - Aug') %>%   
  
  
  
  # layer control
  addLayersControl(
    baseGroups = c('Open SM', 'Toner', 'NG World'),
    
    overlayGroups = c('Home - Jun',
                      'Home - Jul',
                      'Home - Aug'
                      
    ),
    options = layersControlOptions(collapsed = F)
  ) %>%
  
  
  
  # legend for compare to average
  
  addLegend('topleft', pal = month1Col, values = last$BeatTotalLvl,
            title = 'June Quote Count<br>to 3Mos State Avg',
            opacity = 1) %>%

  addLegend('topleft', pal = month2Col, values = last$BeatTotalLvl,
            title = 'Jul Quote Count<br>to 3Mos State Avg',
            opacity = 1) %>%  
  
  addLegend('bottomright', pal = month3Col, values = last$BeatTotalLvl,
            title = 'Aug Quote Count<br>to 3Mos State Avg',
            opacity = 1) 
  
  
  
###############
# auto

m2 <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM')  %>%
  addProviderTiles(providers$Stamen.Toner, group = 'Toner')  %>%
  
  # this is topo map Esri.NatGeoWorldMap
  addProviderTiles(providers$Esri.WorldImagery, group = 'NG World') %>%
  setView(lng = -72, lat = 41, zoom = 8) %>%
  
  
  # june - auto - compare
  addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
                   color = ~month1Col(BeatAutoLvl), popup = Jun$Popup,
                   radius = ~sqrt(BeatAuto*50), group = 'Auto - Jun') %>%
  
  
  # july - auto - compare
  addCircleMarkers(data = Jul, lat = ~Lat, lng = ~Lon,
                   color = ~month2Col(BeatAutoLvl), popup = Jul$Popup,
                   radius = ~sqrt(BeatAuto*50), group = 'Auto - Jul') %>% 
  
  
  # aug - auto - compare
  addCircleMarkers(data = Aug, lat = ~Lat, lng = ~Lon,
                   color = ~month3Col(BeatAutoLvl), popup = Aug$Popup,
                   radius = ~sqrt(BeatAuto*50), group = 'Auto - Aug') %>% 
  
  
  
  
  # layer control
  addLayersControl(
    baseGroups = c('Open SM', 'Toner', 'NG World'),
    
    overlayGroups = c('Auto - Jun',
                      'Auto - Jul',
                      'Auto - Aug'
                      
                      
    ),
    options = layersControlOptions(collapsed = F)
  ) %>%
  
  
  
  # legend for compare to average
  
  addLegend('topleft', pal = month1Col, values = last$BeatTotalLvl,
            title = 'June Quote Count<br>to 3Mos State Avg',
            opacity = 1) %>%
  
  addLegend('topleft', pal = month2Col, values = last$BeatTotalLvl,
            title = 'July Quote Count<br>to 3Mos State Avg',
            opacity = 1) %>%  
  
  addLegend('bottomright', pal = month3Col, values = last$BeatTotalLvl,
            title = 'Aug Quote Count<br>to 3Mos State Avg',
            opacity = 1) 


mapview::latticeView(m1, m2, ncol = 2, 
                     sync = list(c(1, 2)), 
                     sync.cursor = FALSE, 
                     no.initial.sync = FALSE)






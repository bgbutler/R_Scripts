

#### for mapping and age analysis


# make sure that the directories are se
library(RODBC)
library(plyr)
library(dplyr)
library(DT)
library(reshape2)
library(knitr)
library(ggplot2)
library(scales)
library(PerformanceAnalytics)
library(leaflet)
library(zipcode)
library(readxl)
library(data.table)


getwd()
setwd('N:/Bryan/Markov')

# get the file

data <- read.csv('claimsMappingData.csv', stringsAsFactors = F, header = T, sep = ',')
head(data)

str(data)
colnames(data)

# clean and format the transaction date
data$TRANS_DATE <- as.character(data$SRC_ACCT_PRD)
data$TRANS_DATE <- paste(substr(data$SRC_ACCT_PRD, 1,4), substr(data$SRC_ACCT_PRD,5,6), sep= '-')
data$TRANS_DATE <- paste(data$TRANS_DATE, '-01', sep = "")

# make it a date
data$TRANS_DATE <- as.Date(data$TRANS_DATE, format = '%Y-%m-%d')
data$DT_OF_LOSS <- as.Date(data$DT_OF_LOSS, format = '%Y-%m-%d')

# create loss year
# create the period from the transaction
data$LOSS_YR <- as.numeric(format(data$DT_OF_LOSS, format = "%Y"))
data$TRANS_YR <- as.numeric(format(data$TRANS_DATE, format = "%Y"))

# Create the year diff
data$DEV_YEARS <- data$TRANS_YR - data$LOSS_YR 


# cut the data at 2013
dataRed <- data %>% filter(LOSS_YR > 2012 & LOSS_YR < 2019)


# create the bins to hold incurred
dataRed$DEV_MONTHS <- ifelse(dataRed$DEV_YEARS < 1, '12 Mos',
                             ifelse(dataRed$DEV_YEARS < 2, '24 Mos','36 Mos'))

# create th bins to hold features
dataRed$FTR_MONTHS <- ifelse(dataRed$DEV_YEARS < 1, '12 Mos',
                             ifelse(dataRed$DEV_YEARS < 2, '24 Mos','36 Mos'))


dataRed <- dataRed %>% arrange(CLM_NBR)

# roll it up by claim number
# select the data
claim <- dataRed %>% select(CLM_NBR,
                               POL_RATE_ST_ABBR_CD,
                               ACDNT_ZIP_CD,
                               LOSS_YR,
                               DEV_MONTHS,
                               FTR_MONTHS,
                               FTR_INCUR_INCL_SLVG_SUBRO_AMT,
                               FTR_PDLS_INCL_SLVG_SUBRO_AMT,
                               CLM_FTR_NBR)

# make loss year a factor
claim$LOSS_YR <- as.character(claim$LOSS_YR)
claim$LOSS_YR <- ordered(claim$LOSS_YR, levels = c('2013',
                                                   '2014',
                                                   '2015',
                                                   '2016',
                                                   '2017',
                                                   '2018'))

# roll up the claims to aggregate them
claimAgg <- claim %>% group_by(CLM_NBR, ACDNT_ZIP_CD) %>%
                        summarize(
                        TOT_INC = sum(FTR_INCUR_INCL_SLVG_SUBRO_AMT),
                        FTR_TOT = n_distinct(CLM_FTR_NBR))


# joing the zip code data
data(zipcode)
claimAgg$ACDNT_ZIP_CD <- clean.zipcodes(claimAgg$ACDNT_ZIP_CD)

#  append the data to the zipcode
zipData <- merge(claimAgg, zipcode, by.x = 'ACDNT_ZIP_CD', by.y = 'zip')

# change city and state columns names for consistency
zipData <- zipData %>% select_all(toupper)


# group the remaining
claimRoll <- claim %>% group_by(CLM_NBR,
                              POL_RATE_ST_ABBR_CD,
                              ACDNT_ZIP_CD,
                              LOSS_YR,
                              DEV_MONTHS,
                              FTR_MONTHS) %>%
                    summarize(
                      TOT_INC = sum(FTR_INCUR_INCL_SLVG_SUBRO_AMT),
                      TOT_FTRS = n_distinct(CLM_FTR_NBR)
                    )


# use the data table method
claimWide <- dcast(setDT(claimRoll),
                   CLM_NBR + 
                   POL_RATE_ST_ABBR_CD + 
                   LOSS_YR ~ 
                   DEV_MONTHS, 
                   value.var = c('TOT_INC', 'TOT_FTRS'), 
                   fun.aggregate = sum)


claimZip <- merge(zipData, claimWide, by.x = 'CLM_NBR', by.y = 'CLM_NBR')

# make the aggregate columns for incurred
claimZip$TOT_INC_12_MOS <- claimZip$`TOT_INC_12 Mos`
claimZip$TOT_INC_24_MOS <- claimZip$`TOT_INC_12 Mos` + claimZip$`TOT_INC_24 Mos`
claimZip$TOT_INC_36_MOS <- claimZip$`TOT_INC_12 Mos` + claimZip$`TOT_INC_24 Mos` + claimZip$`TOT_INC_36 Mos`


# make the aggregate columns for features
claimZip$TOT_FTRS_12_MOS <- claimZip$`TOT_FTRS_12 Mos`
claimZip$TOT_FTRS_24_MOS <- claimZip$`TOT_FTRS_12 Mos` + claimZip$`TOT_FTRS_24 Mos`
claimZip$TOT_FTRS_36_MOS <- claimZip$`TOT_FTRS_12 Mos` + claimZip$`TOT_FTRS_24 Mos` + claimZip$`TOT_FTRS_36 Mos`


# Add Jittering to the Zips so that they don't stack
jitterFactor <- function(l){
  
  # l = 1
  l <- nrow(zipData)
  
  jF <- runif(l, min = -1, max = 1)
  jF <- jF/100
  
  return(jF)
}

# use randomness so that 0's don't overlap
claimZip$JitterLat <- jitterFactor()
claimZip$JitterLon <- jitterFactor()

# apply the jitter
claimZip$LAT <- claimZip$LATITUDE + claimZip$JitterLat
claimZip$LON <- claimZip$LONGITUDE + claimZip$JitterLon

# make the loss bin
claimZip$LOSS_BIN <- cut(claimZip$TOT_INC,
                         c(0,75000,150000,200000,300000, 500000, 10000000), include.lowest = F,
                         labels = c('<75K', 
                                    '75 - 150K', 
                                    '150 - 200K', 
                                    '200- 300K', 
                                    '300 - 500K',
                                    '500K+'))

#  bins for features
claimZip$FTR_BIN <- cut(claimZip$FTR_TOT,
                         c(0,1,3,5, 10, 50), include.lowest = F,
                         labels = c('1', 
                                    '1 - 3', 
                                    '3 - 5', 
                                    '5- 10',
                                    '10+'))



# check out of state
claimZip$OUT_STATE <- ifelse(claimZip$STATE == claimZip$POL_RATE_ST_ABBR_CD, 'InState','OutofState')



# need to remove 24, 36 months for 2017, 2018
# adjust the loss dev
claimZip$TOT_INC_36_MOS <- ifelse(claimZip$LOSS_YR > 2016, NA, claimZip$TOT_INC_36_MOS)
claimZip$TOT_INC_24_MOS <- ifelse(claimZip$LOSS_YR > 2017, NA, claimZip$TOT_INC_24_MOS)

# adjust the features
claimZip$TOT_FTRS_36_MOS <- ifelse(claimZip$LOSS_YR > 2016, NA, claimZip$TOT_FTRS_36_MOS)
claimZip$TOT_FTRS_24_MOS <- ifelse(claimZip$LOSS_YR > 2017, NA, claimZip$TOT_FTRS_24_MOS)

# make the popup
claimZip$POPUP <- paste('<strong>',claimZip$LOSS_YR, '</strong><br>',
                        'Pol State: ', claimZip$POL_RATE_ST_ABBR_CD, '<br>',
                        'Acc City: ', claimZip$CITY, '<br>',
                        'InState: ', claimZip$OUT_STATE, '<br>',
                        'Tot Inc: ', formatC(claimZip$TOT_INC,0, format = "f", big.mark = ','), '<br>',
                        '<strong>','Loss Development','</strong><br>',
                        '12 Mos: ', formatC(claimZip$TOT_INC_12_MOS,0, format = "f", big.mark = ','), '<br>',
                        '24 Mos: ', formatC(claimZip$TOT_INC_24_MOS,0, format = "f", big.mark = ','), '<br>',
                        '36 Mos: ', formatC(claimZip$TOT_INC_36_MOS,0, format = "f", big.mark = ','), '<br>',
                        '<strong>','Feature Development','</strong><br>',
                        '12 Mos: ', claimZip$TOT_FTRS_12_MOS, '<br>',
                        '24 Mos: ', claimZip$TOT_FTRS_24_MOS, '<br>',
                        '36 Mos: ', claimZip$TOT_FTRS_36_MOS, '<br>')


# create a few color schemes
# Set a palette
hanover <- c('green', 'orange', 'light blue', 'grey', 'blue')

colorState <- colorFactor(hanover, claimZip$OUT_STATE)
colorClaim <- colorFactor(palette = rev(heat.colors(6)), claimZip$LOSS_BIN)
colorYear <- colorFactor(palette = rev(rainbow(6)), claimZip$LOSS_YR)
# colorYear <- colorFactor(palette = rev(heat.colors(6)), claimZip$LOSS_YR)

# create the map opbject
# filter all tot losses < 10K
pltZip <- claimZip %>% filter(TOT_INC > 99999)


m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM')  %>%
  addProviderTiles(providers$Stamen.Toner, group = 'Toner')  %>%
  addProviderTiles(providers$CartoDB.Positron, group = 'CartoDB')  %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = 'Terrain') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
  setView(lng = -77, lat = 41, zoom = 8) %>%
  
  
  # total incurred
  addCircleMarkers(data = pltZip, lat = ~LAT, lng = ~LON,
                   color = ~colorYear(LOSS_YR), popup = pltZip$POPUP,
                   radius = ~sqrt(TOT_INC)/20, group = 'Loss') %>%
  
  # tot Features
  addCircleMarkers(data = pltZip, lat = ~LAT, lng = ~LON,
                   color = ~colorYear(LOSS_YR), popup = pltZip$POPUP,
                   radius = ~FTR_TOT^2, group = 'Features') %>%
  
  # in state
  addCircleMarkers(data = pltZip, lat = ~LAT, lng = ~LON,
                 color = ~colorState(OUT_STATE), popup = pltZip$POPUP,
                 radius = 5, group = 'In State') %>%
  
  # 12 mos
  addCircleMarkers(data = pltZip, lat = ~LAT, lng = ~LON,
                   color = ~colorYear(LOSS_YR), popup = pltZip$POPUP,
                   radius = ~sqrt(TOT_INC_12_MOS)/20, group = '12 Mos') %>%
  
  # 24 mos
  addCircleMarkers(data = pltZip, lat = ~LAT, lng = ~LON,
                   color = ~colorYear(LOSS_YR), popup = pltZip$POPUP,
                   radius = ~sqrt(TOT_INC_24_MOS)/20, group = '24 Mos') %>%
  
  # 36 mos
  addCircleMarkers(data = pltZip, lat = ~LAT, lng = ~LON,
                   color = ~colorYear(LOSS_YR), popup = pltZip$POPUP,
                   radius = ~sqrt(TOT_INC_36_MOS)/20, group = '36 Mos') %>%


#layer control
addLayersControl(
  baseGroups = c('Open SM', 'Toner', 'Carto DB', 'Terrain', 'Satellite'),
  
  overlayGroups = c('Loss',
                    'Features',
                    'In State',
                    '-- Loss Dev --',
                    '12 Mos',
                    '24 Mos',
                    '36 Mos'
  ),
  options = layersControlOptions(collapsed = F)
) %>%
  
  hideGroup(c('-- Loss Dev --', 'Features', 'In State', '12 Mos', '24 Mos', '36 Mos')) %>%
 
# Legends


  # state       
  addLegend('bottomleft', pal = colorState, values = pltZip$OUT_STATE,
            title = 'In State',
            opacity = 1) %>%
  
  # claim bin
  addLegend('topleft', pal = colorYear, values = pltZip$LOSS_YR,
            title = 'Acc Year',
            opacity = 1)


m  # Print the map







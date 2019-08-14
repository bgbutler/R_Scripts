

library(RODBC)
library(tidyverse)
library(DT)
library(leaflet)
library(zipcode)
library(reshape2)
library(RColorBrewer)

library(dplyr)
library(zipcode)



# getwd()
setwd('N:/Bryan/Marketing')
dataURL <- 'NPSDataRaw.csv'


# write.csv(agentData, 'agentZips.csv', row.names = FALSE)

# read the files from csv
nps <- read.csv(dataURL, stringsAsFactors = F)


# function for dealing with strength
convertComments <- function(x) {as.numeric(ifelse(x == 'Strongly Agree',  10, ifelse(
  x == 'Agree', 8, ifelse(x == 'Neutral', 5,ifelse(
    x == 'Disagree', 3,ifelse(x == 'Strongly Disagree', 1,NA))))))
}

# create the promoter field
nps$Promoter <- ifelse(nps$LikelyRecommend > 8, 'Promoter', ifelse(
  nps$LikelyRecommend > 6 & nps$LikelyRecommend < 9, 'Passive', 'Detractor'))

# order it
nps$Promoter <- ordered(nps$Promoter, levels = c("Detractor",
                                                         "Passive",
                                                         "Promoter"))
table(nps$Promoter)

# make role bins
nps$RoleGroup <- ifelse(nps$PrimaryRole == "Producer", "FrontLine", 
                        ifelse(nps$PrimaryRole == "Account Manager/CSR", "FrontLine",
                        ifelse(nps$PrimaryRole ==  "Administrative Assistant","Admin",
                        ifelse(nps$PrimaryRole ==  "Claims","FrontLine",
                        ifelse(nps$PrimaryRole ==  "LOB Manager","Admin",
                        ifelse(nps$PrimaryRole ==  "Marketing","MktgSales",
                        ifelse(nps$PrimaryRole ==  "Office Manager","Admin",
                        ifelse(nps$PrimaryRole ==  "Operations Manager","Admin",
                        ifelse(nps$PrimaryRole ==  "Principal","Exec Mgt",
                        ifelse(nps$PrimaryRole ==  "Producer","FrontLine",
                        ifelse(nps$PrimaryRole ==  "Sales Manager","MktgSales",
                        ifelse(nps$PrimaryRole ==  "Senior Executive","Exec Mgt",
                                                   "Admin"))))))))))))

# convert the columns to numbers
# apply this function to the recommend Questions
for (i in seq(6, 24, by=2)) {nps[,i] <- convertComments(nps[,i])}

# get the relevant data
npsData <- nps %>% dplyr::select(EnrolledinCSC, 
                                 AgentName, 
                                 LikelyRecommend, 
                                 RoleGroup,
                                 PrimaryRole, 
                                 Promoter, 
                                 Q2_RangeofProducts, 
                                 Q3_QuotePlatformEasy, 
                                 Q4_FieldSalesRepRegVisits, 
                                 Q5_UWResponsive, 
                                 Q6_ProcessHomeRepCostAccurate, 
                                 Q7_HomeEvalHassleFree, 
                                 Q8_BillingEasyUnderstand, 
                                 Q9_AgencySupCenExcellent, 
                                 Q10_ClaimServFairResponsive, 
                                 Q11_NBRenPricingStable, 
                                 AgentCode, 
                                 Zip)



# make postal code a character
agentData$PRDCNG_AGNT_FULL_NM <- as.character(agentData$PRDCNG_AGNT_FULL_NM)
agentData$PRDCNG_AGNT_ADDR_POSTL_CD <- as.character(agentData$PRDCNG_AGNT_ADDR_POSTL_CD)
agentData$PRDCNG_AGNT_CD <- as.character(agentData$PRDCNG_AGNT_CD)

# map the zipcode 
data(zipcode)
agentData$PRDCNG_AGNT_ADDR_POSTL_CD <- clean.zipcodes(agentData$PRDCNG_AGNT_ADDR_POSTL_CD)
npsData$Zip <- clean.zipcodes(npsData$Zip)


#  append the data to the zipcode
zipData <- merge(agentData, zipcode, by.x = 'PRDCNG_AGNT_ADDR_POSTL_CD', by.y = 'zip')
#  append the data to the zipcode
npsDataClean <- npsData %>% filter(!is.na(npsData$AgentCode))

mapData <- merge(npsDataClean, zipcode, by.x = 'Zip', by.y = 'zip')



# check the na
# sum(is.na(npsZip$latitude))
# sum(is.na(zipData$PRDCNG_AGNT_ADDR_POSTL_CD))

# clean up agent data for map
# npsData$AgentCode <- as.character(npsData$AgentCode)

# map postal code
# mapData <- left_join(npsDataClean, zipData, by = c("AgentCode" = "PRDCNG_AGNT_CD"))

# remove the na
sum(is.na(mapData$AgentCode))
sum(is.na(mapData$latitude))



# create factors
mapData$EnrolledinCSC <- as.factor(mapData$EnrolledinCSC)
mapData$RoleGroup <- as.factor(mapData$RoleGroup)
mapData$PrimaryRole <- NULL


# make some color palettes
cscPal <- c("darkseagreen1", "deepskyblue")
rolePal <- c("dark green", "orange", "dimgray", "light blue")


colorPromo <- colorFactor(palette = 'RdYlGn', mapData$Promoter)
colorCSC <- colorFactor(palette = cscPal, mapData$EnrolledinCSC)
colorRole <- colorFactor(palette = rolePal, mapData$RoleGroup)
colorQuestions <- colorFactor(palette = "RdYlGn", mapData$Q2_RangeofProducts)

# Add Jittering to the Zips so that they don't stack
l <- nrow(mapData)
jitterFactor <- function(l){
  jF <- runif(l, min = -1, max = 1)
  jF <- jF/100
  
  return(jF)
}

# use randomness so that 0's don't overlap
mapData$JitterLat <- jitterFactor(l)
mapData$JitterLon <- jitterFactor(l)

# alternate jitter works very well
# mapData$TestJitter <- jitter(mapData$latitude, amount = .01)

# apply the jitter
mapData$Lat <- mapData$latitude + mapData$JitterLat
mapData$Lon <- mapData$longitude + mapData$JitterLon


# make the popup
mapData$Popup <- paste('<strong>', mapData$AgentName,'</strong><br>',
                    'Score: ', mapData$LikelyRecommend,'<br>',
                    'Promoter: ', mapData$Promoter,'<br>',
                    'Role: ', mapData$RoleGroup, '<br>',
                    'CSC: ', mapData$EnrolledinCSC, '<br>',
                    'City: ', mapData$city, '<br>',
                    '<strong>','____________','</strong><br>',
                    'RangeofProducts: ', mapData$Q2_RangeofProducts, '<br>',
                    'QuotePlatformEasy: ', mapData$Q3_QuotePlatformEasy, '<br>',
                    'FieldSalesRepVisits: ', mapData$Q4_FieldSalesRepRegVisits, '<br>',
                    'UW Responsive : ', mapData$Q5_UWResponsive, '<br>',
                    'HomeRepCostAcc : ', mapData$Q6_ProcessHomeRepCostAccurate, '<br>',
                    'HomeEvalHassleFree : ', mapData$Q7_HomeEvalHassleFree, '<br>',
                    'BillingEasyUnderstand : ', mapData$Q8_BillingEasyUnderstand, '<br>',
                    'AgencySupCenExc : ', mapData$Q9_AgencySupCenExcellent, '<br>',
                    'ClaimSvcFairResponsive : ',mapData$Q10_ClaimServFairResponsive, '<br>',
                    'NBRenPricStable : ', mapData$Q11_NBRenPricingStable, '<br>')



m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM')  %>%
  addProviderTiles(providers$Stamen.Toner, group = 'Toner')  %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'NG World') %>%
  setView(lng = -72, lat = 41, zoom = 6) %>%
  
  # promoter layer
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~colorPromo(Promoter), popup = mapData$Popup,
                   radius = 5, group = 'Promoter') %>%

  # role layer
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~colorRole(RoleGroup), popup = mapData$Popup,
                   radius = 5, group = 'Role') %>%
  
  
  # csc layer
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~colorCSC(EnrolledinCSC), popup = mapData$Popup,
                   radius = 5, group = 'CSC') %>%
  
  
  ########   ADDA ALL OF THE SCORES ########

  # Q2
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                 color = ~colorQuestions(Q2_RangeofProducts), popup = mapData$Popup,
                 radius = 5, group = 'Prod Range') %>%
  
  # Q3
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~colorQuestions(Q3_QuotePlatformEasy), popup = mapData$Popup,
                   radius = 5, group = 'Platform') %>%
  
  # Q4
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~colorQuestions(Q4_FieldSalesRepRegVisits), popup = mapData$Popup,
                   radius = 5, group = 'Field Sales') %>%
  
  # Q5
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~colorQuestions(Q5_UWResponsive), popup = mapData$Popup,
                   radius = 5, group = 'UW Responsive') %>%
  
  # Q6
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~colorQuestions(Q6_ProcessHomeRepCostAccurate), popup = mapData$Popup,
                   radius = 5, group = 'Home Rep Cost') %>%
  
  # Q7
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~colorQuestions(Q7_HomeEvalHassleFree), popup = mapData$Popup,
                   radius = 5, group = 'Home Eval Proc') %>%
  
  # Q8
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~colorQuestions(Q8_BillingEasyUnderstand), popup = mapData$Popup,
                   radius = 5, group = 'Billing Ease') %>%
  
  # Q9
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~colorQuestions(Q9_AgencySupCenExcellent), popup = mapData$Popup,
                   radius = 5, group = 'Agt Sup Cen') %>%
  
  # Q10
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~colorQuestions(Q10_ClaimServFairResponsive), popup = mapData$Popup,
                   radius = 5, group = 'Claim Service') %>%
  
  # Q11
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~colorQuestions(Q11_NBRenPricingStable), popup = mapData$Popup,
                   radius = 5, group = 'Stable Pricing') %>%
  
  

  
  # layer control
  addLayersControl(
    baseGroups = c('Open SM', 'Toner', 'NG World'),
    
    overlayGroups = c('Promoter',
                      'Role',
                      'CSC',
                      '___',
                      'Prod Range',
                      'Platform',
                      'Field Sales',
                      'UW Responsive',
                      'Home Rep Cost',
                      'Home Eval Proc',
                      'Billing Ease',
                      'Agt Sup Cen',
                      'Claim Service',
                      'Stable Pricing'
                      ),
    options = layersControlOptions(collapsed = F)
  ) %>%
  
  # set defaults to show by hiding groups
  hideGroup(c('Role',
              'CSC',
              '___',
              'Prod Range',
              'Platform',
              'Field Sales',
              'UW Responsive',
              'Home Rep Cost',
              'Home Eval Proc',
              'Billing Ease',
              'Agt Sup Cen',
              'Claim Service',
              'Stable Pricing'
              )) %>%
  
  

  
  addLegend('topleft', pal = colorPromo, values = mapData$Promoter,
            title = 'Promoter Status',
            opacity = 1) %>%

  addLegend('bottomleft', pal = colorRole, values = mapData$RoleGroup,
          title = 'Role',
          opacity = 1) %>%

  addLegend('topleft', pal = colorCSC, values = mapData$EnrolledinCSC,
            title = 'CSC',
            opacity = 1) %>%
  
  addLegend('topright', pal = colorQuestions, values = mapData$Q2_RangeofProducts,
            title = 'Question<br>Score',
            opacity = 1)

m
  

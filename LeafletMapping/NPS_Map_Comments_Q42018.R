

library(RODBC)
library(tidyverse)
library(DT)
library(leaflet)
library(zipcode)
library(reshape2)
library(RColorBrewer)

library(dplyr)
library(zipcode)
library(htmltools)



# getwd()
setwd('N:/Bryan/Marketing')
dataURL <- 'NPS_Q42018.csv'


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
for (i in seq(5, 23, by=2)) {nps[,i] <- convertComments(nps[,i])}


# get the comments
npsData <- nps %>% dplyr::select(EnrolledinCSC, 
                                 AgentName, 
                                 LikelyRecommend, 
                                 RoleGroup,
                                 PrimaryRole, 
                                 Promoter, 
                                 Q2Comment, 
                                 Q3Comment, 
                                 Q4Comment, 
                                 Q5Comment, 
                                 Q6Comment, 
                                 Q7Comment, 
                                 Q8Comment, 
                                 Q9Comment, 
                                 Q10Comment, 
                                 Q11Comment,
                                 AgentCode, 
                                 Zip)



# map the zipcode 
data(zipcode)
# agentData$PRDCNG_AGNT_ADDR_POSTL_CD <- clean.zipcodes(agentData$PRDCNG_AGNT_ADDR_POSTL_CD)
npsData$Zip <- clean.zipcodes(npsData$Zip)

# remove NAs
npsDataClean <- npsData %>% filter(!is.na(npsData$AgentCode))

mapData <- merge(npsDataClean, zipcode, by.x = 'Zip', by.y = 'zip')


# check the na
sum(is.na(mapData$AgentCode))
sum(is.na(mapData$latitude))


# create factors
mapData$EnrolledinCSC <- as.factor(mapData$EnrolledinCSC)
mapData$RoleGroup <- as.factor(mapData$RoleGroup)
mapData$PrimaryRole <- NULL

# clean up
rm(zipcode)


# make promoter color palettes
rolePal <- c("dark green", "orange", "dimgray", "light blue")
colorPromo <- colorFactor(palette = 'RdYlGn', mapData$Promoter)
colorRole <- colorFactor(palette = rolePal, mapData$RoleGroup)

# Add Jittering to the Zips so that they don't stack
# alternate jitter works very well
mapData$Lat <- jitter(mapData$latitude, amount = .01)
mapData$Lon <- jitter(mapData$longitude, amount = .01)

# Create a comment indicator 
mapData$CommentInd <- ifelse(mapData$Q2Comment != '','Comment',
                             ifelse(mapData$Q3Comment != '','Comment',
                             ifelse(mapData$Q4Comment != '','Comment',
                             ifelse(mapData$Q5Comment != '', 'Comment',
                             ifelse(mapData$Q6Comment != '', 'Comment',
                             ifelse(mapData$Q7Comment != '', 'Comment',
                             ifelse(mapData$Q8Comment != '', 'Comment',
                             ifelse(mapData$Q9Comment != '', 'Comment',
                             ifelse(mapData$Q10Comment != '', 'Comment',
                             ifelse(mapData$Q11Comment != '', 'Comment','No Comment'))))))))))


mapData$CommentInd <- as.factor(mapData$CommentInd)


# make the general popup
mapData$Popup <- paste('<strong>', mapData$AgentName,'</strong><br>',
                    'Score: ', mapData$LikelyRecommend,'<br>',
                    'Promoter: ', mapData$Promoter,'<br>',
                    'Role: ', mapData$RoleGroup, '<br>',
                    'CSC: ', mapData$EnrolledinCSC, '<br>',
                    'City: ', mapData$city)

# make the comment popup conditional
mapData$CommentPopup <- ifelse(mapData$CommentInd == "Comment",
                        paste('<strong>', mapData$AgentName,'</strong><br>',
                              '<strong>','____________','</strong><br>',
                             'RangeofProducts: ', mapData$Q2Comment, '<br>',
                             'QuotePlatformEasy: ', mapData$Q3Comment, '<br>',
                             'FieldSalesRepVisits: ', mapData$Q4Comment, '<br>',
                             'UW Responsive : ', mapData$Q5Comment, '<br>',
                             'HomeRepCostAcc : ', mapData$Q6Comment, '<br>',
                             'HomeEvalHassleFree : ', mapData$Q7Comment, '<br>',
                             'BillingEasyUnderstand : ', mapData$Q8Comment, '<br>',
                             'AgencySupCenExc : ', mapData$Q9Comment, '<br>',
                             'ClaimSvcFairResponsive : ',mapData$Q10Comment, '<br>',
                             'NBRenPricStable : ', mapData$Q11Comment), NA)




makePopup <- function(dat, comment) {paste('<strong>', dat$AgentName,'</strong><br>',
                                           'Score: ', dat$LikelyRecommend,'<br>',
                                           'Promoter: ', dat$Promoter,'<br>',
                                           'Role: ', dat$RoleGroup, '<br>',
                                           'City: ', dat$city, '<br>',
                                           '<strong>','____________','</strong><br>',
                                           'Comment: ', comment)
}

# make an indicator for the specific comment
mapData$Q2Ind <- ifelse(mapData$Q2Comment != '', 'Comment', 'No Comment')
mapData$Q3Ind <- ifelse(mapData$Q3Comment != '', 'Comment', 'No Comment')
mapData$Q4Ind <- ifelse(mapData$Q4Comment != '', 'Comment', 'No Comment')
mapData$Q5Ind <- ifelse(mapData$Q5Comment != '', 'Comment', 'No Comment')
mapData$Q6Ind <- ifelse(mapData$Q6Comment != '', 'Comment', 'No Comment')
mapData$Q7Ind <- ifelse(mapData$Q7Comment != '', 'Comment', 'No Comment')
mapData$Q8Ind <- ifelse(mapData$Q8Comment != '', 'Comment', 'No Comment')
mapData$Q9Ind <- ifelse(mapData$Q9Comment != '', 'Comment', 'No Comment')
mapData$Q10Ind <- ifelse(mapData$Q10Comment != '', 'Comment', 'No Comment')
mapData$Q11Ind <- ifelse(mapData$Q11Comment != '', 'Comment', 'No Comment')


# make popups where there are comments
mapData$Q2_pop <- ifelse(mapData$Q2Comment != '', makePopup(mapData, mapData$Q2Comment), NA)
mapData$Q3_pop <- ifelse(mapData$Q3Comment != '', makePopup(mapData, mapData$Q3Comment), NA)
mapData$Q4_pop <- ifelse(mapData$Q4Comment != '', makePopup(mapData,mapData$Q4Comment), NA)
mapData$Q5_pop <- ifelse(mapData$Q5Comment != '', makePopup(mapData,mapData$Q5Comment), NA)
mapData$Q6_pop <- ifelse(mapData$Q6Comment != '', makePopup(mapData,mapData$Q6Comment), NA)
mapData$Q7_pop <- ifelse(mapData$Q7Comment != '', makePopup(mapData,mapData$Q7Comment), NA)
mapData$Q8_pop <- ifelse(mapData$Q8Comment != '', makePopup(mapData,mapData$Q8Comment), NA)
mapData$Q9_pop <- ifelse(mapData$Q9Comment != '', makePopup(mapData,mapData$Q9Comment), NA)
mapData$Q10_pop <- ifelse(mapData$Q10Comment != '', makePopup(mapData,mapData$Q10Comment), NA)
mapData$Q11_pop <- ifelse(mapData$Q11Comment != '', makePopup(mapData,mapData$Q11Comment), NA)






# add radius size to make comments stand out
mapData$Radius <- ifelse(mapData$CommentInd == 1, 5, 1)

# make a comment color
colorComment <- colorFactor(c('darkorange', 'darkgreen'), mapData$CommentInd)


####### FIRST MAP ######

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
  
  
  
  ########   ADD ALL OF THE SCORES ########

  # Q2
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon, popup = ~Q2_pop,
                   color = ~ifelse(mapData$Q2Ind == 'Comment', 'darkorange', 'darkgreen'), 
                   radius = ~ifelse(mapData$Q2Ind == 'Comment', 5, 1),
                   group = 'Prod Range') %>%
  
  # Q3
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~ifelse(mapData$Q3Ind == 'Comment', 'darkorange', 'darkgreen'), 
                   radius = ~ifelse(Q3Ind == 'Comment', 5, 1),
                   group = 'Platform') %>%
  
  # Q4
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   popup = ~Q4_pop,
                   color = ~ifelse(mapData$Q4Ind == 'Comment', 'darkorange', 'darkgreen'), 
                   radius = ~ifelse(mapData$Q4Ind == 'Comment', 5, 1),
                   group = 'Field Sales') %>%
  
  # Q5
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   popup = ~Q5_pop,
                   color = ~ifelse(mapData$Q5Ind == 'Comment', 'darkorange', 'darkgreen'), 
                   radius = ~ifelse(mapData$Q5Ind == 'Comment', 5, 1),
                   group = 'UW Responsive') %>%
  
  # Q6
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   popup = ~Q6_pop,
                   color = ~ifelse(mapData$Q6Ind == 'Comment', 'darkorange', 'darkgreen'), 
                   radius = ~ifelse(mapData$Q6Ind == 'Comment', 5, 1),
                   group = 'Home Rep Cost') %>%
  
  # Q7
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   popup = ~Q7_pop,
                   color = ~ifelse(mapData$Q7Ind == 'Comment', 'darkorange', 'darkgreen'), 
                   radius = ~ifelse(mapData$Q7Ind == 'Comment', 5, 1),
                   group = 'Home Eval Proc') %>%
  
  # Q8
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   popup = ~Q8_pop,
                   color = ~ifelse(mapData$Q8Ind == 'Comment', 'darkorange', 'darkgreen'), 
                   radius = ~ifelse(mapData$Q8Ind == 'Comment', 5, 1),
                   group = 'Billing Ease') %>%
  
  # Q9
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   popup = ~Q9_pop,
                   color = ~ifelse(mapData$Q9Ind == 'Comment', 'darkorange', 'darkgreen'), 
                   radius = ~ifelse(mapData$Q9Ind == 'Comment', 5, 1),
                   group = 'Agt Sup Cen') %>%
  
  # Q10
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   popup = ~Q10_pop,
                   color = ~ifelse(mapData$Q10Ind == 'Comment', 'darkorange', 'darkgreen'), 
                   radius = ~ifelse(mapData$Q10Ind == 'Comment', 5, 1),
                   group = 'Claim Service') %>%
  
  # Q11
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   popup = ~Q11_pop,
                   color = ~ifelse(mapData$Q11Ind == 'Comment', 'darkorange', 'darkgreen'), 
                   radius = ~ifelse(mapData$Q11Ind == 'Comment', 5, 1),
                   group = 'Stable Pricing') %>%
  
  

  
  # layer control
  addLayersControl(
    baseGroups = c('Open SM', 'Toner', 'NG World'),
    
    overlayGroups = c('Promoter',
                      'Role',
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
  
  addLegend('bottomleft', pal = colorComment, values = mapData$CommentInd,
              title = 'Comments Available',
              opacity = 1) %>%

  
  addLegend('topleft', pal = colorPromo, values = mapData$Promoter,
            title = 'Promoter Status',
            opacity = 1) %>%

  addLegend('bottomright', pal = colorRole, values = mapData$RoleGroup,
          title = 'Role',
          opacity = 1) 

  

m
  




######################   SIMPLER MAP ######################
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
  
  # popup layer
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon, popup = mapData$CommentPopup,
                   color = ~colorComment(CommentInd), 
                   radius = ~Radius,
                   options = popupOptions(closeOnClick = FALSE, autoClose = FALSE),
                   group = 'Comments') %>%
  

  
  

  
  # layer control
  addLayersControl(
    baseGroups = c('Open SM', 'Toner', 'NG World'),
    
    overlayGroups = c('Comments', 'Promoter'),
    
    options = layersControlOptions(collapsed = F)
  ) %>%
  
  # set defaults to show by hiding groups
  hideGroup(c('Promoter')) %>%
  
  
  addLegend('bottomleft', pal = colorComment, values = mapData$CommentInd,
            title = 'Comments Available',
            opacity = 1) %>%


  addLegend('bottomright', pal = colorPromo, values = mapData$Promoter,
            title = 'Promoter Status',
            opacity = 1)
  

m




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



newNames <- c('EnrolledinCSC', 
             'AgentName', 
             'LikelyRecommend', 
             'RoleGroup',
             'PrimaryRole', 
             'Promoter', 
             'Q2_RangeofProducts', 
             'Q3_QuotePlatformEasy', 
             'Q4_FieldSalesRepRegVisits', 
             'Q5_UWResponsive', 
             'Q6_ProcessHomeRepCostAccurate', 
             'Q7_HomeEvalHassleFree', 
             'Q8_BillingEasyUnderstand', 
             'Q9_AgencySupCenExcellent', 
             'Q10_ClaimServFairResponsive', 
             'Q11_NBRenPricingStable',
             'AgentCode', 
             'Zip')




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
                                 Q11Comments,
                                 AgentCode, 
                                 Zip)



# rename the layers
colnames(npsData) <- newNames





# make postal code a character
# agentData$PRDCNG_AGNT_FULL_NM <- as.character(agentData$PRDCNG_AGNT_FULL_NM)
# agentData$PRDCNG_AGNT_ADDR_POSTL_CD <- as.character(agentData$PRDCNG_AGNT_ADDR_POSTL_CD)
# agentData$PRDCNG_AGNT_CD <- as.character(agentData$PRDCNG_AGNT_CD)

# map the zipcode 
data(zipcode)
# agentData$PRDCNG_AGNT_ADDR_POSTL_CD <- clean.zipcodes(agentData$PRDCNG_AGNT_ADDR_POSTL_CD)
npsData$Zip <- clean.zipcodes(npsData$Zip)


#  append the data to the zipcode
# zipData <- merge(agentData, zipcode, by.x = 'PRDCNG_AGNT_ADDR_POSTL_CD', by.y = 'zip')
#  append the data to the zipcode
npsDataClean <- npsData %>% filter(!is.na(npsData$AgentCode))

mapData <- merge(npsDataClean, zipcode, by.x = 'Zip', by.y = 'zip')



# check the na
# sum(is.na(npsZip$latitude))
# sum(is.na(zipData$PRDCNG_AGNT_ADDR_POSTL_CD))

# clean up agent data for map
# npsData$AgentCode <- as.character(npsData$AgentCode)

# map postal code
# mapComments <- left_join(npsDataClean, zipData, by = c("AgentCode" = "PRDCNG_AGNT_CD"))

# remove the na
sum(is.na(mapData$AgentCode))
sum(is.na(mapData$latitude))



# create factors
mapData$EnrolledinCSC <- as.factor(mapData$EnrolledinCSC)
mapData$RoleGroup <- as.factor(mapData$RoleGroup)
mapData$PrimaryRole <- NULL

# clean up
rm(zipcode)


# make some color palettes
colorPromo <- colorFactor(palette = 'RdYlGn', mapData$Promoter)


# Add Jittering to the Zips so that they don't stack
# alternate jitter works very well
mapData$Lat <- jitter(mapData$latitude, amount = .01)
mapData$Lon <- jitter(mapData$longitude, amount = .01)


mapData$CommentInd <- ifelse(mapData$Q2Comment != '','Comments',
                             ifelse(mapData$Q3Comment != '','Comments',
                             ifelse(mapData$Q4Comment != '','Comments',
                             ifelse(mapData$Q5Comment != '', 'Comments',
                             ifelse(mapData$Q6Comment != '', 'Comments',
                             ifelse(mapData$Q7Comment != '', 'Comments',
                             ifelse(mapData$Q8Comment != '', 'Comments',
                             ifelse(mapData$Q9Comment != '', 'Comments',
                             ifelse(mapData$Q10Comment != '', 'Comments',
                             ifelse(mapData$Q11Comments != '', 'Comments','No Comment'))))))))))


mapData$CommentInd <- as.factor(mapData$CommentInd)


mapData <- mapData %>% filter(Q2Comment != '' |
                                    Q3Comment != '' |
                                    Q4Comment != '' |
                                    Q5Comment != '' |
                                    Q6Comment != '' |
                                    Q7Comment != '' |
                                    Q8Comment != '' |
                                    Q9Comment != '' |
                                    Q10Comment != '' | 
                                    Q11Comments != '')







# make the popup
mapData$Popup <- paste('<strong>', mapData$AgentName,'</strong><br>',
                    'Score: ', mapData$LikelyRecommend,'<br>',
                    'Promoter: ', mapData$Promoter,'<br>',
                    'Role: ', mapData$RoleGroup, '<br>',
                    'CSC: ', mapData$EnrolledinCSC, '<br>',
                    'City: ', mapData$city)

# make the comment popup conditional


mapData$CommentPopup <- ifelse(mapData$CommentInd ==1,
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
                             'NBRenPricStable : ', mapData$Q11Comments), NA)


makePopup <- function(dat, comment) {paste('<strong>', dat$AgentName,'</strong><br>',
                                           'Score: ', dat$LikelyRecommend,'<br>',
                                           'Promoter: ', dat$Promoter,'<br>',
                                           'Role: ', dat$RoleGroup, '<br>',
                                           'CSC: ', dat$EnrolledinCSC, '<br>',
                                           'City: ', dat$city, '<br>',
                                           '<strong>','____________','</strong><br>',
                                           'Comment: ', dat$comment, '<br>')
  }

# add radius size
mapData$Radius <- ifelse(mapData$CommentInd == 1, 5, 1)

# make a comment color
colorComment <- colorFactor(c('orange', 'green'), mapData$CommentInd)


# mapComments$Q2_pop <- ifelse(mapComments$Q2Comment != '', makePopup(mapComments, "Q2Comment"), NA)
# mapComments$Q3_pop <- ifelse(mapComments$Q3Comment != '', makePopup(mapComments, 'Q3Comment'), NA)
# mapComments$Q4_pop <- ifelse(mapComments$Q4Comment != '', makePopup(mapComments,"Q4Comment"), NA)
# mapComments$Q5_pop <- ifelse(mapComments$Q5Comment != '', makePopup(mapComments,"Q5Comment"), NA)
# mapComments$Q6_pop <- ifelse(mapComments$Q6Comment != '', makePopup(mapComments,"Q6Comment"), NA)
# mapComments$Q7_pop <- ifelse(mapComments$Q7Comment != '', makePopup(mapComments,"Q7Comment"), NA)
# mapComments$Q8_pop <- ifelse(mapComments$Q8Comment != '', makePopup(mapComments,"Q8Comment"), NA)
# mapComments$Q9_pop <- ifelse(mapComments$Q9Comment != '', makePopup(mapComments,"Q9Comment"), NA)
# mapComments$Q10_pop <- ifelse(mapComments$Q10Comment != '', makePopup(mapComments,"Q10Comment"), NA)
# mapComments$Q11_pop <- ifelse(mapComments$Q11Comments != '', makePopup(mapComments,"Q11Comments"), NA)

####### FIRST MAP ######

m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM')  %>%
  addProviderTiles(providers$Stamen.Toner, group = 'Toner')  %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'NG World') %>%
  setView(lng = -72, lat = 41, zoom = 6) %>%
  
  # promoter layer
  addCircleMarkers(data = mapComments, lat = ~Lat, lng = ~Lon,
                   color = ~colorPromo(Promoter), popup = mapComments$Popup,
                   radius = 5, group = 'Promoter') %>%

  # role layer
  addCircleMarkers(data = mapComments, lat = ~Lat, lng = ~Lon,
                   color = ~colorRole(RoleGroup), popup = mapComments$Popup,
                   radius = 5, group = 'Role') %>%
  
  
  # csc layer
  addCircleMarkers(data = mapComments, lat = ~Lat, lng = ~Lon,
                   color = ~colorCSC(EnrolledinCSC), popup = mapComments$Popup,
                   radius = 5, group = 'CSC') %>%
  
  
  ########   ADD ALL OF THE SCORES ########

  # Q2
  addPopups(data = mapComments, lat = ~Lat, lng = ~Lon, popup = ~Q2_pop,
             options = popupOptions(closeButton = FALSE),
             group = 'Prod Range') %>%
  
  # Q3
  addPopups(data = mapComments, lat = ~Lat, lng = ~Lon,
                   popup = ~Q3_pop,
                   options = popupOptions(closeButton = FALSE),
                   group = 'Platform') %>%
  
  # Q4
  addPopups(data = mapComments, lat = ~Lat, lng = ~Lon,
                   popup = ~Q4_pop,
                   options = popupOptions(closeButton = FALSE),
                   group = 'Field Sales') %>%
  
  # Q5
  addPopups(data = mapComments, lat = ~Lat, lng = ~Lon,
                   popup = ~Q5_pop,
                   options = popupOptions(closeButton = FALSE),
                   group = 'UW Responsive') %>%
  
  # Q6
  addPopups(data = mapComments, lat = ~Lat, lng = ~Lon,
                   popup = ~Q6_pop,
                   options = popupOptions(closeButton = FALSE),
                   group = 'Home Rep Cost') %>%
  
  # Q7
  addPopups(data = mapComments, lat = ~Lat, lng = ~Lon,
                   popup = ~Q7_pop,
                   options = popupOptions(closeButton = FALSE),
                   group = 'Home Eval Proc') %>%
  
  # Q8
  addPopups(data = mapComments, lat = ~Lat, lng = ~Lon,
                   popup = ~Q8_pop,
                   options = popupOptions(closeButton = FALSE),
                   group = 'Billing Ease') %>%
  
  # Q9
  addPopups(data = mapComments, lat = ~Lat, lng = ~Lon,
                   popup = ~Q9_pop,
                   options = popupOptions(closeButton = FALSE),
                   group = 'Agt Sup Cen') %>%
  
  # Q10
  addPopups(data = mapComments, lat = ~Lat, lng = ~Lon,
                   popup = ~Q10_pop,
                   options = popupOptions(closeButton = FALSE),
                   group = 'Claim Service') %>%
  
  # Q11
  addPopups(data = mapComments, lat = ~Lat, lng = ~Lon,
                   popup = ~Q11_pop,
                   options = popupOptions(closeButton = FALSE),
                   group = 'Stable Pricing') %>%
  
  

  
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
  
  

  
  addLegend('topleft', pal = colorPromo, values = mapComments$Promoter,
            title = 'Promoter Status',
            opacity = 1) %>%

  addLegend('bottomleft', pal = colorRole, values = mapComments$RoleGroup,
          title = 'Role',
          opacity = 1) %>%

  addLegend('topleft', pal = colorCSC, values = mapComments$EnrolledinCSC,
            title = 'CSC',
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
                  group = 'Comments') %>%
  

  
  # layer control
  addLayersControl(
    baseGroups = c('Open SM', 'Toner', 'NG World'),
    
    overlayGroups = c('Promoter', 'Comments'),
    
    options = layersControlOptions(collapsed = F)
  ) %>%
  
  # set defaults to show by hiding groups
  hideGroup(c('Comments')) %>%
  
  
  addLegend('bottomleft', pal = colorComment, values = mapData$CommentInd,
            title = 'Comments Available',
            opacity = 1) %>%


  addLegend('bottomright', pal = colorPromo, values = mapData$Promoter,
            title = 'Promoter Status',
            opacity = 1)
  

m


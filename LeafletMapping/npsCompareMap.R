
# this is for mapping the nps compare data

library(tidyverse)
library(DT)
library(leaflet)
library(zipcode)
library(reshape2)
library(RColorBrewer)

library(dplyr)
library(zipcode)
library(htmltools)

## @knitr loadData
# getwd()
setwd('N:/Bryan/Marketing')



# read the files from csv
dataURL <- 'NPS_Q12019.csv'
q1NPS <- read.csv(dataURL, stringsAsFactors = F)



# read the files from csv
dataURL <- 'NPS_Q42018.csv'
q4NPS <- read.csv(dataURL, stringsAsFactors = F)


# join agent code
q4NPS$AgentCode <- as.character(q4NPS$AgentCode)

agentCodes <- q1NPS$AgentCode
agentCodes4 <- q4NPS$AgentCode

q4Red <- q4NPS %>% filter(AgentCode %in% agentCodes)
q1Red <- q1NPS %>% filter(AgentCode %in% agentCodes4)

# remove the extra dfs
rm(q1NPS)
rm(q4NPS)

# function for dealing with strength
convertComments <- function(x) {as.numeric(ifelse(x == 'Strongly Agree',  10, ifelse(
  x == 'Agree', 8, ifelse(x == 'Neutral', 5,ifelse(
    x == 'Disagree', 3,ifelse(x == 'Strongly Disagree', 1,NA))))))
}



# create the promoter field
make_promoter <- function(df){
  df$Promoter <- ifelse(df$LikelyRecommend > 8, 'Promoter', ifelse(
    df$LikelyRecommend > 6 & df$LikelyRecommend <9, 'Passive', 'Detractor'))
  return(df)
}

# apply the functions
q1Red <- make_promoter(q1Red)
q4Red <- make_promoter(q4Red)

q1Red$Promoter <- ordered(q1Red$Promoter, levels = c("Detractor", 
                                                     "Passive", 
                                                     "Promoter"))


q4Red$Promoter <- ordered(q4Red$Promoter, levels = c("Detractor", 
                                                     "Passive", 
                                                     "Promoter"))

# make role bins
make_role_group <- function(df){
  df$RoleGroup <- ifelse(df$PrimaryRole == "Producer", "FrontLine", 
                         ifelse(df$PrimaryRole == "Account Manager/CSR", "FrontLine",
                                ifelse(df$PrimaryRole ==  "Administrative Assistant","Admin",
                                       ifelse(df$PrimaryRole ==  "Claims","FrontLine",
                                              ifelse(df$PrimaryRole ==  "LOB Manager","Admin",
                                                     ifelse(df$PrimaryRole ==  "Marketing","MktgSales",
                                                            ifelse(df$PrimaryRole ==  "Office Manager","Admin",
                                                                   ifelse(df$PrimaryRole ==  "Operations Manager","Admin",
                                                                          ifelse(df$PrimaryRole ==  "Principal","Exec Mgt",
                                                                                 ifelse(df$PrimaryRole ==  "Producer","FrontLine",
                                                                                        ifelse(df$PrimaryRole ==  "Sales Manager","MktgSales",
                                                                                               ifelse(df$PrimaryRole ==  "Senior Executive","Exec Mgt",
                                                                                                      "Admin"))))))))))))
  return(df)
}



# role groups
q1Red <- make_role_group(q1Red)
q4Red <- make_role_group(q4Red)

# order the role groups
q1Red$RoleGroup <- ordered(q1Red$RoleGroup, levels = c("FrontLine", "Exec Mgt",
                                                       "Admin", "MktgSales"))

q4Red$RoleGroup <- ordered(q4Red$RoleGroup, levels = c("FrontLine", "Exec Mgt",
                                                       "Admin", "MktgSales"))








# conver the words to numbers
for (i in seq(5, 23, by=2)) {q1Red[,i] <- convertComments(q1Red[,i])}

for (i in seq(5, 23, by=2)) {q4Red[,i] <- convertComments(q4Red[,i])}


# get the key fields
get_questions <- function(df) {
  df %>% dplyr::select(Zip,
                       AgentName,
                       AgentCode,
                       LikelyRecommend,
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
                       Q11_NBRenPricingStable)
}


# make the reduced dfs
q1 <- get_questions(q1Red)
q4 <- get_questions(q4Red)

# fill Nan with row modes
# fill in NAs with Row means
fill_nan <- function(df, dfClean) {
  data <- df[,c(6:15)]
  k <- which(is.na(data), arr.ind = TRUE)
  data[k] <- rowMeans(data, na.rm=TRUE)[k[,1]]
  dfClean <- cbind(df[,c(1:5)], data)
}

q1Clean <- fill_nan(q1, q1Clean)
q4Clean <- fill_nan(q4, q4Clean)


# summarize
get_means <- function(df, grp, name, zip)
{df %>% group_by_(grp, name, zip) %>% 
    summarise(LikelyRecommend = mean(LikelyRecommend, na.rm = T),
              Q2_RangeofProducts = mean(Q2_RangeofProducts, na.rm = T),
              Q3_QuotePlatformEasy = mean(Q3_QuotePlatformEasy, na.rm = T),
              Q4_FieldSalesRepRegVisits = mean(Q4_FieldSalesRepRegVisits, na.rm = T),
              Q5_UWResponsive = mean(Q5_UWResponsive, na.rm = T),
              Q6_ProcessHomeRepCostAccurate = mean(Q6_ProcessHomeRepCostAccurate, na.rm = T),
              Q7_HomeEvalHassleFree = mean(Q7_HomeEvalHassleFree, na.rm = T),
              Q8_BillingEasyUnderstand = mean(Q8_BillingEasyUnderstand, na.rm = T),
              Q9_AgencySupCenExcellent = mean(Q9_AgencySupCenExcellent, na.rm = T),
              Q10_ClaimServFairResponsive = mean(Q10_ClaimServFairResponsive, na.rm = T),
              Q11_NBRenPricingStable = mean(Q11_NBRenPricingStable, na.rm = T))
}


# By promoter
q1Summary <- get_means(q1Clean, 'AgentCode', 'AgentName', 'Zip')
q4Summary <- get_means(q4Clean, 'AgentCode', 'AgentName', 'Zip')

# change zip to character
q1Summary$Zip <- as.character(q1Summary$Zip)
q4Summary$Zip <- as.character(q4Summary$Zip)


# q1 summary stats 
q1Summary <- data.frame(ID=q1Summary[,1:4], Q1Means=rowMeans(q1Summary[5:14]))
colnames(q1Summary) <- c("AgentCode", "AgentName", "Zip","Q1Recommend", "Q1MeanQ")

q4Summary <- data.frame(ID=q4Summary[,1:4], Q4Means=rowMeans(q4Summary[5:14]))
colnames(q4Summary) <- c("AgentCode", "AgentName", "Zip","Q4Recommend", "Q4MeanQ")

# merge
compare <- q1Summary %>% inner_join(q4Summary, by=c("AgentCode" = "AgentCode",
                                                    "AgentName" = "AgentName",
                                                    "Zip" = "Zip"))



# calculate the differences
compare$RecommendDiff <-  compare$Q1Recommend - compare$Q4Recommend
compare$QsDiff <-  compare$Q1MeanQ - compare$Q4MeanQ



# make bins out of the Q
make_bins <- function(field){
  cut(field, c(-10,-2.5, 0, 2.5, 5, 20),
      include.lowest = T,
      labels = c("<-2.5", "-2.5-0", "0-2.5", "2.5-5", "5+"))
}


compare$RecDiffBins <- make_bins(compare$RecommendDiff)
compare$QsDiffBins <- make_bins(compare$QsDiff)



# merge the zipcode data
data(zipcode)

compare$Zip <- clean.zipcodes(compare$Zip)

# sum(is.na(compare$Zip))

mapData <- merge(compare, zipcode, by.x = 'Zip', by.y = 'zip')


# jitter the lat and long
# alternate jitter works very well
mapData$Lat <- jitter(mapData$latitude, amount = .01)
mapData$Lon <- jitter(mapData$longitude, amount = .01)


# make the general popup
mapData$Popup <- paste('<strong>', mapData$AgentName,'</strong><br>',
                       'Likely Recommend<br>',
                       'Q1 2019: ', formatC(mapData$Q1Recommend,zero.print = T, format = 'd', digits = 2),'<br>',
                       'Q4 2018: ', formatC(mapData$Q4Recommend,zero.print = T, format = 'd', digits = 2),'<br>',
                       '<strong>','____________','</strong><br>',
                       'Question Means<br>',
                       'Q1 2019: ', formatC(mapData$Q1MeanQ,zero.print = T, format = 'd', digits = 2), '<br>',
                       'Q4 2018: ', formatC(mapData$Q4MeanQ,zero.print = T, format = 'd', digits = 2), '<br>',
                       'City: ', mapData$city)



# make a color scheme

hanover <- c('darkgreen', 'green', 'yellow', 'orange', 'red')


changeCol <- colorFactor(palette = 'RdYlGn', RecDiffBins)


## MAP ##
mapData$LRRadius <- 5 * sqrt(mapData$RecommendDiff)
mapData$QRadius <- 5 * sqrt(mapData$QsDiff)





m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM')  %>%
  addProviderTiles(providers$Stamen.Toner, group = 'Toner')  %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'NG World') %>%
  setView(lng = -72, lat = 41, zoom = 6) %>%
  
  # likely recommend diff
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~changeCol(RecDiffBins), popup = mapData$Popup,
                   radius = 10, group = 'Likely Recommend') %>%  
  
  
  # mean Qs diff
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~changeCol(QsDiffBins), popup = mapData$Popup,
                   radius = 10, group = 'Question Means') %>%
  

  # layer control
  addLayersControl(
    baseGroups = c('Open SM', 'Toner', 'NG World'),
    
    overlayGroups = c('Likely Recommend',
                      'Question Means'),
    options = layersControlOptions(collapsed = F)) %>% 
  
  hideGroup(c('Question Means')) %>%
      
      
  addLegend('bottomleft', pal = changeCol, values = mapData$RecDiffBins,
            title = 'Change in Scores',
            opacity = 1)
  
  m
      
      
      


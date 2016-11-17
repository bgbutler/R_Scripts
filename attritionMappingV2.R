.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

library(leaflet)
library(dplyr)
library(RColorBrewer)
library(htmltools)

if(!require(jsonlite)) install.packages("jsonlite", repos='http://cran.cnr.berkeley.edu')
library(jsonlite)
library(reshape2)
library(tidyr)


#get the files  with location
url <- "C:/Users/n846490/Documents/DataScience/CSVs/branches.csv"
branchLocs <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get key data from branchlocs
branchLocs <- select(branchLocs, Branch.Name, Cost.Center, District, Latitude, Longitude)
names(branchLocs) <- c("Branch.Name", "Branch.Number", "District", "Lat", "Lon")

#get the CSat scores from the rankings file
url <- "C:/Users/n846490/Documents/DataScience/CSVs/juneRank.csv"
branchPl <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get the performance metrics
branchCsat <- select(branchPl, Branch.Number, District, CSAT.Score, Performance.Level)
names(branchCsat) <- c("Branch.Number", "District", "CSat", "PerfLevel")
rm(branchPl)


#bind the data cand clean  up
#merge the branchLocs and branchPL on branch name
branchData <- inner_join(branchLocs, branchCsat, by = c(
    "Branch.Number" = "Branch.Number", "District" = "District"))
rm(branchLocs); rm(branchCsat)


#get the attrition and active data
url <- "C:/Users/n846490/Documents/DataScience/CSVs/attrition.csv"
branchAttrit <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

url <- "C:/Users/n846490/Documents/DataScience/CSVs/active.csv"
branchActive <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


#merge the data
branchData <- left_join(branchData, branchActive, by = "Branch.Number")
branchData <- left_join(branchData, branchAttrit, by = "Branch.Number")


#create function to clean up the NAs
na.zero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}

for (i in 8:40){
    branchData[,i] <- na.zero(branchData[,i])
}


#export to CSV for viewing
#Write the data to clean .CSV file
write.csv(branchData, file="C:/Users/n846490/Documents/DataScience/CSVs/branchHeadCountsDistrict.csv", row.names=FALSE)

#remove NULL perf level
branchData$PerfLevel <- gsub("NULL", NA, branchData$PerfLevel)
branchData$PerfLevel <- gsub("0", NA, branchData$PerfLevel)
branchData <- na.omit(branchData)


#bucket the data
#clean csat and convert it to usable buckets
branchData$CSat <- as.character(branchData$CSat)
branchData$CSat <- gsub("%", "", branchData$CSat)
branchData$CSat <- as.numeric(branchData$CSat)
branchData$PerfLevel <- as.numeric(branchData$PerfLevel)

#aggregate the data to the district level
districtData <- branchData %>% group_by(District) %>% 
    summarise(
        Lat = mean(Lat),
        Lon = mean(Lon),
        CSat  = mean(CSat),
        PerfLevel = round(mean(PerfLevel),0),
        BankersAct = sum(BankersAct),
        ServiceAct = sum(ServiceAct),
        TotAct = sum(TotAct),
        BankersAttr = sum(BankersAttr),
        ServiceAttr = sum(ServiceAttr),
        TotAttr = sum(TotAttr)) %>%
        select(District, Lat, Lon, CSat, PerfLevel, BankersAct, ServiceAct, 
               TotAct, BankersAttr, ServiceAttr, TotAttr)


districtData$BankAttrRate <- districtData$BankersAttr/districtData$BankersAct
districtData$ServAttrRate <- districtData$ServiceAttr/districtData$ServiceAct

districtData$CsatLvl <- cut(districtData$CSat,
                            c(0,50,60,70,80,100), include.lowest = T,
                            labels = c("<50%", "50-60%", "60-70%", "70-80%", "80-100%"))

districtData$ActBankerLvl <- cut(districtData$BankersAct,
                           c(0,10,20,30,100), include.lowest = T,
                           labels = c("<10", "10-20", "20-30", "30+"))

districtData$ActServiceLvl <- cut(districtData$ServiceAct,
                                c(0,20,30,40,50,80), include.lowest = T,
                                labels = c("<20", "20-30", "30-40", "40-50", "50+"))

########
#Absolute nUmbers
districtData$AttrBankerLvl <- cut(districtData$BankersAct,
                                 c(0,5,10,15,80), include.lowest = T,
                                 labels = c("<5", "5-10", "10-15", "15+"))

districtData$AttrServiceLvl <- cut(districtData$ServiceAttr,
                                   c(0,5,10,15,50), include.lowest = T,
                                   labels = c("<5", "5-10", "10-15", "15+"))
##########

districtData$AttrBankerLvl <- cut(districtData$BankAttrRate,
                                  c(0,.2,.3,.4,1), include.lowest = T,
                                  labels = c("<.2", ".2-.3", ".3-.4", ".4+"))

districtData$AttrServiceLvl <- cut(districtData$ServAttrRate,
                                   c(0,.2,.3,.4,1), include.lowest = T,
                                   labels = c("<.2", ".2-.3", ".3-.4", ".4+"))




#make perf level a factor
districtData$PerfLevel <- as.factor(as.character(districtData$PerfLevel))



#set the color scale
binColors <- colorFactor(rainbow(5), districtData$PerfLevel)
binColorsCsat <- colorFactor(palette = "RdYlBu", districtData$CsatLvl)

binColorsBankersAct <- colorFactor(palette = "Greens", districtData$ActBankerLvl)
binColorsServiceAct <- colorFactor(palette = rainbow(5), districtData$ActServiceLvl)

binColorsBankersAttr <- colorFactor(palette = "PuBu", districtData$AttrBankerLvl)
binColorsServiceAttr <- colorFactor(palette = "Oranges", districtData$AttrServiceLvl)



#create a full popup
districtData$allData <- paste("<strong>", districtData$District, "</strong><br>",
                              "CSAT = ", formatC(districtData$CSat,2), "<br>",
                              "Perf Level = ", districtData$PerfLevel, "<br>",
                              "Bankers = ", districtData$BankersAct, "<br>",
                              "Service = " , districtData$ServiceAct, "<br>",
                              "<strong>Total Active = ", districtData$TotAct,"</strong><br><br>",
                              "Bankers Attr = ", formatC(districtData$BankAttrRate,2), "<br>",
                              "Service Attr = ", formatC(districtData$ServAttrRate,2), "<br>",
                              "<strong>Total Attr = ", districtData$TotAttr, "</strong", sep = "")

m <- leaflet() %>%
    addTiles("Stamen.Toner") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    setView(lng = -72.0572145, lat = 42.3586396, zoom = 8) %>%
    
    
    #csat
    addCircleMarkers(data = districtData, lat = ~Lat, lng = ~Lon,
                     color = ~binColorsCsat(CsatLvl), popup = districtData$allData,
                     radius = ~CSat/10, group = "CSat") %>%
    #performance
    addCircleMarkers(data = districtData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColors(PerfLevel), popup = districtData$allData,
                     radius = ~3*PerfLevel, group = "Performance") %>%
    
    #bankers active
    addCircleMarkers(data = districtData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsBankersAct(ActBankerLvl), popup = districtData$allData,
                     radius = ~BankersAct/2, group = "Bankers Active") %>%
    
    #service active   
    addCircleMarkers(data = districtData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsServiceAct(ActServiceLvl), popup = districtData$allData,
                     radius = ~ServiceAct/2, group = "Service Active") %>%
    
    #bankers attrition 
    addCircleMarkers(data = districtData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsBankersAttr(AttrBankerLvl), popup = districtData$allData,
                     radius = ~BankersAttr*2, group = "Banker Attrition") %>%
    
    #service attrition   
    addCircleMarkers(data = districtData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsServiceAttr(AttrServiceLvl), popup = districtData$allData,
                     radius = ~ServiceAttr*2, group = "Service Attrition") %>%    
    
    
    
    #layer control
    addLayersControl(
        baseGroups = c("Toner", "CartoDb"),
        overlayGroups = c("CSat",
                          "Performance",
                          "Bankers Active",
                          "Service Active",
                          "Banker Attrition",
                          "Service Attrition"
        ),
        options = layersControlOptions(collapsed = F)
    ) %>%
    
    
    #per        
    addLegend("bottomright", pal = binColors, values = districtData$PerfLevel,
              title = "June 2016<br>Performance<br> Level",
              opacity = 1) %>%
    #csat    
    addLegend("bottomright", pal = binColorsCsat, values = districtData$CsatLvl,
              title = "CSat",
              opacity = 1) %>%
    
    #active bankers
    addLegend("topleft", pal = binColorsBankersAct, values = districtData$ActBankerLvl,
              title = "Bankers",
              opacity = 1) %>%
    
    #active service
    addLegend("topleft", pal = binColorsServiceAct, values = districtData$ActServiceLvl,
              title = "Service",
              opacity = 1) %>%
    
    #attrition bankers
    addLegend("bottomleft", pal = binColorsBankersAttr, values = districtData$AttrBankerLvl,
              title = "Bankers Attr",
              opacity = 1) %>%
    
    
    #attrition service
    addLegend("bottomleft", pal = binColorsServiceAttr, values = districtData$AttrServiceLvl,
              title = "Service Attr",
              opacity = 1)


m  # Print the map







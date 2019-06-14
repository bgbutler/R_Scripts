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
url <- "C:/Users/n846490/Documents/DataScience/branches.csv"
branchLocs <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get key data from branchlocs
branchLocs <- select(branchLocs, Branch.Name, Cost.Center, Latitude, Longitude)
names(branchLocs) <- c("Branch.Name", "Branch.Number", "Lat", "Lon")

#get the CSat scores from the rankings file
url <- "C:/Users/n846490/Documents/DataScience/juneRank.csv"
branchPl <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get the performance metrics
branchCsat <- select(branchPl, Branch.Number, CSAT.Score, Performance.Level)
names(branchCsat) <- c("Branch.Number", "CSat", "PerfLevel")
rm(branchPl)


#bind the data cand clean  up
#merge the branchLocs and branchPL on branch name
branchData <- inner_join(branchLocs, branchCsat, by = "Branch.Number")
rm(branchLocs); rm(branchCsat)


#get the attrition and active data
url <- "C:/Users/n846490/Documents/DataScience/attrition.csv"
branchAttrit <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

url <- "C:/Users/n846490/Documents/DataScience/active.csv"
branchActive <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


#merge the data
branchData <- left_join(branchData, branchActive, by = "Branch.Number")
branchData <- left_join(branchData, branchAttrit, by = "Branch.Number")


#create function to clean up the NAs
na.zero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}

for (i in 6:40){
    branchData[,i] <- na.zero(branchData[,i])
}


#export to CSV for viewing
#Write the data to clean .CSV file
write.csv(branchData, file="C:/Users/n846490/Documents/DataScience/branchHeadCounts.csv", row.names=FALSE)

#remove NULL perf level
branchData$PerfLevel <- gsub("NULL", NA, branchData$PerfLevel)
branchData$PerfLevel <- gsub("0", NA, branchData$PerfLevel)
branchData <- na.omit(branchData)


#bucket the data
#clean csat and convert it to usable buckets
branchData$CSat <- as.character(branchData$CSat)
branchData$CSat <- gsub("%", "", branchData$CSat)
branchData$CSat <- as.numeric(branchData$CSat)
branchData$CsatLvl <- cut(branchData$CSat,
                            c(0,50,60,70,80,100), include.lowest = T,
                            labels = c("<50%", "50-60%", "60-70%", "70-80%", "80-100%"))

branchData$ActBankerLvl <- cut(branchData$BankersAct,
                           c(0,1,3,5,20), include.lowest = T,
                           labels = c("0", "1-3", "3-5", "5+"))

branchData$ActServiceLvl <- cut(branchData$ServiceAct,
                                c(0,1,3,5,20), include.lowest = T,
                                labels = c("0", "1-3", "3-5", "5+"))


#make perf level and peer group factors
branchData$PerfLevel <- as.numeric(as.character(branchData$PerfLevel))



#set the color scale
binColors <- colorFactor(rainbow(5), branchData$PerfLevel)
binColorsL <- colorFactor(palette = "RdYlBu", branchData$CsatLvl)
binColorsBankersAct <- colorFactor(palette = "Greens", branchData$ActBankerLvl)
binColorsBankersAttr <- colorFactor(palette = "Blues", branchData$BankersAttr)
binColorsServiceAct <- colorFactor(palette = rainbow(5), branchData$ActServiceLvl)
binColorsServiceAttr <- colorFactor(palette = "RdYlBu", branchData$ServiceAttr)



#create a full popup
branchData$allData <- paste("<strong>", branchData$Branch.Name, "</strong><br>",
                              "CSAT = ", branchData$CSat, "<br>",
                              "Perf Level = ", branchData$PerfLevel, "<br>",
                              "Bankers = ", branchData$BankersAct, "<br>",
                              "Service = " , branchData$ServiceAct, "<br>",
                              "<strong>Total Active = ", branchData$TotAct,"</strong><br><br>",
                              "Bankers Attr = ", branchData$BankersAttr, "<br>",
                              "Service Attr = ", branchData$ServiceAttr, "<br>",
                              "<strong>Total Attr = ", branchData$TotAttr, "</strong", sep = "")

m <- leaflet() %>%
    addTiles("Stamen.Toner") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    setView(lng = -72.0572145, lat = 42.3586396, zoom = 8) %>%
    
    
    #csat
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon,
                     color = ~binColorsL(CsatLvl), popup = branchData$allData,
                     radius = ~CSat/10, group = "CSat") %>%
    #performance
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColors(PerfLevel), popup = branchData$allData,
                     radius = ~3*PerfLevel, group = "Performance") %>%
    
    #bankers active
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsBankersAct(ActBankerLvl), popup = branchData$allData,
                     radius = ~BankersAct*3, group = "Bankers Active") %>%
    
    #service active   
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsServiceAct(ActServiceLvl), popup = branchData$allData,
                     radius = ~ServiceAct*2, group = "Service Active") %>%
    
    #bankers attrition 
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsBankersAttr(BankersAttr), popup = branchData$allData,
                     radius = ~BankersAttr*2, group = "Banker Attrition") %>%
    
    #service attrition   
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsServiceAttr(ServiceAttr), popup = branchData$allData,
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
    addLegend("bottomright", pal = binColors, values = branchData$PerfLevel,
              title = "June 2016<br>Performance<br> Level",
              opacity = 1) %>%
    #csat    
    addLegend("bottomright", pal = binColorsL, values = branchData$CsatLvl,
              title = "CSat",
              opacity = 1) %>%
    
    #active bankers
    addLegend("topleft", pal = binColorsBankersAct, values = branchData$ActBankerLvl,
              title = "Bankers",
              opacity = 1) %>%
    
    #active service
    addLegend("topleft", pal = binColorsServiceAct, values = branchData$ActServiceLvl,
              title = "Service",
              opacity = 1) %>%
    
    #attrition bankers
    addLegend("bottomleft", pal = binColorsBankersAttr, values = branchData$BankersAttr,
              title = "Bankers Attr",
              opacity = 1) %>%
    
    
    #attrition service
    addLegend("bottomleft", pal = binColorsServiceAttr, values = branchData$ServiceAttr,
              title = "Service Attr",
              opacity = 1)


m  # Print the map







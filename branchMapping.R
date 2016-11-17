
.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

library(leaflet)
library(dplyr)
library(RColorBrewer)
library(htmltools)


if(!require(jsonlite)) install.packages("jsonlite", repos='http://cran.cnr.berkeley.edu')
library(jsonlite)


#get the files  with location
url <- "C:/Users/n846490/Documents/DataScience/branches.csv"
branchLocs <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get the rankings file
url <- "C:/Users/n846490/Documents/DataScience/juneRank.csv"
branchPl <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get other data
url <- "C:/Users/n846490/Documents/DataScience/juneDataReducedClean.csv"
branchBiz <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get key data from branchlocs
branchLocs <- select(branchLocs, Branch.Name, Cost.Center, Latitude, Longitude)

#get the performance metrics
branchPl <- select(branchPl, Branch.Number, Peer.Group, Peer.Rank, Performance.Level, CSAT.Score)

#clean the names
names(branchLocs) <- c("Branch.Name", "Branch.Number", "Lat", "Lon")

names(branchBiz) <- c("Branch.Number", "PerfLevel", "PeerGroup", "NewChkgCountsQ2", "District", "NewChkUSD",
                      "TotalChkRev", "TotalBizChkMM")

#merge the branchLocs and branchPL on branch number
branchData <- inner_join(branchLocs, branchPl, by = "Branch.Number")

#merge bizData onto branchData
branchData <- inner_join(branchData, branchBiz, by = "Branch.Number")

#clean up and remove extra dfs
rm(branchPl); rm(branchLocs); rm(branchBiz)

table(branchData$Peer.Group)

#remove closed
branchData = branchData[branchData$Peer.Group != "Closed",]

#clean csat and convert it to usable buckets
branchData$CsatScore <- as.character(branchData$CSAT.Score)
branchData$CsatScore <- gsub("%", "", branchData$CsatScore)
branchData$CsatScore <- as.numeric(branchData$CsatScore)
branchData$CsatLevel <- cut(branchData$CsatScore,
                            c(0,50,60,70,80,100), include.lowest = T,
                            labels = c("CSatLess50", "CSat50to60", "CSat60to70", "CSat70to80", "CSat80to100"))


#change performance level into a number for radius use later
branchData$Performance.Level <- as.numeric(as.character(branchData$Performance.Level))


#set the color scale
binColors <- colorFactor(rainbow(7), branchData$Performance.Level)
binColorsL <- colorFactor(palette = "RdYlBu", branchData$CsatLevel)


#create a nice popup
branchData$branchPop <- paste("<strong>",branchData$Branch.Name, "</strong><br>", "CSAT = ", branchData$CsatScore, "<br>",
                              "Perf Level = ", branchData$Performance.Level, sep = "")





m <- leaflet() %>%
    addTiles("Stamen.Toner") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    setView(lng = -71.0572145, lat = 42.3586396, zoom = 8) %>%
    
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon,
                     color = ~binColorsL(CsatLevel), popup = branchData$branchPop,
                     radius = ~CsatScore/10, group = "CSAT") %>%

    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon, 
                   color = ~binColors(Performance.Level), popup = branchData$branchPop,
                   radius = ~3*Performance.Level, group = "Performance") %>%
    
  
    
    #layer control
    addLayersControl(
        baseGroups = c("Toner", "CartoDb"),
        overlayGroups = c("CSAT", "Performance"),
        options = layersControlOptions(collapsed = F)
    ) %>%
    
    
    addLegend("bottomright", pal = binColors, values = branchData$Performance.Level,
            title = "June 2016<br>Performance<br> Level",
            opacity = 1) %>%
    
    addLegend("bottomright", pal = binColorsL, values = branchData$CsatLevel,
              title = "CSAT",
              opacity = 1)

  
m  # Print the map








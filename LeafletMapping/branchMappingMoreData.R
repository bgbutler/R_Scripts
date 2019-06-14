
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
                      "TotalChkRev", "TotalBizChkMM", "SBLoanBal")

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
                            labels = c("<50%", "50-60%", "60-70%", "70-80%", "80-100%"))


#change performance level into a number for radius use later
branchData$Performance.Level <- as.numeric(as.character(branchData$Performance.Level))

#create some scaled data and convert to segments
#bucket total checking revenue
branchData$ckRev <- branchData$TotalChkRev/1000
branchData$ckRevLvl <- cut(branchData$ckRev,
                           c(0,25,50,100,150,200,300), include.lowest = T,
                           labels = c("<25K", "25-50K", "50-100K", "100-150K", "150-200K", "200K+"))

#bucket biz revenue
branchData$bizRev <-  branchData$TotalBizChkMM/1000
branchData$bizRevLvl <- cut(branchData$bizRev,
                           c(0,25,50,75,100,1000), include.lowest = T,
                           labels = c("<25K", "25-50K", "50-75K", "75-100K", "100K+"))

#bucket new checking counts
branchData$newChkLvl <- cut(branchData$NewChkgCountsQ2,
                            c(0,50,100,150,200,1000), include.lowest = T,
                            labels = c("<50", "50-100", "100-150", "150-200", "200+"))


#bucket new checking revenue
branchData$NewChkRev <-  branchData$NewChkUSD/1000
branchData$newChkRevLvl <- cut(branchData$NewChkRev,
                            c(0,50,100,150,200,1000), include.lowest = T,
                            labels = c("<50K", "50-100K", "100-150K", "150-200K", "200K+"))


#bucket the small biz loans
#bucket new checking revenue
branchData$SBLoanBal <-  branchData$SBLoanBal/1000000
branchData$SBLoanBalLvl <- cut(branchData$SBLoanBal,
                               c(0,.5,1,2,3,100), include.lowest = T,
                               labels = c("<500K", "500K-1M", "1-2M", "2-3M", "3M+"))



#set the color scale
binColors <- colorFactor(rainbow(7), branchData$Performance.Level)
binColorsL <- colorFactor(palette = "RdYlBu", branchData$CsatLevel)
binColorsCk <- colorFactor(palette = "Blues", branchData$newChkLvl)
binColorsNewRev <- colorFactor(palette = "Greens", branchData$newChkRevLvl)
binColorsRev <- colorFactor(palette = rainbow(6), branchData$ckRevLvl)
binColorsBizRev <- colorFactor(palette = "RdYlBu", branchData$bizRevLvl)
binColorsPeer <- colorFactor(palette = "Accent", branchData$Peer.Group)
binColorsSBLoanBal <- colorFactor(palette = "RdYlBu", branchData$SBLoanBalLvl)


#create a full popup
branchData$branchAll <- paste("<strong>",branchData$Branch.Name, "</strong><br>",
                              "CSAT = ", branchData$CsatScore, "<br>",
                              "Perf Level = ", branchData$Performance.Level, "<br>",
                              "Peer Group = ", branchData$Peer.Group, "<br>",
                              "New Ck Accts = " , branchData$NewChkgCountsQ2 , "<br>",
                              "New Ck Rev = $", formatC(branchData$NewChkRev,4), "K","<br>",
                              "Ck Margin Rev = $", formatC(branchData$ckRev,4), "K", "<br>", 
                              "Biz Ck+MM Rev = $", formatC(branchData$bizRev,4), "K", "<br>", 
                              "SB Loan Bal = $", formatC(branchData$SBLoanBal,4), "M", sep = "")




m <- leaflet() %>%
    addTiles("Stamen.Toner") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    setView(lng = -71.0572145, lat = 42.3586396, zoom = 8) %>%
 
    
#csat
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon,
                     color = ~binColorsL(CsatLevel), popup = branchData$branchAll,
                     radius = ~CsatScore/10, group = "CSat") %>%
#performance
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon, 
                   color = ~binColors(Performance.Level), popup = branchData$branchAll,
                   radius = ~3*Performance.Level, group = "Perf Rating") %>%

#new checking counts  
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsCk(newChkLvl), popup = branchData$branchAll,
                     radius = ~NewChkgCountsQ2/20, group = "New Ck Counts") %>%
#total checking rev    
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsRev(ckRevLvl), popup = branchData$branchAll,
                     radius = ~ckRev/10, group = "Ck Margin Rev") %>%
#biz revenue    
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsBizRev(bizRevLvl), popup = branchData$branchAll,
                     radius = ~bizRev/5, group = "Biz Rev") %>%
#new checking revenue    
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsNewRev(newChkRevLvl), popup = branchData$branchAll,
                     radius = ~NewChkRev/10, group = "New Ck Rev") %>%
    
#Small Biz loans   
    addCircleMarkers(data = branchData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsSBLoanBal(SBLoanBalLvl), popup = branchData$branchAll,
                     radius = ~SBLoanBal*4, group = "SB Loan Bal") %>%    
    
    
    
    #layer control
    addLayersControl(
        baseGroups = c("Toner", "CartoDb"),
        overlayGroups = c("CSat",
                          "Perf Rating",
                          "New Ck Counts", 
                          "Ck Margin Rev", 
                          "Biz Rev", 
                          "New Ck Rev",
                          "SB Loan Bal"
                          ),
        options = layersControlOptions(collapsed = F)
    ) %>%
    

#per        
    addLegend("bottomright", pal = binColors, values = branchData$Performance.Level,
            title = "June 2016<br>Performance<br> Level",
            opacity = 1) %>%
#csat    
    addLegend("bottomright", pal = binColorsL, values = branchData$CsatLevel,
              title = "CSat",
              opacity = 1) %>%
#ck counts
    addLegend("topleft", pal = binColorsCk, values = branchData$newChkLvl,
          title = "New Ck Counts",
          opacity = 1) %>%

#total rev    
   addLegend("bottomleft", pal = binColorsRev, values = branchData$ckRevLvl,
              title = "Ck Margin Rev",
              opacity = 1) %>%
#biz rev    
   addLegend("bottomleft", pal = binColorsBizRev, values = branchData$bizRevLvl,
              title = "Biz Rev",
              opacity = 1) %>%
    
#new check rev
    addLegend("bottomleft", pal = binColorsNewRev, values = branchData$newChkRevLvl,
          title = "New Ck Rev",
         opacity = 1) %>%
 

#peer group
addLegend("bottomright", pal = binColorsSBLoanBal, values = branchData$SBLoanBalLvl,
          title = "SB Loan Bal",
          opacity = 1)

 
m  # Print the map








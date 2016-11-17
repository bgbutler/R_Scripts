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
branchCsat <- select(branchPl, Branch.Number, CSAT.Score)
names(branchCsat) <- c("Branch.Number", "CSat")
rm(branchPl)

#bind the data cand clean  up
#merge the branchLocs and branchPL on branch name
branchData <- inner_join(branchLocs, branchCsat, by = "Branch.Number")
rm(branchLocs); rm(branchCsat)


#get the balance sheet items
url <- "C:/Users/n846490/Documents/DataScience/balanceSheetQ2.csv"
branchBal <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)
names(branchBal) <- c("Branch.Number", "BalSheetItem", "PerfLevel", "PeerGroup", "AvgBalQ2")


#reshape branch balance sheet to wide
balWide <- spread(branchBal, BalSheetItem, AvgBalQ2)

#export to CSV for viewing
# Write the data to clean .CSV file
write.csv(balanceSheet, file="C:/Users/n846490/Documents/DataScience/balanceSheetAggQ2.csv", row.names=FALSE)

#clean up the wide file
names(balWide) <- c("Branch.Number", "PerfLevel", "PeerGroup", "BizCD", "BizCk", "BizMM",
                    "BizSav", "CDIRA", "ConsCk", "ConsCC", "ConsMM", "ConsSav", "Heloc",
                    "HomeEq", "Invest", "PersLoans")

#convert factors to numbers
#create function to clean up the NAs
na.zero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}

for (i in 4:16){
    balWide[,i] <- na.zero(balWide[,i])
}


#remove NULL perf level
balWide$PerfLevel <- gsub("NULL", NA, balWide$PerfLevel)
balWide <- na.omit(balWide)


#reimport clean balwide
url <- "C:/Users/n846490/Documents/DataScience/balWideDataQ2.csv"
balWide <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)



#Create some summary info
balWide$BizTotal <- balWide$BizCD + balWide$BizCk + balWide$BizMM + balWide$BizSav

balWide$Home <- balWide$Heloc + balWide$HomeEq

balWide$ConsTotal <- balWide$CDIRA + balWide$ConsCk + balWide$ConsCC + 
    balWide$ConsMM + balWide$ConsSav + balWide$PersLoans


#reduce the dataset
balData <- select(balWide, Branch.Number, PerfLevel, Home, BizTotal, ConsTotal, Invest)


#merge the data to one
balanceSheet <- inner_join(branchData, balData, by = "Branch.Number")

#clean it all up
rm(balData); rm(balWide); rm(branchBal); rm(branchData)


#bucket the data
#clean csat and convert it to usable buckets
balanceSheet$CSat <- as.character(balanceSheet$CSat)
balanceSheet$CSat <- gsub("%", "", balanceSheet$CSat)
balanceSheet$CSat <- as.numeric(balanceSheet$CSat)


#level the Csat score
balanceSheet$CsatLvl <- cut(balanceSheet$CSat,
                            c(0,50,60,70,80,100), include.lowest = T,
                            labels = c("<50%", "50-60%", "60-70%", "70-80%", "80-100%"))

balanceSheet$BizLvl <- cut(balanceSheet$BizTotal,
                           c(0,5000000,10000000,15000000,20000000, 1000000000), include.lowest = T,
                           labels = c("<$5M", "$5-10M", "$10-15M", "$15-20M", "$20M+"))


balanceSheet$ConsTotalLvl <- cut(balanceSheet$ConsTotal,
                           c(0,25000000,50000000,75000000,100000000, 1000000000), include.lowest = T,
                           labels = c("<$25M", "$25-50M", "$50-75M", "$75-100M", "$100M+"))


balanceSheet$HomeLvl <- cut(balanceSheet$Home,
                             c(0,5000000,10000000,15000000,20000000, 100000000), include.lowest = T,
                             labels = c("<$5M", "$5-10M", "$10-15M", "$15-20M", "$20M+"))

balanceSheet$InvestLvl <- cut(balanceSheet$Invest,
                             c(0,5000000,10000000,15000000,20000000, 100000000), include.lowest = T,
                             labels = c("<$5M", "$5-10M", "$10-15M", "$15-20M", "$20M+"))


#make perf level and peer group factos
balanceSheet$PerfLevel <- as.numeric(as.character(balanceSheet$PerfLevel))



#set the color scale
binColors <- colorFactor(rainbow(5), balanceSheet$PerfLevel)
binColorsL <- colorFactor(palette = "RdYlBu", balanceSheet$CsatLvl)
binColorsCons <- colorFactor(palette = "Greens", balanceSheet$ConsTotalLvl)
binColorsInvest <- colorFactor(palette = "Blues", balanceSheet$InvestLvl)
binColorsHome <- colorFactor(palette = rainbow(5), balanceSheet$HomeLvl)
binColorsBiz <- colorFactor(palette = "RdYlBu", balanceSheet$bizLvl)



#scale the values
scale <- function (x) {
    y <- x/1000000
    return(y)
}


for (i in 7:10){
    balanceSheet[,i] <- scale(balanceSheet[,i])
}


#create a full popup
balanceSheet$allData <- paste("<strong>",balanceSheet$Branch.Name, "</strong><br>",
                              "CSAT = ", balanceSheet$CSat, "<br>",
                              "Perf Level = ", balanceSheet$PerfLevel, "<br>",
                              "Consumer Totals = ", formatC(balanceSheet$ConsTotal,4), "M", "<br>",
                              "Biz Total = $" , formatC(balanceSheet$BizTotal,4) , "M", "<br>",
                              "Home Total = $", formatC(balanceSheet$Home,4), "M","<br>",
                              "Investments = $", formatC(balanceSheet$Invest,4), "M", "<br>", sep = "")


m <- leaflet() %>%
    addTiles("Stamen.Toner") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    setView(lng = -71.0572145, lat = 42.3586396, zoom = 8) %>%
    
    
    #csat
    addCircleMarkers(data = balanceSheet, lat = ~Lat, lng = ~Lon,
                     color = ~binColorsL(CsatLvl), popup = balanceSheet$allData,
                     radius = ~CSat/10, group = "CSat") %>%
    #performance
    addCircleMarkers(data = balanceSheet, lat = ~Lat, lng = ~Lon, 
                     color = ~binColors(PerfLevel), popup = balanceSheet$allData,
                     radius = ~3*PerfLevel, group = "Performance") %>%
    
    #consumer
    addCircleMarkers(data = balanceSheet, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsCons(ConsTotalLvl), popup = balanceSheet$allData,
                     radius = ~ConsTotal/5, group = "Consumer") %>%
    
    #biz balances    
    addCircleMarkers(data = balanceSheet, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsBiz(BizLvl), popup = balanceSheet$allData,
                     radius = ~BizTotal/5, group = "Sm Business") %>%
    
    #Home   
    addCircleMarkers(data = balanceSheet, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsHome(HomeLvl), popup = balanceSheet$allData,
                     radius = ~Home, group = "Home") %>%
    
    #Investments   
    addCircleMarkers(data = balanceSheet, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsInvest(InvestLvl), popup = balanceSheet$allData,
                     radius = ~Invest, group = "Investments") %>%    
    
    
    
    #layer control
    addLayersControl(
        baseGroups = c("Toner", "CartoDb"),
        overlayGroups = c("CSat",
                          "Performance",
                          "Consumer", 
                          "Home", 
                          "Sm Business", 
                          "Investments"
        ),
        options = layersControlOptions(collapsed = F)
    ) %>%
    
    
    #per        
    addLegend("bottomright", pal = binColors, values = balanceSheet$PerfLevel,
              title = "June 2016<br>Performance<br> Level",
              opacity = 1) %>%
    #csat    
    addLegend("bottomright", pal = binColorsL, values = balanceSheet$CsatLvl,
              title = "CSat",
              opacity = 1) %>%
    
    #consumer
    addLegend("topleft", pal = binColorsCons, values = balanceSheet$ConsTotalLvl,
              title = "Consumer Bal",
              opacity = 1) %>%
    
    #biz 
    addLegend("bottomleft", pal = binColorsBiz, values = balanceSheet$BizLvl,
              title = "Sm Biz Balance",
              opacity = 1) %>%
    
    #home
    addLegend("bottomleft", pal = binColorsHome, values = balanceSheet$HomeLvl,
              title = "Home Prods",
              opacity = 1) %>%
    
    
    #peer group
    addLegend("bottomleft", pal = binColorsInvest, values = balanceSheet$InvestLvl,
              title = "Investments",
              opacity = 1)


m  # Print the map







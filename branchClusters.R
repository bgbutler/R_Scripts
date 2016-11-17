#perform clustering of the branches


#regression analyses of data
.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

library(Rcpp)
library(dplyr)
library(plyr)
library(ggplot2)
library(caret)
library(dendextend)
library(dendextendRcpp)
library(RColorBrewer)
library(htmltools)

if(!require(jsonlite)) install.packages("jsonlite", repos='http://cran.cnr.berkeley.edu')
library(jsonlite)
library(reshape2)
library(tidyr)
library(leaflet)


#get the files  with location
url <- "C:/Users/n846490/Documents/DataScience/q2OneCombined.csv"
data <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#separate branch number and location
locData <- data[,c(1:6)]

#remove unneeded data and performance metrics
data <- data[,c(2,5,7:10, 12:20)]

branches <- data[,1]

#set pure data for clustering
data <- data[,c(2:15)]

row.names(data) <- branches

head(data, 20)

#check the scree plot for centers
wss <- (nrow(data)-1)*sum(apply(data,2,var))

for (i in 2:8) wss[i] <- sum(kmeans(data,centers=i)$withinss)

plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


#dendrogram
#check the clustering of the responses
#set K
clust <- 4


par(mar=c(2,2,2,4))
d <- dist(data, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = clust) %>%
    set("labels_cex", .75) %>%
    color_branches(k=clust) %>%
    set("branches_lwd", 3) %>%
    hang.dendrogram (hang = -1) %>%
    plot(horiz=F,main = paste("Branch Clusters = ",clust, sep = ""), axes = T)
dend %>% rect.dendrogram(k = clust, horiz=F)



#alternate if we need to pretty it up, scale is 1.0e+08
par(mar=c(2,2,2,4))
d <- dist(data, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = k) %>%
    set("labels_cex", .75) %>%
    color_branches(k=4) %>%
    set("branches_lwd", 3) %>%
    hang.dendrogram (hang = -1) %>%
    plot(horiz=F,main = "Branch Clusters", axes = F, ylim = c(0,1000))
axis(side = 2, col = "blue", at = seq(0,1000,100), labels = FALSE)
mtext(seq(0,1000,100), side = 2, at = seq(0,1000,100),
      line = 1, col = "blue", las = 1)
dend %>% rect.dendrogram(k = k, horiz=F)


#The cutree function returns cluster assignments that are based on a least to greatest ordering of the name of the objects being clustered
cvec <- cutree(dend, k = 4)
cvec


#make the dataframe from cvec
assignment <- as.data.frame(unlist(cvec))
head(assignment)

#get the labels you need
assignment$Branch.Number <- row.names(assignment)

#clean up
names(assignment) <- c("Cluster", "Branch.Number")


#merge it with loc data for mapping
#ensure that the data types are the same
locData$Branch.Number <- as.character(locData$Branch.Number)
locData$Branch.Name <- as.character(locData$Branch.Name)

#merge
branchClusters <- inner_join(locData, assignment, by = 'Branch.Number')

branchClusters$CsatLvl <- cut(branchClusters$CSat,
                            c(0,50,60,70,80,100), include.lowest = T,
                            labels = c("<50%", "50-60%", "60-70%", "70-80%", "80-100%"))


#create a full popup
branchClusters$popup <- paste("<strong>",branchClusters$Branch.Name, "</strong><br>",
                              "CSAT = ", branchClusters$CSat, "<br>",
                              "Perf Level = ", branchClusters$PerfLevel, "<br>",
                              "Cluster = ", branchClusters$Cluster, sep = "")

#set the color scale
binColors <- colorFactor(rainbow(5), branchClusters$PerfLevel)
binColorsCSat <- colorFactor(palette = "RdYlBu", branchClusters$CsatLvl)
binColorsClust <- colorFactor(palette = "RdYlBu", branchClusters$Cluster)


m <- leaflet() %>%
    addTiles("Stamen.Toner") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    setView(lng = -71.0572145, lat = 42.3586396, zoom = 8) %>%
    
    
    #csat
    addCircleMarkers(data = branchClusters, lat = ~Lat, lng = ~Lon,
                     color = ~binColorsCSat(CsatLvl), popup = branchClusters$popup,
                     radius = ~CSat/10, group = "CSat") %>%
    #performance
    addCircleMarkers(data = branchClusters, lat = ~Lat, lng = ~Lon, 
                     color = ~binColors(PerfLevel), popup = branchClusters$popup,
                     radius = ~3*PerfLevel, group = "Performance") %>%
    
    #cons checking  
    addCircleMarkers(data = branchClusters, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsClust(Cluster), popup = branchClusters$popup,
                     radius = ~Cluster*10, group = "Cluster") %>%
    
   
    
    
    
    #layer control
    addLayersControl(
        baseGroups = c("Toner", "CartoDb"),
        overlayGroups = c("CSat",
                          "Performance",
                          "Cluster"
        ),
        options = layersControlOptions(collapsed = F)
    ) %>%
    
    
    #per        
    addLegend("bottomright", pal = binColors, values = branchClusters$PerfLevel,
              title = "June 2016<br>Performance<br> Level",
              opacity = 1) %>%
    #csat    
    addLegend("bottomright", pal = binColorsCSat, values = branchClusters$CsatLvl,
              title = "CSat",
              opacity = 1) %>%
    
    #cluster
    addLegend("bottomleft", pal = binColorsClust, values = branchClusters$Cluster,
              title = "Cluster",
              opacity = 1)
    
  
m  # Print the map



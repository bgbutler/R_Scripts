#testing alternative mapping with leaflet

#install packages for manipulating and plotting data
library(ggplot2)
library(maps)
library(dplyr)
library(htmltools)
library(devtools)
library(leaflet)
library(htmlwidgets)

#get the final cleaned data
#if the file is somewhere else, change the URL
#casts all blanks as NA
url <- "Q:/Analytics/DisneyMap/BoseImagesCleanReducedV2.csv"
boseClean <- read.csv(url, header = TRUE, na.strings=c("","NA"), sep=",", as.is = FALSE)

#set the directory for the output
setwd("Q:/Analytics/DisneyMap")

#clean up links to photos and names
#clean up the column names
names(boseClean)[8:9] <- c("Store","Caption")

# set the map colors up for the store layer
binColors2 <- colorFactor(rainbow(4), boseClean$Store)

#create the popup from the first name and their caption
#this will be appended to the photo later on
boseClean$Popup <- paste(boseClean$First, boseClean$Caption, sep = "-")

#change the ImageKey to be directory of photos
#the photos are currently in the directory mentioned in the repalce text
pattern1 <- "Bose/Your_Mission_Show_Us_The_Store_/"
replaceText <- "Q:/Analytics/DisneyMap/JPGs/"
boseClean$ImgURL <- gsub(pattern1, replaceText, boseClean$ImageKey)

#import an interesting icon for music
#we can choose whichever we want for disney
#K:\Sandbox\R\Mapping\Icons\music-15.svg
photoIcons <- iconList(
  music = makeIcon("K:/Sandbox/R/Mapping/Icons/music-15.svg", 
                   iconWidth = 18, iconHeight = 18)
)

#export to view file
#this step was for QA only and is not necessary
#export the data as a CSV - name the file
#write.csv(boseClean, file = "Q:/Analytics/DisneyMap/boseLinksCleanV2.csv", row.names = FALSE)

#load in a new clean file after removing the old one
#step included for QA only
#rm(boseClean)

#get the final cleaned data
#url <- "Q:/Analytics/DisneyMap/boseLinksClean.csv"
#boseClean <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


#create image text HTML
boseClean$ImgText <- ifelse(boseClean$Popup != "NA",paste("<div>",
                           boseClean$Popup,"</div>", sep = ""),"")

#create Image photo HTML
boseClean$ImgSrc <- ifelse(boseClean$ImgURL !="NA",
                          paste("<img src='",
                          boseClean$ImgURL, "'",
                          " alt='Photo'",
                          " style='width:300px;height:300px;' />", sep = ""),
                          "")


#let's do a test of adding on the text to the photo
#generate the HTML for the content and photo
#this works now, need to do via HTML
boseClean$HTML <- ifelse(boseClean$ImgSrc != "NA",
                        paste("<div>",
                        boseClean$ImgSrc,
                        "</div>",
                        boseClean$ImgText, sep = "<br/>"),
                        "")

#create the map opbject
#this has just the photos and text
m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stamen.Toner")  %>%
  setView(lng = -71.13, lat = 42.3, zoom = 4) %>%
  addMarkers(data = boseClean, lat = ~Latitude, lng = ~Longitude, 
             popup = boseClean$Popup, icon = ~photoIcons, group = "Comments") %>%
  addMarkers(data = boseClean, lat = ~Latitude, lng = ~Longitude, 
             popup = boseClean$HTML, icon = ~photoIcons, group = "Photos") %>%
  addCircleMarkers(data = boseClean, lat = ~ Latitude, lng = ~ Longitude, 
                   color = ~binColors2(Store), radius = 3, group = "Store") %>%
  
  #layers control
  addLayersControl(
    baseGroups = "Base Map",
    overlayGroups = c("Store", "Photos", "Comments"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  
  addLegend("bottomright",pal = binColors2, values = boseClean$Store,
            title = "Store Viewed",
            opacity = 1)
m

saveWidget(m,file = "boseTestPhotosText.html")







#lets add a movie
#mov files do not work, mp4 files do
#boseClean$Movie <- paste("<video width='320' height='240' controls>",
#                         "<source src='",boseClean$Mov,
#                         "' type='video/mov'></video>", sep="")

boseClean$Mp4s <- ifelse(boseClean$MPs != "NA",
                        paste("<video width='320' height='240' controls>",
                        "<source src='",boseClean$MPs,
                        "' type='video/mp4'></video>", sep=""),
                        "")


#now we have photos with text and movies
m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stamen.Toner")  %>%
  setView(lng = -71.13, lat = 42.3, zoom = 4) %>%
  addMarkers(data = boseClean, lat = ~Latitude, lng = ~Longitude, 
             popup = boseClean$Popup, icon = ~photoIcons, group = "Comments") %>%
  
  addMarkers(data = boseClean, lat = ~Latitude, lng = ~Longitude, 
             popup = boseClean$HTML, icon = ~photoIcons, group = "Photos") %>%
  
  addMarkers(data = boseClean, lat = ~Latitude, lng = ~Longitude, 
             popup = boseClean$Mp4s, group = "MP4") %>%
  
  addCircleMarkers(data = boseClean, lat = ~ Latitude, lng = ~ Longitude, 
                   color = ~binColors2(Store), radius = 3, group = "Store") %>%
  
  #layers control
  addLayersControl(
    baseGroups = "Tonor",
    overlayGroups = c("Store", "Photos", "Comments", "MP4"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  
  addLegend("bottomright",pal = binColors2, values = boseClean$Store,
            title = "Store Viewed",
            opacity = 1)
m

saveWidget(m,file = "boseTestTextPhotoMP4.html")

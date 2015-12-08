#Production Version of Wal Mart Mapping
#Map the WalMart Comments

#install packages for manipulating and plotting data
library(zipcode)
library(ggplot2)
library(maps)
library(dplyr)
library(htmltools)
library(devtools)
library(leaflet)
library(stringi)


#get the zipcode data
data(zipcode)


#get the requirements for leaflet and leaflet
if(!require(devtools)) install.packages("devtools")
if(!require(leaflet)) install_github("rstudio/leaflet")

#get zipcode data and cleanse
url <- "K:/Sandbox/R/Data IO/wmDropOut.csv"
walmartRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


#create essay char length
walmartRaw$EssayLength <- sapply(gregexpr("[[:alpha:]]+", walmartRaw$Q28), function(x) sum(x > 0))

mobilePC <- walmartRaw[walmartRaw$renderingmode != 'generic',]

plt0 <- ggplot(mobilePC, aes(x= EssayLength, fill = renderingmode)) + xlim(0,40)
plt0 + geom_density(alpha = 0.3) + xlab("Count of Essay Words") +ylab("Proportion") + 
  theme(
    legend.position="right",
    axis.title.x = element_text(colour = "red"),
    axis.title.y = element_text(colour = "red"),
    legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Histogram of Essay Length by Word for Drop-Outs") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

by_Computer <- group_by(mobilePC, renderingmode)
avgWord <- summarise(by_Computer,
                     avg = mean(EssayLength, na.rm = T),
                     stdev = sd(EssayLength, na.rm = T))
avgWord


computer <- aggregate(mobilePC$EssayLength, list(Computer = mobilePC$EssayLength), FUN = length)
computer

aggregate(mobilePC$so...SO.Location...SO.Location, list(Computer = mobilePC$so...SO.Location...SO.Location), FUN = length)
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
library(lubridate)


#get the zipcode data
data(zipcode)


#get the requirements for leaflet and leaflet
if(!require(devtools)) install.packages("devtools")
if(!require(leaflet)) install_github("rstudio/leaflet")

#get zipcode data and cleanse
url <- "K:/Sandbox/R/Data IO/allCompletes2.csv"
walmartRaw <- read.csv(url, header = TRUE, na.strings = c('', "NA"), sep=",", as.is = FALSE)

completes <- walmartRaw

#create essay char length
walmartRaw$EssayLength <- sapply(gregexpr("[[:alpha:]]+", walmartRaw$Q28), function(x) sum(x > 0))

#check the distribution of computer types
computer <- aggregate(walmartRaw$renderingmode, 
                      list(Computer = walmartRaw$renderingmode), FUN = length)
computer

# since generic is very small drop from data set
mobilePC <- walmartRaw[walmartRaw$renderingmode != 'generic',]

#get the average length of the essay by words
by_Computer <- group_by(mobilePC, renderingmode)
avgWord <- summarise(by_Computer,
                     avg = mean(EssayLength, na.rm = T),
                     stdev = sd(EssayLength, na.rm = T))
avgWord

#convert interview star and end to dates
mobilePC$interview_start <- as.character(mobilePC$interview_start)
mobilePC$interview_end <- as.character(mobilePC$interview_end)

#create a subset of the data
Duration <- select(mobilePC, interview_end, interview_start, renderingmode, EssayLength)

Duration <- na.omit(Duration)

Duration$interview_start <- strptime(Duration$interview_start, format = "%d/%m/%y %H:%M")
Duration$interview_end <- strptime(Duration$interview_end, format = "%d/%m/%y %H:%M")


Duration$duration <- difftime(Duration$interview_end,
                              Duration$interview_start, units = "mins")

Duration$mins <- as.numeric(Duration$duration)


plt0 <- ggplot(sumStats, aes(x= mins, fill = renderingmode)) + xlim(0,40)
plt0 + geom_histogram(binwidth = 1, alpha = .5) + scale_fill_discrete(name = "Rendering") +
  theme(
    legend.position="right",
    axis.title.x = element_text(colour = "red"),
    axis.title.y = element_text(colour = "red"),
    legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Comparison of Complete Times by Rendering") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

plt0 <- ggplot(sumStats, aes(x= mins, fill = renderingmode)) + xlim(0,40)
plt0 + geom_density(alpha = .5) + scale_fill_discrete(name = "Rendering") +
  theme(
    legend.position="right",
    axis.title.x = element_text(colour = "red"),
    axis.title.y = element_text(colour = "red"),
    legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Comparison of Survey Completion Time by Rendering") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

plt0 <- ggplot(sumStats, aes(x= EssayLength, fill = renderingmode)) + xlim(0,100)
plt0 + geom_density(alpha = .5) + scale_fill_discrete(name = "Rendering") +
  theme(
    legend.position="right",
    axis.title.x = element_text(colour = "red"),
    axis.title.y = element_text(colour = "red"),
    legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Comparison of Essay Length (Words) by Rendering") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


#subset for summary
sumStats <- select(Duration, renderingmode, EssayLength, mins)


by_Computer <- group_by(sumStats, renderingmode)
stats <- summarise(by_Computer,
                     medianMins = median(mins, na.rm = T),
                     avgWords = mean(EssayLength, na.rm = T),
                     stdWords = sd(EssayLength, na.rm = T)
                    )
stats

computer <- aggregate(mobilePC$renderingmode, list(Computer = mobilePC$renderingmode), FUN = length)
computer
#script to analyze web analytics

.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(scales)
library(gridExtra)
library(stringr)
library(htmlwidgets)
library(readxl)
library(dendextend)
library(dendextendRcpp)
library(zoo)
library(xts)


url <- "C:/Users/n846490/Documents/DataScience/DigitalAnalytics/CSVs"

bus <- read_excel(url, sheet = "Dataset1", col_names = T, col_types = NULL, skip = 0)

bus <- na.omit(bus)
bus <- bus[-8] 

#split the string by the "/"
#return a list
splits <- str_split(bus$Page, pattern= "/")

urlCapability <- character()
urlSegment <- character()
for (i in 1:40) {
    urlCapability[i] <-  splits[[i]][4]
    urlSegment[i] <-  splits[[i]][5]
}

#bind them to the dataframe
bus$urlCapability <- urlCapability
bus$urlSegment <- urlSegment


#test for products (last segment of url)
product <- character()
for (i in 1: length(splits)) {
    l <- length(splits[[i]])
    product[i] <- splits[[i]][l]
}

bus$Product <- product


#one last cleanup
bus$Product <- ifelse(bus$Product == "", NA, bus$Product)

bus <- na.omit(bus)

##############################################################################
#not always needed
#make na blanks
#create function to clean up the NAs
na.zero <- function (x) {
    x[is.na(x)] <- ""
    return(x)
}

for (i in 7:9){
    bus <- na.zero(bus)
}



#make short url

bus$ShortUrl <- substr(bus$Page,14, nchar(bus$Page))
###########################################################################

#make factors for plotting
bus$urlCapability <- as.factor(bus$urlCapability)
bus$urlSegment <- as.factor(bus$urlSegment)

##############################################
#Get the time series
url <- "C:/Users/n846490/Documents/DataScience/DigitalAnalytics/businessBanking2016.xlsx"

#social referrals
url <- "C:/Users/n846490/Documents/DataScience/DigitalAnalytics/socialReferralChecking.xlsx"

ts <- read_excel(url, sheet = "Dataset2", col_names = T, col_types = NULL, na = "", skip = 0)

colnames(ts) <- c ("Date", "Pageviews")
ts$Date <- as.POSIXct(ts$Date, origin="2016-01-01")
ts$DateChar <- as.character(ts$Date)


#remove the summary
ts <- na.omit(ts)

#date formatting
ts$Month <- format(ts$Date, format = "%b")
ts$Month <- as.factor(ts$Month)

ts$Month <- ordered(ts$Month, levels = c("Jan", "Feb", "Mar", 
                                        "Apr", "May", "Jun", "Jul",
                                        "Aug", "Sep", "Oct"))


ts$Day <- as.POSIXlt(ts$Date)$mday


#plot the time series
P <- ggplot(ts, aes(x = Day, y = Pageviews)) +
    geom_line(aes(group=Month)) + geom_point() + 
    facet_wrap(~Month, ncol = 3) +
    xlab("") +
    ylab("") + 
    ylim(0,5000) + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          legend.position = "bottom",
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"),
          strip.text.x = element_text(size = 10, face = "bold", color = "white"),
          strip.background = element_rect(fill = "red"))
P



#add moving average to dataframe series
ma30 <- ma30 <- rollmean(z, 30, align = "right")

tsRed <- ts[c(30:290),]

tsRed$ma30 <- ma30

##########################################################################
#plot the time series
#overlay the moving average
#work on a way to add a legeng of moving averages

P <- ggplot(tsRed, aes(x = Date, y = Pageviews, color = Month)) +
    geom_line() + geom_point() + 
    geom_line(aes(x = Date, y = ma30), color = "blue", size = 1.25) + 
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          legend.position = "bottom",
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red")) +
    annotate("text", label = "Blue Line is 30 Day Mvg Avg", color = "red", x= as.POSIXct("2016-08-01"), y = 2000)
P





#############################################################################
#clean  up with NA omits

mat <- as.matrix(bus[c(2:32),c(2:7)])

row.names(mat) <- bus$Product[2:32]


mat <- scale(mat, center = TRUE, scale = TRUE)

#check the scree plot for centers
wss <- (nrow(mat)-1)*sum(apply(mat,2,var))

for (i in 2:10) wss[i] <- sum(kmeans(mat,centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#pageviews
matPV <- mat[,c(1,2,4)]
matBounce <- mat[,c(3,5,6)]
clust <- 7

par(mar=c(2,1,1,12))
d <- dist(mat, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = clust) %>%
    set("labels_cex", .75) %>%
    color_branches(k=clust) %>%
    set("branches_lwd", 3) %>%
    hang.dendrogram (hang = -1) %>%
    plot(horiz=T, axes = T, xlim = c(4,0))
dend %>% rect.dendrogram(k = clust, horiz=T)


#convert time to avg mins on page
bus$`Avg. Time on Page` <- bus$`Avg. Time on Page`/60


#check pageviews and time on page
#scatter plots
bus <- bus[bus$urlCapability != "https:",]


g <- ggplot(bus, aes(x = Pageviews, y = `Avg. Time on Page`)) + 
    geom_point(alpha = .6) +  
    scale_x_continuous(labels = comma) +
    geom_smooth(size = 2) +
    ylim(0,4) + 
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


#split the group
busLarge <- filter(bus, Pageviews >5000)
busSmall <- filter(bus, Pageviews <= 5000)


busLarge <- arrange(busLarge, desc(Pageviews))
busSmall <- arrange(busSmall, desc(Pageviews))

#clean up the column names
newNames = c("Page", "Pageviews", "UniquePgViews", "AvgMinsonPage",
             "Entrances", "BounceRate", "Exit", "urlCapability",
             "urlSegment", "Product")

colnames(bus) <- newNames

colnames(busLarge) <- newNames
colnames(busSmall) <- newNames


#melt the data for plotting metrics
busMeltL <- melt(busLarge, id = c("Page", "urlCapability", "urlSegment", "Product"),
                 variable.name = "Metric",
                 value.name = "Value")



busMeltS <- melt(busSmall, id = c("Page", "urlCapability", "urlSegment", "Product"),
                  variable.name = "Metric",
                  value.name = "Value")



#remove /us/business
busMeltL <- filter(busMeltL,Prodcut != "/us/business")



busMeltLSub <- filter(busMeltL,
                      Metric != "AvgMinsonPage" & Metric != "BounceRate" & 
                          Metric != "Exit")
busMeltSSub <- filter(busMeltS, Metric != "AvgMinsonPage" & Metric != "BounceRate" & 
                          Metric != "Exit")

busMeltLSub <- arrange(busMeltLSub, desc(Value))
busMeltSSub <- arrange(busMeltSSub, desc(Value))


#make a plot
P <- ggplot(busMeltLSub, aes(x = reorder(Product, Value), y = Value, fill = urlCapability)) +
    geom_bar(stat = "identity") + coord_flip() + 
    facet_wrap(~Metric, nrow = 1) +
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          legend.position = "bottom",
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red"))
P


P <- ggplot(busMeltSSub, aes(x = reorder(ShortUrl, Value), y = Value, fill = Metric)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) +
    facet_wrap(~Metric, nrow = 1) +
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          legend.position = "bottom",
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red"))
P



#make a nice table

library(formattable)

busTable <- select(busLarge, Product, Pageviews, UniquePgViews)
busTable$Pageviews <- busTable$Pageviews/10
busTable$UniquePgViews <- busTable$UniquePgViews/10

as.htmlwidget(formattable(busTable, list(
    Product = formatter(
    "span",
    style = x ~ ifelse( x > 0, style( color = "red", font.weight = "bold" ), NA ))
    )))

as.htmlwidget(formattable(busTable))




#make other plots
busMeltLSubX <- filter(busMeltL, Metric == "AvgMinsonPage" | Metric == "BounceRate" | Metric == "Exit")
busMeltSSubX <- filter(busMeltS, Metric == "AvgMinsonPage" | Metric == "BounceRate" | Metric == "Exit") 

P <- ggplot(busMeltLSubX, aes(x = reorder(ShortUrl, Value), y = Value,  fill = urlCapability)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) +
    facet_wrap(~Metric, nrow = 1) +
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red"))
P


busMeltSSubX <- filter(busMeltSSubX, 
                urlCapability %in% c("banking","borrowing", "business"))

P <- ggplot(busMeltSSubX, aes(x = reorder(ShortUrl, Value), y = Value , fill = urlCapability)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) +
    facet_wrap(~Metric, nrow = 1) +
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          legend.position = "bottom",
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red"))
P



#remove /us/business/ from master set
#remove /us/business/
bus<- filter(bus,Page != "/us/business")


busSegment <- group_by(bus, urlCapability, urlSegment)

busPages <- summarise(busSegment,
                       totViews = sum(Pageviews))

busPages <- filter(busPages, urlCapability %in% c("banking","borrowing"))


P <- ggplot(busPages, aes(x = reorder(urlSegment, totViews), y = totViews, fill = urlCapability)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) + facet_wrap(~urlCapability) +
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"),
          axis.text.y=element_text(color = "red"),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red"))
P


bus<- filter(bus,Page != "/us/business")
busClean <- filter(bus, urlCapability %in% c("banking","borrowing", "business"))



busSegment <- group_by(busClean, urlCapability, urlSegment)

busTime <- summarise(busSegment,
                       totTime = mean(AvgMinsonPage))


P <- ggplot(busTime, aes(x = reorder(urlSegment, totTime), y = totTime, fill = urlCapability)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) + facet_wrap(~urlCapability) +
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          legend.position = "bottom",
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red"))
P





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



url <- "C:/Users/n846490/Documents/DataScience/DigitalAnalytics/CorporateDataPages20160101-20161003.xlsx"

corp <- read_excel(url, sheet = "MktURLs", col_names = T, col_types = NULL, na = "", skip = 0)



#remove 1 URL with NAs
#/us/corporate/corporate-businesses/specialty-banking/healthcare-education-and-not-for-profit
corp <- na.omit(corp)
 

#split the string by the "/"
#return a list
splits <- str_split(corp$URLRequested, pattern= "/")


for (i in 1:45) {
    corp$urlCapability[i] <-  splits[[i]][4]
    corp$urlSegment[i] <-  splits[[i]][5]
}


#make na blanks
#create function to clean up the NAs
na.zero <- function (x) {
    x[is.na(x)] <- ""
    return(x)
}

for (i in 7:10){
    corp <- na.zero(corp)
}

#make short url

corp$ShortUrl <- substr(corp$URLRequested,14, nchar(corp$URLRequested))


#make factors for plotting
corp$urlCapability <- as.factor(corp$urlCapability)
corp$urlSegment <- as.factor(corp$urlSegment)


#create a matrix for clustering the short urls

mat <- as.matrix(corp[c(2:45),c(2:6)])

row.names(mat) <- corp$ShortUrl[2:45]


matS <- scale(mat, center = TRUE, scale = TRUE)

#check the scree plot for centers
wss <- (nrow(mat)-1)*sum(apply(mat,2,var))

for (i in 2:10) wss[i] <- sum(kmeans(mat,centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")




clust <- 8

par(mar=c(2,2,2,35))
d <- dist(matS, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = clust) %>%
    set("labels_cex", .75) %>%
    color_branches(k=clust) %>%
    set("branches_lwd", 3) %>%
    hang.dendrogram (hang = -1) %>%
    plot(horiz=T, axes = T, xlim = c(10,0))
dend %>% rect.dendrogram(k = clust, horiz=T)






#split the group
corpLarge <- filter(corp, PageViews >5000)
corpSmall <- filter(corp, PageViews <= 5000)



corpLarge <- arrange(corpLarge, desc(PageViews))
corpSmall <- arrange(corpSmall, desc(PageViews))



#melt the data for plotting metrics
corpMeltL <- melt(corpLarge, id = c("URLRequested", "urlCapability", "urlSegment", "ShortUrl"),
                 variable.name = "Metric",
                 value.name = "Value")



corpMeltS <- melt(corpSmall, id = c("URLRequested", "urlCapability", "urlSegment", "ShortUrl"),
                  variable.name = "Metric",
                  value.name = "Value")



#remove /us/corporate
corpMeltL <- filter(corpMeltL,URLRequested != "/us/corporate")


corpMeltLSub <- filter(corpMeltL, Metric != "AvgMinsonPage" & Metric != "BounceRate")
corpMeltSSub <- filter(corpMeltS, Metric != "AvgMinsonPage" & Metric != "BounceRate")


#make a plot
P <- ggplot(corpMeltLSub, aes(x = reorder(ShortUrl, Value), y = Value)) +
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) +
    facet_wrap(~Metric, nrow = 1) +
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"))
P


P <- ggplot(corpMeltSSub, aes(x = reorder(ShortUrl, Value, fill = urlCapability), y = Value)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) +
    facet_wrap(~Metric, nrow = 1) +
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"))
P



#make other plots
corpMeltLSubX <- filter(corpMeltL, Metric == "AvgMinsonPage" | Metric == "BounceRate")
corpMeltSSubX <- filter(corpMeltS, Metric == "AvgMinsonPage" | Metric == "BounceRate")

P <- ggplot(corpMeltLSubX, aes(x = reorder(ShortUrl, Value, fill = urlCapability), y = Value)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) +
    facet_wrap(~Metric, nrow = 1) +
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"))
P




P <- ggplot(corpMeltSSubX, aes(x = reorder(ShortUrl, Value, fill = urlCapability), y = Value)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) +
    facet_wrap(~Metric, nrow = 1) +
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"))
P



#remove /us/corporate from master set
#remove /us/corporate
corp<- filter(corp,URLRequested != "/us/corporate")


corpSegment <- group_by(corp, urlCapability, urlSegment)

corpPages <- summarise(corpSegment,
                       totViews = sum(PageViews))


P <- ggplot(corpPages, aes(x = reorder(urlSegment, totViews), y = totViews)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) + facet_wrap(~urlCapability) +
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"))
P


corp<- filter(corp,URLRequested != "/us/corporate")


corpSegment <- group_by(corp, urlCapability, urlSegment)

corpTime <- summarise(corpSegment,
                       totTime = Avg(AvgMinsonPage))


P <- ggplot(corpTime, aes(x = reorder(urlSegment, totTime), y = totTime)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) + facet_wrap(~urlCapability) +
    xlab("") +
    ylab("") + 
    theme(plot.title = element_text(face = "italic", color = "red"),
          axis.title.x=element_text(color = "red"),
          axis.title.y=element_text(color = "red"),
          axis.text.x=element_text(color = "red", angle = 60),
          axis.text.y=element_text(color = "red"))
P





.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

library(Rcpp)
library(dplyr)
library(plyr)
library(ggplot2)
library(caret)
library(Metrics)
library(pROC)
library(rpart)
library(rpart.plot)
library(gbm)
library(e1071)
library(glmnet)
library(dendextend)
library(dendextendRcpp)
library(RColorBrewer)
library(htmltools)

#get the data
#get the ONE transaction data
url <- "C:/Users/n846490/Documents/DataScience/CSVs/netInsight0729.csv"
trans <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


trans$AllNAoCategories <- as.character(trans$AllNAoCategories)


#create function to clean up the NAs
na.zero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}

for (i in 2:31){
    trans[,i] <- na.zero(trans[,i])
}



#use grob2x to remove the c2c
grx <- glob2rx(("*c2s*"))

allOther <- with(trans, subset(trans, subset = !grepl(grx, trans$AllNAoCategories), drop = TRUE))


allOtherRed <- allOther[,c(2:31)]

allOtherCS <- scale(allOtherRed, center = T, scale = T)
row.names(allOtherCS) <- allOther$AllNAoCategories



transT <- as.data.frame(t(allOtherRed))
colnames(transT) <- allOther$AllNAoCategories






#center and scale the data

transCS <- scale(transT, center = T, scale = T)

#cluster the transactions
clust <- 7



myPath <- file.path("C:","Users", "n846490", "Documents", "DataScience", "Exhibits",
                    paste("nonC2SClusters", ".pdf", sep="")) 

pdf(file = myPath, onefile = F, paper = "USr", width = 8.5, height = 11)


par(mar=c(2,2,2,12))
d <- dist(allOtherCS, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = clust) %>%
    set("labels_cex", .75) %>%
    color_branches(k=clust) %>%
    set("branches_lwd", 3) %>%
    hang.dendrogram (hang = -1) %>%
    plot(horiz=T,main = paste("ONE Activities Cluster = ",clust, sep = ""), axes = T,  xlim = c(10,0))


dev.off()



dend %>% rect.dendrogram(k = clust, horiz=T)











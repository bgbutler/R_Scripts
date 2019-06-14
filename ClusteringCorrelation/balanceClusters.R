#script to analyze cross sell opportunities

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
#get the files  with location
url <- "C:/Users/n846490/Documents/DataScience/balWideDataQ2.csv"
bal <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


#just get the individual balances
balRed <- bal[,c(4:16)]

#transpose the data so that it is clustering by transaction type

balRedT <- as.data.frame(t(balRed))
colnames(balRedT) <- balRed$Branch.Number


#cluster the transactions
clust <- 7


par(mar=c(2,2,2,10))
d <- dist(balRedT, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
dend %>% set("labels_col", k = clust) %>%
    set("labels_cex", .75) %>%
    color_branches(k=clust) %>%
    set("branches_lwd", 3) %>%
    hang.dendrogram (hang = -1) %>%
    plot(horiz=T,main = paste("Cross Sell Opps By Balances Clusters = ",clust, sep = ""), axes = T, xlim = c(600000000,0))
dend %>% rect.dendrogram(k = clust, horiz=T)










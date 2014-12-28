#get the data first

library(reshape2)
library(reshape)

dataURL <- "//ABYSS/Finance/Finance - SugarSync/Operational Metrics/Mix Analysis/State Data.csv"
stateData <- read.csv(dataURL, header = TRUE, as.is = TRUE, sep = "," )
stateAllMonths <- stateData[1:50,1:41]

#create melted data
meltState <- melt(stateAllMonths, id = c("Date", "ID"))

#make date a factor sort date
meltState <- meltState[order(meltState$Date),]
rownames(meltState) = meltState$State
stateLabels = meltState$State

#drop non-numerical data
data <-meltState[,4]
rownames(data) = stateLabels




#clean up column names
colnames(meltState) <- c("date", "day", "state", "binds")

#make a subset
stateBinds <- subset(meltState, select = c(state, day, binds))
labs <- paste('X', 1:39, sep='')


#this is for clusting of the data
dm <-dist(data)
cluster <- hclust(dm)
plot(cluster, hang = -1)

#use melted data
dmelt <- dist(stateBinds)
cluster <- hclust(dmelt)
plot(cluster, hang = -1,  labels = F)

## mark clusters
clusters <- as.factor (cutree (cluster, k = 10))
levels (clusters) <- LETTERS [1 : 4]
mark.dendrogram (cluster, clusters, label = "cluster")


# load package ape; remember to install it: install.packages('ape')
library(ape)
# plot basic tree
plot(as.phylo(cluster), cex = 0.9, label.offset = 1)


hcd = as.dendrogram(cluster)
op = par(mfrow = c(2, 1))
plot(cut(hcd, h = 25)$upper, main = "Upper tree of cut at h=75")
plot(cut(hcd, h = 25)$lower[[2]], main = "Second branch of lower tree with cut at h=75")





# heloc map

library(tidyverse)
library(DT)
library(leaflet)
library(zipcode)
library(reshape2)
library(RColorBrewer)
library(ggmap)
library(ggplot2)
library(scales)

library(gridExtra)
library(tidytext)

library(plotly)

library(dplyr)
library(htmltools)

library(markovchain)
library(dendextend)

# disable scientific notation
options(scipen=999)

# set wd
setwd('Z:/Client Analytics and Product Discovery/ANALYTICS_PROJECTS/ZDriveHELOC')

# load in the necessary files
list.files()

# get the final data file
heloc <- readr::read_csv('helocCampaign.csv')
branches <- readr::read_csv('BranchData.csv')


# change the lan lon for branches
heloc <- heloc %>% rename(BranchLon = Longitude)
heloc <- heloc %>% rename(BranchLat = Latitude)

# geocode property
# check NA for PropertyAdd
paste("Missing", sum(is.na(heloc$PropertyAdd)), "addresses ", sep=" ")

# drop missing addresses
helocAdd <- heloc %>% filter(!is.na(PropertyAdd))

# map the zipcode 
data(zipcode)


# clean zipcodes
helocAdd$propertyZip <- clean.zipcodes(helocAdd$propertyZip)

# get the data on zips
mapData <- merge(helocAdd, zipcode, by.x = 'propertyZip', by.y = 'zip')


# check for na
paste('There are', sum(is.na(mapData$propertyZip)), 'missing zip codes', sep = ' ')


# get lat and lon for property
# change the lan lon for branches
mapData <- mapData %>% rename(propertyLon = longitude)
mapData <- mapData %>% rename(propertyLat = latitude)

# make some factors
mapData$InitialDecision <- as.factor(mapData$InitialDecision)

mapData$LastDecision <- as.factor(mapData$LastDecision)

mapData$BookedYN <- as.factor(mapData$BookedYN)

mapData$Underwriter <- as.factor(mapData$Underwriter)

mapData$Region <- as.factor(mapData$Region)

mapData$PropertyType <-  as.factor(mapData$PropertyType)

# make some factors
mapData$BranchName <- as.factor(mapData$BranchName)

# clean up LoanToValue
mapData$LoanToValue <- ifelse(mapData$LoanToValue > 1, mapData$LoanToValue/100, mapData$LoanToValue)

# clean up DTI
mapData$DTI <- ifelse(mapData$DTI > 1, mapData$DTI/100, mapData$DTI)

# create difference of estimate field (final - initial)
mapData$DeltaEstimate <- abs(mapData$FinalEstimatedAmt - mapData$InitialEstimatedAmt)

summary(mapData$DeltaEstimate)

# make some plots
# use this theme
theme_bryan <- function () { 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 0),
        legend.position = 'bottom',
        axis.text.y = element_text(color = 'blue'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = 'darkblue', size = 10, face = 'bold'),
        strip.background = element_rect(fill = 'light blue'))
}



# check numerical for cuts
p <- ggplot(mapData, aes(x=APR)) + geom_histogram() + theme_bryan() + scale_x_continuous(labels = comma)
p


# check mortgage balance
p <- ggplot(mapData, aes(x=MortBal)) + geom_histogram(binwidth = 50000) + 
  scale_x_continuous(labels = comma, limits = c(0,1000000)) + theme_bryan() 
p


# check contract amount
p <- ggplot(mapData, aes(x=contractAmt)) + geom_histogram() + scale_x_continuous(labels = comma, limits = c(0,1000000)) + theme_bryan()
p


# check delta estimate
p <- ggplot(mapData, aes(x=DeltaEstimate)) + geom_histogram() +scale_x_continuous(labels = comma, limits = c(-5000,1000000)) + theme_bryan()
p

# check credit score
p <- ggplot(mapData, aes(x=Score)) + geom_histogram() +scale_x_continuous(labels = comma) +  theme_bryan()
p


# check loan to value
p <- ggplot(mapData, aes(x=LoanToValue)) + geom_histogram() + scale_x_continuous(labels = comma, limits=c(0, 1)) + theme_bryan()
p


# check DTI
p <- ggplot(mapData, aes(x=DTI)) + geom_histogram() + scale_x_continuous(labels = comma, limits=c(0, 1)) + theme_bryan()
p



# check days to decision
p <- ggplot(mapData, aes(x=ApplicationDaystoDecision)) + geom_histogram() + scale_x_continuous(labels = comma) + theme_bryan()
p


# cut estimate
mapData$DeltaEstLvl <- cut(mapData$DeltaEstimate, c(0,10000,50000, 100000, 5000000), include.lowest = F,
                          labels = c('< $10K', 
                                     '$10-50K', 
                                     '$50-100K', 
                                     '$100K+'
                                     ))


# cut mort bal
mapData$MortBalLvl <- cut(mapData$MortBal, c(0,100000,250000, 500000, 1000000, 1000000000), include.lowest = F,
                          labels = c('< $100K', 
                                     '$100-250K', 
                                     '$250-500K', 
                                     '$500K-$1M',
                                     '$1M+'
                          ))

# cut contract
mapData$contractAmtLvl <- cut(mapData$contractAmt, c(0,100000,250000, 500000, 1000000, 1000000000), include.lowest = F,
                          labels = c('< $100K', 
                                     '$100-250K', 
                                     '$250-500K', 
                                     '$500K-$1M',
                                     '$1M+'
                                     ))

# cut score
mapData$ScoreLvl <- cut(mapData$Score, c(0,650, 700, 750, 800, 1000), include.lowest = F,
                           labels = c('< 650', 
                                      '650-700', 
                                      '700-750', 
                                      '750-800',
                                      '800+'
                                      ))

# cut DTI
mapData$DTILvl <- cut(mapData$DTI, c(0,.25, .5, .75, 1), include.lowest = F,
                        labels = c('< .25', 
                                   '.25-0.5', 
                                   '.5-.75', 
                                   '.75-1'
                                   ))

# cut days
mapData$DaystoDecisionLvl <- cut(mapData$ApplicationDaystoDecision, c(0, 7, 14, 21, 50), include.lowest = F,
                                 labels = c('< 1 Wk', 
                                            '1-2 Wks', 
                                            '2-3 Wks', 
                                            '3+ Wks'
                                            ))




sum(is.na(mapData$AppDate))


# get month and make it a factor
#create a month factor for plotting
mapData$AppDate <- as.Date(mapData$AppDate, format = '%m/%d/%Y')




# get the month
mapData$Month <- format(mapData$AppDate, format = "%b")


sum(is.na(mapData$Month))


unique(mapData$Month)


# make month and state factors
mapData$Month <- ordered(mapData$Month, levels = c(
                                                   "Mar"
                                                   "Apr",
                                                   "May",
                                                   "Jun",
                                                   "Jul"
                                                   ))




# get a subset of data for melting
plotData <- mapData %>% select(
  Analyst,
  BranchName,
  Region,
  OriginationChannelCode,
  DistributionChannelCode,
  appid,
  Score,
  contractAmt,
  InitialDecision,
  LastDecision,
  BookedYN,
  Underwriter,
  APR,
  ApplicationDaystoDecision,
  LoanToValue,
  DTI,
  DeltaEstimate,
  Month,
  MortBal
)

# row.names(mat) <- stringr::str_to_title(branchDist)
plotData$BranchName <- stringr::str_to_title(plotData$BranchName)

# make a palette
eastPal <-  c('red','royalblue3')


##### Crosstabs for contractAmt ------------->

# count apps
g <- ggplot(plotData, aes(x = reorder(BranchName, BranchName, length), fill = BookedYN)) + coord_flip() + geom_bar(stat = 'count') +  
  theme_bryan() + theme(axis.text.y = element_text(size = 8, color = 'blue', angle = 0)) + 
  ggtitle("Contract Counts by Originating Branch") + 
  scale_fill_manual(values = c('tomato','cornflowerblue')) + 
  theme(plot.title = element_text(color = 'blue', size = 20, face = 'bold'))
g

# box plot by month
# contract amt month
b <- ggplot(plotData, aes(x = Month, y = contractAmt, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE) + scale_y_continuous(labels = comma, limits=c(0, 1000000)) + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  scale_fill_manual(values = eastPal) + 
  theme_bryan() + facet_wrap(~BookedYN) + ggtitle('Contract Amt by Month')
b


# by underwriter
b <- ggplot(plotData, aes(x = Underwriter, y = contractAmt, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE) + scale_y_continuous(labels = comma, limits=c(0, 1000000)) + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN) + theme(axis.text.x = element_text(size = 12, color = 'blue', angle = 45, hjust = 1))
b



# by region
b <- ggplot(plotData, aes(x = Region, y = contractAmt, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE) + scale_y_continuous(labels = comma, limits=c(0, 1000000)) + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN) + theme(axis.text.x = element_text(size = 12, color = 'blue', angle = 45, hjust = 1))
b



###### crosstab for Score ------------------->
# month
b <- ggplot(plotData, aes(x = Month, y = Score, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN)
b


# regions
b <- ggplot(plotData, aes(x = Region, y = Score, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN)
b


# underwriter
b <- ggplot(plotData, aes(x = Underwriter, y = Score, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN) + theme(axis.text.x = element_text(size = 12, color = 'blue', angle = 45, hjust = 1))
b


###### crosstab for DTI ------------------->
# month
b <- ggplot(plotData, aes(x = Month, y = DTI, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN)
b


# regions
b <- ggplot(plotData, aes(x = Region, y = DTI, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN)
b


# underwriter
b <- ggplot(plotData, aes(x = Underwriter, y = DTI, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN) + theme(axis.text.x = element_text(size = 12, color = 'blue', angle = 45, hjust = 1))
b




###### crosstab for LoanToValue ------------------->
# month
b <- ggplot(plotData, aes(x = Month, y = LoanToValue, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN)
b


# regions
b <- ggplot(plotData, aes(x = Region, y = LoanToValue, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN)
b


# underwriter
b <- ggplot(plotData, aes(x = Underwriter, y = LoanToValue, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN) + theme(axis.text.x = element_text(size = 12, color = 'blue', angle = 45, hjust = 1))
b



###### crosstab for DeltaEstimate ---------->

# month
b <- ggplot(plotData, aes(x = Month, y = DeltaEstimate, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + scale_y_continuous(labels = comma, limits=c(0, 500000)) +
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN)
b


# regions
b <- ggplot(plotData, aes(x = Region, y = DeltaEstimate, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE) + scale_y_continuous(labels = comma, limits=c(0, 500000)) +
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN)
b


# underwriter
b <- ggplot(plotData, aes(x = Underwriter, y = DeltaEstimate, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + scale_y_continuous(labels = comma, limits=c(0, 500000)) +
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN) + theme(axis.text.x = element_text(size = 12, color = 'blue', angle = 45, hjust = 1))
b




###### crosstab for ApplicationDaystoDecision ---------->

# month
b <- ggplot(plotData, aes(x = Month, y = ApplicationDaystoDecision, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + scale_y_continuous(labels = comma, limits=c(0, 30)) +
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN)
b


# regions
b <- ggplot(plotData, aes(x = Region, y = ApplicationDaystoDecision, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE) + scale_y_continuous(labels = comma, limits=c(0, 30)) +
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN)
b


# underwriter
b <- ggplot(plotData, aes(x = Underwriter, y = ApplicationDaystoDecision, fill = BookedYN)) + 
  geom_boxplot(notch=TRUE)  + scale_y_continuous(labels = comma, limits=c(0, 30)) +
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BookedYN) + theme(axis.text.x = element_text(size = 12, color = 'blue', angle = 45, hjust = 1))
b




########  get contract volume by branch
branchData <- plotData %>% group_by(BranchName, BookedYN, Region, Underwriter) %>% 
                                    summarise(TotContract = sum(contractAmt))
head(branchData)




# sum contract Amt
g1 <- ggplot(subset(branchData, BookedYN == 'No'), aes(y = reorder(BranchName, TotContract, sum), x = TotContract, fill = BookedYN)) + 
  scale_x_continuous(labels = comma) +
  geom_bar(stat='identity') +  
  scale_fill_manual(values = 'red') +
  theme_bryan() + 
  theme(axis.text.y = element_text(size = 8, color = 'blue', angle = 0))



# sum contract Amt
g2 <- ggplot(subset(branchData, BookedYN == 'Yes'), aes(y = reorder(BranchName, TotContract, sum), x = TotContract, fill = BookedYN)) + 
  scale_x_continuous(labels = comma) +
  geom_bar(stat='identity') +  
  scale_fill_manual(values = 'blue') +
  theme_bryan() + 
  theme(axis.text.y = element_text(size = 8, color = 'blue', angle = 0))

grid.arrange(g1, g2, ncol=2)



# get new groupings
include <- c(412, 414)

channel <- mapData %>% select(OriginationChannelCode, DistributionChannel, BookedYN, contractAmt) %>% 
  group_by(OriginationChannelCode, DistributionChannel, BookedYN) %>% 
  filter(OriginationChannelCode %in% include) %>% 
  summarise(TotContract = sum(contractAmt))


channel$OriginatingBranch <- ifelse(channel$OriginationChannelCode == 412, 'CSC', 'EB.Com')

booked <- channel %>% filter(BookedYN == 'Yes') 

# sum contract Amt
g1 <- ggplot(subset(channel, OriginatingBranch == 'CSC'), aes(y = reorder(DistributionChannel, TotContract, sum), x = TotContract, fill = OriginatingBranch)) + 
  scale_x_continuous(labels = comma) +
  scale_y_reordered() + 
  geom_bar(stat='identity') + 
  scale_fill_manual(values = 'blue') +
  theme_bryan() + 
  theme(axis.text.y = element_text(size = 8, color = 'blue', angle = 0))

# sum contract Amt
g2 <- ggplot(subset(channel, OriginatingBranch == 'EB.Com'), aes(y = reorder(DistributionChannel, TotContract, sum), x = TotContract, fill = OriginatingBranch)) + 
  scale_x_continuous(labels = comma) +
  scale_y_reordered() + 
  geom_bar(stat='identity') + 
  scale_fill_manual(values = 'cornflowerblue') + 
  theme_bryan() + 
  theme(axis.text.y = element_text(size = 8, color = 'blue', angle = 0))
  


grid.arrange(g1, g2, ncol=2)



# splom(~iris[1:4])
pairs <- plotData %>% select(MortBal, Score, contractAmt, LoanToValue, DeltaEstimate)
lattice::splom(~pairs[1:4])

pairs(pairs[1:4])


# remove outliers
pairsFilt <- pairs %>% filter(contractAmt <101000)
pairs(pairsFilt[1:4])



# check the decision process
mcDec <- mapData %>% select(InitialDecision, LastDecision)

# remove the factors
mcDec$InitialDecision <- as.character(mcDec$InitialDecision)
mcDec$LastDecision <- as.character(mcDec$LastDecision)

# clean the initial decision
mcDec$InitialDecision <- ifelse(mcDec$InitialDecision == 'Counteroffer Cancelled', 'CntOff Canx', mcDec$InitialDecision)
mcDec$InitialDecision <- ifelse(mcDec$InitialDecision == 'Declined', 'Decl', mcDec$InitialDecision)
mcDec$InitialDecision <- ifelse(mcDec$InitialDecision == 'Approved', 'Appr', mcDec$InitialDecision)
mcDec$InitialDecision <- ifelse(mcDec$InitialDecision == 'Review', 'Revw', mcDec$InitialDecision)
mcDec$InitialDecision <- ifelse(mcDec$InitialDecision == 'Incomplete', 'Incomp', mcDec$InitialDecision)
mcDec$InitialDecision <- ifelse(mcDec$InitialDecision == 'Withdrawn', 'Withdrn', mcDec$InitialDecision)
mcDec$InitialDecision <- ifelse(mcDec$InitialDecision == 'Counteroffer', 'CntOff', mcDec$InitialDecision)
mcDec$InitialDecision <- ifelse(mcDec$InitialDecision == 'Cancelled', 'Canx', mcDec$InitialDecision)

mcDec$LastDecision <- ifelse(mcDec$LastDecision == 'Declined', 'Decl', mcDec$LastDecision)
mcDec$LastDecision <- ifelse(mcDec$LastDecision == 'Approved', 'Appr', mcDec$LastDecision)
mcDec$LastDecision <- ifelse(mcDec$LastDecision == 'Review', 'Revw', mcDec$LastDecision)
mcDec$LastDecision <- ifelse(mcDec$LastDecision == 'Incomplete', 'Incomp', mcDec$LastDecision)
mcDec$LastDecision <- ifelse(mcDec$LastDecision == 'Withdrawn', 'Withdrn', mcDec$LastDecision)
mcDec$LastDecision <- ifelse(mcDec$LastDecision == 'Aprvd. Rejected by Applicant', 'Appr Rej/App', mcDec$LastDecision)
mcDec$LastDecision <- ifelse(mcDec$LastDecision == 'Counteroffer Rejected by Applicant', 'CntOff Rej/App', mcDec$LastDecision)
mcDec$LastDecision <- ifelse(mcDec$LastDecision == 'Cancelled', 'Canx', mcDec$LastDecision)
mcDec$LastDecision <- ifelse(mcDec$LastDecision == 'Counteroffer', 'CntOff', mcDec$LastDecision)

# for markov chains
options(digits=4)

# make the markov chain
decision <- markovchainFit(data = mcDec, name = "Decision")
decision$estimate

# for counts
counts <- createSequenceMatrix(stringchar = mcDec)
counts


# check similarities
dendData <- mapData %>% select(
                        DistributionChannel,
                        DistributionChannelCode,
                        Score,
                        contractAmt,
                        APR,
                        ApplicationDaystoDecision,
                        LoanToValue,
                        DTI,
                        DeltaEstimate
                        )


# roll it up
#dendData$BranchName <- as.character(dendData$BranchName)

dendData$DistributionChannel <- paste(dendData$DistributionChannel, dendData$DistributionChannelCode, sep = '-')


branchDend <- dendData %>%  group_by(DistributionChannel) %>% 
                          summarise(
                            Score = mean(Score, na.rm = T),
                            Amt = sum(contractAmt, na.rm = T),
                            APR = mean(APR, na.rm = T),
                            Days = mean(ApplicationDaystoDecision, na.rm = T),
                            LTV = median(LoanToValue, na.rm = T),
                            DTI = mean(DTI, na.rm = T),
                            ValDelta = sum(DeltaEstimate, na.rm = T)
                          )




branchDist <- branchDend$DistributionChannel
branchDist


# get matrix of numerical
mat <- as.matrix(branchDend[,c(2:8)])

row.names(mat) <- stringr::str_to_title(branchDist)

# center and scale for clustering
mat <- scale(mat, center = TRUE, scale = TRUE)


# scree plot
#check the scree plot for centers
wss <- (nrow(mat)-1)*sum(apply(mat,2,var))

for (i in 2:20) wss[i] <- sum(kmeans(mat,centers=i)$withinss)

plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# set the number of clusters
clust_k <- 8


# make the hierarchical clustering
par(mar = c(2.5, 0.5, 1.0, 7))
d <- dist(mat, method = "euclidean")
hc <- hclust(d)
dend <- d %>% hclust %>% as.dendrogram
labels_cex(dend) <- .65
dend %>% 
  color_branches(k=clust_k) %>%
  color_labels() %>%
  highlight_branches_lwd(3) %>% 
  plot(horiz=TRUE, main = "Branch (Distribution) Clusters by Heloc Attributes", axes = T)


# send tree to file
sink("dendRed5.txt", type = "output")
str(dend)

# get the cluster assignments
# cutree(as.hclust(test$rowDendrogram), 1:dim(y)[1])
myclusters <- cutree(dend, k=clust_k, h=4)


# make the dataframe
clusterDF <-  data.frame(Cluster = as.numeric(unlist(myclusters)),
                         Branch = names(myclusters))

# sort by cluster ascending
clusterDFSort <- clusterDF %>% arrange(Cluster)


head(clusterDFSort)


# join branch lat/lon
# function for strings
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

clusterDF$Branch <- as.character(clusterDF$Branch)

clusterDF$Code <- as.numeric(substrRight(clusterDF$Branch, 3))

branches$BranchName <-  stringr::str_to_title(branches$BranchName)

branchLL <-  branches %>% select(Branch, BranchName, Latitude, Longitude)



# merge for plotting
clusterDF <- left_join(clusterDF, branchLL, by = c('Code' = 'Branch'))

# make cluster a factor
clusterDF$Cluster <- as.factor(clusterDF$Cluster)


clusterCol <- colorFactor(brewer.pal(8, 'Accent'), clusterDF$Cluster)


## make the map
## check for missing lat/lon
# check for na
paste('There are', sum(is.na(mapData$propertyLat)), 'missing latitiudes', sep = ' ')

# add a jitter to the property lat and lon
#  jitter works very well
mapData$Lat <- jitter(mapData$propertyLat, amount = .01)
mapData$Lon <- jitter(mapData$propertyLon, amount = .01)

# make PropertyType a character
mapData$PropertyType <- as.character(mapData$PropertyType)

# make a property popup
mapData$PropertyPopup <- paste('<strong>', mapData$propertyZip,'</strong><br>',
                    'Addr: ', mapData$PropertyAdd,'<br>',
                    'Branch Dist: ', mapData$DistributionChannel,'<br>',
                    'Amt: $', formatC(mapData$contractAmt, format = 'd', big.mark=','),'<br>',
                    'Rate: ', mapData$APR*100,'%', '<br>',
                    'Status: ', mapData$LastDecision, '<br>',
                    'Booked: ', mapData$BookedYN, '<br>',
                    
                    'Days: ', mapData$ApplicationDaystoDecision, '<br>',
                    
                    'LtV: ', formatC(mapData$LoanToValue, zero.print = T, format = 'f', digits = 2), '<br>',
                    
                    'Value: $', formatC(mapData$FinalEstimatedAmt, zero.print = T, format = 'd', digits = 0, big.mark = ','), '<br>',
                    
                    'DTI: ', formatC(mapData$DTI, zero.print = T, format = 'f', digits = 2), '<br>',
                    
                    'Type : ', formatC(mapData$PropertyType,2), '<br>',
                    
                    'Mort Bal : $', formatC(mapData$MortBal ,zero.print = T, format = 'd', digits = 0, big.mark = ','), '<br>',
                    
                    'Score : ', formatC(mapData$Score, format = 'd'), '<br>',
                    
                    'Est Diff: $', formatC(mapData$DeltaEstimate, format = 'd', big.mark=','), sep = "")

# filter the data on booked
branchesBooked <- mapData %>% filter(BookedYN == 'Yes') %>% 
  distinct(DistributionChannelCode)
                              


# make branch popup
branches$BranchPopup <-  paste('<strong>', branches$BranchName,'</strong><br>',
                                 'Region: ', branches$Region, '<br>',
                                 'Addr: ', branches$Address,'<br>',
                                 'City: ', branches$City,'<br>',
                                 'Zip: ', branches$Zip, sep = "")


# make a cluster popup
clusterDF$Popup <-  paste('<strong>', clusterDF$Branch,'</strong><br>',
                               'Cluster: ', clusterDF$Cluster, '<br>', sep = "")


# make the color palettes
#set the color scale
contractCol <- colorFactor(palette = 'Spectral', mapData$contractAmtLvl)
scoreCol <- colorFactor(palette = 'RdYlBu', mapData$ScoreLvl)
estimateCol <- colorFactor(palette = 'Blues', mapData$DeltaEstLvl)
mortCol <- colorFactor(palette = 'Greens', mapData$MortBalLvl)
daysCol <- colorFactor(palette = rainbow(6), mapData$DaystoDecisionLvl)

# test pal
daysCol <- colorFactor(brewer.pal(6, 'Dark2'), mapData$DaystoDecisionLvl)

# make a palette for booked
bookedCol <- colorFactor(c('tomato', 'blue'), mapData$BookedYN)


# split dataframe into months
#split function might be better alternative
bookedMonth <- split(mapData, mapData$Month)


# map(list.DFs, ~filter(.x, Gold.fish.count == "Total"))
monthDF <- bookedMonth %>% 
  map(.,~filter(., BookedYN == 'Yes'))


# add radius size
mapData$Radius <- ifelse(is.na(mapData$ApplicationDaystoDecision), 0, mapData$ApplicationDaystoDecision)


# make the map
# make the map
m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Street')  %>%
  addProviderTiles(providers$Stamen.Toner, group = 'Toner')  %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
  setView(lng = -71.1, lat = 42.4, zoom = 9) %>%
  
  
  
  # mar layer 
  addCircleMarkers(data = monthDF$Mar, lat = ~Lat, lng = ~Lon,
                   color = ~contractCol(contractAmtLvl), popup = monthDF$Mar$PropertyPopup,
                   radius = ~sqrt(contractAmt/1000), group = 'Mar', opacity = 1) %>%
  
  
  # apr layer 
  addCircleMarkers(data = monthDF$Apr, lat = ~Lat, lng = ~Lon,
                   color = ~contractCol(contractAmtLvl), popup = monthDF$Apr$PropertyPopup,
                   radius = ~sqrt(contractAmt/1000), group = 'Apr', opacity = 1) %>%
  
  # may layer 
  addCircleMarkers(data = monthDF$May, lat = ~Lat, lng = ~Lon,
                   color = ~contractCol(contractAmtLvl), popup = monthDF$May$PropertyPopup,
                   radius = ~sqrt(contractAmt/1000), group = 'May', opacity = 1) %>%
  
  # jun layer 
  addCircleMarkers(data = monthDF$Jun, lat = ~Lat, lng = ~Lon,
                   color = ~contractCol(contractAmtLvl), popup = monthDF$Jun$PropertyPopup,
                   radius = ~sqrt(contractAmt/1000), group = 'Jun', opacity = 1) %>%
  
  # jul layer 
  addCircleMarkers(data = monthDF$Jul, lat = ~Lat, lng = ~Lon,
                   color = ~contractCol(contractAmtLvl), popup = monthDF$Jul$PropertyPopup,
                   radius = ~sqrt(contractAmt/1000), group = 'Jul', opacity = 1) %>%
  
  
  # booked layer 
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~bookedCol(BookedYN), popup = mapData$PropertyPopup,
                   radius = ~sqrt(contractAmt/1000), group = 'Booked', opacity = 1) %>% 
  
  # score layer 
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~scoreCol(mapData$ScoreLvl), popup = mapData$PropertyPopup,
                   radius = ~sqrt(Score/3), group = 'Score', opacity = 1) %>%
  
  # delta estimate layer 
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~estimateCol(mapData$DeltaEstLvl), popup = mapData$PropertyPopup,
                   radius = ~sqrt(DeltaEstimate/1000), group = 'Est Delta', opacity = 1) %>%
  
  
  # days layer 
  addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                   color = ~daysCol(mapData$DaystoDecisionLvl), popup = mapData$PropertyPopup,
                   radius = ~Radius, group = 'Days', opacity = 1) %>%
  
  
  
  # branch layer - circles represent 5 mi radius of bank
  addCircles(data = branches, lat = ~Latitude, lng = ~Longitude,
             color = 'black', popup = branches$BranchPopup,
             fillColor = 'springgreen', radius = 8000, opacity = .2, group = 'Branch Rad') %>%
  
  # branch layer - circles represent 5 mi radius of bank
  addMarkers(data = branches, lat = ~Latitude, lng = ~Longitude,
             popup = branches$BranchPopup, group = 'Branch') %>%
  
  # branch clusters
  addCircleMarkers(data = clusterDF, lat = ~Latitude, lng = ~Longitude,
                   color = ~clusterCol(clusterDF$Cluster), popup = clusterDF$Popup,
                   radius = ~10, group = 'Cluster', opacity = 1) %>%
  
  
  # layer control
  addLayersControl(
    baseGroups = c('Street', 'Toner', 'Satellite'),
    
    overlayGroups = c('Mar',
                      'Apr',
                      'May',
                      'Jun',
                      'Jul',
                      '---',
                      'Branch',
                      'Branch Rad',
                      'Booked',
                      'Score',
                      'Est Delta',
                      'Days',
                      'Cluster'
                      
    ),
    options = layersControlOptions(collapsed = F)
  ) %>% 
  
  # set defaults to show by hiding groups
  hideGroup(c('Apr',
              'May',
              'Jun',
              'Jul', 
              'Score',
              '---',
              'Branch',
              'Branch Rad',
              'Est Delta',
              'Days',
              'Cluster')) %>%
  
  
  
  addLegend('bottomleft', pal = contractCol, values = monthDF$Apr$contractAmtLvl,
            title = 'HELOCs Booked',
            opacity = 1) %>% 
  
  addLegend('bottomright', pal = bookedCol, values = mapData$BookedYN,
            title = 'Booked',
            opacity = 1) %>% 
  
  addLegend('bottomleft', pal = scoreCol, values = mapData$ScoreLvl,
            title = 'Score',
            opacity = 1) %>% 
  
  addLegend('bottomleft', pal = estimateCol, values = mapData$DeltaEstLvl,
            title = 'Est Diff',
            opacity = 1) %>% 
  
  addLegend('bottomright', pal = daysCol, values = mapData$DaystoDecisionLvl,
            title = 'Days',
            opacity = 1) %>% 

  addLegend('topleft', pal = clusterCol, values = clusterDF$Cluster,
            title = 'Cluster',
            opacity = 1)


m















#libraries for reshaping and plotting data
library(ggplot2)
library(reshape2)
library(dplyr)
library(caret)


#load the file
url <- "K:/Sandbox/Bryan Projects/CQ/BusinessMetrics.csv"
cqDataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

plot(cqDataRaw$totalMentions,cqDataRaw$cqScoreNew)
#total mentions is not correlated with CQScore

plot(cqDataRaw[,c(6:10,22)])

##########################################
#make the first model based on pe
#pe not relevant and not to be used
peModel <- lm(pe ~ cqScoreNew, data = cqDataRaw)
summary(peModel)

peDiffModel <- lm(peDiff ~ cqScoreNew, data = cqDataRaw)
summary(peDiffModel)

#############################################

#second model based on ROA
roaModel <- lm(roa ~ cqScoreNew, data = cqDataRaw)
summary(roaModel)

roaDiffModel <- lm(roaDiff ~ cqScoreNew, data = cqDataRaw)
summary(roaDiffModel)


############################################

#third model based on rev growth
#its correlated, but does not have predictive power
revGrowthModel <- lm(revGrowth ~ cqScoreNew, data = cqDataRaw)
summary(revGrowthModel)

revGrowthDiffModel <- lm(revGrowthDiff ~ cqScoreNew, data = cqDataRaw)
summary(revGrowthDiffModel)



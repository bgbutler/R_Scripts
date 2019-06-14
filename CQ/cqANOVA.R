#data for performing CQ comparison


library(ggplot2)
library(dplyr)
library(reshape2)

#get all data
urlAll <- "K:/Sandbox/Bryan Projects/CQ/cqAllDataClean.csv"
cqAll <- read.csv(urlAll, header = TRUE, na.strings = c('', "NA"), sep=",", as.is = FALSE)

#get fs data
urlFS <- "K:/Sandbox/Bryan Projects/CQ/cqFSDataClean.csv"
cqFS <- read.csv(urlFS, header = TRUE, na.strings = c('', "NA"), sep=",", as.is = FALSE)


subFS <- cqFS[cqFS$PickedComm == 0,]

#subset the data to get the intersections
#remove the extra columns from FS data set
subFS <- cqFS[,c(1:27)]
subFS <- subFS[subFS$InAllCQ == 1,]
subAll <- cqAll[cqAll$inFSData ==1,]

head(subAll,10)

#set the column names for future merging
newNames <- c("Intersect", "RowID", "Loop","RespID", "Industry", "Industry1", "Industry2", "Company",
              "Q1", "Q2", "Q3", "Q7", "Q9", "Q10", "Q11", "Q13", "Q16", "Q17", "Q19", "Q20",
              "Q21", "Q22", "Q23", "Q24", "Q25", "meanAll", "meanNonRespVar")

colnames(subFS) <- newNames
colnames(subAll) <- newNames



#cut the data to check the good and bad scores

subFSGood <- subFS[subFS$Loop == "Good",]
subFSBad <- subFS[subFS$Loop == "Bad",]

subAllGood <- subAll[subAll$Loop == "Good",]
subAllBad <- subAll[subAll$Loop == "Bad",]


#recode the Intersect with the text and then remove extraneous
subAllGood$Intersect <- ifelse(subAllGood$Intersect ==1,"CQGood", 0)
subAllBad$Intersect <- ifelse(subAllBad$Intersect ==1,"CQBad", 0)

subFSGood$Intersect <- ifelse(subFSGood$Intersect ==1,"FSGood", 0)
subFSBad$Intersect <- ifelse(subFSBad$Intersect ==1,"FSBad", 0)

#make factors
subAllGood$Intersect <- as.factor(subAllGood$Intersect)
subAllBad$Intersect <- as.factor(subAllBad$Intersect)
subFSGood$Intersect <- as.factor(subFSGood$Intersect)
subFSBad$Intersect <- as.factor(subFSBad$Intersect)


subAllGood <- subAllGood[subAllGood$Intersect == "CQGood",]
subAllBad <- subAllBad[subAllBad$Intersect == "CQBad",]
subFSGood <- subFSGood[subFSGood$Intersect =="FSGood",]
subFSBad <- subFSBad[subFSBad$Intersect =="FSBad",]


goodLoop <- rbind(subAllGood, subFSGood)
badLoop <- rbind(subAllBad, subFSBad)

#goodLoop$Intersect <- as.factor(goodLoop$Intersect)
#badLoop$Intersect <- as.factor(badLoop$Intersect)


myPath <- file.path("K:","Sandbox", "Bryan Projects","CQ")
setwd(myPath)

write.csv(goodLoop, file = "goodLoop.csv", 
          row.names = FALSE)

write.csv(badLoop, file = "badLoop.csv", 
          row.names = FALSE)



pdf(file = "CQ.pdf", onefile = T, paper = "USr", width = 11, height = 8.5)
par(mfrow=c(2,2), oma = c(0,0,3,0), mar = c(0,0,1,0))


#histogram plot
P1 <- ggplot(goodLoop, aes(x = meanNonRespVar, fill = Intersect)) + 
  geom_density(position = "identity", alpha = .5) + ylim(0,.5) + 
  ggtitle("Distribution of Avg Good CQ Response (ex Resp Var)") + 
  ylab("Frequency") +
  xlab("Avg Score") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(P1)


#histogram plot
P2 <- ggplot(badLoop, aes(x = meanNonRespVar, fill = Intersect)) + 
  geom_density(position = "identity", alpha = .5) +
  ggtitle("Distribution of Avg Bad CQ Response (ex Resp Var)") + 
  ylab("Frequency") +
  xlab("Avg Score") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(P2)


#histogram plot
P3 <- ggplot(goodLoop, aes(x = meanAll, fill = Intersect)) + 
  geom_density(position = "identity", alpha = .5) + ylim(0,.55) + 
  ggtitle("Distribution of Avg Good CQ Response (all Qs)") + 
  ylab("Frequency") +
  xlab("Avg Score") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(P3)


#histogram plot
P4 <- ggplot(badLoop, aes(x = meanAll, fill = Intersect)) + 
  geom_density(position = "identity", alpha = .5) +
  ggtitle("Distribution of Avg Bad CQ Response (all Qs)") + 
  ylab("Frequency") +
  xlab("Avg Score") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(P4)

dev.off()


# these assume normality which does not hold
testGood <- oneway.test(meanAll ~ Intersect, data = goodLoop)
testGood

testBad <- oneway.test(meanAll ~ Intersect, data = badLoop)
testBadSumm


#try a k-S test
ks.test(subFSGood$meanNonRespVar,subAllGood$meanNonRespVar, alternative = "l")

ks.test(subFSBad$meanNonRespVar,subAllBad$meanNonRespVar, alternative = "g")

bad_loop <- group_by(badLoop, Intersect)
avgBad <- summarise(bad_loop,
                      avg = mean(meanNonRespVar, na.rm = T),
                      stdev = sd(meanNonRespVar, na.rm = T))
avgBad


good_loop <- group_by(goodLoop, Intersect)
avgGood <- summarise(good_loop,
                    avg = mean(meanNonRespVar, na.rm = T),
                    stdev = sd(meanNonRespVar, na.rm = T))
avgGood




bad_loop <- group_by(badLoop, Intersect)
avgBadAll <- summarise(bad_loop,
                    avg = mean(meanAll, na.rm = T),
                    stdev = sd(meanAll, na.rm = T))
avgBadAll


good_loop <- group_by(goodLoop, Intersect)
avgGoodAll <- summarise(good_loop,
                     avg = mean(meanAll, na.rm = T),
                     stdev = sd(meanAll, na.rm = T))
avgGoodAll


plot(ecdf(subAllBad$meanNonRespVar))
plot(ecdf(subFSBad$meanNonRespVar), add = TRUE, lty = "dashed")

#clean up the NA
badLoop <- badLoop[badLoop$Intersect != "NA",]
na.omit(badLoop)

#make more plots using the ECDF function
g1 <- ggplot(badLoop, aes(x = meanNonRespVar, color = Intersect)) + stat_ecdf() + 
  ylab("Cum Probability") + xlab("Avg Score") + 
  ggtitle("Empirical Cumulative Distribution of Bad Score") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
g1


#make more plots using the ECDF function
g1 <- ggplot(badLoop, aes(x = meanAll, color = Intersect)) + stat_ecdf() + 
  ylab("Cum Probability") + xlab("Avg Score") + 
  ggtitle("Empirical Cumulative Distribution of Bad Score (All Qs)") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
g1


#make more plots using the ECDF function
g1 <- ggplot(goodLoop, aes(x = meanAll, color = Intersect)) + stat_ecdf() + 
  ylab("Cum Probability") + xlab("Avg Score") + 
  ggtitle("Empirical Cumulative Distribution of Good Score (All Qs)") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
g1


#make more plots using the ECDF function
g1 <- ggplot(goodLoop, aes(x = meanNonRespVar, color = Intersect)) + stat_ecdf() + 
  ylab("Cum Probability") + xlab("Avg Score") + 
  ggtitle("Empirical Cumulative Distribution of Good Score") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
g1


#histogram plot
P4 <- ggplot(badLoop, aes(x = meanAll, fill = Intersect)) + 
  geom_density(position = "identity", alpha = .5) + facet_wrap(~Company) + 
  ggtitle("Distribution of Avg Bad CQ Response (all Qs)") + 
  ylab("Frequency") +
  xlab("Avg Score") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(P4)


#histogram plot
P4 <- ggplot(goodLoop, aes(x = meanAll, fill = Intersect)) + 
  geom_density(position = "identity", alpha = .5) + facet_wrap(~Company) + 
  ggtitle("Distribution of Avg Good CQ Response (all Qs)") + 
  ylab("Frequency") +
  xlab("Avg Score") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(P4)


goodSort <- arrange(goodLoop, desc(meanAll))
goodSort <- 

goodRed <- select(goodSort)


#get the average length of the essay by words
by_CQ <- group_by(goodSort, Company, Intersect)
avgCQ <- summarise(by_CQ,
                     avg = mean(meanAll, na.rm = T))
avgCQ



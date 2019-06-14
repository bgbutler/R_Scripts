#this is for the community analyses
#load up a bunch of libraries

library(ggplot2)
library(plyr)
library(RODBC)
library(reshape2)
library(gdata)

#set up the SQL connection
ch <- odbcConnect("ApprBudget")
dataRaw <- sqlQuery(ch, "SELECT Period, ClientName, EnterpriseGUID, CommunityName,
                    CommunityGUID, Concat, Active, Moderate, Low, Lurkers, DidNotLogin,
                    UniqueLogins, TotalLogins, TotalContributions, TotalUniqueContributors,
                    ContractSize, Branded, MonthsActive, DirectRegistration, MobileOptimizedScreener,
                    GenSeg, Industry, RecruitSource
                    FROM BenchmarkingHistory
                    WHERE Solomon_Office = 'Boston'
                    AND Period between '2009-01' and '2015-07'
                    ORDER by Period desc")
odbcClose(ch)

head(dataRaw)

#add total members column
dataRaw$TotalMembers <- dataRaw$Active + dataRaw$Moderate +dataRaw$Low + dataRaw$Lurkers

#create data from period
dataRaw$Period <- as.character(dataRaw$Period)
dataRaw$Period <- as.Date(paste(dataRaw$Period,"-01", sep =""))

#remove client list
dataRaw <-dataRaw[dataRaw$RecruitSource != 'Client list',]

pairs(~ TotalContributions + TotalMembers + MonthsActive + Lurkers, data = dataRaw)
pairs(~ TotalContributions + TotalMembers + MonthsActive + ContractSize, data = dataRaw)


plt1 <- ggplot(dataRaw, aes(x= TotalContributions, fill = Branded))
plt1 + geom_density() + facet_wrap(~RecruitSource, ncol = 5) + xlim(0,4000)  +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Total Contributions by Recruit Source") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

plt1 <- ggplot(dataRaw, aes(x= TotalUniqueContributors, fill = Branded))
plt1 + geom_density(alpha = 0.8) + facet_wrap(~RecruitSource, ncol = 5) + xlim(0,500)  +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Unique Contributors") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

plt1 <- ggplot(dataRaw, aes(x= TotalUniqueContributors, fill = Branded))
plt1 + geom_histogram(opacity = 0.5) + facet_wrap(~RecruitSource, ncol = 5) + xlim(0,500)  +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Unique Contributors") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


g <- ggplot(dataRaw, aes(x =  Period, y = TotalUniqueContributors, color = RecruitSource))
g  + ylim(0,12000) + geom_bar(stat = "identity")+ facet_wrap(~RecruitSource) +
  theme(axis.text.x=element_text(angle=90)) + 
  theme(strip.text.x = element_text(size=8, angle=0), 
        strip.text.y = element_text(size=12, face="bold"), 
        strip.background = element_rect(colour="red", fill="#CCCCFF")) + 
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  theme(legend.position="right") + 
  theme(legend.background=element_rect(fill="gray90", size=.5, linetype="dotted"))

#recruit source insignificant
m1 <- lm(TotalContributions ~ TotalUniqueContributors + Branded + 
           MonthsActive + TotalMembers + ContractSize -1,
         data = dataRaw)
summary(m1)

m2 <- lm(TotalUniqueContributors ~ Branded + 
           MonthsActive + TotalMembers + RecruitSource + ContractSize -1,
         data = dataRaw)
summary(m2)

m2 <- lm(TotalLogins ~  
           MonthsActive + TotalMembers + Industry + ContractSize -1,
         data = dataRaw)
summary(m2)


#check out some clusters
dataCluster <- dataRaw[,c(1:2,11:15,22)]


na.omit(dataCluster)

dataInd <- dataCluster[,c(8,3:7)]

i <- sapply(dataInd, is.factor)
dataInd[i] <- lapply(dataInd[i], as.character)

newNames <- unique(dataInd$Industry)

data <- asdataInd[,c(2:6)]





                      
                      

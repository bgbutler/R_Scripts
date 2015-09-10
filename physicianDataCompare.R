#this is for the physician analysis
#load up a bunch of libraries

library(ggplot2)
library(plyr)
library(RODBC)


#set up the SQL connection
ch <- odbcConnect("ApprBudget")
dataRaw <- sqlQuery(ch, "SELECT Period, ClientName, EnterpriseGUID, CommunityName,
                    CommunityGUID, Concat, Active, Moderate, Low, Lurkers, DidNotLogin,
                    UniqueLogins, TotalLogins, TotalContributions, TotalUniqueContributors,
                    ContractSize, Branded, MonthsActive, DirectRegistration, MobileOptimizedScreener,
                    GenSeg, Industry, RecruitSource, ParticipationPercentageActual, PeriodID
                    FROM BenchmarkingHistory
                    WHERE Solomon_Office = 'Boston'
                    ORDER by Period ")
odbcClose(ch)

head(dataRaw)

#add total members column
dataRaw$TotalMembers <- dataRaw$Active + dataRaw$Moderate +dataRaw$Low + dataRaw$Lurkers

#clean up the period
#create dates from period
dataRaw$Period <- as.character(dataRaw$Period)
dataRaw$Period <- as.Date(paste(dataRaw$Period,"-01", sep ="")) 




#create physician dataset

grx <- glob2rx("*Physicians*")

#create the two data sets
physRaw <- with(dataRaw, subset(dataRaw, subset = grepl(grx, dataRaw$GenSeg), drop = TRUE))
allOther <- with(dataRaw, subset(dataRaw, subset = !grepl(grx, dataRaw$GenSeg), drop = TRUE))

#remove blank comunities or non-people doctors
physclean <- physRaw[physRaw$CommunityName != 'Anthem Physicians Forum',]
physclean <- physclean[physclean$CommunityName != 'Cardiovascular Connection',]
physclean <- physclean[physclean$CommunityName != 'Vet Connect',]
physclean <- physclean[physclean$CommunityName != 'Veterinary Online Interactive Community',]


#direct compare between the two and all other
physclean$type <- "Physicians"
allOther$type <- "All Other"
commCompare <- rbind(physclean, allOther)


#make the PeriodID a factor
as.factor(as.character(physclean$PeriodID))
as.factor(as.character(allOther$PeriodID))
as.factor(as.character(commCompare$PeriodID))
                                                
#start exploratory analysis on pysician data
plt0 <- ggplot(physclean, aes(x= TotalContributions, fill = Branded))
plt0 + geom_density(na.rm = TRUE) + facet_wrap(~CommunityName, ncol = 4) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Contributions") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


plt1 <- ggplot(physclean, aes(x= TotalContributions, fill = Branded))
plt1 + geom_density() + facet_wrap(~CommunityName, ncol = 4) + xlim(0,4000)  +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Contributions") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

# make a line plot with the data

g <- ggplot(physclean, aes(x =  Period, y = TotalContributions, color = CommunityName))
g + ylim(0,2000) + facet_wrap(~CommunityName, ncol=4) + geom_line(aes(group=CommunityName))+ 
  theme(axis.text.x=element_text(angle=90)) + 
  theme(strip.text.x = element_text(size=8, angle=0), 
        strip.text.y = element_text(size=12, face="bold"), 
        strip.background = element_rect(colour="red", fill="#CCCCFF")) + 
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  theme(legend.position="right") + 
  theme(legend.background=element_rect(fill="gray90", size=.5, linetype="dotted"))

#look at all other industries
plt1 <- ggplot(dataRaw, aes(x= TotalContributions, fill = Branded))
plt1 + geom_density(alpha = 0.8) + facet_wrap(~Industry, ncol = 4)  + xlim(0,10000) + 
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Total Contributions by Industry") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

plt1 <- ggplot(dataRaw, aes(x= ContractSize))
plt1 + geom_histogram(fill = "blue") + facet_wrap(~Industry, ncol = 4)  + xlim(0,500) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Contributions") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

plt4 <- ggplot(commCompare, aes(TotalContributions, fill = type)) + 
  geom_density(na.rm = TRUE,alpha = .8) + xlim(0,10000) +
  ggtitle("Density Distribution of Total Contributions") +
  theme(legend.position="right") + 
#  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Contributions") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  guides(fill=guide_legend(title = Null))
plt4

plt5 <- ggplot(commCompare, aes(TotalUniqueContributors)) + 
  geom_histogram(fill = "blue") + facet_wrap(~type) +
  ggtitle("Density Distribution of Total Unique Contributions")
plt5

plt6 <- ggplot(commCompare, aes(TotalUniqueContributors, fill = type)) + 
  geom_density(alpha = 0.8)  +
  ggtitle("Density Distribution of Total Unique Contributions")
plt6

plt6 <- ggplot(commCompare, aes(x = TotalContributions, fill = type)) + 
  geom_density(opacity = 0.8) + facet_wrap(~Period) + xlim(0,6000) + 
  ggtitle("Density Distribution of Total Unique Contributions")
plt6

plt6 <- ggplot(allOther, aes(x = ParticipationPercentageActual)) + 
  geom_histogram(opacity = 0.8) + facet_wrap(~Period) + 
  ggtitle("")
plt6


#make a model
m1 <- lm(TotalContributions ~ TotalLogins + ContractSize + 
           MonthsActive + Branded, data = physclean)

summary(m1)

#tested both direct registration and mobileoptimized screener with no significance

m2 <- lm(TotalContributions ~ TotalLogins + ContractSize + 
           MonthsActive + Branded + MobileOptimizedScreener, data = physclean)

summary(m2)


m3 <- lm(TotalUniqueContributors ~ ContractSize + Branded
         + MonthsActive, data = physclean)

summary(m3)


m4 <- lm(TotalContributions/TotalUniqueContributors ~ TotalLogins + 
           ContractSize + Branded
         + MonthsActive + DirectRegistration, data = physclean)

summary(m4)

m4 <- lm(TotalContributions ~ TotalLogins, data = physclean)
summary(m4)


#start exploratory analysis on pysician data
#create factor variable for contract size

as.factor(physclean$ContractSize)

plt0 <- ggplot(physclean, aes(x= TotalContributions, fill = ContractSize))
plt0 + geom_density(na.rm = TRUE) + facet_wrap(~CommunityName, ncol = 4) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Contributions") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


plot(physclean[,c(7:11,24,16)])
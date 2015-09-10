#this is for the physician analysis
#load up a bunch of libraries

library(ggplot2)
library(plyr)
library(RODBC)


#set up the SQL connection
ch <- odbcConnect("ApprBudget")
physRaw <- sqlQuery(ch, "SELECT Period, ClientName, EnterpriseGUID, CommunityName,
                    CommunityGUID, Concat, Active, Moderate, Low, Lurkers, DidNotLogin,
                    UniqueLogins, TotalLogins, TotalContributions, TotalUniqueContributors,
                    ContractSize, Branded, MonthsActive, DirectRegistration, MobileOptimizedScreener
                    FROM BenchmarkingHistory
                    WHERE GenSeg like '%physicians%' AND Solomon_Office = 'Boston'
                    AND Period between '2014-11' and '2015-07'
                    ORDER by Period desc")
odbcClose("ApprBudget")

head(physRaw)

#remove blank comunities or non-people doctors
physclean <- physRaw[physRaw$CommunityName != 'Anthem Physicians Forum',]
physclean <- physclean[physclean$CommunityName != 'Cardiovascular Connection',]
physclean <- physclean[physclean$CommunityName != 'Vet Connect',]
physclean <- physclean[physclean$CommunityName != 'Veterinary Online Interactive Community',]
                                                
physRaw1 <- physRaw[physRaw$CommunityName != 'Vet Connect',]                      

#start exploratory analysis
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


physClean <- na.omit(physclean)
p1 <- with(physclean, plot(ContractSize,Active, Moderate, Low, DidNotLogin))




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



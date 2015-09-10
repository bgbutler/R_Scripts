#this is for the VPRM analysis
#load up a bunch of libraries

library(ggplot2)
library(plyr)
library(RODBC)


#set up the SQL connection
ch <- odbcConnect("CORE_Warehouse")
vprmRaw <- sqlQuery(ch, "SELECT PCID, VendorName, v.VendorID, vp.ProjectRefNum, Name, C.Country,
  DateEntered, TotalResponses, TotalDropOuts, PureQualified, 
	NotQualified, CommunityDuplicates, Suspicious, OtherBackEndScreenoutRate,
  TotalQualified, Invited, Logins,ConversionRate, InputCurrency, TotalProjectCost, 
  CostPerLoginQuoted, FixedProjectCost, CostPerLogin, CostPerResponse,CostPerInvited,
  SourceType, Type
  FROM tblVendorPerformance vp
  JOIN tblVendors v ON
  vp.VendorID = v.VendorID	
  JOIN tblProjects p ON
  vp.ProjectRefNum = p.ProjectRefNum
  JOIN CORE_APP_DUPL_CHECK.dbo.Countries C ON
  vp.CountryID = C.ID
  WHERE OfficeLocation like '%Boston%' or officelocation like  '%New York%'")
odbcClose(ch)

head(vprmRaw)


#clean up date formats create a factor for the date
vprmRaw$DateEntered <- as.Date(vprmRaw$DateEntered, format="%m/%d/%Y")
vprmRaw$DateEntered <-as.factor(vprmRaw$DateEntered)
vprmRaw$VendorID <- as.factor(vprmRaw$VendorID)

pairs(~ Logins + TotalResponses + PureQualified + Suspicious, data = vprmRaw)

write.csv(vprmRaw,"K:/Sandbox/Bryan Projects/vprmData.csv")

plt0 <- ggplot(vprmRaw, aes(x= ConversionRate))
plt0 + geom_density(fill = "blue") + facet_wrap(~Country, ncol = 6) + xlim(0,1) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Conversion Rate by Country of Member") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

plt1 <- ggplot(vprmRaw, aes(x= ConversionRate))
plt1 + geom_density(fill = "blue") + facet_wrap(~VendorName, ncol = 10) + xlim(0,1) +
  theme(legend.position="right") + 
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) + 
  ggtitle("Density Distribution of Conversion Rate by Vendor Name") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

plt3 <- ggplot(vprmRaw, aes(x= PureQualified))
plt3 + geom_histogram(fill = "blue", binwidth = 20) + xlim(0,2000)
 



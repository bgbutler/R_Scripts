

library(RODBC)
library(tidyverse)
library(DT)
library(leaflet)
library(zipcode)
library(reshape2)
library(RColorBrewer)

library(dplyr)
library(zipcode)




# getwd()
setwd('N:/Bryan/Marketing')
dataURL <- 'NPSDataRaw.csv'

# read the files from csv
nps <- read.csv(dataURL, stringsAsFactors = F)


# function for dealing with strength
convertComments <- function(x) {as.numeric(ifelse(x == 'Strongly Agree',  10, ifelse(
  x == 'Agree', 8, ifelse(x == 'Neutral', 5,ifelse(
    x == 'Disagree', 3,ifelse(x == 'Strongly Disagree', 1,NA))))))
}

# create the promoter field
nps$Promoter <- ifelse(nps$LikelyRecommend > 8, 'Promoter', ifelse(
  nps$LikelyRecommend > 6 & nps$LikelyRecommend <9, 'Passive', 'Detractor'))


# make role bins
nps$RoleGroup <- ifelse(nps$PrimaryRole == "Producer", "FrontLine", 
                        ifelse(nps$PrimaryRole == "Account Manager/CSR", "FrontLine",
                        ifelse(nps$PrimaryRole ==  "Administrative Assistant","Admin",
                        ifelse(nps$PrimaryRole ==  "Claims","FrontLine",
                        ifelse(nps$PrimaryRole ==  "LOB Manager","Admin",
                        ifelse(nps$PrimaryRole ==  "Marketing","MktgSales",
                        ifelse(nps$PrimaryRole ==  "Office Manager","Admin",
                        ifelse(nps$PrimaryRole ==  "Operations Manager","Admin",
                        ifelse(nps$PrimaryRole ==  "Principal","Exec Mgt",
                        ifelse(nps$PrimaryRole ==  "Producer","FrontLine",
                        ifelse(nps$PrimaryRole ==  "Sales Manager","MktgSales",
                        ifelse(nps$PrimaryRole ==  "Senior Executive","Exec Mgt",
                                                   "Admin"))))))))))))

# convert the columns to numbers
# apply this function to the recommend Questions
for (i in seq(6, 24, by=2)) {nps[,i] <- convertComments(nps[,i])}

# get the relevant data
mapData <- nps %>% dplyr::select(EnrolledinCSC,
                                   RoleGroup,
                                   PrimaryRole,
                                   Promoter,
                                   Q2_RangeofProducts,
                                   Q3_QuotePlatformEasy,
                                   Q4_FieldSalesRepRegVisits,
                                   Q5_UWResponsive,
                                   Q6_ProcessHomeRepCostAccurate,
                                   Q7_HomeEvalHassleFree,
                                   Q8_BillingEasyUnderstand,
                                   Q9_AgencySupCenExcellent,
                                   Q10_ClaimServFairResponsive,
                                   Q11_NBRenPricingStable, 
                                   AgentCode)


# reduce the agentData
agentDataRed <- agentData %>% dplyr::select(PRDCNG_AGNT_FULL_NM, PRDCNG_AGNT_CD, PRDCNG_AGNT_ADDR_POSTL_CD) %>%
  filter(!is.na(PRDCNG_AGNT_ADDR_POSTL_CD))


# make postal code a character
agendDataRed$PRDCNG_AGNT_FULL_NM
agentDataRed$PRDCNG_AGNT_ADDR_POSTL_CD <- as.character(agentDataRed$PRDCNG_AGNT_ADDR_POSTL_CD)
agentDataRed$PRDCNG_AGNT_CD <- as.character(agentDataRed$PRDCNG_AGNT_CD)

mapData$AgentCode <- as.character(mapData$AgentCode)


# join postal code
joinData <- left_join(mapData, agentDataRed, by = c("AgentCode" = "PRDCNG_AGNT_CD"))







#script to analyze survey data


.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

#these packages are more data cleaning and tefor text analytics
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(readxl)
library(SnowballC)
library(tm)
library(RTextTools)
library(stringi)
library(wordcloud)
library(RWeka)


#this series  of pacakges is for modeling
library(caret)
library(Metrics)
library(pROC)
library(rpart)
library(rpart.plot)
library(gbm)
library(e1071)
library(glmnet)
library(ROCR)


url <- "C:/Users/n846490/Documents/DataScience/CSVs/NewBranchDataCleanQuestions.csv"
sat <- read.csv(url, header = TRUE, sep=",", na.strings=c("", "NA"))

#make the hour a variable
sat$HourStarted <- substr(sat$TimeSurveyStarted,1,2)

#remove the ":"
sat$HourStarted <- gsub(":","",sat$HourStarted)

sat$HourStarted <- as.numeric(sat$HourStarted)


#clean up the date field and create a duration factor for survey
sat$TenureDate <- as.character(sat$TenureDate)
sat$TenureDate <- as.Date(sat$TenureDate, format = "%m/%d/%Y")

sat$SurveyDate <- as.character(sat$SurveyDate)
sat$SurveyDate <- as.Date(sat$SurveyDate, format = "%m/%d/%Y")

sat$CustDuration <- as.numeric((as.Date("2016-10-31") - sat$TenureDate)/365)

satClean <- filter(sat, CustDuration > 0)

sat$Day <- as.POSIXlt(sat$SurveyDate)$mday

############################################################
#clean up the satisfaction levels
satClean <- filter(sat, GeneralSatisfaction != "")

satClean$GeneralSatisfaction <- ordered(satClean$GeneralSatisfaction,
                                        levels = c("Highly Dissatisfied",
                                                   "Dissatisfied", "Neutral",
                                                   "Satisfied", "Highly Satisfied"))






satClean$GeneralSatisfaction <- ifelse(satClean$GeneralSatisfaction == "Highly Dissatisfied",  1, ifelse(
    satClean$GeneralSatisfaction == "Dissatisfied", 2, ifelse(satClean$GeneralSatisfaction == "Neutral", 3,ifelse(
        satClean$GeneralSatisfaction == "Satisfied", 4,ifelse(satClean$GeneralSatisfaction == "Highly Satisfied", 5,NA)))))

#####################################################################################
#make the conversion a function

convertComments <- function(x) {x <- ifelse(x == "Highly Dissatisfied",  1, ifelse(
    x == "Dissatisfied", 2, ifelse(x == "Neutral", 3,ifelse(
        x == "Satisfied", 4,ifelse(x == "Highly Satisfied", 5,NA)))))
return(x)
}



convertRec <- function(x) {x <- ifelse(x == "Definitely Will Not",  1, ifelse(
    x == "Likely Will Not", 2, ifelse(x == "May or May Notl", 3,ifelse(
        x == "Likely Will", 4,ifelse(x == "Definitely Will", 5,NA)))))
return(x)
}

###########################################################################


#apply the conversion

for (i in 46:53) {
    sat[,i] <- convertComments(sat[,i])
}
#this is a test
head(sat$GenuineListening)

sat$SatisfactionResolutionProblem <- convertComments(sat$SatisfactionResolutionProblem)
sat$SatisfactionRecommendations <- convertComments(sat$SatisfactionRecommendations)

sat$GeneralSatisfaction <- convertComments(sat$GeneralSatisfaction)


#create a month factor for plotting
sat$Month <- format(sat$SurveyDate, format = "%b")
sat$Month <- as.factor(sat$Month)


sat$Month <- ordered(sat$Month, levels = c("Jul","Aug","Sep", "Oct"))





##################################################################
###############     This section is for plotting

#this is the histogram of time of survey by channel
g <- ggplot(sat, aes(x = HourStarted, fill = Segment)) + facet_wrap(~Channel) +
    geom_bar(stat="count") + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


#plot of hour and type of survey
g <- ggplot(sat, aes(x = HourStarted, fill = Segment)) + facet_wrap(~SurveyType) +
    geom_bar(stat="count") + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


#plot of hour and consumer
g <- ggplot(sat, aes(x = HourStarted, fill = Segment)) + facet_wrap(~ConsumerorSmallBusinessSurvey) +
    geom_bar(stat="count") + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


#plot of survey by day of week
g <- ggplot(sat, aes(x = Day, fill = Segment)) + 
    geom_histogram(binwidth = 1) + facet_wrap(~Month) + 
    xlab("Day of Month") + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_text(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g



#plot of survey by duration
g <- ggplot(satClean, aes(x = CustDuration, fill = Gender)) + 
    geom_histogram(binwidth = 1) + xlim(0,50) + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


#density plot of survey by duration
g <- ggplot(satClean, aes(x = CustDuration, fill = Gender)) + 
    geom_density(alpha = .6) + xlim(0,50) + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


visitClean <- filter(satClean, !is.na(Whatwasyourreasonforyourvisit..Teller.))
visitClean <- filter(satClean, !is.na(Whatwasthereasonforyourvisit..Platform.))

#reason for visit
#density plot of survey by duration
g <- ggplot(visitClean, aes(x = Whatwasyourreasonforyourvisit..Teller.,
                          fill = Whatwasyourreasonforyourvisit..Teller.)) + 
    geom_bar(stat = "count") + facet_wrap(~ConsumerorSmallBusinessSurvey) + 
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 90),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g



g <- ggplot(visitClean, aes(x = Whatwasthereasonforyourvisit..Platform.,
                            fill = Whatwasthereasonforyourvisit..Platform.)) + 
    geom_bar(stat = "count") + facet_wrap(~ConsumerorSmallBusinessSurvey) + 
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 90),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g




satClean <- filter(sat, OverallExperience != "")



#need to adjust the EHI levels in the survey:
levels(satClean$EHI)[levels(satClean$EHI)=="'$ - $24,999'"] <- "Less than $50K"

levels(satClean$EHI)[levels(satClean$EHI)=="$ - $24,999"] <- "Less than $50K"

levels(satClean$EHI)[levels(satClean$EHI)=="'$0 - $24,999'"] <- "Less than $50K"
levels(satClean$EHI)[levels(satClean$EHI)=="$0 - $24,999"] <- "Less than $50K"

levels(satClean$EHI)[levels(satClean$EHI)=="'$25,000 - $49,999'"] <- "Less than $50K"
levels(satClean$EHI)[levels(satClean$EHI)=="'$50,000 - $74,999'"] <- "$50K - $100K"
levels(satClean$EHI)[levels(satClean$EHI)=="'$75,000 - $99,999'"] <- "$50K - $100K"

levels(satClean$EHI)[levels(satClean$EHI)=="$25,000 - $49,999"] <- "Less than $50K"
levels(satClean$EHI)[levels(satClean$EHI)=="$50,000 - $74,999"] <- "$50K - $100K"
levels(satClean$EHI)[levels(satClean$EHI)=="$75,000 - $99,999"] <- "$50K - $100K"


levels(satClean$EHI)[levels(satClean$EHI)=="'$100,000 - $124,999'"] <- "$100K - $150K"
levels(satClean$EHI)[levels(satClean$EHI)=="'$125,000 - $149,999'"] <- "$100K - $150K"
levels(satClean$EHI)[levels(satClean$EHI)=="'$150,000 - $174,999'"] <- "$150K - $200K"
levels(satClean$EHI)[levels(satClean$EHI)=="'$175,000 - $199,999'"] <- "$150K - $200K"

levels(satClean$EHI)[levels(satClean$EHI)=="$100,000 - $124,999"] <- "$100K - $150K"
levels(satClean$EHI)[levels(satClean$EHI)=="$125,000 - $149,999"] <- "$100K - $150K"
levels(satClean$EHI)[levels(satClean$EHI)=="$150,000 - $174,999"] <- "$150K - $200K"
levels(satClean$EHI)[levels(satClean$EHI)=="$175,000 - $199,999"] <- "$150K - $200K"

levels(satClean$EHI)[levels(satClean$EHI)=="$200,000 - $224,999"] <- "$200K - $250K"
levels(satClean$EHI)[levels(satClean$EHI)=="$225,000 - $249,999"] <- "$200K - $250K"
levels(satClean$EHI)[levels(satClean$EHI)=="$250,000 - $274,999"] <- "$250K - $300K"
levels(satClean$EHI)[levels(satClean$EHI)=="$275,000 - $299,999"] <- "$250K - $300K"

levels(satClean$EHI)[levels(satClean$EHI)=="'$200,000 - $224,999'"] <- "$200K - $250K"
levels(satClean$EHI)[levels(satClean$EHI)=="'$225,000 - $249,999'"] <- "$200K - $250K"



levels(satClean$EHI)[levels(satClean$EHI)=="$300,000 - $324,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$325,000 - $349,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$350,000 - $374,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$375,000 - $399,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$400,000 - $424,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$425,000 - $449,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$450,000 - $474,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$475,000 - $499,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$500,000 - $524,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$525,000 - $549,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$550,000 - $574,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$575,000 - $599,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$600,000 - $624,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$625,000 - $649,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$650,000 - $674,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$675,000 - $699,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$700,000 - $724,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$725,000 - $749,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$750,000 - $774,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$775,000 - $799,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$800,000 - $824,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$825,000 - $849,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$850,000 - $874,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$875,000 - $899,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$900,000 - $924,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$925,000 - $949,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$950,000 - $974,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$975,000 - $999,999"] <- "$300K+"

levels(satClean$EHI)[levels(satClean$EHI)=="$1,150,000 - $1,174,999"] <- "$300K+"
levels(satClean$EHI)[levels(satClean$EHI)=="$2,000,000 - $2,024,999"] <- "$300K+"


#levels(satClean$EHI)[levels(satClean$EHI)=="$300K+"] <- "$300,000+"
levels(satClean$EHI)[levels(satClean$EHI)=="-"] <- "NA"


#now order it

satClean$EHI <- ordered(satClean$EHI,
                        levels = c(NA, "Less than $50K", "$50K - $100K", "$100K - $150K",
                                   "$150K - $200K", "$200K - $250K", "$250K - $300K",
                                   "$300K+"))


satClean$OverallExperience <- ordered(satClean$OverallExperience,
                                        levels = c("Highly Dissatisfied",
                                                   "Dissatisfied", "Neutral",
                                                   "Satisfied", "Highly Satisfied"))




g <- ggplot(satClean, aes(x = EHI, fill = EHI)) + coord_flip() + 
    geom_bar(stat = "count") +  
    facet_wrap(~OverallExperience, ncol = 2) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 0),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g 






############################################################################

#series of box plots to show satisfaction
#get a subset of the data

satRed <- select(sat,
                 SurveyType,
                 Region,
                 Age,
                 Segment,
                 Gender,
                 Month,
                 #CustDuration,
                 SatisfactionRecommendations,
                 SatisfactionResolutionProblem,
                 GenuineListening,
                 KnowledgeProductsServices,
                 SolutionAddressedNeed,
                 TellerMetNeed,
                 SpeedofTransaction,
                 SincerelyThanking,
                 WelcomingEnvironment,
                 BranchAppearance,
                 GeneralSatisfaction,
                 LikelyRecommend)

satRed$LikelyRecommend <- convertRec(satRed$LikelyRecommend)

#fill in scores with row means

#fill in the NAs with the row means
data <- as.matrix(satRed[,c(9:18)])


k <- which(is.na(data), arr.ind = TRUE)
data[k] <- rowMeans(data, na.rm=TRUE)[k[,1]]


data <- data.frame(data)
satClean <- cbind(satRed[,c(1:8)], data)


######################################################
satMelt <- melt(satRed, id = c("SurveyType", "Age", "Segment", "Gender", "Month", 
                               "CustDuration", "SatisfactionRecommendations",
                               "SatisfactionResolutionProblem"),
                variable.name = "Question",
                value.name = "Score")


satMelt <- melt(satRed, id = c("SurveyType", "Age", "Segment", "Gender", "Month", 
                               "Region", "SatisfactionRecommendations",
                               "SatisfactionResolutionProblem"),
                variable.name = "Question",
                value.name = "Score")

###################################################





satMelt$AgeFactor <- cut(satMelt$Age,
                         c(0,20,30,40,50, 100), include.lowest = T,
                         labels = c("Under 20", "20 to 30", "30 to 40", "40 to 50", "50+"))


satMeltRed <- filter(satMelt, Question != "SatisfactionRecommendations" | 
                         Question != "SatisfactionResolutionProblem")

g <- ggplot(satMeltRed, aes(x = Question, y = Score,
                         fill = Question)) + coord_flip() + 
    geom_boxplot(notch=F) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~Gender, ncol = 1) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g   





g <- ggplot(satMeltRed, aes(x = Question, y = Score,
                            fill = Question)) + coord_flip() + 
    geom_boxplot(notch=F) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~Region) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g   


g <- ggplot(satMeltRed, aes(x = Region, y = Score,
                            fill = Region)) + 
    geom_boxplot(notch=F) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~Question, nrow = 2) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g   












g <- ggplot(satMeltRed, aes(x = Question, y = Score,
                            fill = Question)) + coord_flip() +
    geom_boxplot(notch=F) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~Month) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 0),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g  



g <- ggplot(satMeltRed, aes(x = Question, y = Score,
                            fill = Question)) + coord_flip() + 
    geom_boxplot(notch=F) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~AgeFactor, ncol = 2) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 0),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g 


g <- ggplot(satMeltRed, aes(x = Question, y = Score,
                            fill = Question)) + coord_flip() + 
    geom_boxplot(notch=F) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~Segment, ncol = 2) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 0),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g 




#######################################################
## The next two questions are on problem resolution

#create a different melted dataset
#get a subset of the data
    
res <- select(sat,
              SurveyType,
              Age,
              Segment,
              Gender,
              Month,
              CustDuration,
              SatisfactionRecommendations,
              SatisfactionResolutionProblem)


resMelt <- melt(res, id = c("SurveyType", "Age", "Segment", "Gender", "Month", "CustDuration"),
            variable.name = "Question",
            value.name = "Score")


resMelt$AgeFactor <- cut(resMelt$Age,
                         c(0,20,30,40,50, 100), include.lowest = T,
                         labels = c("Under 20", "20 to 30", "30 to 40", "40 to 50", "50+"))



g <- ggplot(resMelt, aes(x = Question, y = Score,
                            fill = Question)) + 
    geom_boxplot(notch=F) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~Gender, ncol = 1) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 0),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g 


g <- ggplot(resMelt, aes(x = Question, y = Score,
                         fill = Question)) + 
    geom_boxplot(notch=F) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~Month, ncol = 1) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 0),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g 


g <- ggplot(resMelt, aes(x = Question, y = Score,
                         fill = Question)) + 
    geom_boxplot(notch=F) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~Segment, ncol = 2) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 0),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g 


g <- ggplot(resMelt, aes(x = Question, y = Score,
                         fill = Question)) + 
    geom_boxplot(notch=F) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~Segment, ncol = 2) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 0),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g 








############################################################################
##### Begin the text mining 

textData <- select(sat,
              SurveyType,
              Age,
              Segment,
              Gender,
              Month,
              OverallExperience,
              PleaseDescribeOtherReasonforVisit,
              WhyHighlySatisfied,
              Whatdoyouthinkwecouldhavedonedifferently.,
              ExplainProblem,
              Pleasetelluswhyyouwere.responsetopreviousquestion.forthequalityofserviceyoureceived.)

#clean up some names
colnames(textData)[7] <- "OtherReasonForVisit"
colnames(textData)[9] <- "DoDifferently"
colnames(textData)[11] <- "WhyRatedQuality"


#ensure utf-8
for (i in 7:11) {
    textData[,i] <- stri_enc_toutf8(textData[,i])
    textData[,i] <- tolower(textData[,i])
}


#adjust the stopwords so that 'not" ect. is included
#create revised stopwords list
newWords <- stopwords("english")
keep <- c("no", "more", "not", "can't", "cannot", "isn't", "aren't", "wasn't",
          "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't",
          "")
newWords <- newWords [! newWords %in% keep]

#create a grob for drive through
grx <- glob2rx("*drive*|*Drive*")
grx <- glob2rx("*drive thr*")


drive <- with(textData, subset(textData, subset = grepl(grx, textData$ExplainProblem), drop = TRUE))

driveThru <- Corpus(VectorSource(drive$ExplainProblem))
driveThru <- tm_map(driveThru,PlainTextDocument)
driveThru <- tm_map(driveThru, removeWords, newWords)
driveThru <- tm_map(driveThru, removePunctuation)
#driveThru <- tm_map(driveThru, removeNumbers)
driveThru <- tm_map(driveThru, stripWhitespace)

#find the dissatisfaction issues





#get data without special chars
#cleanup  text fields with a loop
ReasonForVisit <- filter(textData, OtherReasonForVisit != "" | OtherReasonForVisit != "NA")

visit <- Corpus(VectorSource(ReasonForVisit$OtherReasonForVisit))
visit <- tm_map(visit,PlainTextDocument)
visit <- tm_map(visit, removeWords, stopwords("english"))
visit <- tm_map(visit, removePunctuation)
visit <- tm_map(visit, removeNumbers)
visit <- tm_map(visit, stripWhitespace)
    

#why highly  satisfied
highlySatisfied <- filter(textData, WhyHighlySatisfied != "" | WhyHighlySatisfied != "NA")

highSat <- Corpus(VectorSource(highlySatisfied$WhyHighlySatisfied))
highSat <- tm_map(highSat,PlainTextDocument)
highSat <- tm_map(highSat, removeWords, newWords)
highSat <- tm_map(highSat, removePunctuation)
highSat <- tm_map(highSat, removeNumbers)
highSat <- tm_map(highSat, stripWhitespace)


#what could we have done differently

doDifferent <- filter(textData, DoDifferently != "" | DoDifferently != "NA")

doDifferently <- Corpus(VectorSource(doDifferent$DoDifferently))
doDifferently <- tm_map(doDifferently,PlainTextDocument)
doDifferently <- tm_map(doDifferently, removeWords, stopwords("english"))
doDifferently <- tm_map(doDifferently, removePunctuation)
doDifferently <- tm_map(doDifferently, removeNumbers)
doDifferently <- tm_map(doDifferently, stripWhitespace)


#explain the problem
explainProblem <- filter(textData, ExplainProblem != "" | ExplainProblem != "NA")

problem <- Corpus(VectorSource(explainProblem$ExplainProblem))
problem <- tm_map(problem,PlainTextDocument)
problem <- tm_map(problem, removeWords, newWords)
problem <- tm_map(problem, removePunctuation)
problem <- tm_map(problem, removeNumbers)
problem <- tm_map(problem, stripWhitespace)

#why rated quality
ratedQuality <- filter(textData, WhyRatedQuality != "" | WhyRatedQuality != "NA")


rateQuality <- Corpus(VectorSource(ratedQuality$WhyRatedQuality))
rateQuality <- tm_map(rateQuality,PlainTextDocument)
rateQuality <- tm_map(rateQuality, removeWords, newWords)
rateQuality <- tm_map(rateQuality, removePunctuation)
rateQuality <- tm_map(rateQuality, removeNumbers)
rateQuality <- tm_map(rateQuality, stripWhitespace)
rateQuality <- tm_map(rateQuality, removeWords, c("see", "above", "previous", "comments"))


m = 2
ngramTokenizer <- function (x) {NGramTokenizer(x,
                                               Weka_control(min = m, max = m))}




#make the TDMs
visitTDM <- TermDocumentMatrix(visit, control = list(tokenize = ngramTokenizer))
highSatTDM <- TermDocumentMatrix(highSat, control = list(tokenize = ngramTokenizer))
differentTDM <- TermDocumentMatrix(doDifferently, control = list(tokenize = ngramTokenizer))
problemTDM <- TermDocumentMatrix(problem, control = list(tokenize = ngramTokenizer))
rateQualityTDM <- TermDocumentMatrix(rateQuality, control = list(tokenize = ngramTokenizer))


driveThruTDM <- TermDocumentMatrix(driveThru, control = list(tokenize = ngramTokenizer))


# work with sparseness
tokenMatrixvisit <- as.matrix(visitTDM)

tokenMatrixhighSat <- as.matrix(removeSparseTerms(x = highSatTDM, sparse = .9992))

tokenMatrixdifferent <- as.matrix(differentTDM)

tokenMatrixproblem <- as.matrix(removeSparseTerms(x = problemTDM, sparse = .9995))

tokenMatrixrateQuality <- as.matrix(removeSparseTerms(x = rateQualityTDM, sparse = .9995))

tokenMatrixdriveThru <- as.matrix(driveThruTDM)




#format the matrices for plotting
tokenV_visit <- sort(rowSums(tokenMatrixvisit), decreasing = TRUE)
token.d_visit <- data.frame(word = names(tokenV_visit), freq=tokenV_visit)
table(token.d_visit$freq)

tokenV_highSat <- sort(rowSums(tokenMatrixhighSat), decreasing = TRUE)
token.d_highSat <- data.frame(word = names(tokenV_highSat), freq=tokenV_highSat)
table(token.d_highSat$freq)

tokenV_different <- sort(rowSums(tokenMatrixdifferent), decreasing = TRUE)
token.d_different <- data.frame(word = names(tokenV_different), freq=tokenV_different)
table(token.d_different$freq)

tokenV_problem <- sort(rowSums(tokenMatrixproblem), decreasing = TRUE)
token.d_problem <- data.frame(word = names(tokenV_problem), freq=tokenV_problem)
table(token.d_problem$freq)

tokenV_rateQuality <- sort(rowSums(tokenMatrixrateQuality), decreasing = TRUE)
token.d_rateQuality <- data.frame(word = names(tokenV_rateQuality), freq=tokenV_rateQuality)
table(token.d_rateQuality$freq)


tokenV_driveThru <- sort(rowSums(tokenMatrixdriveThru), decreasing = TRUE)
token.d_driveThru <- data.frame(word = names(tokenV_driveThru), freq=tokenV_driveThru)
table(token.d_driveThru$freq)


###################################################
####    Make Word Clouds


myPath <- file.path("C:","Users", "n846490", "Documents", "DataScience", "OutputsMapsEtc") 
setwd(myPath)



png("ReasonVisit4Word.png", width = 1280, height = 800)
#par(mfrow=c(3,2), oma = c(0,0,3,0), mar = c(0,0,1,0))
wordcloud(token.d_visit$word, token.d_visit$freq,scale=c(4,.5), min.freq = 1, max.words = 1000,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off() 


png("Different3Word.png", width = 1280, height = 800)
#par(mfrow=c(3,2), oma = c(0,0,3,0), mar = c(0,0,1,0))
wordcloud(token.d_different$word, token.d_different$freq,scale=c(4,.5), min.freq = 1, max.words = 1000,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off() 

png("Problem3WordNoStop.png", width = 1280, height = 800)
#par(mfrow=c(3,2), oma = c(0,0,3,0), mar = c(0,0,1,0))
wordcloud(token.d_problem$word, token.d_problem$freq,scale=c(4,.5), min.freq = 1, max.words = 1000,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off() 

png("RateQuality3WordNoStop.png", width = 1280, height = 800)
#par(mfrow=c(3,2), oma = c(0,0,3,0), mar = c(0,0,1,0))
wordcloud(token.d_rateQuality$word, token.d_rateQuality$freq,scale=c(4,.5), min.freq = 1, max.words = 1000,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off() 


png("highSat2WordNoStopTest.png", width = 1280, height = 800)
#par(oma = c(0,0,1,0), mar = c(0,0,1,0))
wordcloud(token.d_highSat$word, token.d_highSat$freq,scale=c(4,.5), min.freq = 1, max.words = 1000,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off() 


png("driveThrough3a.png", width = 1280, height = 800)
#par(oma = c(0,0,1,0), mar = c(0,0,1,0))
wordcloud(token.d_driveThru$word, token.d_driveThru$freq,scale=c(4,1), min.freq = 1, max.words = 1000,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off() 



#############################################
### Try some word associations


highDTM <- DocumentTermMatrix(highSat)

#check words like service, teller, always
highAssoc <- findAssocs(highDTM, "always", .10)
highAssoc


highDTM <- DocumentTermMatrix(highSat)

#check words like service, teller, always
drivethru <- findAssocs(driveThruTDM, "always line", .05)
drivethru





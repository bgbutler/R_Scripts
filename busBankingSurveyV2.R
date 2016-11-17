#script to analyze survey data


.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(readxl)
library(tm)
library(SnowballC)
library(RTextTools)
library(stringi)
library(wordcloud)
library(RWeka)


#this series is for modeling
library(caret)
library(Metrics)
library(pROC)
library(rpart)
library(rpart.plot)
library(gbm)
library(e1071)
library(glmnet)
library(ROCR)

url <- "C:/Users/n846490/Documents/DataScience/CSVs/BusinessBankingV3.csv"
bus <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


url <- "C:/Users/n846490/Documents/DataScience/CSVs/textBus.csv"
txtNew <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


#convert data short to date for use
bus$DateShort <- as.character(bus$DateShort)
bus$DateShort <- as.Date(bus$DateShort, format = "%m/%d/%Y")
bus$Month <- format(bus$DateShort, format = "%b")
bus$Month <- as.factor(bus$Month)


bus$Month <- ordered(bus$Month, levels = c("Jun", "Jul","Aug","Sep", "Oct"))

bus$OverallBusinessBankingExperience <- ordered(bus$OverallBusinessBankingExperience,
                                        levels = c("NA", "1 - Highly Dissatisfied",
                                                   "2 - Dissatisfied", "3 - Neutral",
                                                   "4 - Satisfied", "5 - Highly Satisfied"))


#clean up the revenue size
bus$BusRev <- as.character(bus$BusRev)

bus$BusRev <- ifelse(bus$BusRev == "&lt;$1MM",  "Less than $1MM", ifelse(
    bus$BusRev == "&gt;25MM", "$25MM+", ifelse(bus$BusRev == "&amp;gt;25MM", "$25MM+",ifelse(
        bus$BusRev == "", "NA",bus$BusRev))))

bus$BusRev <- ordered(bus$BusRev, levels = c("NA", "Less than $1MM",
                                             "$1MM - $3MM", "$3MM - $25MM", "$25MM+"))

bus <- bus[!is.na(bus$OverallBusinessBankingExperience),]



#this is the bar plot of overall satisfaction
g <- ggplot(bus, aes(x = reorder(OverallBusinessBankingExperience, OverallBusinessBankingExperienceNum),
                     y = OverallBusinessBankingExperienceNum, fill = BizRev)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) + 
    facet_wrap(~BizRev) + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


#boxplot to compare
g <- ggplot(bus, aes(x = BizRev, y = OverallBusinessBankingExperienceNum,
                     fill = BizRev)) + 
    geom_boxplot(notch=TRUE) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g

#boxplot to compare
g <- ggplot(bus, aes(x = BizRev, y = OverallBusinessBankingExperienceNum,
                     fill = BizRev)) + 
    geom_boxplot(notch=TRUE) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~Month) +
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g



################################################################################

#create a smaller dataset to melt

busRed <- select(bus, OverallBusinessBankingExperience, BizRev, OverallBusinessBankingExperienceNum,
                 HomePageNum, EaseofNavNum, SiteSearchNum, RelevanceofInfonum, OnlineCalculatorsNum)


busRed <- bus[,c(22, 24,58,42:47)]


busMelt <- melt(busRed, id = c("BizRev", "reasonforvisit", "OverallBusinessBankingExperience"),
                     variable.name = "SiteAttribute",
                     value.name = "Score")
                

colnames(busMelt) <- c("BizRev", "Reason Visit", "Overall Experience", "SiteAttribute", "Score")

                
g <- ggplot(busMelt, aes(x = SiteAttribute, y = Score,
                     fill = SiteAttribute)) + 
    geom_boxplot(notch=TRUE) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~BizRev) +
    theme(axis.text.x = element_blank(),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g                
                
  



g <- ggplot(busMelt, aes(x = BizRev, y = Score,
                         fill = BizRev)) + 
    geom_boxplot(notch=TRUE) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~SiteAttribute) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g                

                
g <- ggplot(busMelt, aes(x = BizRev, y = Score,
                         fill = BizRev)) + 
    geom_boxplot(notch=F) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~`Reason Visit`) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g    


###################################################################
#plots of the second part of the survey

busRed <- select(bus, OverallBusinessBankingExperience, BizRev, BrandITrustNum, BrandwithStrongmomentumNum,
                 BrandthatCaresNum, HonestandOpenNum, LeaderNum)

busMelt <- melt(busRed, id = c("BizRev", "OverallBusinessBankingExperience"),
                     variable.name = "CompanyValue",
                     value.name = "Score")


g <- ggplot(busMelt, aes(x = BizRev, y = Score,
                         fill = BizRev)) + 
    geom_boxplot(notch=T) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~CompanyValue) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g   



g <- ggplot(busMelt, aes(x = CompanyValue, y = Score,
                         fill = CompanyValue)) + 
    geom_boxplot(notch=T) +  
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~BizRev) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g   





#################################################################
#try some text mining

#make the clouds
cwd <- getwd()
myPath <- file.path("C:","Users", "n846490", "Documents", "DataScience", "DigitalAnalytics") 
setwd(myPath)




txtData <- bus[,c(18,19,21,22, 24,58)]

#get data without special chars
textData<- txtData[stri_enc_isutf8(txtData$whysat)== T,]


#start building the corpus
c1 <- Corpus(VectorSource(txtData$whysat))
c1 <- tm_map(c1,PlainTextDocument)
c1 <- tm_map(c1, removeWords, stopwords("english"))
c1 <- tm_map(c1, removePunctuation)
c1 <- tm_map(c1, removeNumbers)
c1 <- tm_map(c1, stripWhitespace)


txtImprove <- txtNew$WhatImprovements

improv1 <- Corpus(VectorSource(txtNew$WhatImprovements))
improv1 <- tm_map(improv1,PlainTextDocument)
#improv1 <- tm_map(improv1, removeWords, stopwords("english"))
improv1 <- tm_map(improv1, removePunctuation)
improv1 <- tm_map(improv1, removeNumbers)
improv1 <- tm_map(improv1, stripWhitespace)



improv2 <- Corpus(VectorSource(txtNew$ImprovementsCouldHaveCompleted))
improv2 <- tm_map(improv2,PlainTextDocument)
#improv2 <- tm_map(improv2, removeWords, stopwords("english"))
improv2 <- tm_map(improv2, removePunctuation)
improv2 <- tm_map(improv2, removeNumbers)
improv2 <- tm_map(improv2, stripWhitespace)






#make a tokenizer
ngramTokenizer <- function (x) {NGramTokenizer(x,
                                               Weka_control(min = 7, max = 7))}


allTDM <- TermDocumentMatrix(c1, control = list(tokenize = ngramTokenizer))
improv1TDM <- TermDocumentMatrix(improv1, control = list(tokenize = ngramTokenizer))
improv2TDM <- TermDocumentMatrix(improv2, control = list(tokenize = ngramTokenizer))




tokenMatrixall <- as.matrix(allTDM)
tokenV_all <- sort(rowSums(tokenMatrixall), decreasing = TRUE)
token.d_all <- data.frame(word = names(tokenV_all), freq=tokenV_all)
table(token.d_all$freq)


tokenMatriximprov1 <- as.matrix(improv1TDM)
tokenV_improv1 <- sort(rowSums(tokenMatriximprov1), decreasing = TRUE)
token.d_improv1 <- data.frame(word = names(tokenV_improv1), freq=tokenV_improv1)
table(token.d_improv1$freq)

tokenMatriximprov2 <- as.matrix(improv2TDM)
tokenV_improv2 <- sort(rowSums(tokenMatriximprov2), decreasing = TRUE)
token.d_improv2 <- data.frame(word = names(tokenV_improv2), freq=tokenV_improv2)
table(token.d_improv2$freq)



##################################################################
#word clouds


png("busSat4.png", width = 1280, height = 800)
#par(mfrow=c(3,2), oma = c(0,0,3,0), mar = c(0,0,1,0))
wordcloud(token.d_all$word, token.d_all$freq,scale=c(4,.5), min.freq = 1, max.words = 1000,
          random.order = FALSE,rot.per=0.15, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off() 



png("busImproveVerbose2.png", width = 1280, height = 800)
#par(mfrow=c(3,2), oma = c(0,0,3,0), mar = c(0,0,1,0))
wordcloud(token.d_improv2$word, token.d_improv2$freq,scale=c(4,.5), min.freq = 1, max.words = 1000,
          random.order = FALSE,rot.per=0.1, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off() 


png("busImproveHelpVerbose7.png", width = 1280, height = 800)
#par(mfrow=c(3,2), oma = c(0,0,3,0), mar = c(0,0,1,0))
wordcloud(token.d_improv2$word, token.d_improv2$freq,scale=c(4,.5), min.freq = 1, max.words = 1000,
          random.order = FALSE,rot.per=0.1, use.r.layout=FALSE,
          colors = brewer.pal(8,"Dark2"))
dev.off() 


############################################################################

#build a dataset to perform glm or glmnet for analyzing drivers of satisfaction

#########################################################################

leader <- select(bus,
               BrandITrustNum,
               BrandwithStrongmomentumNum,
               BrandthatCaresNum,
               HonestandOpenNum,
               LeaderNum)


data <- select(bus,
               OverallBusinessBankingExperienceNum,
               HomePageNum,
               EaseofNavNum,
               SiteSearchNum,
               RelevanceofInfoNum,
               OnlineCalculatorsNum)




#fill in the NAs with the row means
data <- as.matrix(data[,c(1:6)])

data <- as.matrix(leader[,c(1:5)])


k <- which(is.na(data), arr.ind = TRUE)
data[k] <- rowMeans(data, na.rm=TRUE)[k[,1]]



data <- data.frame(data)




#te = name of testing dataset, p = partition ratio (usually .70)
#this is a function only and is called later in the analysis

splitData <- function(df,outcomeName, tr, te, p){
    set.seed(1000)
    splitIndex <- createDataPartition(df[,outcomeName], 
                                      p=p, list = FALSE, times = 1)
    trainDf <- df[splitIndex,]
    testDf <- df[-splitIndex,]
    assign(tr, trainDf, envir = .GlobalEnv)
    assign(te, testDf, envir = .GlobalEnv)
}


highSat <- data.frame(data)
leader <- data.frame(data)

#start with highly satisfied
#convert the outcome
highSat$Outcome <- ifelse(highSat$OverallBusinessBankingExperienceNum > 4,"High","Low")

leader$Outcome <- ifelse(leader$LeaderNum > 4,"Leader","Not")


#onvert to a factor for GLM

highSat$Outcome <- as.factor(highSat$Outcome)
leader$Outcome <- as.factor(leader$Outcome)


highSat <- highSat[,-1]

leader <- leader[,c(1:4,6)]

outcome <- "Outcome"

predictorNames <- names(highSat)[names(highSat) != outcome]

predictorNames <- names(leader)[names(leader) != outcome]


predictorNames <- c("HomePageNum", "RelevanceofInfoNum", "SiteSearchNum")


predictorNames <- c("BrandwithStrongmomentumNum", "BrandthatCaresNum","HonestandOpenNum")


#use  the function above to make the segment splits
splitData(highSat,"Outcome", "trainDf", "testDf", .70)

splitData(leader,"Outcome", "trainDf", "testDf", .70)

#set up the model
objControl <- trainControl(method = 'cv',
                           number = 5,
                           savePredictions = TRUE,
                           returnResamp = 'none',
                           summaryFunction = twoClassSummary,
                           classProbs = T)
objModel <- train(trainDf[,predictorNames], trainDf[,outcome],
                  method = 'glm',
                  family = "binomial",
                  metric = "ROC",
                  trControl = objControl)


#check coef look for significance
summary(objModel)

exp(coef(objModel$finalModel))

#check for variable importance
plot(varImp(objModel, scale = T))


#make predictions
predictionsRaw <- predict(object = objModel, testDf[,predictorNames], type = "raw")
head(predictionsRaw)
print(postResample(pred=predictionsRaw, obs = as.factor(testDf[,outcome])))

#check the accuracy of the model
confusionMatrix(predictionsRaw, testDf$Outcome)

#get the roc
predictionsProb <- predict(object = objModel, testDf[,predictorNames], type = 'prob')
auc <- roc(ifelse(testDf[,outcome] == "High", 1,0), predictionsProb[[1]])
auc

s1 <- 4
s2 <- 3
s3 <- 3
B0 <- 25.9436
B1 <- -1.7961
B2 <- -1.6182
B3 <- -2.3727

P <- B0 + B1*s1 + B2*s2 + B3*s3

H <- 1/(1 + exp(P))
H



o1 <- exp(22.3775 + B1*3)
o2 <- exp(22.3775 + B1*4)
o1
o2

p1 <- o1/(1+o1)
p2 <- o2/(1+o2)
p1
p2



















               
                

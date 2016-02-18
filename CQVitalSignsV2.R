#libraries for reshaping and plotting data
library(ggplot2)
library(reshape2)
library(dplyr)
library(caret)
library(circular)
library(plotrix)


#load the file
url <- "K:/Sandbox/Bryan Projects/CQ/IndustryCompanyCQv2.csv"
cqDataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)






###########################################################
#model to test for questions
#openness Q11
#relevance Q2, Q7, 24
#empathy Q1, Q13, Q17
#CX Q3, Q9, Q10 , Q16
#Emotional Validation Q21, Q22, Q23, Q25

newNames <- c("Company", "Industry2", "CQScore", "Q1", "Q2", "Q3", "Q7", "Q9",
              "Q10", "Q11", "Q13", "Q16", "Q17", "Q19", "Q20", "Q21", "Q22", "Q23",
              "Q24", "Q25", "Openness", "Relevance", "Empathy", "CX", "EmotionalValidation")

colnames(cqDataRaw) <- newNames

#subset the data
radialData <- cqDataRaw[,c(1:2,21:25)]


########################
### Create Multivariate Linear Models
cqModel <- lm(CQScore ~ Industry2 + Q1 + Q2 + Q3 + Q7 + Q9 + Q10 + 
                Q11 + Q13 + Q16 + Q17 + Q21 + Q22 + Q23 + Q24 + Q25,
              data = cqDataRaw)
summary(cqModel)


cqModel2 <- lm(CQScore ~ Industry2 + Openness + Relevance + Empathy + CX + EmotionalValidation, 
               data = cqDataRaw)
summary(cqModel2)

cqModel3 <- lm(CQScore ~ Industry2 + Q1 + Q2 + Q3 + Q7 + Q9 + Q10 + 
                Q11 + Q13 + Q16 + Q17 + Q19 + Q20 + Q21 + Q22 + Q23 + Q24 + Q25-1,
              data = cqDataRaw)
summary(cqModel3)

plot(cqModel)


cqModel4 <- lm(CQScore ~ Openness + Relevance + Empathy + CX + EmotionalValidation, 
               data = cqDataRaw)
summary(cqModel4)
plot(cqModel4)

cqModel5 <- lm(CQScore ~ Industry2 + Empathy + CX + EmotionalValidation, 
               data = cqDataRaw)
summary(cqModel5)

cqModel6 <- lm(CQScore ~ Industry2 + Openness + I(Relevance/3) + I(Empathy/3) + I(CX/4) + I(EmotionalValidation/4), 
               data = cqDataRaw)
summary(cqModel6)


#final simplified model
cqModelRed <- lm(CQScore ~ Industry2 + Q1 + Q3 + Q7 + Q9 + 
                Q11 + Q17 + Q21 + Q23 + Q24 + Q25,
              data = cqDataRaw)
summary(cqModelRed)

cqModelRed2 <- lm(CQScore ~ Industry2 + Q1 + Q3 + Q7 + Q9 + Q17 + Q21 + Q23 + Q25,
                 data = cqDataRaw)
summary(cqModelRed2)




##############
#explore relevance
#start exploratory analysis
plt0 <- ggplot(cqDataRaw, aes(x= Relevance))
plt0 <- plt0 + geom_histogram(na.rm = TRUE, binwidth = 1, fill = "tomato")  +
  ggtitle("Distribution of Relevance") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
plt0

plt0 <- ggplot(cqDataRaw, aes(x= Q24))
plt0 <- plt0 + geom_histogram(na.rm = TRUE, binwidth = .5, fill = "tomato")  +
  ggtitle("Distribution of Q24") + xlim(1,7) + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
plt0

plt0 <- ggplot(cqDataRaw, aes(x= Q11))
plt0 <- plt0 + geom_histogram(na.rm = TRUE, binwidth = .5, fill = "tomato")  +
  ggtitle("Distribution of Q11 Openness") + xlim(1,7) + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
plt0

plt0 <- ggplot(cqDataRaw, aes(x= EmotionalValidation))
plt0 <- plt0 + geom_histogram(na.rm = TRUE, binwidth = .5, fill = "tomato")  +
  ggtitle("Distribution of Emotional Validation") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
plt0

plt0 <- ggplot(cqDataRaw, aes(x= Q2))
plt0 <- plt0 + geom_histogram(na.rm = TRUE, binwidth = .5, fill = "tomato")  +
  ggtitle("Distribution of Q2 Relevance") + xlim(1,7) + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
plt0

plt0 <- ggplot(cqDataRaw, aes(x= Q7))
plt0 <- plt0 + geom_histogram(na.rm = TRUE, binwidth = .5, fill = "tomato")  +
  ggtitle("Distribution of Q7 Relevance") + xlim(1,7) + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
plt0

############################################################
#radial plots

#this is only used if the data has the max score in it
#calculate average scores
radialData$percentOpenness <- radialData$Openness/radialData$maxOpenness
radialData$percentRelevance <- radialData$Relevance/radialData$maxRelevance
radialData$percentEmpathy <- radialData$Empathy/radialData$maxEmpathy
radialData$percentCX <- radialData$CX/radialData$maxCX
radialData$percentEmotionalValidation <- radialData$EmotionalValidation/radialData$maxEmotionalVal


############################################
#select a company
co <- "Victoria's Secret"

companyData <- filter(radialData, Company == co)
head(companyData)

companyDF <- melt(companyData, id=c("Company", "Industry2"), 
                                    variable.name = "Dimension",
                                    value.name = "Score")
companyDF$Loc <- rep(seq(pi/5,2*pi,2*pi/5),3)

z <- rep(seq(pi/5,2*pi,2*pi/5),3)

companyDF

companyDFRed <- companyDF[c(1:5),]
companyDFRed

#test of the radial plot
percentData <- companyData[,13:17]
labels <- c("Openness", "Relevance", "Empathy", 
            "Customer Experience", "Emotional Validation")
pal <- rainbow(6)
pal1 <- c("red", "blue", "yellow", "green", "light blue")

p1 <- radial.plot(companyDFRed$Score, companyDFRed$Loc, labels = companyDFRed$Dimension,
                  main=paste("CQ Dimensions for ",co, sep = ""),
                  label.pos = companyDFRed$Loc, start= pi/3,
                  line.col = pal1, lwd = 5, radial.lim= c(.70,.80))

p1 <- radial.plot(companyDFRed$Score, companyDFRed$Loc, labels = companyDFRed$Dimension,
                  main=paste("CQ Dimensions for ",co, sep = ""),
                  label.pos = companyDFRed$Loc, start= pi/3,
                  line.col = pal1, lwd = 5, radial.lim= c(0,28))

###########################################
#ggplots

g1 <- ggplot(companyDFRed, aes(x=companyDFRed$Score, y=companyDFRed$Loc, fill=companyDFRed$Dimension))
g1 + geom_bar(stat = "identity") + coord_polar() + theme_bw() + 
  ylab("Score") + ggtitle("CQ Dimensions") + 
  scale_x_continuous("", limits = c(0,35), breaks = seq(0, 35,5), labels = seq(0, 35,5))

g2 <- ggplot(companyDFRed, aes(x=companyDFRed$Score, y=companyDFRed$Loc, fill=companyDFRed$Dimension))
g2 + geom_bar(stat = "identity", width = 3) + coord_polar() + theme_bw() + 
  ylab("Score") + ggtitle("CQ Dimensions")


#######################################
#radial plots for the airline industry
unique(cqDataRaw$Industry2)

#ind <- "Restaurant"

#companyData <- radialData[radialData$Industry2 == ind,]

#subset the data to agroup of companies
companyData <- filter(radialData, 
                      Company == "McDonald's"|
                      Company == "Burger King"|
                      Company == "Taco Bell"|
                      Company == "Subway"|
                      Company == "In-N-Out Burger"|
                      Company == "Wendy's"|
                      Company == "Chipotles"|
                      Company == "Starbucks"|
                      Company == "Dunkin' Donuts"|
                      Company == "Panera Bread"|
                      Company == "Starbucks"|
                      Company == "Apple"|
                      Company == "Southwest Airlines"|  
                      Company == "Ford"|
                      Company == "Amazon"|
                      Company == "Wal-Mart")

head(companyData,15)
companyData$Company 


#calculate average scores for all companies in the large slice
companyData$percentOpenness <- companyData$Openness/7
companyData$percentRelevance <- companyData$Relevance/21
companyData$percentEmpathy <- companyData$Empathy/21
companyData$percentCX <- companyData$CX/28
companyData$percentEmotionalValidation <- companyData$EmotionalValidation/28

companyData$DimensionTotal <- companyData$Openness + companyData$Relevance + companyData$Empathy +
  companyData$CX + companyData$EmotionalValidation

head(companyData)

#take the percentage only columns
companyData <- companyData[,c(1,8:12)]


#this for the total plot
companyDataTotal <- companyData[,c(1,3:8)]

companyData$Company <- as.character(companyData$Company)

co <- companyData$Company

#change column names
newCols <- c("Company", "Openness", "Relevance", "Empathy",
             "CX", "Emotional Validation")


newCols <- c("Company", "Openness", "Relevance", "Empathy",
             "CX", "EV","DimensionTotal")


colnames(companyData) <- newCols

colnames(companyDataTotal) <- newCols

companyData$Company <- as.factor(companyData$Company)

head(companyData)
head(companyDataTotal)



#####################
#make a radial plot for each company
#co is a lst of the names

meltCompanyDF <- melt(CompanyData, id=c("Company"), 
                  variable.name = "Dimension",
                  value.name = "Score")

meltCompanyDF <- arrange(meltCompanyDF, Company, Dimension)
meltCompanyDF
meltCompanyDF$Loc <- rep(seq(pi/5,2*pi,2*pi/5),14)

companies <- split(meltCompanyDF, meltCompanyDF$Company)
names(companieslines) <- co

companies[[1]][[4]]


pal1 <- c("red", "blue", "yellow", "green", "light blue")

pal2 <- "spectral"

#radial plot of lines using the mfrow to plot more than 2 companies
par(mfrow = c(4,4), mar = c(2,2,6,4), oma = c(0,1,0,0))

#used for two companies
par(mfrow = c(1,2), mar = c(2,2,6,4), oma = c(0,0,0,0))

par(cex.axis = .5)   #labels
par(cex.lab=.5)       #values 
par(col.main="blue", cex.main = 1.1)
par(col="blue")       #changes number and legend font color
pl <- list()
for (i in 1:14){
pl <- radial.plot(companies[[i]][[3]],companies[[i]][[4]] , labels = companies[[i]][[2]],
                  main=(co[i]), cex.lab = 0.1,
                  label.pos = companies[[i]][[4]], start= pi/3,
                  boxed.radial = FALSE, radial.lim = c(0,1),
                  line.col = pal1, lwd = 5)
}
dev.off()


#plot as polygons
pl <- list()
for (i in 1:14){
  pl <- radial.plot(companies[[i]][[3]],companies[[i]][[4]] , labels = companies[[i]][[2]],
                    main=(co[i]), cex.lab = .1, rp.type = "p",
                    label.pos = companies[[i]][[4]], start= pi/3,
                    boxed.radial = FALSE, radial.lim = c(0,.8),
                    line.col = pal1, lwd = 5)
}


######################################################
#this section allows for the plotting of several companies on one plot
#create the data matrix for plotting


companyData1 <- filter(companyData, 
                       Company == "McDonald's"|
                       Company == "Burger King"|
                       Company == "Taco Bell"|
                       Company == "Subway"|
                       Company == "In-N-Out Burger"|
                       Company == "Wendy's")

companyData2 <- filter(companyData,
                       Company == "McDonald's"|
                         Company == "Chipotles"|
                         Company == "Starbucks"|
                         Company == "Dunkin' Donuts"|
                         Company == "Panera Bread"|
                         Company == "Starbucks") 

companyData2


companyData3 <- filter(companyData,
                       Company == "McDonald's"|
                         Company == "Apple"|
                         Company == "Southwest Airlines"|  
                         Company == "Ford"|
                         Company == "Amazon"|
                         Company == "Wal-Mart")








companyMat <-as.matrix(companyData2[,2:6])
companyMat

#create the labels and count them
companyLabs <- as.vector(companyData2$Company)
numberCo = length(companyLabs)

co <- companyData2$Company
co


#get the colors
pal2 <- rainbow(numberCo)
pal3 <- "Dark2"


#################################################
#this is for radial plots


#matrix of locations
zMat <- matrix(nrow = numberCo, ncol = 5)
z1 <- matrix(seq(pi/5,2*pi,2*pi/5), ncol = 5)
for (i in 1:numberCo){
  zMat[i,] <- z1 + (i*.1)
}
zMat



#adjust plot to try a multiplot as polygons
par(cex.axis = .65)   #labels
par(cex.lab=.5)       #values 
par(col.main="blue", cex.main = 1.1)
par(col="grey48")       #changes number and legend font color
rP <- radial.plot(companyMat,zMat , labels = newCols[2:6],
                       main=("McDonald's & Tier 2 Competitors"), cex.lab = .3,
                       start= pi/4, rp.type = "p",label.pos = zMat[1,],
                       boxed.radial = FALSE, radial.lim = c(0,1),
                       line.col = pal2, lwd = 4)
                       par(xpd=TRUE)
                       legend(1.2,1.2,co,lty=1,lwd=2,col=pal2, bty="n", cex = .75)
par(xpd=FALSE)
dev.off()

#adjust plot to try a multiplot as radials
#set the params first
par(mfrow = c(1,1))
par(cex.axis = .65)   #labels
par(cex.lab=.5)       #values 
par(col.main="blue", cex.main = 1.1)
par(col="grey48")       #changes number and legend font color
rP <- radial.plot(companyMat,zMat , labels = newCols[2:6],
                  main=("McDonald's & Tier 2 Competitors"), radlab = FALSE,
                  start= pi/4,label.pos = zMat[1,],
                  boxed.radial = FALSE, radial.lim = c(0,1),
                  line.col = pal3, lwd = 4)
                  legend(1,1.2,co,lty=1,lwd=2,col=pal3, bty="n", cex = .75)
dev.off()

par()

###########################################
#this is for bar plots

#plot tier 1 competitors
#reshpe the data by melting

meltCompanyData1 <- melt(companyData1, id=c("Company"), 
                                      variable.name = "Dimension",
                                      value.name = "Score")

meltCompanyData2 <- melt(companyData2, id=c("Company"), 
                         variable.name = "Dimension",
                         value.name = "Score")

meltCompanyData3 <- melt(companyData3, id=c("Company"), 
                         variable.name = "Dimension",
                         value.name = "Score")



p1 <- ggplot(data = meltCompanyData1, aes(x = reorder(Dimension, Score), y = Score, fill = Dimension)) + 
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~Company) +
  ggtitle("McDonalds and Tier 1 Competitors") + 
  xlab("") +
  ylab("") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(p1)


p2 <- ggplot(data = meltCompanyData2, aes(x = reorder(Dimension, Score), y = Score, fill = Dimension)) + 
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~Company) +
  ggtitle("McDonalds and Tier 2 Competitors") + 
  xlab("") +
  ylab("") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(p2)


p3 <- ggplot(data = meltCompanyData3, aes(x = reorder(Dimension, Score), y = Score, fill = Dimension)) + 
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~Company) +
  ggtitle("McDonalds and CQ Benchmarks") + 
  xlab("") +
  ylab("") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(p3)


meltAllCompanyDF <- melt(companyData, id=c("Company"), 
                         variable.name = "Dimension",
                         value.name = "Score")

p4 <- ggplot(data = meltAllCompanyDF, aes(x = reorder(Dimension, Score), y = Score, fill = Dimension)) + 
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~Company, nrow = 5) + 
  scale_y_continuous(limits = c(0,1)) + 
  ggtitle("McDonalds and CQ Comparisons") + 
  xlab("") +
  ylab("") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(p4)


######################################################
#for alternate plot

#make a subset


meltCompanyData <- melt(companyDataTotal, id=c("Company","DimensionTotal"), 
                         variable.name = "Dimension",
                         value.name = "Score")

#for working with percents
meltCompanyData <- melt(companyData, id=c("Company"), 
                        variable.name = "Dimension",
                        value.name = "Score")


head(meltCompanyData,20)

by_DimTotal <- group_by(meltCompanyData, Company, Dimension, DimensionTotal)
cq_Summary <- summarise(by_DimTotal,
                         totDims = sum(Score, na.rm = T))


by_DimTotal <- group_by(meltCompanyData, Company, Dimension)
cq_Summary <- summarise(by_DimTotal,
                        totDims = sum(Score, na.rm = T))


arrange(cq_Summary, desc(totDims))



p1 <- ggplot(data = cq_Summary, aes(x = reorder(Company, totDims), y = totDims, fill = Dimension)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  ggtitle("McDonalds and CQ Comparisons (Max Total = 105)") + 
  xlab("") +
  ylab("") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(p1)


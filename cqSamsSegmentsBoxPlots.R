
#this is for extracting company data and making significance comparisons


library(ggplot2)
library(dplyr)
library(reshape2)
library(MASS)


#get all data
urlAll <- "Q:/Analytics/CQ/CSV Data/samsSegments.csv"
cqAll <- read.csv(urlAll, header = TRUE, na.strings = c('', "NA"), sep=",", as.is = FALSE)


# if using the full data set
cqAll <- cqAll[,c(7:25)]

#reduced set
cqAll <- cqAll[,c(1,4:18)]

#remove outcome variables
cqDims <- cqAll[,c(3,5:21)]

#for smaller set relevant Q's only

unique(cqAll$Company)

#select the companies to compare

#subset the data to agroup of companies
companyData <- filter(cqAll, 
                      Company == "Federal Express"|
                        Company == "UPS")


#melt the data
cqMelt <- melt(cqDims, id=c("Company"), 
               variable.name = "Question",
               value.name = "Score")                        


#melt the data
cqMelt <- melt(cqDims, id=c("Segment"), 
               variable.name = "Question",
               value.name = "Score")                        

cqMelt$k_8 <- as.factor(as.character(cqMelt$k_8))                        
head(cqMelt)                        
                        
# add the dimensions
cqMelt$Dimension <- ifelse(cqMelt$Question == "Q11", "Openness",
                           ifelse(cqMelt$Question %in% c("Q2", "Q7", "Q24"), "Relevance",
                           ifelse(cqMelt$Question  %in% c("Q3","Q9","Q10","Q16"), "CX",
                           ifelse(cqMelt$Question %in% c("Q1","Q13","Q17"), "Empathy",
                                                "EV"))))

# add the questions
cqMelt$Text <- ifelse (cqMelt$Question == "Q3", "CX is everyone's job",
               ifelse (cqMelt$Question == "Q9", "Cross-channel consistency",
               ifelse (cqMelt$Question == "Q10","Good use of my time",
               ifelse (cqMelt$Question == "Q16","Appreciates my loyalty",
               ifelse (cqMelt$Question == "Q21","I feel Smart",
               ifelse (cqMelt$Question == "Q22","I feel Proud",
               ifelse (cqMelt$Question == "Q23","Not ripped off",
               ifelse (cqMelt$Question == "Q25","Sense of Belonging",
               ifelse (cqMelt$Question == "Q11", "Open dialogue",
               ifelse (cqMelt$Question == "Q1","Gets me",
               ifelse (cqMelt$Question == "Q13","Use own prods/svcs",
               ifelse (cqMelt$Question == "Q17","Better intuition than others",
               ifelse (cqMelt$Question == "Q2","Speaks my language",
               ifelse (cqMelt$Question == "Q7","Shares my values", "Meets needs like no other"
                ))))))))))))))
                              
                        

head(cqMelt,20)

position <- "bottom"
comp1Color <- "midnightblue"
comp2Color <- "orange4"

#co order:  3m, FedEx, GE, HP, MS, Mot, UPS
pal4 <- c("orangered1","mediumblue","gold1", "seagreen", 
          "cornflowerblue" ,"midnightblue","darkgoldenrod")
pal3 <- c("orangered1", "seagreen", "mediumblue")

pal5 <- rainbow(8)

titleText <- "Sam's Club Segments"


P1 <- ggplot(cqMelt, aes(x = Segment, y = Score, fill = Segment)) + 
  geom_boxplot(notch = TRUE,col=c("black")) + scale_fill_manual(values = pal5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size = 3, fill = "white") +  
  ggtitle(titleText) + facet_wrap(~Dimension) +
  theme(legend.position = position) +
  ylab("Score") + 
  xlab("") +
  theme(plot.title = element_text(face = "bold.italic", color = "blue"),
        axis.title.x=element_text(color = "red"),
        axis.title.y=element_text(color = "red"),
        axis.text.x=element_text(color = "white"),
        axis.text.y=element_text(color = "red"),
        strip.text.x = element_text(color = "blue"))
P1


#boxplots for question
P1 <- ggplot(cqMelt, aes(x = k_8, y = Score, fill = k_8)) + 
  geom_boxplot(notch = F) + scale_fill_manual(values = pal5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size = 3, fill = "white") +
  ggtitle(titleText) + facet_wrap(~Question, nrow = 5) +
  ylab("Score") +
  xlab("") +
  theme(legend.position = position) +
  theme(plot.title = element_text(face = "bold.italic", color = "blue"),
        axis.title.x=element_text(color = "red"),
        axis.title.y=element_text(color = "red"),
        axis.text.x=element_text(color = "white"),
        axis.text.y=element_text(color = "red"),
        strip.text.x = element_text(color = "blue"))
print(P1)

#boxplots for question
P1 <- ggplot(cqMelt, aes(x = Segment, y = Score, fill = Segment)) + 
  geom_boxplot(notch = F) + scale_fill_manual(values = pal5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size = 3, fill = "white") +
  ggtitle(titleText) + facet_wrap(~Text, nrow = 5) +
  ylab("Score") +
  xlab("") +
  theme(legend.position = position) +
  theme(plot.title = element_text(face = "bold.italic", color = "blue"),
        axis.title.x=element_text(color = "red"),
        axis.title.y=element_text(color = "red"),
        axis.text.x=element_text(color = "white"),
        axis.text.y=element_text(color = "red"),
        strip.text.x = element_text(color = "blue"))
print(P1)








                        
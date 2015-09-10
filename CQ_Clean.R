
#libraries for reshaping and plotting data
library(ggplot2)
library(reshape2)


#load the file
url <- "K:/Sandbox/R/CQ 07-28.csv"
cqDataRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


#change the characters of the data for the Q's this will cause a warning for the NA's
factorToNumeric <- function(f) as.numeric(levels(f))[as.integer(f)]
cols <- c(6:ncol(cqDataRaw))
cqDataRaw[cols] <- lapply(cqDataRaw[cols], factorToNumeric)

#remove NA rows from the data
cqData <- na.omit(cqDataRaw)

#split out data for analysis by questions

cq_q18_4 <- cqData[cqData$Q18 == '4', ]
cq_q19_4 <- cqData[cqData$Q19 == '4', ]
cq_q20_4 <- cqData[cqData$Q20 == '4', ]



#reshape the data for plotting in more R-like fashion
cq18_4Melt <- melt(cq_q18_4, 
                   id = c("Loop", "Row.ID", "ResponseID", "Industry", "Company"))

cq19_4Melt <- melt(cq_q19_4, 
                   id = c("Loop", "Row.ID", "ResponseID", "Industry", "Company"))

cq20_4Melt <- melt(cq_q20_4, 
                   id = c("Loop", "Row.ID", "ResponseID", "Industry", "Company"))

#change column names for melted data
newnames <- c("Loop", "RowID", "ResponseID", "Industry",
              "Company", "Question", "Answer")

colnames(cq18_4Melt) <- newnames
colnames(cq19_4Melt) <- newnames
colnames(cq20_4Melt) <- newnames

#begin plots as first visualization on question 18
cq18Plot <- ggplot(cq18_4Melt, aes(x =Answer, fill = Industry))
cq18Plot + geom_histogram(binwidth = .5) + facet_wrap(~Question, ncol = 5) + ylim(0,1200) +
ggtitle("Distribution of Answers to Other Questions when Q18 = 4") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))



cq18Plot <- ggplot(cq18_4Melt, aes(x =Answer), color = "blue")
cq18Plot + geom_histogram(binwidth = .5, fill = "red") + facet_wrap(~Question, ncol = 5) + ylim(0,1200) +
  ggtitle("Distribution of Answers to Other Questions when Q18 = 4") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

cq18Plot <- ggplot(cq18_4Melt, aes(x =Answer))
cq18Plot + geom_histogram(binwidth = .5, fill = "red") + facet_wrap(~Industry, ncol = 5) + ylim(0,1200) + 
  ggtitle("Distribution of Answers to Other Questions when Q18 = 4 by Industry") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

#begin plots as first visualization on question 19
cq19Plot <- ggplot(cq19_4Melt, aes(x =Answer, fill = Industry))
cq19Plot + geom_histogram(binwidth = .5) + facet_wrap(~Question, ncol = 5) + ylim(0,1200) + 
  ggtitle("Distribution of Answers to Other Questions when Q19 = 4") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

cq19Plot <- ggplot(cq19_4Melt, aes(x =Answer))
cq19Plot + geom_histogram(binwidth = .5, fill = "red") + facet_wrap(~Question, ncol = 5) + ylim(0,1200) + 
  ggtitle("Distribution of Answers to Other Questions when Q19 = 4") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

cq19Plot <- ggplot(cq19_4Melt, aes(x =Answer))
cq19Plot + geom_histogram(binwidth = .5, fill = "red") + facet_wrap(~Industry, ncol = 5) + ylim(0,1200)
ggtitle("Distribution of Answers to Other Questions when Q19 = 4 by Industry") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

#begin plots as first visualization on question 20
cq20Plot <- ggplot(cq20_4Melt, aes(x =Answer, fill = Industry))
cq20Plot + geom_histogram(binwidth = .5) + facet_wrap(~Question, ncol = 5) + ylim(0,1200) + 
  ggtitle("Distribution of Answers to Other Questions when Q20 = 4") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

cq20Plot <- ggplot(cq20_4Melt, aes(x =Answer))
cq20Plot + geom_histogram(binwidth = .5, fill = "red") + facet_wrap(~Question, ncol = 5) + ylim(0,1200) + 
  ggtitle("Distribution of Answers to Other Questions when Q20 = 4") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

cq20Plot <- ggplot(cq20_4Melt, aes(x =Answer))
cq20Plot + geom_histogram(binwidth = .5, fill = "red") + facet_wrap(~Industry, ncol = 5) + ylim(0,1200)
ggtitle("Distribution of Answers to Other Questions when Q20 = 4 by Industry") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))


#results are as follows:
#q18, 4's can be coded as 5,6,7
#q19, 4's can be coded as 1,2,3
#q20 - too difficult to sort


#compute the average accross all data sets look at median too
q18Avg = tapply(cq18_4Melt$Answer, cq18_4Melt$Question, mean)
q18Median = tapply(cq18_4Melt$Answer, cq18_4Melt$Question, median)

q19Avg = tapply(cq19_4Melt$Answer, cq19_4Melt$Question, mean)
q19Median = tapply(cq19_4Melt$Answer, cq19_4Melt$Question, median)

q20Avg = tapply(cq20_4Melt$Answer, cq20_4Melt$Question, mean)
q20Median = tapply(cq20_4Melt$Answer, cq20_4Melt$Question, median)

#do some clean up and remove uneeded data
rm(cqDataRaw)
rm(cq18_4Melt)
rm(cq19_4Melt)
rm(cq20_4Melt)


#copy the test data
TestData <- cqData

#apply the changes to the groupings based on the analysis
change18 <- function(x) {
  if (x>3) x = 1
  x = 0
}

TestData$Q18 <- sapply(TestData$Q18, change18)

#change the q19 values
change19 <- function(x) {
  if (x>4) x = 1
  x = 0
}
TestData$Q19 <- sapply(TestData$Q19, change19)

#Cut the data for the specific question
TestData18 <- TestData[,c(1:23, 26:30)]


#set seed and create a random number generator using the uniform dist
set.seed(8888)
TestData18$P <- runif(28652)


#create the training data
TestData18Train <- TestData18[TestData18$P < .75,]
TestData18Test <- TestData18[TestData18$P >= .75,]

logDataQ18 <-glm(Q18 ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + 
                 Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + 
                   Q21 + Q22 + Q23 + Q24 + Q25, data = TestData18, 
                 family = "binomial")




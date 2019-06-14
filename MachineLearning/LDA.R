#this does a linear discriminant analysis

library(MASS)

data1 <- read.csv()

#Plot of the data

#group data cannot be a number must be a category
data1.lda <- lda(Group ~ Pred1 + Pred2, data = data1)

#predict function
data1.lda.p <- predict(data1.lda, newdata = data)$class

#get the classifications
table(data1.lda.p, data1[,1])


#leave one out cross validation
data1.lda.2 <- lda(Group ~ Pred1 + Pred2, data = data1, CV = TRUE)

#change the numeric to factors don't need to run on all data
NumericToFactor <- function(f) as.factor(as.character(f))
cols <- c(2:ncol(lda.data))
lda.data[cols] <- lapply(lda.data[cols], NumericToFactor)

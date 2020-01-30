######## Multiclass classififcation

library(nnet)
library(caret)

setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearningKeras/Data and Code/section 4")




data(iris)
head(iris)
summary(iris)

# fit model
fit <- nnet(Species~., data=iris, size=4, decay=0.0001, maxit=500)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4], type="class")
# summarize accuracy

confusionMatrix(predictions, iris$Species)

## LOAN PAYMENT MODEL
lp=read.csv("LoanPay.csv")
head(lp)
summary(lp)

lp[is.na(lp)] = 0 #replace NA with 0

nameList=c("loan_status","Principal","terms","age","education","Gender","past_due_days")

df= lp[,colnames(lp) %in% nameList] 

head(df)

fit2 <- nnet(loan_status~., data=df, size=4, decay=0.0001, maxit=500)

summary(fit2)

predictions <- predict(fit2, df[,2:7], type="class")

#p=as.data.frame(predictions)

table(predictions, df$loan_status)

library(caret)
confusionMatrix(predictions, df$loan_status)




# binary classification
library(caret)


setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearningKeras/Data and Code/section 4")

credit = read.csv("UCI_Credit_Card.csv")

################################################################
############## binary classification with neural networks
# predict if the customer
# will default next month 
head(credit)

#drop columns
drops =c("ID")
df=credit[ , !(names(credit) %in% drops)]

head(df)

# cleanup the column name
names(df)[names(df) == 'default.payment.next.month'] <- 'default'

str(df)

## 1 is default and 0 is not default. Lets recode
df$default[df$default==1] <- 'Yes'
df$default[df$default==0] <- 'No'


# split data in 75%-25% ratio
set.seed(99)
Train = createDataPartition(df$default, p=0.75, list=FALSE)
#split data in 75%-25% ratio

training = df[ Train, ] #75% data for training 
testing = df[ -Train, ] #25% testing

## implement cross-validation
fitControl = trainControl(method="cv",
                          number = 10,
                          preProcOptions = list(thresh = 0.95), # threshold for pca preprocess
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)


################

modelNnet = train(default~.,
                  training,
                  method="nnet",
                  metric="ROC",
                  preProcess=c('center', 'scale'),
                  trace=FALSE,
                  tuneLength=10,
                  trControl=fitControl)

modelNnet

summary(modelNnet)

# plot main and CV folds
plot(modelNnet)

# make predictions
predNnet= predict(modelNnet, testing)

# confusion matrix
cmNnet= confusionMatrix(predNnet, testing$default)
cmNnet

varImp(modelNnet)

plot(varImp(modelNnet))
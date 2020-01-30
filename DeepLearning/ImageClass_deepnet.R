# immage class with deepnet

setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearningKeras/Data and Code/section 4")


require(deepnet)
require(nnet)
library(readr)

train = read_csv("digit_train.csv")


######### explore data

head(train)

train$label = as.factor(train$label)

#check for levels
levels(train$label)

library(caret)
library(nnet)
set.seed(1234)

model_nn = nnet(label ~ ., data=train, size=50, maxit=300, MaxNWts=100000, decay=1e-4)

prediction_nn = predict(model_nn, test, type = "class")

head(prediction_nn)



library(RSNNS)

setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearningKeras/Data and Code/section 4")


d=read.csv("glassClass.csv")


head(d)
str(d)

d$Type =as.factor(d$Type)

df = d[sample(1:nrow(d),length(1:nrow(d))),1:ncol(d)]

dfValues = df[,1:9] #predictors
dfTargets = decodeClassLabels(df[,10])#response

df = splitForTrainingAndTest(dfValues, dfTargets, ratio=0.20)
##80% training & 20% testing
df = normTrainingAndTestSet(df)#normalize the data

## MLPs are fully connected feed-forward networks
model = mlp(df$inputsTrain, 
            df$targetsTrain, 
            size=5, 
            learnFuncParams=c(0.1),
            maxit=50, 
            inputsTest=df$inputsTest, 
            targetsTest=df$targetsTest)

summary(model)
# function prints out a summary of the network
weightMatrix(model)

# function plots the iterative training and test error of the net of the model.
plotIterativeError(model)

predictions = predict(model,df$inputsTest)

# plotRegressionError(predictions[,2], df$targetsTest[,2])

## calculated the confusion matrix for the data used in the training
confusionMatrix(df$targetsTrain,fitted.values(model))

## calculated the confusion matrix for the data used in the test
## (which is the remaining 20 percent of data
confusionMatrix(df$targetsTest,predictions)

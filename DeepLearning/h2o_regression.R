##### regression 

setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearningKeras/Data and Code/section 4")


library(tidyverse)
library(ggplot2)
library(mlbench)
library(h2o)

data(BostonHousing)
head(BostonHousing)
b=BostonHousing

## medv is a response variable (continuous numerical variable)
#1-13 are perdictors
str(b) 



h2o.init()


d.hex = as.h2o(b, destination_frame= "d.hex")
##hex file is h2o compatible

head(d.hex)
str(d.hex)
summary(d.hex)
ncol(d.hex)

g.split = h2o.splitFrame(data = d.hex,ratios = 0.75)
train = g.split[[1]]#75% training data
test = g.split[[2]]

# 3 layer dnn
m = h2o.deeplearning(x = 1:13,
                     y = "medv",
                     training_frame = train,
                     #distribution = "multinomial",
                     model_id = "m",
                     activation = "Tanh",
                     l2 = 0.00001,
                     hidden = c(162,162,100),
                     )





preds=as.vector(h2o.predict(m,test))

# mse
mean(abs(preds-as.vector(test$medv))) 







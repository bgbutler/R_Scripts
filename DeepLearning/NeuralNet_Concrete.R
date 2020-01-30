
library(caret)
library(neuralnet)
library(tidyverse)

## data uploaded to:
##https://github.com/Jojo666/Sentimental-Analysis_R/blob/master/Concrete_Data.xls

setwd("/Users/bryanbutler/Documents/RProjects/PastWork")

require(readxl)
con=read_xls("Concrete_Data.xls")

head(con)

colnames(con) = c("cement", "slag", "ash","water", "superplastic",
                  "coarseagg","fineagg","age","strength")

con=as.data.frame(con)

head(con)
######normalize all variables

normal=function(x){
    return((x-min(x))/(max(x)-min(x)))}

con_norm=as.data.frame(lapply(con,normal))

head(con_norm)

#pre-process 
trainIndex = createDataPartition(con$strength, p=.75, list=F)
training = con[trainIndex, ]
testing = con[-trainIndex, ]

# simple ANN with only a single hidden neuron
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                                ash + water + superplastic + 
                                coarseagg + fineagg + age,
                            data = training)

plot(concrete_model)

con_r = compute(concrete_model, testing[1:8]) #implement on 
#predictors in the testing set

predicted_strength = con_r$net.result

srmse = sqrt(mean((predicted_strength - testing$strength)^2)) 

srmse


###########improve the model, add more neurons

# simple ANN with only 5 hidden neurons
concrete_model2 = neuralnet(formula = strength ~ cement + slag +
                                ash + water + superplastic + 
                                coarseagg + fineagg + age,
                            data = training,hidden=5)

plot(concrete_model2)

con_r2 = compute(concrete_model2, testing[1:8]) #implement on 
#predictors in the testing set

predicted_strength2 = con_r2$net.result

srmse2 = sqrt(mean((predicted_strength2 - testing$strength)^2)) 

srmse2
################# variable importance in neural network models
## data uploaded to:
##https://github.com/Jojo666/Sentimental-Analysis_R/blob/master/Concrete_Data.xls

library(caret)
library(neuralnet)

setwd("/Users/bryanbutler/Documents/RProjects/PastWork")

require(readxl)
con=read_xls("Concrete_Data.xls")


head(con)

colnames(con) = c("cement", "slag", "ash","water", "superplastic",
                  "coarseagg","fineagg","age","strength")

con=as.data.frame(con)

head(con)
######normalize all variables 0-1

normal=function(x){
    return((x-min(x))/(max(x)-min(x)))}

con_norm=as.data.frame(lapply(con,normal))

head(con_norm)

#pre-process 
trainIndex = createDataPartition(con$strength,, p=.75, list=F)
training = con[trainIndex, ]
testing = con[-trainIndex, ]


concrete_model2 = neuralnet(formula = strength ~ cement + slag +
                                ash + water + superplastic + 
                                coarseagg + fineagg + age,
                            data = training,hidden=5)

plot(concrete_model2)

library(NeuralNetTools)

# plot
par(mar = numeric(4))
plotnet(concrete_model2)
#neural interpretation diagram (NID). 
#The default settings are to plot as NID with positive weights between
#layers as black lines and negative weights as grey lines. 
#Line thickness is in proportion to relative magnitude of each weight.

# relative predictor importance

## Garson's algorithm
## Evaluate relative variable importance. 
##This function identifies the relative importance of predictors
##for a single response variable by deconstructing the model weights.

##The importance of each variable can be determined by identifying all
##weighted connections between the layers in the network. 
##That is, all weights connecting the specific input node that pass 
##through the hidden layer to the response variable are identified. 
##This is repeated for all other explanatory variables until a list 
##of all weights that are specific to each input variable is obtained. 
garson(concrete_model2)

##Olden
##Calculates importance as the product of the raw input-hidden 
##and hidden-output connection weights between each input and output 
##neuron and sums the product across all hidden neurons.

olden(concrete_model2)

#also identifies if a variable has a positive or negative effect
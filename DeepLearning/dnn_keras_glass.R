


###################################################################
####### Keras DNN on Real Data

library(keras)
#data("iris")
#head(iris)
library(caret)


setwd("/Users/bryanbutler/Documents/RProjects/PastWork")
glass = read.csv("glassClass.csv")

head(glass)
class(glass$Type)
ncol(glass)

glass$Type = as.factor(glass$Type)
## response variable: 6 different glass clasess
class(glass$Type)

index<-createDataPartition(glass$Type,p=0.7,list=F)
### 70% training and 30% testing 

Train_Features <- data.matrix(glass[index,-10]) ##numerical Xs need to be a matrix 
Train_Labels <- glass[index,10] ##y

Test_Features <- data.matrix(glass[-index,-10]) #30%
Test_Labels <- glass[-index,10]

### One hot encoding (keras)

##transform your target attribute from a vector that contains values
##for each class value to a matrix with a boolean for each class 
##value and whether or not a given instance has that class value 
##or not
to_categorical(as.numeric(Train_Labels))[,c(-1)] -> Train_Labels
to_categorical(as.numeric(Test_Labels))[,c(-1)] -> Test_Labels

### Normalize the predictor values
### predictor values should be between 0-1

as.matrix(apply(Train_Features, 2, function(x) (x-min(x))/(max(x) - min(x)))) -> Train_Features

as.matrix(apply(Test_Features, 2, function(x) (x-min(x))/(max(x) - min(x)))) -> Test_Features

### set up Keras for 2 hidden layer DNN

model <- keras_model_sequential()

# Add layers to the model (2 hidden layers) with "relu" activation
#the first layer needs the input_shape argument to equal the 
#number of features in your data
model %>%
    layer_dense(units=10,activation = "relu",input_shape = ncol(Train_Features)) %>%
    layer_dense(units = 10, activation = "relu") %>%
    layer_dense(units = 6, activation = "softmax")

summary(model)

model %>% compile(loss = "categorical_crossentropy",
                  optimizer = optimizer_adagrad(),
                  metrics = c('accuracy')
)

history <- model %>% fit(Train_Features,Train_Labels,
                         validation_split = 0.10,epochs=300,batch_size = 5,shuffle = T)

model %>% evaluate(Test_Features,Test_Labels)

# auto encoder on credi data

#########A stacked autoencoder can have n layers, 
###where each layer is trained using one layer at a time. 
### For example, the previous layer

library(keras)
library(caret)


setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearning")
credit = read.csv("creditcard.csv")

head(credit)

### remove Time column
credit_df = subset(credit, select = -c(Time))
head(credit_df)

ncol(credit_df)

# create the class
credit_df$Class=as.factor(credit_df$Class)

# split the data
index<-createDataPartition(credit_df$Class,p=0.7,list=F)
### 70% training and 30% testing 

Train_Features <- data.matrix(credit_df[index,-30]) 
##numerical Xs need to be a matrix 
Train_Labels <- credit_df[index,30] ##y

ncol(Train_Features) #29 predictors

Test_Features <- data.matrix(credit_df[-index,-30]) #30%
Test_Labels <- credit_df[-index,30]


### Normalize the predictor values
### predictor values should be between 0-1

as.matrix(apply(Train_Features, 2, function(x) (x-min(x))/(max(x) - min(x)))) -> Train_Features

as.matrix(apply(Test_Features, 2, function(x) (x-min(x))/(max(x) - min(x)))) -> Test_Features

#### Keras Model

input_dim = 29 ## no. of predictors

outer_layer_dim = 14
inner_layer_dim = 7

input_layer = layer_input(shape=c(input_dim))

encoder = layer_dense(units = outer_layer_dim, activation = 'relu')(input_layer)

encoder = layer_dense(units = inner_layer_dim, activation = 'relu')(encoder)

###### decoder
decoder = layer_dense(units = inner_layer_dim)(encoder)

decoder = layer_dense(units = outer_layer_dim)(decoder)

decoder = layer_dense(units = input_dim)(decoder)

auto =keras_model(inputs = input_layer,outputs = decoder)

auto 

auto %>% compile(optimizer = "adam",
                 loss = "mean_squared_error",
                 metrics = c("accuracy"))

history = auto %>% fit (Train_Features,Train_Features,
                        epochs = 10, batch_size = 32,
                        validation_split = 0.2)

plot(history)

### reconstruct on the test set

pred_test = auto %>% predict(Test_Features)

r=RMSE(pred_test,Test_Features)

r


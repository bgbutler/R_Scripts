
# autoencoder on cancer data

############# Cancer with autoencoder



library(caret)
library(ggplot2)
library(dplyr)
library(keras)


setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearning")
cancer = read.csv("cancer_tumor.csv")
head(cancer)

#drop columns
drops =c("id","X")
df=cancer[ , !(names(cancer) %in% drops)]

head(df)

summary(df)

ncol(df)

index<-createDataPartition(df$diagnosis,p=0.7,list=F)
### 70% training and 30% testing 

Train_Features <- data.matrix(df[index,-1]) ## remove the diagnosis
##numerical Xs need to be a matrix 
Train_Labels <- df[index,1] ##y

Test_Features <- data.matrix(df[-index,-1]) #30%
Test_Labels <- df[-index,1]

### rescale 0-1
as.matrix(apply(Train_Features, 2, function(x) (x-min(x))/(max(x) - min(x)))) -> Train_Features

as.matrix(apply(Test_Features, 2, function(x) (x-min(x))/(max(x) - min(x)))) -> Test_Features


input_dim <- 30 ##30 xs
inner_layer_dim <- 10

# Create the autoencoder
input_layer <- layer_input(shape=c(input_dim)) ##predictors
encoder <- layer_dense(units=inner_layer_dim, activation='relu')(input_layer)
decoder <- layer_dense(units=30)(encoder)

autoencoder <- keras_model(inputs=input_layer, outputs = decoder)

autoencoder %>% compile(optimizer='adam', 
                        loss='mean_squared_error', 
                        metrics=c('accuracy'))

history <- autoencoder %>% fit(
    Train_Features,Train_Features, 
    epochs = 50, batch_size = 256, 
    validation_split=0.2
)
plot(history)

# Reconstruct on the test set
preds <- autoencoder %>% predict(Test_Features)

### error bw the actual and predicted xs
error <- rowSums((preds-Test_Features)**2)
eval <- data.frame(error=error, class=as.factor(Test_Labels))
head(eval)


eval %>% 
    group_by(class) %>% 
    summarise(avg_error=mean(error)) %>% 
    ggplot(aes(x=class,fill=class,y=avg_error))+geom_boxplot()


threshold <- 1.1 ## of deciding what is an error or not
y_preds <- sapply(error, function(x){ifelse(x>threshold,"M","B")})

# Confusion matrix
table(y_preds,Test_Labels)


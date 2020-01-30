############ Detect outliers with encoders

library(keras)


mnist = dataset_mnist()


## obtain the training and testing datasets
c(X_train, y_train) %<-% mnist$train #60k images
c(X_test, y_test) %<-% mnist$test

## Exclude "8" from the training set. "8" will be the outlier
outlier_idxs <- which(y_train!=8, arr.ind = T)
X_train <- X_train[outlier_idxs,,]
y_test <- sapply(y_test, function(x){ ifelse(x==8,"outlier","normal")})

##we convert the 3-d arrays into matrices 
# by reshaping width and height into a single dimension 
# (28x28 images are flattened into length 784 vectors). 

# reshape
dim(X_train) <- c(nrow(X_train), 784)
dim(X_test) <- c(nrow(X_test), 784)

# rescale
X_train <- X_train / 255
X_test <- X_test / 255

input_dim <- 28*28 #784
inner_layer_dim <- 32

# Create the autoencoder
input_layer <- layer_input(shape=c(input_dim))
encoder <- layer_dense(units=inner_layer_dim, activation='relu')(input_layer)
decoder <- layer_dense(units=784)(encoder)
autoencoder <- keras_model(inputs=input_layer, outputs = decoder)

autoencoder %>% compile(optimizer='adam', 
                        loss='mean_squared_error', 
                        metrics=c('accuracy'))

history <- autoencoder %>% fit(
  X_train,X_train, 
  epochs = 50, batch_size = 256, 
  validation_split=0.2
)
plot(history)

# Reconstruct on the test set
preds <- autoencoder %>% predict(X_test)
error <- rowSums((preds-X_test)**2)

eval <- data.frame(error=error, class=as.factor(y_test))



library(ggplot2)
library(dplyr)
eval %>% 
  group_by(class) %>% 
  summarise(avg_error=mean(error)) %>% 
  ggplot(aes(x=class,fill=class,y=avg_error))+geom_boxplot()


threshold <- 15 ## of deciding what is an error or not
y_preds <- sapply(error, function(x){ifelse(x>threshold,"outlier","normal")})

# Confusion matrix
table(y_preds,y_test)

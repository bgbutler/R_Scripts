
# auto encoder keras

####### autoencoder with keras

library(keras)

mnist = dataset_mnist()
##s28 * 28 greyscale images

head(mnist)
## training data
X_train = mnist$train$x # predictors
y_train = mnist$train$y #label

## testing data
X_test = mnist$test$x
y_test = mnist$test$y

image(X_train[4,,],col=gray.colors(3))
y_train[4]

### Predictors are a 3D array of the greyscale values
### Convert arrays to a single vector
### 28* 28 is 784; this will be the vector size

dim(X_train) = c(nrow(X_train),784)
dim(X_test) = c(nrow(X_test),784)

### rescale

X_train = X_train/255
X_test = X_test/255

### Define an encoding layer with 32 neurons
### achieve compression ratio 784/32=24.5

input_dim = 28*28

input_layer_dim = 32

input_layer = layer_input(shape=c(input_dim))

encoder = layer_dense(units = 32, activation = 'relu')(input_layer)

decoder = layer_dense(units = 784)(encoder)

auto = keras_model(inputs=input_layer,outputs = decoder)

auto %>% compile(optimizer = "adam",
                 loss = "mean_squared_error",
                 metrics = c('accuracy'))

hist = auto %>% fit(X_train,X_train,
                    epochs = 50,
                    batch_size = 256,
                    validation_split = 0.2)

preds = auto %>% predict(X_test)

error = rowSums((preds-X_test)**2) ## reconstruction error
error


#### Reshape 
# recast as greyscale
dim(X_test) = c(nrow(X_test),28,28)

dim(preds) = c(nrow(preds),28,28)


y_test[2] ## actual digit
image(255*X_test[2,,],col = gray.colors(3))

### predicted image
image(255*preds[2,,],col = gray.colors(3))
library(keras)
library(ggplot2)
# for %>% operator
library(tidyverse)

data = dataset_mnist()

head(data)


# separating train and test file
# pixel values
# labels
train_x<-data$train$x 
train_y<-data$train$y 

test_x<-data$test$x
test_y<-data$test$y

######## visualize data
image_1 <- as.data.frame(train_x[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

# the digit 5
ggplot(image_1, aes(x = x, y = y, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "black", na.value = NA) +
    scale_y_reverse() +
    theme_minimal() +
    theme(panel.grid = element_blank())   +
    theme(aspect.ratio = 1) +
    xlab("") +
    ylab("")


# converting a 2D array into a 1D array for feeding 
# into the MLP and normalising the matrix
# normalize by dividing by 255 for greyscale
train_x <- array(train_x, dim = c(dim(train_x)[1], prod(dim(train_x)[-1]))) / 255
test_x <- array(test_x, dim = c(dim(test_x)[1], prod(dim(test_x)[-1]))) / 255

#c onverting the target/response variable to once hot encoded vectors 
# using keras inbuilt function
train_y<-to_categorical(train_y,10)
test_y<-to_categorical(test_y,10)

# each label is a number/digit
head(train_y)

#defining a keras sequential model
model <- keras_model_sequential()

# defining the model with 1 input layer[784 neurons], 1 hidden layer[784 neurons] with dropout rate 0.4 and 1 output layer[10 neurons]
# 28 x 28 = 784
# i.e number of digits from 0 to 9

model %>% 
    layer_dense(units = 784, input_shape = 784) %>% 
    layer_dropout(rate=0.4)%>%
    layer_activation(activation = 'relu') %>% 
    # output digits 0 to 9 
    layer_dense(units = 10) %>% 
    layer_activation(activation = 'softmax')

#compiling the defined model with metric = accuracy and optimiser as adam.
model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = 'adam',
    metrics = c('accuracy')
)


#fitting the model on the training dataset
model %>% fit(train_x, train_y, epochs = 100, batch_size = 128)

#Evaluating model on the cross validation dataset
loss_and_metrics <- model %>% evaluate(test_x, test_y, batch_size = 128)

loss_and_metrics



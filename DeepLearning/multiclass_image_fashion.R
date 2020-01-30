######################################################################


library(nnet)
library(caret)

set.seed(1234)

# loading data
setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearningKeras/Data and Code/section 4")
f.data = read.csv("dataset.csv")

# dataset is 28 x 28 pixels (784 columns) plus a label column

##1)T-shirt/top (2) Trouser (3) Pullover (4)Dress (5)Coat
## 6)Sandal (7)Shirt (8) Sneaker (9) Bag (10) Ankle boot

head(f.data)

##some items
##28 by 28 pixels
sample_4 =matrix(as.numeric(f.data[4,-1]), nrow = 28, byrow = TRUE)
image(sample_4, col = grey.colors(255))

sample_1 =matrix(as.numeric(f.data[1,-1]), nrow = 28, byrow = TRUE)
image(sample_1, col = grey.colors(255)) #t-shirt

f.data$label = as.factor(f.data$label)

class(f.data$label)

# 6000 images of each
summary(f.data$label)

### training & testing data

# 75% split
train_index = createDataPartition(f.data$label, p=0.75, list=FALSE)

train =f.data[train_index,]
test = f.data[-train_index,]

### tacckle the training data
# drop the label column . retain predictors
f.X = train[, -1]

#label column 
f.y =train[, 1]

# training with caret
f.m1 = train(x = f.X, y = f.y,
             method = "nnet",
             tuneGrid = expand.grid(
                 # 5 neuron hidden layer
                 .size = c(5),
                 .decay = 0.1),
             trControl = trainControl(method = "none"),
             MaxNWts = 10000,
             maxit = 100) 

prediction_nn = predict(f.m1, test, type = "raw")

head(prediction_nn)

# confusion matrix
cm_nn = table(test$label, prediction_nn) 

## of test labels vs predicted labels
cm_nn

# call the confusion matrix package
confusionMatrix(test$label, prediction_nn)


############################################################
#### USE H2O


library(nnet)
library(caret)

set.seed(1234)

# loading data
f.data = read.csv("dataset.csv")

head(f.data)

f.data$label = as.factor(f.data$label)#response

class(f.data$label)

summary(f.data$label)

str(f.data)

library(h2o)
# paramters for H2O
# memory of 2GB, 2 threads, run local
h2o.init(max_mem_size = "2G", 
         nthreads = 2, 
         ip = "localhost", 
         port = 54321)

# make a hex file
# hex file is h2o compatible
d.hex = as.h2o(f.data, destination_frame= "d.hex")


head(d.hex)

g.split = h2o.splitFrame(data = d.hex,ratios = 0.75)

#75% training data
train = g.split[[1]]
test = g.split[[2]]


# ann specify layeers with c(layer1, layer 2)
fmnist_nn = h2o.deeplearning(x = 2:785,
                             y = "label", 
                             training_frame = train,
                             distribution = "multinomial",
                             model_id = "fmnist_nn",
                             l2 = 0.4,
                             ignore_const_cols = FALSE,
                             hidden = c(5,15), 
                             export_weights_and_biases = TRUE)

# confusion matrix of labels
h2o.confusionMatrix(fmnist_nn, test)

# accuracy
# 1 - (Error/Rate)




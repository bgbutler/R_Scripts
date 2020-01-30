setwd("/Users/bryanbutler/Documents/RProjects/PastWork")

set.seed(1234)

# loading data
f.data = read.csv("dataset.csv")

##1)T-shirt/top (2) Trouser (3) Pullover (4)Dress (5)Coat
## 6)Sandal (7)Shirt (8) Sneaker (9) Bag (10) Ankle boot

head(f.data)

f.data$label = as.factor(f.data$label)#response

class(f.data$label)

summary(f.data$label)

library(h2o)
h2o.init(max_mem_size = "2G", 
         nthreads = 2, 
         ip = "localhost", 
         port = 54321)


d.hex = as.h2o(f.data, destination_frame= "d.hex")
##hex file is h2o compatible

head(d.hex)

g.split = h2o.splitFrame(data = d.hex,ratios = 0.75)
train = g.split[[1]]#75% training data
test = g.split[[2]]


### DNN with 3 hidden layers (1000 neurons each)
### activation function: Rectifier with droput (improve generalization.)
### epochs:The number of iterations to be carried out. More epochs=more accuracy

### Adaptive learning rates self adjust to avoid local minima or slow convergence.
###  learning rate annealing gradually reduces learning
### Momentum modifies back-propagation by allowing prior iterations to influence
##the current update. 

fmnist_nn = h2o.deeplearning(x = 2:785,
                             y = "label",
                             training_frame = train,
                             distribution = "multinomial",
                             model_id = "fmnist_nn",
                             activation = "RectifierWithDropout",
                             hidden=c(1000, 1000, 2000),
                             epochs = 180,
                             adaptive_rate = FALSE,
                             rate=0.01,
                             rate_annealing = 1.0e-6,
                             rate_decay = 1.0,
                             momentum_start = 0.4,
                             momentum_ramp = 384000,
                             momentum_stable = 0.98, 
                             input_dropout_ratio = 0.22,
                             l1 = 1.0e-5,
                             max_w2 = 15.0, 
                             initial_weight_distribution = "Normal",
                             initial_weight_scale = 0.01,
                             nesterov_accelerated_gradient = TRUE,
                             loss = "CrossEntropy",
                             fast_mode = TRUE,
                             diagnostics = TRUE,
                             ignore_const_cols = TRUE,
                             force_load_balance = TRUE,
                             seed = 3.656455e+18)

h2o.confusionMatrix(fmnist_nn, test)
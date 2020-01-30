

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
h2o.init(max_mem_size = "8G", 
         nthreads = 2, 
         ip = "localhost", 
         port = 54321)


d.hex = as.h2o(f.data, destination_frame= "d.hex")
##hex file is h2o compatible

head(d.hex)

g.split = h2o.splitFrame(data = d.hex,ratios = 0.75)
train = g.split[[1]]#75% training data
test = g.split[[2]]




## DNN with 2 hidden layers
## default- 2 hidden layers (200 neurons each)
m = h2o.deeplearning(x = 2:785,
                     y = "label",
                     training_frame = train,
                     distribution = "multinomial",
                     model_id = "m",
                     activation = "Tanh",
                     l2 = 0.00001,
                     #hidden = c(162,162),
                     loss = "CrossEntropy")





h2o.confusionMatrix(m, test)
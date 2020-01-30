

# h20 unsupervised


library(tidyverse)
library(ggplot2)
library(h2o)



setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearning")

creditcard = read.csv("creditcard.csv")

head(creditcard) #v1-v28 are numerical perdictors
## Class is a response variable (0: no fraud ; 1: fraud)



# initialize h2o
h2o.init()

# convert to hex
# hex file is h2o compatible
d.hex = as.h2o(creditcard, destination_frame= "d.hex")


head(d.hex)
str(d.hex)
summary(d.hex)
ncol(d.hex)

g.split = h2o.splitFrame(data = d.hex,ratios = 0.75)
train = g.split[[1]]#75% training data
test = g.split[[2]]



##3 hidden layers
model_unsup = h2o.deeplearning(x = 1:30,
                               training_frame = train,
                               model_id = "model_unsup",
                               autoencoder = TRUE,
                               reproducible = TRUE, #slow - turn off for real problems
                               ignore_const_cols = FALSE,
                               seed = 42,
                               hidden = c(10, 10, 10), 
                               epochs = 100,
                               activation = "Tanh")


preds = h2o.predict(model_unsup, test)
head(preds)

# h2o anomaly function for detection
model.anon = h2o.anomaly(model_unsup, d.hex, per_feature=TRUE)
## cases of fraud

anomaly = h2o.anomaly(model_unsup, test) %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    mutate(Class = as.vector(test[, 31]))

anomaly$Class = as.factor(anomaly$Class)

mean_mse = anomaly %>%
    group_by(Class) %>%
    summarise(mean = mean(Reconstruction.MSE))
## model error

mean_mse

library(ggplot2)
ggplot(anomaly, aes(x = as.numeric(rowname), y = Reconstruction.MSE, color = as.factor(Class))) +
    geom_point(alpha = 0.3) +
    geom_hline(data = mean_mse, aes(yintercept = mean, color = Class)) +
    scale_color_brewer(palette = "Set1") +
    labs(x = "instance number",
         color = "Class")



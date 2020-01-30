library(AmesHousing)
library(caret)

head(ames_raw)

ames <- make_ames() 


##Create a Processed Version of the Ames Housing Data

head(ames)

summary(ames)

## one hot encode --> we use model.matrix(...)[, -1] 
## to discard the intercept
encd=model.matrix(~ ., AmesHousing::make_ames())[, -1] %>% as.data.frame()

head(encd)

# split into train/testing as 70/30%
index<-createDataPartition(encd$Sale_Price,p=0.7,list=F)

Train_Features <- data.matrix(encd[index,-81]) 


##numerical Xs need to be a matrix 
Train_Labels <- encd[index,81] ##y

Test_Features <- data.matrix(encd[-index,-81]) #30%
Test_Labels <- encd[-index,81]

########### normalize

# Normalize training data
Train_Features = scale(Train_Features) ## training xs

# Use means and standard deviations 
#from training set to normalize test set
col_means_train = attr(Train_Features, "scaled:center") 
col_stddevs_train = attr(Train_Features, "scaled:scale")
Test_Features = scale(Test_Features, center = col_means_train, scale = col_stddevs_train)


# zero variance variables (after one hot encoded) cause NaN so we need to remove
zv1 <- which(colSums(is.na(Train_Features)) > 0, useNames = FALSE)
Train_Features <- Train_Features[, -zv1]

zv2 <- which(colSums(is.na(Test_Features)) > 0, useNames = FALSE)
Test_Features  <- Test_Features[, -zv1]

model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", input_shape = ncol(Train_Features)) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)

# backpropagation
model %>%compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
)

learn <- model %>% fit(
    x = Train_Features,
    y = Train_Labels,
    epochs = 25,
    batch_size = 32,
    validation_split = .2,
    verbose = FALSE
)


learn

plot(learn)

model %>% evaluate(Test_Features, Test_Labels)

test_predictions = model %>% predict(Test_Features) #test xs
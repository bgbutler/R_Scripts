############################## CNN on Lungs


# improved CNN with dropout of 0.6

library(keras)

train_dir = '~/Desktop/python_datamine/deeplearn_R/Deep Learn_R/chest_xray/train/'
validation_dir = '~/Desktop/python_datamine/deeplearn_R/Deep Learn_R/chest_xray/val/'
test_dir = '~/Desktop/python_datamine/deeplearn_R/Deep Learn_R/chest_xray/test/'


#The train generator uses augmentation: image_data_generator,
#augment images while training, 
#creating more diversity in the training data

train_datagen = image_data_generator(
    rescale = 1/255, 
    rotation_range = 50, ##0-180; rotate image
    width_shift_range = 0.2, ##randomly translate images H or V
    height_shift_range = 0.2,
    shear_range = 0.15, #shearing transformations (0.1-0.5)
    zoom_range = 0.15,
    horizontal_flip = TRUE,
    vertical_flip = FALSE,
    fill_mode = "reflect"
)

## rescale 
validation_datagen <- image_data_generator(rescale = 1/255)  
test_datagen <- image_data_generator(rescale = 1/255)  

#training_batch_size = 32
#validation_batch_size = 32

train_generator <- flow_images_from_directory(
    train_dir,                            # Target directory  
    train_datagen,                        # Data generator
    classes = c('NORMAL', 'PNEUMONIA'),
    target_size = c(150, 150),
    batch_size = 20,
    # Since we use binary_crossentropy loss, we need binary labels
    class_mode = "binary",
    seed = 123
)

validation_generator <- flow_images_from_directory(
    validation_dir,
    classes = c('NORMAL', 'PNEUMONIA'),
    validation_datagen,
    target_size = c(150, 150),
    batch_size = 20,
    # Since we use binary_crossentropy loss, we need binary labels
    class_mode = "binary",
    
    seed = 123
)

test_generator <- flow_images_from_directory(
    test_dir,
    classes = c('NORMAL', 'PNEUMONIA'),
    test_datagen,
    target_size = c(150, 150),
    batch_size = 20,
    # Since we use binary_crossentropy loss, we need binary labels
    class_mode = "binary",
    seed = 123
)

batch <- generator_next(train_generator)
str(batch)

## 4 hidden layers
model <- keras_model_sequential() %>% 
    layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                  input_shape = c(150, 150, 3)) %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    
    layer_flatten() %>% 
    layer_dropout(rate = 0.6) %>%
    layer_dense(units = 512, activation = "relu") %>% 
    layer_dense(units = 1, activation = "sigmoid")

##Dropout is a technique where randomly 
##selected neurons are ignored during training. 
##They are “dropped-out” randomly. 
##  http://jmlr.org/papers/v15/srivastava14a.html

summary(model)


model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_rmsprop(lr = 1e-4),
    metrics = c("acc")
)

history <- model %>% fit_generator(
    train_generator,
    steps_per_epoch = 100,
    epochs = 6,
    validation_data = validation_generator,
    validation_steps = 50)


model %>% evaluate_generator(test_generator,steps=50)


model %>% save_model_hdf5("mod1.h5")

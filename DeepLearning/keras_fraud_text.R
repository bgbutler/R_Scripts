############## Detect fraud using Keras

####### Word Embedding (captures context of a word in a document)

####### represent words and documents using a dense vector representation
####### improvement over more the traditional bag-of-word model 
###### position of a word within the vector space (embedding) is learned from 
### text and is based on the words that surround the word when it is used

#### Deep learning needed !! 

library(tm) ##process textual data
library(SnowballC)
library(keras)
library(caret)
library(tidyverse)


setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearning")

email = read.csv("enron_emails.csv")

head(email)

txtdata <- email$email

# l are the labels
l=as.numeric(email$responsive)

# We will cut emails after 100 words
maxlen <- 100 

# We will be training on 500 samples
training_samples <- 500 

# We will be validating on 10000 samples
validation_samples <- 10000

# set dictionary size
# We will only consider the top 10,000 words in the dataset
max_words <- 10000   
###1) Text data needs to be integer encoded, 
### each word is represented by a unique integer. 

## Keras provides a Tokenizer class that can be fit on the text data
## convert text to sequences  by calling the texts_to_sequences() method on the Tokenizer class, 
tokenizer <- text_tokenizer(num_words = max_words) %>% 
    fit_text_tokenizer(txtdata) ##tokens

# generate sequences
sequences <- texts_to_sequences(tokenizer, txtdata)

word_index = tokenizer$word_index
##provides access to the dictionary mapping of words to integers in a word_index
cat("Found", length(word_index), "unique tokens.\n")

df <- pad_sequences(sequences, maxlen = maxlen)
labels <- as.array(l)
cat("Shape of data tensor:", dim(df), "\n")

### 2) Split the data etc
indices <- sample(1:nrow(df))
training_indices <- indices[1:training_samples]
validation_indices <- indices[(training_samples + 1): 
                                  (training_samples + validation_samples)]

### training data
x_train <- df[training_indices,]
y_train <- labels[training_indices]

### testing data
x_val <- df[validation_indices,]
y_val <- labels[validation_indices]

### 3) The Deep learning model
## Keras offers an Embedding layer for neural networks on text data.
## input_dim: size of the vocabulary in the text data
## output_dim: This is the size of the vector space in which words will be embedded
# test different values of neighboring words
## input_length:  the length of input sequences,


## network will learn 8-dimensional embeddings for each of the 
## 10,000 words, turn the input integer sequences (2D integer tensor) 
## into embedded sequences (3D float tensor), 
## flatten the tensor to 2D, 
## and train a single dense layer on top for classification.

model <- keras_model_sequential() %>% 
    # We specify the maximum input length to our Embedding layer
    # so we can later flatten the embedded inputs
    layer_embedding(input_dim = 10000, output_dim = 8, 
                    input_length = maxlen) %>% 
    
    # We flatten the 3D tensor of embeddings 
    # into a 2D tensor of shape `(samples, maxlen * 8)`
    layer_flatten() %>% 
    # We add the classifier on top
    layer_dense(units = 1, activation = "sigmoid") 

model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("acc")
)


summary(model)


history <- model %>% fit(
    x_train, y_train,
    epochs = 10,
    batch_size = 32,
    validation_split = 0.2
)

plot(history)








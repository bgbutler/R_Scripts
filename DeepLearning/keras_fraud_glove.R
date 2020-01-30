#####################################################################
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

l=as.numeric(email$responsive)

maxlen <- 100                 # We will cut emails after 100 words
training_samples <- 500       # We will be training on 500 samples
validation_samples <- 10000   # We will be validating on 10000 samples
max_words <- 10000   # We will only consider the top 10,000 words in the dataset

###1) Text data needs to be integer encoded, 
### each word is represented by a unique integer. 

##Keras provides a Tokenizer class that can be fit on the text data
##convert text to sequences  by calling the texts_to_sequences() method on the Tokenizer class, 
tokenizer <- text_tokenizer(num_words = max_words) %>% 
    fit_text_tokenizer(txtdata) ##tokens

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


#################### Use GloVe (word-embedding model)

#### Global Vectors, is a model for distributed word representation. 
#### The model is an unsupervised learning algorithm for obtaining 
#### vector representations for words.
##https://github.com/stanfordnlp/GloVe

glove_dir = '/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearning/glove'
lines <- readLines(file.path(glove_dir, "glove.6B.100d.txt"))


# get the embedding index for word context
# set protection
options(expressions = 5e5)

embeddings_index <- new.env(hash = TRUE, parent = emptyenv())
for (i in 1:length(lines)) {
    line <- lines[[i]]
    values <- strsplit(line, " ")[[1]]
    word <- values[[1]]
    embeddings_index[[word]] <- as.double(values[-1])
}

# the the umber of words surrounding to 100
embedding_dim <- 100
embedding_matrix <- array(0, c(max_words, embedding_dim))
for (word in names(word_index)) {
    index <- word_index[[word]]
    if (index < max_words) {
        embedding_vector <- embeddings_index[[word]]
        if (!is.null(embedding_vector))
            # Words not found in the embedding index will be all zeros.
            embedding_matrix[index+1,] <- embedding_vector
    }
}

### 3) The Deep learning model
##Keras offers an Embedding layer for neural networks on text data.
## input_dim: size of the vocabulary in the text data
## output_dim: This is the size of the vector space in which words will be embedded
#test different values !!
##input_length:  the length of input sequences,


model <- keras_model_sequential() %>% 
    layer_embedding(input_dim = max_words, output_dim = embedding_dim, 
                    input_length = maxlen) %>% 
    layer_flatten() %>% 
    layer_dense(units = 32, activation = "relu") %>% 
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









####### RNN

library(keras)

#### Deep learning needed !! 

setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearningKeras/Data and Code/section 9")


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

##ensure that all sequences in a list have the same length
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

model <- keras_model_sequential() %>%
    layer_embedding(input_dim = max_words, output_dim = 32) %>%
    ##Fully-connected RNN where the output is to be fed back to input.
    layer_simple_rnn(units = 32) %>%
    layer_dense(units = 1, activation = "sigmoid")


model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("acc")
)


history <- model %>% fit(
    x_train, y_train,
    epochs = 10,
    batch_size = 128,
    validation_split = 0.2
)




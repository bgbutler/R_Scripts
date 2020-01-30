 # text processing

setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearning")

email = read.csv("enron_emails.csv")

head(email)

library(tm) ##process textual data
library(SnowballC)
library(keras)
library(caret)
library(tidyverse)

### Step 1: turn the text column into a corpus
##Corpora are collections of documents containing (natural language) text
##create a collection of documents/text
corpus <- VCorpus(VectorSource(email$email))
inspect(corpus[[1]])

### Step 2: Clean the text data
## convert all words to lower case, remove punctuations
## most common Eng language words (stopwords) and

corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <-  tm_map(corpus, removePunctuation)
corpus <-  tm_map(corpus, removeWords, stopwords("english"))
corpus <-  tm_map(corpus, stemDocument)
##Stem words in a text document: get to the root word


### Step 3: Create a document term matrix
##a data.frame with 1 row per document/term as returned by document term frequencies
dtm <-  DocumentTermMatrix(corpus)
dtm <-  removeSparseTerms(dtm, 0.97)
## retain the most common terms

## Keras Autoencoder



##### Prepare the data
X <- as.data.frame(as.matrix(dtm))
X$responsive <- email$responsive ## response variable

## 70% training
index<-createDataPartition(X$responsive,p=0.7,list=F)

## remove the response variable
Train_Features <- as.matrix(X[index,-1]) 

## numerical Xs need to be a matrix 
##y
Train_Labels <- X[index,1] 

Test_Features <- as.matrix(X[-index,-1]) #30%
Test_Labels <- X[-index,1]

####### Autoencoder

## training xs
input_dim <- ncol(Train_Features) 

# 32 is a common starting point
inner_layer_dim <- 32
input_layer <- layer_input(shape=c(input_dim))

encoder <- layer_dense(units=inner_layer_dim, activation='relu')(input_layer)
decoder <- layer_dense(units=input_dim)(encoder)

autoencoder <- keras_model(inputs=input_layer, outputs = decoder)

autoencoder %>% compile(optimizer='adam', 
                        loss='mean_squared_error', 
                        metrics=c('accuracy'))

## Xs only Predictors only - Labels
history <- autoencoder %>% fit(
    Train_Features,Train_Features, 
    epochs = 100, batch_size = 32, 
    validation_data = list(Test_Features, Test_Features)
)

plot(history)

# Reconstruct on the test set
preds <- autoencoder %>% predict(Test_Features)



### predicted 30% xs value
# rmse
error <- rowSums((preds-Test_Features)**2)

eval <- data.frame(error=error, class=as.factor(Test_Labels))
head(eval)


# flag the outliers Class 2 and Class 5?
eval %>% 
    group_by(class) %>% 
    summarise(avg_error=mean(error)) %>% 
    ggplot(aes(x=class,fill=class,y=avg_error))+geom_boxplot()


threshold <- 30000 ## of deciding what is an error or not
y_preds <- sapply(error, function(x){ifelse(x>threshold,"outlier","normal")})

# Confusion matrix
table(y_preds,Test_Labels)








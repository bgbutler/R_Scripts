


library(ggplot2)

set.seed(1234)

# loading data

setwd("/Users/bryanbutler/Documents/RProjects/PastWork/DeepLearning")

tumor=read.csv("cancer_tumor.csv") #predict if the tumor is
#malignant (M) or benign (B)

head(tumor)

#drop columns
drops =c("id","X")
df=tumor[ , !(names(tumor) %in% drops)]

head(df)

table(df$diagnosis)

prop.table(table(df$diagnosis))

library(h2o)
h2o.init(max_mem_size = "4G", 
         nthreads = 2, 
         ip = "localhost", 
         port = 54321)


d.hex = as.h2o(df, destination_frame= "d.hex")
##hex file is h2o compatible

head(d.hex)
str(d.hex)


NN_model = h2o.deeplearning(
    x = 2:31,
    training_frame = d.hex,
    hidden = c(400, 200, 2, 200, 400 ),
    epochs = 600,
    activation = "Tanh",
    autoencoder = TRUE
)

train_supervised_features2 = h2o.deepfeatures(NN_model, d.hex, layer=3)

plotdata2 = as.data.frame(train_supervised_features2)
plotdata2$label = as.character(as.vector(d.hex[,1]))

qplot(DF.L3.C1, DF.L3.C2, data = plotdata2, color = label,
      main = "Neural network: 400 - 200 - 2 - 200 - 4000 ")

#basic artificial neural network to approximate a quadratic distribution
install.packages("neuralnet")
library("neuralnet")

set.seed(2016) 
attribute <- as.data.frame( sample(seq( -2 ,2, length =50), 50, replace = FALSE), ncol =1) 
response <-attribute ^2
data <- cbind( attribute, response) 
colnames(data) <- c( "attribute","response")


plot(data)

fit <- neuralnet(response ~ attribute, data=data, hidden = c(3 ,3), threshold =0.01)

?neuralnet

summary(fit)

testdata <- as.matrix( sample(seq( -2 ,2, length =10) , 10, replace = FALSE) , ncol =1)
pred <- compute (fit, testdata)

attributes(pred)
summary(pred)

result <- cbind(testdata, pred$net.result, testdata^2) 
colnames(result) <- c( "Attribute" ,"Prediction", "Actual") 
round( result ,4)

plot(result)


#Example of deep neural network regression 
install.packages("Metrics")
require("Metrics")

data("Boston" , package = "MASS") 
data <-Boston

keeps <- c( "crim", "indus", "nox", "rm" ,"age", "dis", "tax","ptratio", "lstat" , "medv") 
data <- data[ keeps] 
data <- scale(data)

apply(data, 2, function(x) sum(is.na(x)))

pairs(data)

f<-medv~ crim + indus + nox + rm + age + dis + tax + ptratio + lstat

set.seed(2016)
n=nrow(data)
train <- sample(1:n, 400, FALSE)

fit <- neuralnet (f, data = data[ train,], hidden =c(10 ,12 ,20), algorithm = "rprop+" , err.fct = "sse" , act.fct = "logistic" , threshold =0.1, linear.output = TRUE)

pred <- compute(fit, data[ - train ,1:9])

round(cor(pred$net.result, data[ - train ,10])^2 ,6)

mse(data [-train ,10], pred$net.result) 
rmse(data [-train ,10], pred$net.result) 

#beginning of deep learning / deep neural network models
install.packages("deepnet")
require(deepnet)

set.seed(2016)
X = data[train, 1:9]
Y = data[train,10]

colnames(data)

fitB <-nn.train (x =X, y =Y, initW = NULL, initB = NULL, hidden = c(10 ,12 ,20), learningrate = 0.58, momentum =0.74, learningrate_scale =1, activationfun = "sigm" , output = "linear" , numepochs = 970, batchsize = 60, hidden_dropout = 0, visible_dropout = 0)

Xtest <- data[ -train ,1:9] 
predB <- nn.predict(fitB, Xtest)

round(cor(predB ,data[ -train ,10])^2 ,6)

#medical data DNN for classification
install.packages("mlbench")
library(mlbench)

data("PimaIndiansDiabetes2", package = "mlbench")
ncol(PimaIndiansDiabetes2)
nrow(PimaIndiansDiabetes2)
str(PimaIndiansDiabetes2)

sapply(PimaIndiansDiabetes2, function(x) sum(is.na(x)))

temp <- (PimaIndiansDiabetes2)
temp$insulin <- NULL
temp$triceps <- NULL
temp <- na.omit(temp)

nrow(temp)
ncol(temp)

y <- (temp$diabetes)
levels(y) <- c("0","1")
y <- as.numeric(as.character(y))

temp$diabetes <- NULL
temp <- cbind(temp, y)
temp <- scale(temp)

summary(temp)

set.seed(2016)
n = nrow(temp)
n_train <- 600
n_test <- -n-n_train
train <- sample(1:n, n_train, FALSE)

#Another package to do classification with RSNNS
install.packages("RSNNS")
library("RSNNS")

set.seed(2016)
X <- temp[train, 1:6]
Y <- temp[train, 7]


fitMLP <- mlp (x =X, y =Y, size = c(12 ,8), maxit = 1000, initFunc = "Randomize_Weights" , initFuncParams = c( -0.3, 0.3), learnFunc = "Std_Backpropagation" , learnFuncParams = c(0.2, 0), updateFunc = "Topological_Order" , updateFuncParams = c(0), hiddenActFunc = "Act_Logistic" , shufflePatterns = TRUE, linOut = TRUE)
predMLP <- sign(predict(fitMLP, temp [- train ,1:6]))
table(predMLP ,sign( temp [- train ,7]), dnn =c("Predicted", "Observed"))
error_rate = (1 - sum( predMLP == sign( temp [- train ,7]))/ 124) 
round( error_rate ,3)

#Classification using the AMORE package
install.packages("AMORE")
library("AMORE")
net <- newff(n.neurons =c(6 ,12 ,8 ,1), learning.rate.global =0.01, momentum.global =0.5, error.criterium = "LMLS" , Stao = NA, hidden.layer = "sigmoid" , output.layer = "purelin" , method = "ADAPTgdwm")

X<-temp[train,]
Y<-temp[train ,7]

fit <- train(net, P =X , T =Y , error.criterium = "LMLS" , report= TRUE, show.step =100, n.shows =5)

pred <- sign(sim(fit$net, temp[-train,]))

table(pred ,sign( temp[- train ,7]), dnn =c( "Predicted" , "Observed"))

error_rate = (1 - sum( pred == sign( temp [- train ,7]))/ 124) 
round( error_rate ,3)


#Deep neural network regression with the neuralnetwork package and bodyfat data
install.packages("TH.data")
library("TH.data")

data("bodyfat", package="TH.data")

str(bodyfat)


library("neuralnet")
require(Metrics)

set.seed(2016)
train <- sample(1:71,50, FALSE)

scale_bodyfat <-as.data.frame(scale(log( bodyfat))) 
f<- waistcirc + hipcirc ~ DEXfat + age + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4

fit <- neuralnet (f, data = scale_bodyfat[train,], hidden =c(8 ,4), threshold =0.1, err.fct = "sse" , algorithm = "rprop+" , act.fct = "logistic" , linear.output = FALSE )

without_fat <- scale_bodyfat 
without_fat$waistcirc <-NULL 
without_fat$ hipcirc <-NULL

pred <- compute(fit, without_fat [- train,]) 
pred$net.result


fw <- waistcirc ~ DEXfat + age + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4 
fh <- hipcirc ~ DEXfat + age + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4


regw <-linReg <-lm( fw ,data = scale_bodyfat [ train,]) 
regh <-linReg <-lm( fh ,data = scale_bodyfat [ train,])


predw <- predict (regw, without_fat [- train,]) 
predh <- predict (regh, without_fat [- train,])


mse(scale_bodyfat [- train ,10], pred$net.result [,1])

mse(scale_bodyfat [- train ,10], predw)

mse(scale_bodyfat [- train ,10], predh)


#more regression on body fat with the deepnet package
require(deepnet) 
set.seed(2016) 
X = as.matrix( without_fat [train,]) 
Y = as.matrix(scale_bodyfat [train ,3:4]) 
fitB <-nn.train(x =X, y =Y, initW = NULL, initB = NULL, hidden = c(8 ,4), activationfun = "sigm" , learningrate = 0.02, momentum =0.74, learningrate_scale =1, output = "linear" , numepochs = 970, batchsize = 60, hidden_dropout = 0, visible_dropout = 0)

Xtest <- as.matrix( without_fat [- train,]) 
predB <- nn.predict(fitB, Xtest)

mse (scale_bodyfat [- train ,10], predB [,1])
mse (scale_bodyfat [- train ,10], predB [,2])



#Elman neural networks training for time series data
library("RSNNS")
install.packages("quantmod")
library("quantmod")

require("datasets")


data("UKLungDeaths", package = "datasets")
par(mfrow=c(3,1))

plot(ldeaths,xlab="year", ylab="Both sexes", main = "Total")

plot(mdeaths,xlab="year",ylab="Males", main = "Males")

plot(fdeaths,xlab="year",ylab="Females", main = "Females")

sum(is.na(ldeaths))

class(ldeaths)

par(mfrow = c(3,1))
plot(ldeaths)

x <- density(ldeaths)

plot(x, main ="UK total deaths from lung diseases")

polygon(x,col="green", border = "black")

boxplot(ldeaths, col="cyan",ylab="Number of deaths per month")

y = as.ts(ldeaths)

y <- log(y)

y <- as.ts(scale(y))

y <- as.zoo(y)

x1 <- Lag(y, k = 1)
x2 <- Lag(y, k = 2)
x3 <- Lag(y, k = 3)
x4 <- Lag(y, k = 4)
x5 <- Lag(y, k = 5)
x6 <- Lag(y, k = 6)
x7 <- Lag(y, k = 7)
x8 <- Lag(y, k =8)
x9 <- Lag(y, k = 9)
x10 <- Lag(y, k = 10)
x11 <- Lag(y, k = 11)
x12 <- Lag(y, k = 12)

deaths <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
deaths <- cbind(y, deaths)
head(round(deaths,2),13)

deaths <- deaths[-(1:12),]

n = nrow(deaths)
set.seed(465)

n_train <- 45
train <- sample(1:n, n_train,FALSE)

inputs <- deaths[,2:13]
outputs <- deaths[,1]

fit <- elman(inputs[train], outputs[train], size = c(1,1), learnFuncParams = c(0.1), maxit = 1000)
par(mfrow=c(1,1))
plotIterativeError(fit)
summary(fit)

pred <- predict(fit, inputs[-train])

cor(outputs[-train],pred)^2

######Jordan neural networks also great for timeseries data######
require(RSNNS)

data("nottem", package = "datasets")
require(quantmod)

nottem
class(nottem)

plot(nottem)

y <- as.ts(nottem)
y <- log(y)
y <- as.ts(scale(y))

y <- as.zoo(y)

x1 <- Lag(y, k = 1)
x2 <- Lag(y, k = 2)
x3 <- Lag(y, k = 3)
x4 <- Lag(y, k = 4)
x5 <- Lag(y, k = 5)
x6 <- Lag(y, k = 6)
x7 <- Lag(y, k = 7)
x8 <- Lag(y, k =8)
x9 <- Lag(y, k = 9)
x10 <- Lag(y, k = 10)
x11 <- Lag(y, k = 11)
x12 <- Lag(y, k = 12)


temp <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
temp <- cbind(y, temp)

#takes out stuff that isn't in lag 1-12
temp <- temp[-(1:12), ]

plot(temp)

n = nrow(temp)
n

set.seed(465)
n_train <- 190

train <- sample(1:n,n_train, FALSE)

inputs <- temp[,2:13]
outputs <- temp[,1]

fit <- jordan(inputs[train], outputs[train], size = 2, learnFuncParams = c(0.01), maxit = 1000)

plotIterativeError(fit)

pred <- predict(fit, inputs[-train])
cor(outputs[-train], pred)^2


#######Autoencoder used for image recognition####
install.packages("autoencoder")
library("autoencoder")

install.packages("ripa")
require("ripa")

data(logo)

x_train <- t(logo)
x_train

set.seed(2016)

fit <- autoencode(X.train = x_train, X.test = NULL, nl = 3, N.hidden = 60, unit.type = "logistic", lambda = 1e-5, beta = 1e-5,
                  rho = .3, epsilon = .1, max.iterations = 100, optim.method = c("BFGS"), rel.tol = 0.01, rescale.flag = TRUE, rescaling.offset = 0.001)


attributes(fit)

fit$mean.error.training.set

#get the features of the hidden notes
features <- predict (fit, X.input = x_train, hidden.output = TRUE)

image(t(features$X.output))

pred <- predict(fit, X.input = x_train, hidden.output = FALSE)
pred$mean.error

recon <- pred$X.output
image(t(recon))



#abalone exercise 
aburl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"

names = c('sex','length','diameter','height','whole.weight','shucked.weight','viscera.weight','shell.weight','rings')

data = read.table(aburl,header = F, sep = ',',col.names = names)

#data[data$height==0,]

data$height[data$height==0] = NA
data <- na.omit(data)
data$sex <- NULL

summary(data)

data1 <- t(data)
#data1 <- as.numeric(data1)
data1 <- as.matrix(data1)

require(autoencoder)
set.seed(2016)

n = nrow(data)
train <- sample(1:n,10,FALSE)

str(data1)

fit <-autoencode(X.train = data1[, train], X.test = NULL, nl = 3, N.hidden = 5, 
                 unit.type = "logistic" , lambda = 1e-5, beta = 1e-5, rho = 0.07, 
                 epsilon =0.1, max.iterations = 100, optim.method = c("BFGS"), 
                 rel.tol =0.01, rescale.flag = TRUE, rescaling.offset = 0.001)

fit$mean.error.training.set

features <- predict(fit, X.input=data1[,train], hidden.output = TRUE)
features$X.output

pred <- predict(fit, X.input=data1[,train], hidden.output = FALSE)

##### BUILD SOME RADAR DIAGRAMS TO VISUALIZE THE OUTPUTS! ######

install.packages("radarchart")
require("radarchart")

chartJSRadar(pred, labs = rownames(pred))
as.data.frame(pred)




#Stacked autoencoder
install.packages("SAENET")
library("SAENET")

aburl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"

names = c('sex','length','diameter','height','whole.weight','shucked.weight','viscera.weight','shell.weight','rings')

data = read.table(aburl,header = F, sep = ',',col.names = names)

#data[data$height==0,]
data$sex <- NULL
data$height[data$height==0] = NA
data <- na.omit(data)

data1 <- as.matrix(data)
set.seed(2016)
n = nrow(data)
train <- sample(1:n,10,FALSE)

fit <-SAENET.train(X.train = data1[train,], n.nodes = c(5 ,4 ,2), unit.type = "logistic" , lambda = 1e-5, beta = 1e-5, rho = 0.07, epsilon =0.1, max.iterations = 100, optim.method = c( "BFGS"), rel.tol =0.01, rescale.flag = TRUE, rescaling.offset = 0.001)

plot(fit[[3]]$X.output)



#Denoising stacked autoencoder
install.packages("RcppDL")
install.packages("ltm")

require("RcppDL")
require("ltm")

data(Mobility)
data <-Mobility

set.seed(17)
n=nrow(data)
sample <- sample(1:n,1000,FALSE)
data <- as.matrix(Mobility[sample,])
n=nrow(data)
train <- sample(1:n,800,FALSE)

x_train <- matrix(as.numeric (unlist(data[ train,])) ,nrow=nrow(data[ train,])) 
x_test <- matrix(as.numeric (unlist(data[ -train,])) ,nrow=nrow(data[ -train,]))

nrow(x_train)
nrow(x_test)

x_train <- x_train[,-3]
x_test <- x_test[,-3]


head(x_train)
head(x_test)


y_train <- data[ train ,3] 
temp <- ifelse( y_train ==0, 1, 0) 
y_train <- cbind( y_train, temp)

head(y_train)

nrow(y_train)

y_test <- data[ - train ,3] 
temp1 <- ifelse( y_test ==0, 1, 0) 
y_test <- cbind( y_test, temp1) 
head(y_test)
nrow(y_test)

hidden = c(10,10)
fit <- Rsda(x_train, y_train, hidden)

setCorruptionLevel(fit, x = 0.0)
summary(fit)

pretrain(fit)
finetune(fit)

predProb <- predict(fit,x_test)
head(predProb,6)

head(y_test, 3)

#Confusion matrix
pred1 <- ifelse( predProb[,1] >=0.5, 1, 0) 
table( pred1, y_test [,1], dnn =c( "Predicted" , "Observed"))


setCorruptionLevel(fit, x = 0.25) 
pretrain(fit) 
finetune(fit) 
predProb <- predict(fit, x_test) 
pred1 <- ifelse( predProb [,1] >=0.5, 1, 0) 
table( pred1, y_test [,1], dnn =c( "Predicted" , "Observed"))



#Restricted boltzman machine
require("RcppDL")
require(ltm)
data(Mobility)
data <- Mobility


set.seed(2395) 
n =nrow(data) 
sample <- sample(1: n, 1000, FALSE) 
data <-as.matrix( Mobility [sample,]) 
n =nrow(data) 
train <- sample (1: n, 800, FALSE)


x_train <- matrix(as.numeric (unlist(data[ train,])) ,nrow=nrow(data[ train,])) 
x_test <- matrix(as.numeric (unlist(data[ - train,])) ,nrow=nrow(data[ - train,])) 
x_train <-x_train [, -c(4 ,6)] 
x_test <-x_test [, -c(4 ,6)] 
head (x_train)
head(x_test)

fit <- Rrbm(x_train)

setHiddenRepresentation(fit, x= 3)
setLearningRate(fit,x = .01)
summary(fit)

train(fit)
reconProb <- reconstruct(fit,x_train)
head(reconProb,6)

recon <- ifelse(reconProb >=.5,1,0)
head(recon)

table(recon,x_train,dnn=c("Predicted","Observed"))
par(mfrow = c(1,2))
image(x_train,main = "Train")
image(recon,main="Reconstruction")

#can also use the deepnet package to create an RBM
fit2 <-rbm.train(x_train, hidden =3, numepochs = 3, batchsize = 100, learningrate = 0.8, learningrate_scale = 1, momentum = 0.5, visible_type = "bin" , hidden_type = "bin" , cd = 1)


# Deep belief network - the code for this is in the book but the dataset referenced wasn't identified. Silly.







#START OF BOOK TWO ON DEEP LEARNING

#Repeat of deep autoencoders 
install.packages("fpc")

require(autoencoder)
require(ripa)

data(logo)

image(logo)

logo

#transpose the image so that it works with the autoencoder package
x_train <-t(logo)
x_train


set.seed(2016) 
fit2l <-autoencode(X.train = x_train, X.test = NULL, nl = 4, N.hidden = c(65 ,60), unit.type = c( "logistic"), lambda = 1e-5, beta = 1e-5, rho = 0.1, epsilon =0.1, max.iterations = 50, optim.method = c( "BFGS"), rel.tol =0.01, rescale.flag = TRUE, rescaling.offset = 0.001)

pred2l <- predict(fit2l, X.input = x_train, hidden.output = FALSE)

pred2l$X.output

image(t(pred2l$X.output))



set.seed(2016) 
fit2 <-autoencode(X.train = x_train, X.test = NULL, nl = 4, N.hidden = c(65 ,60), unit.type = c( "tanh"), lambda = 1e-5, beta = 1e-5, rho = 0.1, epsilon =0.1, max.iterations = 50, optim.method = c( "BFGS"),
                                    rel.tol =0.01, rescale.flag = TRUE, rescaling.offset = 0.001)
pred2 <- predict(fit2, X.input = x_train, hidden.output = FALSE) 
image(t(pred2$X.output))

fit1 <-autoencode (X.train = x_train, X.test = NULL, nl = 3, N.hidden = c(65), unit.type = c( "tanh"), lambda = 1e-5, beta = 1e-5, rho = 0.3, epsilon =0.1, max.iterations = 100, optim.method = c( "BFGS"), rel.tol =0.01, rescale.flag = TRUE, rescaling.offset = 0.001)
fit3 <-autoencode(X.train = x_train, X.test = NULL, nl = 5, N.hidden = c(65 ,65 ,55), unit.type = c( "tanh"), lambda = 1e-5, beta = 1e-5, rho = 0.2, epsilon =0.1, max.iterations = 500, optim.method = c( "BFGS"), rel.tol =0.01, rescale.flag = TRUE, rescaling.offset = 0.001)

pred1 <- predict (fit1, X.input = x_train, hidden.output = FALSE) 
pred3 <- predict (fit3, X.input = x_train, hidden.output = FALSE)
predAve <-( pred1$X.output + pred2$X.output + pred3$X.output )/3


require (Metrics) 
?mae()

mae(x_train, pred1$X.output) 
mae(x_train, pred2$X.output) 
mae(x_train, pred3 $X.output) 
mae(x_train, predAve) 
image(t( predAve))


#deep learning for Sonar
require(mlbench)
data(Sonar)

x_train <- as.matrix(Sonar[,1:60])

require(MASS)
pca <- prcomp(x_train, center= TRUE, scale = TRUE) #principal components model
pca.prop = pca$sdev^2/sum(pca$sdev^2) #compute the principal components proportions
round(1-pca.prop,3) #what percent of the variance each principal component explains

x_train <- as.matrix(pca$x)

set.seed(2016)
require(autoencoder) #autoencoder model for feature reduction
fit <- autoencode(X.train = x_train, X.test = NULL, nl = 3, N.hidden = 40, unit.type = c( "tanh"), lambda = 1e-5, beta = 1e-5, rho = 0.15, epsilon =0.1, max.iterations = 1000, optim.method = c( "BFGS"), rel.tol =0.001, rescale.flag = TRUE, rescaling.offset = 0.001)


pred <- predict(fit, X.input = x_train, hidden.output = TRUE) 
features <-pred$X.output

fit$mean.error.training.set

colnames(features) <- paste(rep("f",ncol(features)), c(1: ncol(features)), sep = "_")
head(features[,1:3])


#Kernel Deep Convex Neural Networks
install.packages("kernDeepStackNet")
library("kernDeepStackNet")

require(mlbench)

data("Vehicle",package = "mlbench")
x <- scale(Vehicle[,-19])

y<-Vehicle[,19] 
levels(y)[levels(y) == "saab"] <- "-1" 
levels(y)[levels(y) == "opel"] <- "-1" 
levels(y)[levels(y) == "van"] <- "+1" 
levels(y)[levels(y) == "bus"] <- "+1" 
y<-as.numeric(levels(y))[y] 
y<-as.matrix(y)

set.seed(465) 
train <- sample(1:846 ,800, FALSE)


require(kernDeepStackNet) 
fit <- fitKDSN(y =y[train], X = x[train,], levels =3, Dim =c(100, 100, 100), sigma =c(6, 8, 10), lambda =c(1, 0.1, 0.01), alpha =rep(0, 3), info = TRUE, standX = TRUE, seedW =c(3103, -4217553, -73461845))

fit$Input$Dim

fit$Input$levels

pred <- predict(fit,x[-train,])

predclass <- factor(ifelse( pred <0, "-1", "1")) 
table( y [- train], predclass)

mean(predclass != y[-train])

install.packages("pROC")
require(pROC)

auc(response = y[-train,], predictor = c(pred))

y_train <-y[train] 
y_test <-y[- train] 
x_train <- x[train,] 
x_test <-x[- train,]

library("caret")
set.seed(1964)
cvTrainInd <- createFolds(y=y_train, k = 10, list = TRUE, returnTrain = TRUE)

lossFunc <- function(preds, ytest){ -c(auc(response=ytest,predictor = c(preds)))}

fit_fine <- fineTuneCvKDSN(estKDSN = fit, y = y_train, X = x_train, fineTuneIt =100, info = TRUE, cvIndex = cvTrainInd, seedInit =1964)


pred_fine <- predict(fit_fine, newx = x_test) 
predclass_fine <- factor( ifelse( pred_fine <0, "-1", "1")) 
table( y [- train], predclass_fine)

mean(predclass_fine != y [- train])
auc(response = y_test, predictor =c( pred_fine))





#House price hedonic model
install.packages("Ecdat")
require("Ecdat")

data("Housing",package="Ecdat")
str(Housing)

#split response and input variables off from each other
y<-Housing[,1] 
x<-Housing[, -1]

levels(x$driveway)[levels(x$driveway) == "yes"] <- "1" 
levels(x$driveway)[levels(x$driveway) == "no" ] <- "0" 
x$driveway <-as.numeric(levels(x$driveway))[x$driveway]

levels(x$recroom)[levels(x$recroom) == "yes"] <- "1" 
levels(x$recroom)[levels(x$recroom) == "no"] <- "0" 
x$recroom <-as.numeric(levels(x$recroom))[x$recroom]

levels(x$fullbase)[levels(x$fullbase) == "yes"] <- "1"
levels(x$fullbase)[levels(x$fullbase) == "no" ] <- "0" 
x$fullbase <-as.numeric(levels(x$fullbase))[x$fullbase]

levels(x$gashw)[levels(x$gashw) == "yes"] <- "1" 
levels(x$gashw)[levels( x$gashw) == "no"] <- "0" 
x$ gashw <-as.numeric(levels( x$gashw))[x$gashw] 
levels( x$airco) [levels(x$airco) == "yes"] <- "1" 
levels( x$airco) [levels(x$airco) == "no"] <- "0" 
x$airco <-as.numeric(levels( x$airco))[x$airco] 
levels(x$prefarea)[levels( x$prefarea) == "yes"] <- "1" 
levels(x$prefarea)[levels( x$prefarea) == "no" ] <- "0" 
x$prefarea <-as.numeric(levels( x$prefarea))[x$prefarea]

#scale the data
x <- scale(as.matrix(x))

set.seed(465) 
train <- sample(1:540 ,500, FALSE) 
y_train <-y[train] 
y_test <-y[-train] 
x_train <-x[train,] 
x_test <-x[- train,]

library(caret)
cvTrainInd <- createFolds(y=y_train,k = 10, list = TRUE, returnTrain = TRUE)

require(kernDeepStackNet) 
fit_tune <- tuneMboLevelCvKDSN(y =y_train, X = x_train, levels =3, fineTuneIt =100, nStepMult =2, designMult =9, cvIndex = cvTrainInd)
pred_tune <- predict(fit_tune, newx =x_test) 
round(cor(pred_tune, y [- train]) ^2 ,3)
fit_tune$Input$seedW


#Build an extreme learning model

install.packages("archdata")
require(archdata)

data("Handaxes", package = "archdata")

y <- Handaxes[,2]
x <- scale(Handaxes[,3:8])
set.seed(1066)
train <- sample(1:600,550,FALSE)

n = length(train)
x_train <- x[train,]
x_test <- x[-train,]
y_train <- y[train]
y_test <- y[-train]

install.packages("elmNN")
library("elmNN")

fit <- elmtrain(y=y_train,x=x_train, nhid=40,actfun="sig")

pred <- predict(fit, newdata = x_test)
require(Metrics)

rmse(pred,y_test)
cor(pred, y_test)^2

data_train <- cbind(y_train,x_train)
library(caret)
train_control <- trainControl(method = "CV", number = 10)

grid <- expand.grid(.nhid = c(seq( from = 5, to = 40, by =5)),.actfun =c( "sig"))

set.seed(1066)
model <- train(y_train~., data=data_train, trControl=train_control, method = 'elm',tuneGrid = grid)
print(model)
plot(model)

pred2 <- predict(model,newdata=x_test)

rmse(pred2,y_test)

cor(pred2,y_test)^2

str(model)


# an ELM for the demographic and health survey from India
install.packages("quantreg.nonpar")
require(quantreg.nonpar)

data("india", package = "quantreg.nonpar")
str(india)


y<-india$cheight
keeps <- c( "cage" , "breastfeeding" , "mbmi" , "mage" , "medu" , "edupartner") 
x <-india[keeps] 
summary (x)

N=nrow(india) 
train <- sample (1:N ,37000, FALSE)

x_train <- x[train,]
x_test <- x[-train,]
y_train <- y[train]
y_test <- y[-train]

data_train <- cbind(y_train,x_train)


library (caret) 
train_control <- trainControl(method = "cv" , number =10) 
grid <- expand.grid(.nhid = c(seq( from = 30, to = 150, by =30)), .actfun =c( "sig"))


model <- train (y_train ~., data=data_train, trControl = train_control, method = "elm" , tuneGrid = grid)
print(model)


pred <- predict(model, newdata = x_test)
require(Metrics)

rmse(pred, y_test)
cor(pred,y_test)^2

plot(pred,y_test, col = "blue")
abline(lm(y_test~pred), col = "red")
summary(lm(y_test~pred))


#Self organizing polynomial neural networks

data(sunspot.month)

data <- sunspot.month[3113:3172]

data <- ts(matrix(data),end = c(2013,4), frequency = 12)

plot(data, xlab = "Year", ylab = "Number of Sunspots")

install.packages("GMDH")
require("GMDH")

fit = fcast(data, input = 3, layer = 2, f.number = 5)
?fcast

#more inputs to increase the fit of the observations
fit = fcast(data, input = 15, layer = 2, f.number = 5)

#change confidence interval to 99%
fit = fcast(data, input = 3, layer = 2, f.number = 5, level = .99)

plot(fit$mean)

attributes(fit)
fit$mean

#long term time series DNN forecasting - General Method of Data Handling
start = 3157
len = 60
end = 21
k = -1

for (i in start :(( start+end) -1)) { 
  k = k +1 
  data <-sunspot.month[(start - len+1+k) :( start+k)] 
  data <- ts( matrix(data) ,frequency = 12) 
  fit = fcast (data, input = 4, layer = 2, f.number = 1) 
  if( i == start) forecast <-as.numeric (fit$ mean) 
  if (i >start) forecast <- cbind( forecast ,as.numeric (fit$mean)) }


y = sunspot.month[(start+1):(start+end)-1]
require(Metrics)

rmse(forecast,y)

#Use Akaike's information criterion for assessing partial descriptors
k = -1 
for (i in start :(( start+end) -1)) 
  {k = k +1 
  data <-sunspot.month[(start - len +1+ k) : ( start+ k)] 
  data <- ts( matrix(data) ,frequency = 12) 
  fit = fcast (data, input = 3, layer = 3, f.number = 1, method = "RGMDH") 
  if( i == start) forecast1 <-as.numeric (fit$ mean ) 
  if (i >start) forecast1 <- cbind( forecast1 ,as.numeric(fit$mean)) }

y = sunspot.month[(start+1):(start+end)-1]
require(Metrics)

rmse(forecast1,y)

#Deep ensemble learning
install.packages("ada")
require(ada)


require(mlbench)
data("PimaIndiansDiabetes2", package="mlbench")

ncol(PimaIndiansDiabetes2)
nrow(PimaIndiansDiabetes2)


str(PimaIndiansDiabetes2)
sapply(PimaIndiansDiabetes2, function(x) sum(is.na(x)))

data <- (PimaIndiansDiabetes2)
data$insulin <- NULL
data$triceps <- NULL
data <- na.omit(data)

nrow(data)
data

ncol(data)


set.seed(2016)
n=nrow(data)
train <- sample(1:n,600, FALSE)

install.packages("deepboost")
library(deepbost)
require(deepboost)

fit <-deepboost(diabetes ~.,data, tree_depth = 5, num_iter = 20, beta = 0, lambda = 0.0, loss_type = "l", verbose = TRUE)

pred_train <- predict(fit, data[train,])

table(data$diabetes[train],pred_train)

(sum(pred_train == data$diabetes[train]) )/( sum(table(data$diabetes[train], pred_train)))

sum(diag(table(data$diabetes[train],pred_train))) / sum(table(data$diabetes[train],pred_train))


pred <- predict (fit, data[-train,]) 
table(data$diabetes [- train], pred)


#Deep monotone neural networks
require(TH.data)
data(data = 'bodyfat', package = "TH.data")
data = bodyfat

pairs(data, col = c("red"))

y <- log(bodyfat$DEXfat)
x <- log(bodyfat)
x$DEXfat <- NULL
x <- as.matrix(x)

install.packages("monmlp")
require(monmlp) 
set.seed(465) 
train <- sample(1:71,60, FALSE)

fit <- monmlp.fit(x = x [train,], y =as.matrix( y [train]), hidden1 =8, hidden2 =4, monotone =c(1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9))

pred <- monmlp.predict(x = x[-train,], weights = fit)

require(Metrics)
mse(y[-train],pred)

set.seed(465) 
fit <- monmlp.fit(x = x[train,], y =as.matrix( y[train]), hidden1 =8, hidden2 =4, monotone =c(1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9), n.ensemble = 100, bag = TRUE)

pred <- monmlp.predict(x = x [-train,], weights = fit) 
mse(y [-train], pred)

pca <- prcomp(x, center = TRUE, scale. = TRUE)
summary(pca)

#improve model performance by reducing the number of dimensions through principal components analysis
pca1 <-pca$x[,1] 
pca2 <-pca$x[,2] 
pca3 <-pca$x[,3] 
pca4 <-pca$x[,4] 
pca5 <-pca$x[,5] 
data <- cbind(pca1, pca2, pca3, pca4, pca5)

pairs(data)

set.seed(465) 
fit2 <- monmlp.fit (x = data[train,], y =as.matrix( y[train]), hidden1 =8, hidden2 =4, monotone =1, n.ensemble = 100, bag = TRUE)

pred2 <- monmlp.predict (x = data[ -train,] , weights = fit2) mse (y [-train], pred2)
mse(y[-train], pred2)

set.seed(465) 
fit3 <- monmlp.fit(x = data[ train,], y =as.matrix( y [train]), hidden1 =8, hidden2 =4, n.ensemble = 100, bag = TRUE) 
pred3 <- monmlp.predict (x = data[ -train,], weights = fit3) mse (y [-train], pred3)

mse (y [-train], pred3)


#Loading the abalone dataset for machine learning classification using DMNN (Deep Monotone Neural Networks)
aburl = 'http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data' 
names = c( 'sex' ,'length', 'diameter' , 'height' , 'whole.weight' , 'shucked.weight' , 'viscera.weight' , 'shell.weight', 'rings') 
data = read.table( aburl, header = F, sep = ',' , col.names = names)

#Loading the Crabs dataset from the pass package to do DMNN
library("MASS")

x<- scale(crabs[,4:8]) 
y<-crabs$sex 
levels(y)[levels(y) == "M" ]<-"-1" 
levels(y) [levels(y) == "F" ]<-"1" 
y<-as.numeric(levels(y))[y] 
y<-as.matrix(y)

set.seed(465) 
train <- sample(1:200 ,150, FALSE)

require(monmlp) 
set.seed (465) 
fit <- monmlp.fit (x = x [train,], y =as.matrix( y [train]), hidden1 =2, hidden2 =2, n.ensemble = 100, bag = TRUE) 
pred <- monmlp.predict (x = x [-train,], weights = fit)


pred <- monmlp.predict (x = x [-train,], weights = fit) 
predclass <- factor(ifelse( pred <0, "M", "F" )) 
table( crabs$sex [-train], predclass)
mean(predclass != crabs$sex [-train])

pairs(crabs)




set.seed (465) 
fit <- monmlp.fit(x = x [train,], y =as.matrix( y [train]), hidden1 = 2, hidden2 = 2, n.ensemble = 100, bag = TRUE, monotone = c(1 ,2 ,3 ,4 ,5)) 


pred <- monmlp.predict (x = x [-train,], weights = fit) 
predclass <- factor( ifelse( pred <0, "M", "F")) 
table( crabs$sex [-train], predclass)

mean(predclass != crabs$sex[- train])

#bad performance on the complicated model means reduce the number of dimensions down and try again via PCA
pca <- prcomp (crabs[,4:8], center = TRUE, scale. = TRUE) 
summary(pca)

pca$rotation


pca1 <-pca$x[,1] 
pca2 <-pca$x[,2] 
x<- cbind(pca1,pca2)

set.seed(465) 
fit <- monmlp.fit(x = x [train,], 
                  y =as.matrix( y [train]), hidden1 =2, hidden2 =2, n.ensemble = 100, bag = TRUE, monotone =1) pred <- monmlp.predict (x = x [- train,], weights = fit) 

predclass <- factor( ifelse( pred <0, "M", "F" )) 
table( crabs $sex [- train], predclass) 
predclass


#Using DNN techniques to evaluate an online news article database

#ETL process for downloading zip files, saving locally, opening them as CSVs, and then reading the data out 
urlloc = "https://archive.ics.uci.edu/ml/machine-learning-databases/00332/OnlineNewsPopularity.zip" 
download.file(urlloc, destfile = "OnlineNewsPopularity.zip" , method = "libcurl")

setwd('Q:/Innovation/CQ/AdamWkDir/Text Analytics')
getwd()

unzip("OnlineNewsPopularity.zip") 
fileloc = "Q:/Innovation/CQ/AdamWkDir/Text Analytics/OnlineNewsPopularity/OnlineNewsPopularity.csv" 
dataset <- read.table( fileloc, sep = ",", skip =0, header = T)

str(dataset)

summary(dataset$shares)

target <- as.numeric(dataset$shares > 1400)

head(target)
head(dataset$shares)

summary(as.factor(target))
class(target)

max_min_Range <- function (x) {(x -min( x) )/( max( x) -min( x))}
kword <- max_min_Range(dataset [,19:27]) 
day <- dataset [,31:38] 
channel <-dataset [,13:18] 
lda <-dataset [,39:43] 
data <- cbind( day, channel, kword, lda, target)

rand_seed =2016 
set.seed(rand_seed) 
train <- sample (1:nrow(data) ,39500, FALSE)

head(train)

x_train <-as.matrix(data[ train ,1:28]) 
y_train <- data[ train ,29] 
x_test <-as.matrix(max_min_Range(data[ - train ,1:28])) 
y_test <- data[ - train ,29]

require(deepnet) 
set.seed(rand_seed) 
fit1 <-nn.train(x = x_train, y = y_train, hidden = c(5 ,5), numepochs =10, activationfun = "sigm" , output = "sigm")

attributes(fit1)

fit1$activationfun
fit1$size #indicates a deep neural network with X input attributes, Y hidden layers, containing N nodes, and O output layer nodes

score1 <- nn.predict (fit1, x_train)
head(score1) #scores less than .5 indicate less than a median number of shares

summary(score1)

set.seed(rand_seed) 
fit2 <-nn.train (x = x_train, y = y_train, hidden = c(60 ,60), activationfun = "sigm" , numepochs =10, output = "sigm") 
score2 <- nn.predict(fit2, x_train)
summary(score2)
head(score2,3)

pred2 <- factor(ifelse( score2 <0.5, "0","1")) 
table( y_train, pred2)

1-mean(pred2!=y_train) # model accuracy



#####Customer Churn Model


urlloc = "http://www.dataminingconsultant.com/data/churn.txt"

getwd()

download.file(urlloc, destfile = "churn.txt" , method = "libcurl")

dataset <- read.csv("Q:/Innovation/CQ/AdamWkDir/Text Analytics/churn.txt" , header = T) 
str(dataset)

dataset$State <- NULL
dataset$Phone <- NULL

#install.packages("nnet")
require("nnet")

Int.l.Plan = class.ind(dataset$Int.l.Plan) 
colnames(Int.l.Plan) =c( "no","yes") 
VMail.Plan = class.ind (dataset$VMail.Plan) 
colnames(VMail.Plan) =c( "no","yes")

head(Int.l.Plan,3)
head(VMail.Plan,3)

head(dataset$Churn. ,4)

target <-dataset$Churn.

levels( target)[levels(target) == "False."] <-"-1"
levels( target)[levels(target) == "True."] <-"+1" 
target <-as.numeric(levels(target))[target] 
unique(target)

dataset$Int.l.Plan <-NULL 
dataset$VMail.Plan <-NULL 
dataset$Churn. <-NULL
#dataset$Phone <- NULL

samples <- cbind( dataset, Int.l.Plan, VMail.Plan) 
summary (samples)
str(samples)


samples <- scale(as.matrix( samples)) 
rand_seed =2016 
set.seed(rand_seed) 
train <- sample(1:nrow( samples) ,3000, FALSE) 
x_train = samples[train,]
y_train = target[train] 

str(y_train)

is.numeric(y_train)

x_test = samples[-train,] 
y_test = target[- train]

require (deepnet) 
set.seed(rand_seed) 
fit1 <-nn.train(x = x_train, y = y_train, hidden = c(5 ,3), activationfun = "sigm" , output = "sigm") 
score1 <- nn.predict(fit1, x_train)

pred1 <- factor(ifelse(score1 < 0.5, "-1","+1")) 
library (pROC) 
auc(response = y_train, predictor =c(pred1))

#An alternative activation - the hyperbolic tangent to improve model accuracy

set.seed(rand_seed) 
fit2 <-nn.train (x = x_train, y = y_train, hidden = c(5 ,3), activationfun = "tanh" , output = "linear") 

score2 <- nn.predict (fit2, x_train) 
pred2 <- factor( ifelse( score2 <0, "-1","+1")) 
auc(response = y_train, predictor =c( pred2)) 

set.seed(rand_seed) 
fit3 <-nn.train (x = x_train, y = y_train, hidden = c(5 ,3), momentum =0.98, learningrate =0.27, activationfun = "tanh" , output = "linear")
score3 <- nn.predict(fit3, x_train) 
pred3 <- factor( ifelse( score3 <0, " -1 "," +1 ")) 
auc(response = y_train, predictor =c( pred3))

summary(as.factor(target))

table(y_train,pred3)




output <-as.factor( y_train) 
levels(output) [levels( output) == "-1"] <-"0" 
input <-x_train
set.seed(rand_seed)

install.packages("unbalanced")
library(unbalanced) 
oversample <- ubOver(X= input, Y = output, k =1.93) 
newx_train = oversample$X
newy_train = oversample$Y

summary(as.factor(y_train))
summary(newy_train)


levels( newy_train)[levels(newy_train) == "0"] <-"-1" 
adjy_train <-as.numeric (levels( newy_train))[ newy_train] 
adjy_train <-as.matrix( adjy_train)



set.seed(rand_seed) 
fit4 <-nn.train(x = newx_train, y = adjy_train, hidden = c(5 ,3), momentum =0.98, learningrate =0.27, activationfun = "tanh" , output = "linear")

score4 <- nn.predict(fit4, x_train) 
pred4 <- factor(ifelse( score4 <0, "-1","+1")) 
auc(response = y_train, predictor =c( pred4))

#Test performance of each model
score1_test <- nn.predict(fit1, x_test) 
pred1_test <- factor( ifelse( score1_test <0, "-1","+1")) 
auc(response = y_test, predictor =c( pred1_test) )


score2_test <- nn.predict(fit2, x_test) 
pred2_test <- factor( ifelse( score2_test <0, " -1 "," +1")) 
auc (response = y_test, predictor =c( pred2_test) )

score3_test <- nn.predict(fit3, x_test) 
pred3_test <- factor(ifelse(score3_test <0, "-1","+1")) 
auc(response = y_test, predictor =c(pred3_test))

score4_test <- nn.predict(fit4, x_test) 
pred4_test <- factor(ifelse( score4_test <0, "-1","+1")) 
auc(response = y_test, predictor =c( pred4_test) )


#Demand forecasting deep neural network for bike sharing - i.e. demand planning

setwd('Q:/Innovation/CQ/AdamWkDir/Text Analytics')
urlloc = "http://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip"

download.file( urlloc, destfile = "BikeSharingDataset.zip" , method = "libcurl") 
unzip ("BikeSharingDataset.zip")

dataset <- read.table("Q:/Innovation/CQ/AdamWkDir/Text Analytics/day.csv", sep = ",", skip =0, header = T)
str(dataset)

library(corrplot)

cr <- cor(dataset[,c("temp", "atemp", "hum", "weathersit")])
corrplot(cr)

hist(dataset$registered, col = "red", xlab = "Count of registered users", main = NULL)

dataset[1] <- NULL
dataset$casual <-NULL 
dataset$cnt <- NULL 
dataset$yr <-NULL

require(nnet) 
season <- class.ind(dataset $season) 
colnames(season) =c( "spring","summer" ,"winter","fall")

head(season)

month <- class.ind(dataset$mnth) 
colnames (month) = c( "jan","feb","mar" , "april","may","jun","jul" , "aug","sep","oct","nov","dec") 
weekday <- class.ind(dataset$weekday) 
colnames (weekday) =c( "sun","mon","tue" , "wed","thu","fri","sat")

weather <- class.ind(dataset$weather) 
colnames(weather) = c( "clear","mist","light")

holiday <-as.data.frame( dataset$holiday) 
colnames (holiday) = c( "holiday") 
workingday <-as.data.frame( dataset$workingday) 
colnames(workingday) = c( "workingday") 
temp <-as.data.frame( dataset$temp) 
colnames (temp) = c( "temp") 
atemp <-as.data.frame( dataset $atemp) 
colnames (atemp) = c( "atemp") 
hum <-as.data.frame( dataset $hum) 
colnames(hum) = c( "hum") 
windspeed <-as.data.frame( dataset$windspeed) 
colnames (windspeed) = c( "windspeed")

sample <- cbind( season, month, holiday, weekday, workingday, weather, temp, atemp, hum, windspeed)

sample <- scale(sample)

target <-as.matrix( dataset$registered) 
colnames (target )<-c( "target") 
data <- cbind(sample ,log(target)) 
data <-as.data.frame(data)

head(data,3)
rand_seed =2016 
set.seed(rand_seed) 
train <- sample(1:nrow(data) ,700, FALSE)

#Automate formula generation
formu <- function(y_label, x_labels) { as.formula(sprintf ("%s ~ %s", y_label, paste( x_labels, collapse = "+"))) }

f<- formu("target" ,colnames(data [, -33]))
f

#Resilient backpropagation for deep neural networks so that you don't have to specify the learning rate!!!!!!
require(neuralnet) 
set.seed(rand_seed) 
fit1 <- neuralnet(f, data = data[ train,], hidden =c(1 ,3), algorithm = "rprop+" , err.fct = "sse" , act.fct = "logistic" , threshold =0.01, rep=1, linear.output = TRUE)
scores1 <-compute(fit1, data[ train ,1:32]) 
pred1 <- scores1$net.result 
y_train =data[ train ,33] 
require (Metrics) 
round( mse (pred1, y_train) ,4)
round(cor( pred1, y_train) ^2 ,4)

#Alternative model
set.seed (rand_seed) 
fit2 <- neuralnet (f, data = data[ train,], hidden =c(5 ,6), algorithm = "rprop+" , err.fct = "sse" , act.fct = "logistic" , threshold =0.01, rep=1, linear.output = TRUE)

scores2 <-compute(fit2, data[ train ,1:32]) 
pred2 <-scores2$net.result 
round( mse(pred2, y_train) ,4)
round(cor( pred2, y_train)^2 ,4)


set.seed (rand_seed) 
fit3 <- neuralnet (f, data = data[ train,], hidden =c(5 ,6), algorithm = "rprop+" , err.fct = "sse" , act.fct = "logistic" , threshold =0.01, rep =10, linear.output = TRUE)

scores3 <-compute(fit2, data[ train ,1:32])

pred3_1 = as.data.frame( fit3$net.result[1]) 
round( mse (pred3_1[ ,1], y_train) ,4)

round(cor( pred3_1[ ,1], y_train)^2 ,4)

pred3_2 =as.data.frame( fit3$net.result[2]) 
round( mse (pred3_2[ ,1], y_train) ,4)

y_test =data[ - train ,33] 
scores_test2 <-compute (fit2, data[ - train ,1:32]) 
pred_test2 = scores_test2$net.result 
round( mse (pred_test2, y_test) ,4)[1]  
round(cor( pred_test2, y_test) ,4)

scores_test1 <-compute (fit1, data[ -train ,1:32]) pred_test1 = scores_test1$net.result 
round( mse (pred_test1 ,y_test) ,4)
round(cor( pred_test1 ,y_test) ,4)


#DNN for predicting credit card loading ratios

install.packages("AER")
require("AER")

data("CreditCard",package = "AER")
str(CreditCard)

hist(CreditCard$share, col = "blue", xlab = "Ratio of Monthly Credit Card Expenditure to Yearly Income", main = NULL)

require (nnet) 
card <- class.ind(CreditCard$card) 
colnames(card) =c( "card_no","card_yes") 
owner <- class.ind(CreditCard$owner) 
colnames(owner) =c( "owner_no","owner_yes") 
selfemp <- class.ind(CreditCard$selfemp) 
colnames (selfemp) =c( "selfemp_no","selfemp_yes")

factor <- cbind(card, owner ,selfemp)
head(factor)


drops <- c( "card","owner","selfemp","share" ) 
contvars <-CreditCard [, !(names(CreditCard) %in% drops)] 
max_min_Range <- function (x) {(x -min( x) )/( max( x) -min( x))}
contvars <- max_min_Range(contvars)

share <-CreditCard$share
data <-as.matrix(cbind(factor, contvars, share )) 
rand_seed =2016 
set.seed (rand_seed) 
train <- sample (1:nrow(data) ,1200, FALSE) 
x_train <- data[ train ,1:14] 
y_train <- data[ train ,15] 
x_test <- data[ - train ,1:14] 
y_test <- data[ - train ,15]

require (deepnet) 
set.seed(rand_seed) 
fit1 <-nn.train (x = x_train, y = y_train, hidden = c(8 ,7 ,5 ,5), activationfun = "tanh" , output = "sigm")

pred1 <- nn.predict(fit1, x_train) 
require (Metrics) 
round( mse (pred1, y_train) ,4)

round(cor( pred1, y_train) ^2 ,4)
cor( pred1, y_train)

set.seed(rand_seed) 
fit2 <-nn.train(x = x_train, y = y_train, hidden = c(9 ,6 ,7 ,9), activationfun = "tanh" , output = "sigm")
pred2 <- nn.predict(fit2, x_train) 
mse(pred2, y_train)

cor( pred2, y_train) ^2

cor( pred2, y_train)

summary(pred2)

set.seed(rand_seed) 
fit3 <-nn.train (x = x_train, y = y_train, hidden = c(9 ,6 ,7 ,9), numepochs =1000, activationfun = "tanh" , output = "sigm")

pred3 <- nn.predict (fit3, x_train) 
mse (pred3, y_train)

cor( pred3, y_train) ^2
cor( pred3, y_train)

summary(pred3)


#Try even MORE epochs
set.seed (rand_seed) 
fit4 <-nn.train (x = x_train, y = y_train, hidden = c(9 ,6 ,7 ,9), numepochs =10000, activationfun = "tanh" , output = "sigm") 
pred4 <- nn.predict (fit4, x_train) 
mse (pred4, y_train)

cor( pred4, y_train) ^2

pred3_test <- nn.predict (fit3, x_test) 
mse(pred3_test, y_test)

cor( pred3_test, y_test)^2

pred4_test <- nn.predict(fit4, x_test) 
mse(pred4_test, y_test)
cor(pred4_test, y_test)^2


install.packages("MNP")
library("MNP")

data("detergent", package = "MNP")
str(detergent)

data <-detergent [!detergent$choice %in% c( 'All', 'EraPlus', 'Solo', 'Surf'),] 
data$choice <- factor(data$choice)

summary(data$choice)



require (nnet) 
response <- class.ind(data$choice) 
y<- max.col(response, 'first') 
rand_seed =2016 
set.seed(rand_seed) 
train <- sample(1:nrow(data) ,1300, FALSE) 
x_train <- scale(data[ train ,2:7]) 
y_train <- response [train,] 
x_test <- data[ -train ,2:7] 
y_test <- response [-train,]

require (deepnet)
set.seed(rand_seed) 
fit1 <-nn.train (x =as.matrix(x_train), y=as.matrix( y_train), hidden = c(42 ,18), momentum =0.6, learningrate =exp (-10), activationfun = "sigm" , output = "sigm")

score1 <- nn.predict(fit1, x_train) 
pred1 <- max.col( score1, 'first') 
1 -mean( pred1 !=y [train])

table( y[train], pred1)
summary(score1)

install.packages("versions")
require (versions) 
install.versions ('darch', '0.10.0')

install.packages("deeplearning")
require (deeplearning) 
set.seed(rand_seed) 
generateWeights (5, 2)


set.seed(rand_seed) 
modelR <- new_dnn ( c(6, 42 ,18, 2), hidden_layer_default = rectified_linear_unit_function, output_layer_default = sigmoidUnitDerivative )

fitR <- train_dnn ( modelR, x_train, y_train, input_valid = x_test, target_valid = y_test, error_function = crossEntropyErr, report_classification_error = TRUE )

scoreR <- predict(fitR) 
predR <- max.col(scoreR, 'first') 
table( y[train], predR)

1 -mean( predR !=y [train])


#using cross validation to properly train a model

set.seed(rand_seed) 
train <- sample(1:nrow(data) ,1299, FALSE) 
x_train <- scale(data[train ,2:7]) 
y_train <- response [train,] 
x_test <- data[ -train ,2:7] 
y_test <- response [- train,]


fit2 <- train_dnn ( modelR, x_train, y_train, input_valid = x_test, target_valid = y_test, error_function = crossEntropyErr, report_classification_error = TRUE )

score2 <- predict (fit2) 
pred2 <- max.col( score2, 'first') 
table( y [train], pred2)

1 - mean( pred2 !=y [train])


score2_test <- predict (fit2, newdata = x_test ) 
pred2_test <- max.col( score2_test, 'first') 
table( y [-train], pred2_test)

1 -mean( pred2_test!=y [- train])

summary(data[,2:7])


x_train <-(data[ train ,2:7]) 
fit3 <- train_dnn( modelR, x_train, y_train, input_valid = x_test, target_valid = y_test, error_function = crossEntropyErr, report_classification_error = TRUE )

score3 <- predict(fit3) 
pred3 <- max.col( score3, 'first') 
table( y [train], pred3)

1 -mean( pred3 !=y [train])


score3_test <- predict(fit3, newdata = x_test ) 
pred3_test <- max.col( score3_test, 'first') 
table( y [- train], pred3_test)

1 -mean( pred3_test!=y [- train])


set.seed(rand_seed +1) 
train <- sample(1:nrow(data) ,1000, FALSE) 
x_train <-(data[ train ,2:7]) 
y_train <- response [train,] 
x_test <- data[ - train ,2:7] y_test <- response [- train,] 
fit4 <- train_dnn ( modelR, x_train, y_train, input_valid = x_test, target_valid = y_test, error_function = crossEntropyErr, report_classification_error = TRUE )


score4 <- predict (fit4) 
pred4 <- max.col( score4, 'first') 
table( y [train], pred4)
1 -mean( pred4 !=y [train])

score4_test <- predict (fit4, newdata = x_test ) 
pred4_test <- max.col( score4_test, 'first')

#This completely does not work - elements not the same length
table( y[-train], pred4_test)

length(y[-train])


#Forecast vehicle value using a DNN
library("readxl")
urlloc = "http://www.amstat.org/publications/jse/datasets/kuiper.xls" 

read_excel(path = urlloc, sheeet = 1)

?read_excel

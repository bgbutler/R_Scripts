
#load in the libraries


library(MASS)   #for LDA
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(class)  #for knn
library(rpart)  #other machine learning
library(caret)  #machine learning package
library(dendextend)
library(dendextendRcpp)
library(xgboost)
library(Metrics)

#get a list of the files
url <- "K:/Sandbox/R/Segmentation/TrainingCoke2.csv"
cokeRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

cokeRaw$Cluster <- as.factor(cokeRaw$Cluster)

#clean up the feminino in the data
cokeRaw$Gender <- as.character(cokeRaw$Gender)
cokeRaw$Gender <- ifelse(cokeRaw$Gender %in% c("Female", "Femenino"),"Female", "Male")
cokeRaw$Gender <- as.factor(cokeRaw$Gender)


#exploratory plot
p <- ggplot(cokeRaw, aes(x = Country, y = Cluster, color = Cluster)) +
  geom_point(size = 5)
p

cokeDist <- cokeRaw[,c(8:84)]


#plot tree diagram for entire matrix
d <- dist(cokeDist)
hc <- hclust(d)
op = par(bg = "white")
dend <- d %>% hclust %>% as.dendrogram
dend %>% color_branches(k=5) %>% plot(horiz=FALSE,
                                      main = "Hierchical Clusters of 7 Segments", axes = FALSE)
axis(side = 2, col = "blue", at = seq(1,25,5), labels = FALSE)
mtext(seq(1,25,5), side = 2, at = seq(1,25,5),
      line = 1, col = "blue", las = 2)
dend %>% rect.dendrogram(k = 5, horiz = FALSE)


#look for near zero variances to return percentage of zero variance for each featuer
nzv <- nearZeroVar(cokeRaw, saveMetrics = TRUE)
print(paste("Range:", range(nzv$percentUnique)))
head(nzv,10)

#check for percent unique > .10
dim(nzv[nzv$percentUnique > 0.1,])


#create dataframe for evaluation and determine cluster
cokeRaw$Cluster <- as.numeric(cokeRaw$Cluster)
dfEvaluate <- cbind(as.data.frame(sapply(cokeDist, as.numeric)),
                    cluster=cokeRaw$Cluster)

# build a PCS function to perform cross validation and test AUC (area under the curve)
EvaluateAUC <- function(dfEvaluate) {
  require(xgboost)
  require(Metrics)
  CVs <- 5
  cvDivider <- floor(nrow(dfEvaluate) / (CVs+1))
  indexCount <- 1
  outcomeName <- c('cluster')
  predictors <- names(dfEvaluate)[!names(dfEvaluate) %in% outcomeName]
  lsErr <- c()
  lsAUC <- c()
  for (cv in seq(1:CVs)) {
    print(paste('cv',cv))
    dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
    dataTest <- dfEvaluate[dataTestIndex,]
    dataTrain <- dfEvaluate[-dataTestIndex,]
    
    bst <- xgboost(data = as.matrix(dataTrain[,predictors]),
                   label = dataTrain[,outcomeName],
                   max.depth=6, eta = 1, verbose=0,
                   nround=5, nthread=4, 
                   objective = "reg:linear")
    
    predictions <- predict(bst, as.matrix(dataTest[,predictors]), outputmargin=TRUE)
    err <- rmse(dataTest[,outcomeName], predictions)
    auc <- auc(dataTest[,outcomeName],predictions)
    
    lsErr <- c(lsErr, err)
    lsAUC <- c(lsAUC, auc)
    gc()
  }
  print(paste('Mean Error:',mean(lsErr)))
  print(paste('Mean AUC:',mean(lsAUC)))
}


EvaluateAUC(dfEvaluate)

nComp <- 1











#use the distance data for principal component analysis to check the variances
pca1 <- prcomp(cokeDist, scores = TRUE, cor = TRUE)
plot(pca1, main = "PCA of Coke Data")

screeplot(pca1, type = "line", main = "Scree Plot of PCA")

biplot(pca1)


#perform a factor analysis on the questions start with 6 questions
#reject n factors until p value is larger than .05, 19 is the number, but 20 may be the actual

f <- factanal(cokeDist, factors = 10)
summary(f)

loading <- f[2]

loadMatrix <- as.matrix(loading)

#alternate
pc <- prcomp(~ cokeDist$Q1 + cokeDist$Q2 + cokeDist$Q3 + cokeDist$Q4 + cokeDist$Q5 + cokeDist$Q6 + 
               cokeDist$Q7 + cokeDist$Q8 + cokeDist$Q9 + cokeDist$Q10 + cokeDist$Q11 + 
               cokeDist$Q12 + cokeDist$Q13 + cokeDist$Q14 + cokeDist$Q15 + cokeDist$Q16 + cokeDist$Q17 + 
               cokeDist$Q18 + cokeDist$Q19 + cokeDist$Q20 + cokeDist$Q21 +
               cokeDist$Q22 + cokeDist$Q23 + cokeDist$Q24 + cokeDist$Q25 + cokeDist$Q26 + cokeDist$Q27 +
               cokeDist$Q28 + cokeDist$Q29 + cokeDist$Q30 + cokeDist$Q31 + cokeDist$Q32 + cokeDist$Q33 + 
               cokeDist$Q34 + cokeDist$Q35 + cokeDist$Q36 + cokeDist$Q37 + cokeDist$Q38 + cokeDist$Q39 + 
               cokeDist$Q40 + cokeDist$Q41 + cokeDist$Q42 + cokeDist$Q43 + cokeDist$Q44 + cokeDist$Q45 + 
               cokeDist$Q46 + cokeDist$Q47 + cokeDist$Q48 + cokeDist$Q49 + cokeDist$Q50 + cokeDist$Q51 + 
               cokeDist$Q52 + cokeDist$Q53 + cokeDist$Q54 + cokeDist$Q55 + cokeDist$Q56 + cokeDist$Q57 + 
               cokeDist$Q58 + cokeDist$Q59 + cokeDist$Q60 + cokeDist$Q61 + cokeDist$Q62 + cokeDist$Q63 + 
               cokeDist$Q64 + cokeDist$Q65 + cokeDist$Q66 + cokeDist$Q67 + cokeDist$Q68 + cokeDist$Q69 + 
               cokeDist$Q70 + cokeDist$Q71 + cokeDist$Q72 + cokeDist$Q73 + cokeDist$Q74 + cokeDist$Q75 + 
               cokeDist$Q76 + cokeDist$Q77 + cokeDist$Q8 + cokeDist$Q9 + cokeDist$Q10 + cokeDist$Q11)
summary(pc)



#find the correlations between the variables and remove the highly correlated sets
corrMatrix <- cor(cokeDist, method = "pearson")

write.csv(corrMatrix, file = "K:/Sandbox/R/Segmentation/corrMatrix.csv", row.names = FALSE)


#look at correlations to the factors and percent of inertia
#first two components in plot capture 16% of the total variance
res.pca = PCA(cokeDist, scale.unit=TRUE, ncp=21, graph=T)
dimdesc(res.pca, axes=c(1:20))

res.pca$var


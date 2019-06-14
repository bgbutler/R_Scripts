
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
par(mai=c(2.0,1.0, 1.0,1.0))
d <- dist(cokeDist)
hc <- hclust(d)
op = par(bg = "white")
dend <- d %>% hclust %>% as.dendrogram
  dend %>% set("labels_col", k = 7) %>%
  set("labels_cex", .75) %>%
  color_branches(k=7) %>%
  set("branches_lwd", 2) %>%
  hang.dendrogram (hang = -1) %>%
plot(horiz=FALSE, main = "Hierchical Clusters of 7 Segments", axes = FALSE)
axis(side = 2, col = "blue", at = seq(1,25,5), labels = FALSE)
mtext(seq(1,25,5), side = 2, at = seq(1,25,5),
      line = 1, col = "blue", las = 1)
dend %>% rect.dendrogram(k = 7, horiz = FALSE)


#look for near zero variances to return percentage of zero variance for each featuer
nzv <- nearZeroVar(cokeDist, saveMetrics = TRUE)
print(paste("Range:", range(nzv$percentUnique)))
head(nzv,10)

#check for percent unique > .10
dim(nzv[nzv$percentUnique > 0.1,])




















#use the distance data for principal component analysis to check the variances
plot(prcomp(cokeDist), main = "PCA of Coke Data")
pca1 <- prcomp(cokeDist, scores = TRUE, cor = TRUE)
plot(pca1)

screeplot(pca1, type = "line", main = "Scree Plot")

biplot(pca1)

pca1$scores[1:10,]




res.pca = PCA(cokeDist, scale.unit=TRUE, ncp=21, graph=T)

#perform a factor analysis on the questions start with 6 questions
#reject n factors until p value is larger than .05, 19 is the number, but 20 may be the actual

f <- factanal(cokeDist, factors = 21)
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


a1 <- pc$rotation[,1]
a1

a2 <- pc$rotation[,2]
a2


#find the correlations between the variables and remove the highly correlated sets
corrMatrix <- cor(cokeDist, method = "pearson")

write.csv(corrMatrix, file = "K:/Sandbox/R/Segmentation/corrMatrix.csv", row.names = FALSE)


#look at correlations to the factors
dimdesc(res.pca, axes=c(1,2))

res.pca$var


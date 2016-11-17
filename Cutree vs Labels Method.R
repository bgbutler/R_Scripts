USArrests

TestClust <- hclust(dist(USArrests, method= "euclidean"), method = "average")
TestClust

#Shows that the dendrogram X axis labels will be driven by the hierarchy of the dendrogram
plot(as.dendrogram(TestClust))

#The labels function of hclust function are a vector the matches the left to right ordering of the dendrogram
labels(TestClust)


#The cutree function returns cluster assignments that are based on a least to greatest ordering of the name of the objects being clustered
cvec <- cutree(TestClust, k = 2)
cvec

#It's possible to see the size of each cluster by creating a table. Since the cluster number is used to do the grouping
#it will match the cluster order of the dendrogram. This will let you loop through the group sizes to dynamically
#assign K clusters instead of doing this manually. 
groups.2 <- as.data.frame(table(cvec))




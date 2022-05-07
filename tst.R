library(factoextra)

data("USArrests")      # Loading the data set
df <- scale(USArrests)

boxplot(USArrests)$out

#kmeans(x, centers, iter.max = 10, nstart = 1)

# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

# Print the results
print(km.res).

aggregate(USArrests, by=list(cluster=km.res$cluster), mean)


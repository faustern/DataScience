# Question 1
# Use the iris dataset built in R.
# Part (a): k-means clustering
# 1. Cluster the dataset into 2 groups using k-means clustering. 
# 2. Draw a plot to visualize the clusters.
# 3. Cluster the dataset into 3 groups using k-means clustering. 
# 4. Draw a plot to visualize the clusters.
# Part (b): Hierarchical Clustering
# 1. Cluster the observations using complete linkage.
# 2. Cluster the observations using average and single linkage. 
# 3. Plot the dendrogram for the above clustering methods.

# 1.a.1
data(iris)
iris_data = iris[,1:4]
head(iris_data)

set.seed(123)
kmeans_2_iris = kmeans(iris_data, centers=2)
print(kmeans_2_iris$cluster)

# 1.a.2
pp = prcomp(iris_data)
plot(pp$x[,1:2], col=fitted(kmeans_2_iris, "classes")+1)
# exist some outlier in group 2

# 1.a.3
kmeans_3_iris = kmeans(iris_data, centers=3)
print(kmeans_3_iris$cluster)

# 1.a.4
pp = prcomp(iris_data)
plot(pp$x[,1:2], col=fitted(kmeans_3_iris, "classes")+1)
# probably more accurate than using 2 cluster

# 1.b.1
hier_iris_data_comp = hclust(dist(iris_data), method="complete")
hier_iris_data_comp

# 1.b.2
hier_iris_data_avg = hclust(dist(iris_data), method="average")
hier_iris_data_avg
hier_iris_data_single = hclust(dist(iris_data), method="single")
hier_iris_data_single

# 1.b.3
plot(hier_iris_data_comp, xlab=" ", sub="Complete link cluster analysis")
plot(hier_iris_data_avg, xlab=" ", sub="Average link cluster analysis")
plot(hier_iris_data_single, xlab=" ", sub="Single link cluster analysis")

# 2.a.1
set.seed(2)
x = matrix(rnorm(50*2), ncol = 2) 
x[1:25, 1]= x[1:25, 1] + 3 
x[1:25, 2] = x[1:25, 2] - 4

set.seed(123)
kmeans_2_x = kmeans(x, centers=2)
print(kmeans_2_x$cluster)

# 2.a.2
pp = prcomp(x)
plot(pp$x[,1:2], col=fitted(kmeans_2_x, "classes")+1)

# 2.a.3
kmeans_3_x = kmeans(x, centers=3)
print(kmeans_3_x$cluster)

# 2.a.4
pp = prcomp(x)
plot(pp$x[,1:2], col=fitted(kmeans_3_x, "classes")+1)

# 2.b.1
hier_x_comp = hclust(dist(x), method="complete")
hier_x_comp

# 2.b.2
hier_x_avg = hclust(dist(x), method="average")
hier_x_avg
hier_x_single = hclust(dist(x), method="single")
hier_x_single

# 1.b.3
plot(hier_x_comp, xlab=" ", sub="Complete link cluster analysis")
plot(hier_x_avg, xlab=" ", sub="Average link cluster analysis")
plot(hier_x_single, xlab=" ", sub="Single link cluster analysis")


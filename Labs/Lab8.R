# 1.a
library(ISLR)
data(iris)
head(iris)

# 1.b
#The iris dataset contains measurements of four features 
#(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) 
#for 150 samples from three species of iris flowers: 
#Setosa, Versicolor, and Virginica.

# 1.c
apply(iris[, 1:4], 2, mean)

# 1.d
apply(iris[, 1:4], 2, var)

# 1.e
pca_iris <- prcomp(iris[, 1:4], scale = TRUE)
summary(pca_iris)

# 1.f
pca_iris$rotation

# 1.g
pca_iris$x[, 1:2]

# 1.h
plot(pca_iris, type = "l")

# 1.i
biplot(pca_iris, scale=0)

# 1.j
biplot(pca_iris, choices = c(2, 3))

# 2.a
data(USArrests)
head(USArrests)

# 2.b
summary(USArrests)
#The USArrests dataset contains statistics, in arrests per 100,000 residents 
#for assault, murder, and rape in each of the 50 US states in 1973. It also 
#includes the percent of the population living in urban areas.

# 2.c
apply(USArrests, 2, mean)

# 2.d
apply(USArrests, 2, var)

# 2.e
pca_usarrests <- prcomp(USArrests, scale = TRUE, center = TRUE)
summary(pca_usarrests)

# 2.f
pca_usarrests$rotation

# 2.g
pca_usarrests$x[, 1:2]

# 2.h
plot(pca_usarrests, type = "l")

# 2.i
biplot(pca_usarrests)

# 2.j
biplot(pca_usarrests, choices = c(2, 3))

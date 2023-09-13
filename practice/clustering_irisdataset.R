data("iris")
head(iris)
iris.scaled <- scale(iris[, -5])
x=iris[,3:4]
head(x)

# model=kmeans(x,3)
#   library(cluster)
# clusplot(x,model$cluster)
# 

# K-means clustering
km.res <- kmeans(iris.scaled, 3, nstart = 10)

# Visualize kmeans clustering
fviz_cluster(km.res, iris[, -5], ellipse.type = "norm")

fviz_cluster(km.res, iris[, -5],
             palette = "Set2", ggtheme = theme_minimal())

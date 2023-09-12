data <- ("iris")
head(iris)

x=iris[,3:4]
head(x)

model=kmeans(x,3)
  library(cluster)
clusplot(x,model$cluster)



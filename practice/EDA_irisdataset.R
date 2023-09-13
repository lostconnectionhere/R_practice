library(tidyverse)

data <- ("diamonds")

#view first 6 rows
head(diamonds)

#summarize the data
summary(diamonds)

#dimensions of the data
dim(diamonds)

#visualize the data with a histogram
ggplot(data=diamonds, aes(x=price)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of price values")

#scatterplot
ggplot(data=diamonds, aes(x=carat, y=price, color=cut)) +
  geom_point()

#boxplot
ggplot(data=diamonds, aes(x=cut, y=price)) +
  geom_boxplot(fill="steelblue")

#correlation matrix (rounded to 2 decimals)
round(cor(diamonds[c('carat', 'depth', 'price', 'x', 'y', 'z')]), 2)


#missing values
sapply(diamonds, function(x) sum(is.na(x)))

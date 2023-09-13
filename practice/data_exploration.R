#import spotify 2023 data from Kaggle: https://www.kaggle.com/datasets/nelgiriyewithana/top-spotify-songs-2023

data <- read_csv('spotify-2023.csv',show_col_types = FALSE)

#print first rows 
head(data, 5)

summary(data)

dim(data)

#visualize the data with a histogram
ggplot(data=data, aes(x=released_year)) +
  geom_histogram(fill="pink", color="white",bins=100) +
  ggtitle("Histogram of released_year")

sapply(data, function(x) sum(is.na(x)))

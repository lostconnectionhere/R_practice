library(tidyverse)
library(tidyr)
library(dplyr)

#Inlezen dataset
kerncijfers <- read.csv2('data/regionale_kerncijfers_subset.csv', sep=";")
print(kerncijfers)

colnames(kerncijfers) 

# kerncijfers[, c('Perioden')]

#Clusters op basis van data uit het jaar 2022
kerncijfers_2022 <- kerncijfers[kerncijfers$Perioden %in% '2022JJ00',]

#Controleren of in de kolom Perioden alleen 2022 staat
# dim(kerncijfers_2022)
kerncijfers_2022[, c('Perioden')]

#Onderzoek of er lege rijen zijn
print("Totaal aantal missende waarden - ")
sum(is.na(kerncijfers_2022))

colSums(is.na(kerncijfers_2022))
kerncijfers_2022 <- na.omit(kerncijfers_2022)
colSums(is.na(kerncijfers_2022))

# Categorische waarde eruit om te clusteren, zoals ID
kerncijfers_2022 <- kerncijfers_2022[, !names(kerncijfers_2022) %in% c("ID")]

# RegioS kolom verplaatsen naar rijnummer
row.names(kerncijfers_2022) <- kerncijfers_2022$RegioS

# RegioS kolom verwijdered
kerncijfers_2022 <- kerncijfers_2022[, !names(kerncijfers_2022) %in% c("RegioS")]

# Check of goed is gegaan
summary(kerncijfers_2022)
colnames(kerncijfers_2022)

set.seed(123)

kerncijfers_2022 <- as.numeric(unlist(x))



# sum(sapply(kerncijfers_2022, is.na))
# sum(sapply(kerncijfers_2022, is.infinite))
# sum(sapply(kerncijfers_2022, is.nan))
# 
# kerncijfers_2022[apply(sapply(kerncijfers_2022, is.finite), 1, all),]

# sum(sapply(kerncijfers_2022, is.na))
# sum(!sapply(kerncijfers_2022, is.finite))

km.out <- kmeans(kerncijfers_2022, centers = 5, nstart = 20)
km.out

# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:n) {
  # Fit the model: km.out
  km.out <- kmeans(kerncijfers_2022, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:n, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')

scree_plot


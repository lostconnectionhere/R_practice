library(tidyverse)
library(tidyr)
library(dplyr)

#Inlezen dataset + analyseren
kerncijfers <- read.csv2('data/regionale_kerncijfers_subset.csv', sep=";")
print(kerncijfers)

colnames(kerncijfers) 


# Data uit alleen het jaar 2022 gebruiken voor de clusters
kerncijfers_2022 <- kerncijfers[kerncijfers$Perioden %in% '2022JJ00',]

# Controleren of in de kolom Perioden alleen 2022 staat
kerncijfers_2022[, c('Perioden')]

#Onderzoek of er lege rijen zijn
print("Totaal aantal missende waarden - ")
sum(is.na(kerncijfers_2022))

kerncijfers_2022 <- na.omit(kerncijfers_2022)
colSums(is.na(kerncijfers_2022))

# Categorische kolommen verwijderen, ID, RegioS & Perioden
kerncijfers_2022 <- kerncijfers_2022[, !names(kerncijfers_2022) %in% c("ID")]
kerncijfers_2022 <- kerncijfers_2022[, !names(kerncijfers_2022) %in% c("Perioden")]

# RegioS kolom verplaatsen naar rijnummer
row.names(kerncijfers_2022) <- kerncijfers_2022$RegioS

# RegioS kolom verwijdered
kerncijfers_2022 <- kerncijfers_2022[, !names(kerncijfers_2022) %in% c("RegioS")]

# Controleren of dataset alleen uit numerieke waarden bestaat
summary(kerncijfers_2022)
colnames(kerncijfers_2022)

# voor reproduceerbaarheid seed 123 gekozen
set.seed(123)


km.out <- kmeans(kerncijfers_2022, centers = 5, nstart = 20)
km.out

# Max clusters op 10
n_clusters <- 10

# Sum of squares initialiseren
wss <- numeric(n_clusters)

# De verschillende aantallen clusters analyseren
for (i in 1:n) {
  # fit het model: km.out
  km.out <- kmeans(kerncijfers_2022, centers = i, nstart = 20)
  # within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# scree plot
wss_df <- tibble(clusters = 1:n, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')

scree_plot

scree_plot +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000',4),'#FF0000', rep('#000000', 5))
  )

# Plot de 5 clusters
fviz_cluster(km.out, kerncijfers_2022, ellipse.type = "norm")


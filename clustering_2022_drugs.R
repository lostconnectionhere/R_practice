#Benodigde libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(factoextra)

#Inlezen dataset + analyseren uit 2022
data_md<- read.csv2('data/gemeenten_misdrijven_drugshandel.csv', sep=";")
names(data_md)

summary(data_md)

#Onderzoek of er lege rijen zijn
print("Totaal aantal missende waarden in de dataframe - ")
sum(is.na(data_md))
colSums(is.na(data_md))

#Remove NA
data_md <- na.omit(data_md) 
sum(is.na(data_md))

# Gemeentenaam kolom verplaatsen naar rijnummer
row.names(data_md) <- data_md$Gemeenten

# Gemeentenaam kolom verwijdered
data_md <- data_md[, !names(data_md) %in% c("Gemeenten")]

# voor reproduceerbaarheid seed 123 gekozen
set.seed(123)

# data scalen --> zodat clusteren niet afhankelijk is van een variabele
data_md_scaled <- scale(data_md)
head(data_md_scaled)

km.out <- kmeans(data_md_scaled, centers = 5, nstart = 20)
km.out

# clustered_df <- cbind(df_scaled, Cluster = km.out$cluster)
clustered_df <- data.frame(data_md_scaled, Cluster = km.out$cluster)

# Max clusters op 10
n_clusters <- 5

# Sum of squares initialiseren
wss <- numeric(n_clusters)

n <- 5

# De verschillende aantallen clusters analyseren
for (i in 1:n) {
  # fit het model: km.out
  km.out <- kmeans(clustered_df, centers = i, nstart = 20)
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
fviz_cluster(km.out, data_md_scaled, ellipse.type = "norm")

# Weergeef bijv. alle gemeentes die in cluster 1, 5 zitten
names(clustered_df)
unique(clustered_df$Cluster)
cluster_5 <- subset(clustered_df, Cluster == 5)
cluster_1 <- subset(clustered_df, Cluster == 1)
print(cluster_1)
print(cluster_5)

#Benodigde libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(factoextra)
library(plotly)

#Inlezen dataset uit 2022
df1 <- read.csv2('data/kerncijfers_22_pt1.csv', sep=";")
df2 <- read.csv2('data/kerncijfers_22_pt2.csv', sep=";")
df3 <- read.csv2('data/kerncijfers_22_pt3.csv', sep=";")

#Data analyseren
head(df1)
head(df2)
head(df3)

# Ter controle een kolom selecteren of er waardes zijn met een lege spatie voorafgaande de waarde
# sum(startsWith(df2$OpleidingsniveauLaag_64, " "))
# In 18002 gevallen zit er in ieder geval 1 spatie voorafgaande de waarde

# Functie om leading en trailing spaties te verwijderen voor character- of factor-kolommen
trim_character_or_factor <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col) || is.factor(col)) {
      return(trimws(as.character(col)))
    } else {
      return(col)
    }
  })
  return(df)
}

# Pas de functie toe op je dataframes df1, df2, en df3
df1 <- trim_character_or_factor(df1)
df2 <- trim_character_or_factor(df2)
df3 <- trim_character_or_factor(df3)

# Controleren of het probleem is opgelost
# sum(startsWith(df2$OpleidingsniveauLaag_64, " "))

# Hoeveel punten bij iedere kolom aanwezig
apply(df1, 2,  function(col) sum(col == '.'))
apply(df2, 2,  function(col) sum(col == '.'))
apply(df3, 2,  function(col) sum(col == '.'))

# Alle punten omzetten in NA's
df1 <- df1 %>% mutate(across(.cols = everything(), ~replace(., . == '.', NA)))
df2 <- df2 %>% mutate(across(.cols = everything(), ~replace(., . == '.', NA)))
df3 <- df3 %>% mutate(across(.cols = everything(), ~replace(., . == '.', NA)))

# Hoeveel missende waarden per kolom
sapply(df1, function(x) sum(is.na(x)))
sapply(df2, function(x) sum(is.na(x)))
sapply(df3, function(x) sum(is.na(x)))

# Kolommen die uitgezonderd moeten worden
columns_to_exclude <- c("Gemeentenaam_1", "ID", "WijkenEnBuurten")

# Welke class hebben alle kolommen
sapply(df1, class)
sapply(df2, class)
sapply(df3, class)

# Functie die char" kolommen verandert naar numerieke, behalve de kolommen die "char" moeten blijven
convert_and_exclude <- function(df, columns_to_exclude) {
  for (col_name in colnames(df)) {
    if (col_name %in% columns_to_exclude) {
      next  # 
    }
    
    if (is.character(df[[col_name]])) {
      df[[col_name]] <- as.numeric(df[[col_name]])
    }
  }
  return(df)
}

# Functie toepassen op iedere df
df1 <- convert_and_exclude(df1, columns_to_exclude)
df2 <- convert_and_exclude(df2, columns_to_exclude)
df3 <- convert_and_exclude(df3, columns_to_exclude)

# Controleer of het goed is gegaan
print(df1)
print(df2)
print(df3)

# Bekijken hoeveel rows iedere df heeft voor het joinen
nrow(df1) # 18003
nrow(df2) # 18003
nrow(df3) # 18003

# Inner join df1 met df2 
innerJoinDf1 <- inner_join(df1, df2, by = c("ID", 'Gemeentenaam_1', 'WijkenEnBuurten'))
nrow(innerJoinDf1) # ook 18003

# Vervolgens, het resultaat joinen met df 3
innerJoinDf <- inner_join(innerJoinDf1, df3, by = c("ID",  'Gemeentenaam_1', 'WijkenEnBuurten'))
nrow(innerJoinDf) # ook 18003

# View the resulting inner-joined data frame
#View(innerJoinDf)

# verwijder alle rijen die geen Gemeente zijn, behalve de rij met waardes voor NL
# De rijen filteren gebaserd op de gekozen regel
length(which(grepl('GM', df1$WijkenEnBuurten)))

df_filtered <- subset(innerJoinDf, grepl("^GM", WijkenEnBuurten))
nrow(df_filtered) # klopt

# Print the filtered data frame
print(df_filtered)

#verwijder kolommen waarin bijna alle rijen NA zijn
new_df <- df_filtered[, !names(df_filtered) %in% c("ID", "WijkenEnBuurten.y", "Gemeentenaam_1.y", "WijkenEnBuurten", "WijkenEnBuurten.x","AfstandTotSchool_109", "ScholenBinnen3Km_110", "PersonenPerSoortUitkeringWW_85", 
                                                   "PersonenPerSoortUitkeringAOW_86", "GemGestandaardiseerdInkomenVanHuish_75", "PersonenPerSoortUitkeringBijstand_83", "PersonenPerSoortUitkeringAO_84", "OpleidingsniveauMiddelbaar_65",
                                                   "OpleidingsniveauMiddelbaar_65", "OpleidingsniveauHoog_66", "Nettoarbeidsparticipatie_67", "PercentageWerknemers_68", "PercentageZelfstandigen_69", "OpleidingsniveauLaag_64")]
names(new_df)
print(new_df)

# Gemeentenaam kolom verplaatsen naar rijnummer
row.names(new_df) <- new_df$Gemeentenaam_1

# Gemeentenaam kolom verwijdered
new_df <- new_df[, !names(new_df) %in% c("Gemeentenaam_1")]

# voor reproduceerbaarheid seed 123 gekozen
set.seed(123)

# data scalen --> zodat clusteren niet afhankelijk is van een variabele
new_df_scaled <- scale(new_df)
head(new_df_scaled)

# Max clusters op 10
n_clusters <- 10

# Sum of squares initialiseren
wss <- numeric(n_clusters)

n <- 10  # Aantal clusters selecteren 

# De verschillende aantallen clusters analyseren
for (i in 1:n) {
  # fit het model: km.out
  km.out <- kmeans(new_df_scaled, centers = i, nstart = 20)  # Change centers to i
  # within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# scree plot
wss_df <- tibble(clusters = 1:n, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')

scree_plot

scree_plot +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000', 9), '#FF0000')
  )

# Plot the 10 clusters -> smooth ellipse.type = "norm"
km.out <- kmeans(new_df_scaled, centers = 10, nstart = 20) 
fviz_cluster(km.out, new_df_scaled)

# Voeg de clusternummers toe aan een nieuwe kolom
df_with_cluster <- new_df
df_with_cluster$Cluster <- km.out$cluster

# Weergeef de nieuwe df met de clusters kolom
head(df_with_cluster)

# Tel het aantal rijen (Gemeenten) van iedere cluster in het kolom Cluster 
counts <- table(df_with_cluster$Cluster)

# Vind het minmum aantal
min_count <- min(counts)

# Filter de data om alleen de rijen te behouden met de minste aantal kolommen
filtered_df <- df_with_cluster[df_with_cluster$Cluster %in% names(counts[counts == min_count]), ]

# View the filtered data
print(filtered_df)

# # Geef weer bijv. alle gemeentes die in cluster 1, of een andere cluster
# cluster_assignments <- km.out$cluster
# 
# # Vind de indices van datapunten in Cluster 1
# cluster_1_indices <- which(cluster_assignments == 10)
# 
# # Haal de datapunten in Cluster 1 op uit de geschaalde data
# data_in_cluster_1 <- new_df_scaled[cluster_1_indices, ]
# 
# # Toon de datapunten in Cluster 1
# print(data_in_cluster_1)

# PCA om dimensionaliteit te verminderen tot 3 componenten
pca <- prcomp(new_df_scaled, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca$x[, 1:3])  # Keep the first three components

# Voeg clusterinfo toe
pca_data$Cluster <- as.factor(km.out$cluster)

# Creeër 3D scatterplot
plot_ly(data = pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, type = "scatter3d", mode = "markers+text", text = rownames(pca_data), textposition = 'top center') %>%
  layout(scene = list(title = "3D Scatterplot with Clusters (PCA)"))

# Bereken correlaties van alle paren van de kolommen 
correlation_matrix <- cor(new_df)

# Print de correlatie matrix
print(correlation_matrix)

# Print de correlatie matrix met drie decimalen
print(correlation_matrix, digits = 3)

# Sla de correlatie matrix op in een .csv bestand
write.csv(correlation_matrix, file = "correlation_matrix.csv")

# Set een correlatie "drempel" op 0.9
threshold <- 0.9

# Creeër een logische matrix met hoge correlaties
high_correlations <- abs(correlation_matrix) > threshold

# Weergeef de rij en kolom indices met hoge correlaties 
high_correlation_pairs <- which(high_correlations, arr.ind = TRUE)
print(high_correlation_pairs)

#Euclidean Distance
# Bereken de Euclidean distance
euclidean_dist <- dist(new_df)

# Print de Euclidean distance matrix
print(euclidean_dist)

# # Calculate summary statistics for the distances
# min_distance <- min(euclidean_dist)
# max_distance <- max(euclidean_dist)
# mean_distance <- mean(euclidean_dist)
# median_distance <- median(euclidean_dist)
# 
# # Print the summary statistics
# cat("Min Distance:", min_distance, "\n")
# cat("Max Distance:", max_distance, "\n")
# cat("Mean Distance:", mean_distance, "\n")
# cat("Median Distance:", median_distance, "\n")
# 
# # Convert the distance matrix to a data frame (optional, but can make the output more readable)
# distance_df <- as.data.frame(as.matrix(euclidean_dist))
# 
# # Define the file path where you want to save the CSV file
# file_path <- "euclidean_distances.csv"

# Top 10 kortste afstanden om te selecteren
top_n <- 10

# Zet het 'euclidean_dist'-object om naar een matrix
euclidean_mat <- as.matrix(euclidean_dist)

# Haal de rijnamen op
row_names <- row.names(euclidean_mat)

# Een lijst om de topafstanden en hun overeenkomstige rijnamen op te slaan
top_distances_with_names <- vector("list", length = nrow(euclidean_mat))

# Loop door elke rij om de topafstanden te vinden
for (i in 1:nrow(euclidean_mat)) {
  # Haal de Euclidische afstanden op voor de huidige rij
  row_distances <- euclidean_mat[i, ]
  
  # Sluit de afstand tot de huidige rij uit (nul afstand)
  row_distances <- row_distances[-i]
  
  # Vind de indices van de topafstanden
  top_indices <- order(row_distances)[1:top_n]
  
  # Haal de overeenkomstige rijnamen op voor de topafstanden
  top_row_names <- row_names[top_indices]
  
  # Sla de topafstanden en hun overeenkomstige rijnamen op in de lijst
  top_distances_with_names[[i]] <- data.frame(RowName = top_row_names, Distance = row_distances[top_indices])
}

# Print de top kortste Eucliden afstanden met de rij-namen (Gemeenten) for iedere Gemeente
for (i in 1:length(top_distances_with_names)) {
  cat("Row Name:", row_names[i], "\n")
  print(top_distances_with_names[[i]], row.names = FALSE)
  cat("\n")
}

# Check the current row names in your dataset
current_row_names <- rownames(euclidean_mat)
print(current_row_names)

cat("Row Names in Dataset:\n")
cat(paste0("'", rownames(euclidean_mat), "'\n"))


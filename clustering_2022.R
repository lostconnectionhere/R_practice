#Benodigde libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(factoextra)
library(plotly)

#Inlezen dataset uit 2022
df1 <- read.csv2('data/kerncijfers_22_pt1.csv', sep=";")
df2 <- read.csv2('data/kerncijfers_22_pt2.csv', sep=";")
df3 <- read.csv2('data/kerncijfers_22_pt3.csv', sep=";")

#Data analyseren
summary(df1)
summary(df2)
summary(df3)

# Hoeveel missende waarden per kolom
missing_count1 <- sapply(df1, function(x) sum(is.na(x)))
missing_count2 <- sapply(df2, function(x) sum(is.na(x)))
missing_count3 <- sapply(df3, function(x) sum(is.na(x)))

# Print het aantal missende waarden
print(missing_count1)
print(missing_count2)
print(missing_count3)

# Identify the column you want to exclude (e.g., "Name")
columns_to_exclude <- c("Gemeentenaam_1", "ID", "WijkenEnBuurten")

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

# De kolommen die uitgezonderd moeten worden
columns_to_exclude <- c("Gemeentenaam_1", "ID", "WijkenEnBuurten")

# Functie toepassen op iedere df
df1 <- convert_and_exclude(df1, columns_to_exclude)
df2 <- convert_and_exclude(df2, columns_to_exclude)
df3 <- convert_and_exclude(df3, columns_to_exclude)

# Controleer of het goed is gegaan
print(df1)
print(df2)
print(df3)

# Inner join df1 met df2 
innerJoinDf1 <- inner_join(df1, df2, by = "ID")

# Vervolgens, het resultaat joinen met df 3
innerJoinDf <- inner_join(innerJoinDf1, df3, by = "ID")

# View the resulting inner-joined data frame
#View(innerJoinDf)

# verwijder alle rijen die geen Gemeente zijn, behalve de rij met waardes voor NL
# De rijen filteren gebaserd op de gekozen regel
df_filtered <- subset(innerJoinDf, grepl("^GM", WijkenEnBuurten.x)| WijkenEnBuurten.x == "NL00")

# Print the filtered data frame
print(df_filtered)

#verwijder kolommen waarin bijna alle rijen NA zijn
new_df <- df_filtered[, !names(df_filtered) %in% c("ID", "WijkenEnBuurten.y", "Gemeentenaam_1.y", "WijkenEnBuurten", "WijkenEnBuurten.x", "Gemeentenaam_1","AfstandTotSchool_109", "ScholenBinnen3Km_110", "PersonenPerSoortUitkeringWW_85", 
                                                   "PersonenPerSoortUitkeringAOW_86", "GemGestandaardiseerdInkomenVanHuish_75", "PersonenPerSoortUitkeringBijstand_83", "PersonenPerSoortUitkeringAO_84", "OpleidingsniveauMiddelbaar_65",
                                                   "OpleidingsniveauMiddelbaar_65", "OpleidingsniveauHoog_66", "Nettoarbeidsparticipatie_67", "PercentageWerknemers_68", "PercentageZelfstandigen_69", "OpleidingsniveauLaag_64")]
names(new_df)
print(new_df)

# Gemeentenaam kolom verplaatsen naar rijnummer
row.names(new_df) <- new_df$Gemeentenaam_1.x

# Gemeentenaam kolom verwijdered
new_df <- new_df[, !names(new_df) %in% c("Gemeentenaam_1.x")]

# voor reproduceerbaarheid seed 123 gekozen
set.seed(123)

# data scalen --> zodat clusteren niet afhankelijk is van een variabele
new_df_scaled <- scale(new_df)
head(new_df_scaled)

# Max clusters op 10
n_clusters <- 10

# Sum of squares initialiseren
wss <- numeric(n_clusters)

n <- 10  # Change this to 10 for 10 clusters

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
head(new_df)

# Vind de cluster met het minste aantal datapunten (rijen) 
min_count_cluster <- which.min(table(new_df$Cluster))

# Filter de df om alleen de rijen (Gemeenten) te zien met het minste aantal datapunten 
cluster_data <- new_df[new_df$Cluster == min_count_cluster, ]

# Laat de gemeentes zien 
print(cluster_data)

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



# Specify the target row name (with or without spaces)
target_row_name <- "Aa en Hunze                             "

# Clean up the target row name by removing leading and trailing spaces
target_row_name <- trimws(target_row_name)

# Convert the 'euclidean_dist' object to a matrix
euclidean_mat <- as.matrix(euclidean_dist)

# Get the row names
row_names <- row.names(euclidean_mat)

# Find the index of the target row
target_row_index <- which(row_names == target_row_name)

# Check if the target row name exists in the data
if (length(target_row_index) == 0) {
  cat("Row name not found: ", target_row_name, "\n")
} else {
  # Get the Euclidean distances for the target row
  row_distances <- euclidean_mat[target_row_index, ]
  
  # Exclude the distance to the target row itself (zero distance)
  row_distances <- row_distances[-target_row_index]
  
  # Find the indices of the top distances
  top_indices <- order(row_distances)[1:10]
  
  # Get the corresponding row names for the top distances
  top_row_names <- row_names[top_indices]
  
  # Print the top distances for the target row
  cat("Row Name:", target_row_name, "\n")
  print(data.frame(RowName = target_row_name, Distance = row_distances[top_indices], TopRowName = top_row_names), row.names = FALSE)
}


# Specify the target row name with leading/trailing spaces
target_row_name <- "Aa en Hunze                             "

# Clean up the target row name by removing leading and trailing spaces
target_row_name <- trimws(target_row_name)

# Convert the 'euclidean_dist' object to a matrix
euclidean_mat <- as.matrix(euclidean_dist)

# Get the row names
row_names <- row.names(euclidean_mat)

# Find the index of the target row
target_row_index <- which(row_names == target_row_name)

# Check if the target row name exists in the data
if (length(target_row_index) == 0) {
  cat("Row name not found: ", target_row_name, "\n")
} else {
  # The target row name was found
  # Continue with your code to retrieve and print the top distances
}


class(target_row_name)

cat("Row Names in Dataset:\n")
cat(paste0("'", rownames(euclidean_mat), "'\n"))

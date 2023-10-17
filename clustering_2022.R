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
  km.out <- kmeans(new_df_scaled, centers = i, nstart = 20)
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

# Filter data points that belong to Cluster 3
cluster_to_plot <- 3
data_in_cluster_3 <- new_df_scaled[km.out$cluster == cluster_to_plot, ]

# Plot all 10 clusters with smoothed ellipses
fviz_cluster(km.out, data = new_df_scaled, ellipse.type = "norm", geom = "point")

# Add points from Cluster 3 (different color for emphasis)
fviz_cluster(km.out, data = data_in_cluster_3, ellipse.type = "none", geom = "point", add = TRUE, pointshape = 19, fill = "red")

#fviz_cluster(km.out, data = new_df_scaled, ellipse.type = "euclid")

# Voeg de clusternummers toe aan een nieuwe kolom
df_with_cluster <- new_df
df_with_cluster$Cluster <- km.out$cluster

# Weergeef de nieuwe df met de clusters kolom
head(df_with_cluster)

# Tel het aantal rijen (Gemeenten) van iedere cluster in het kolom Cluster 
counts <- table(df_with_cluster$Cluster)

# Vind het minmum aantal & maximum aantal in één cluster
min_count <- min(counts)
max_count <- max(counts)

# Filter de data om alleen de rijen te behouden met het minste & meeste aantal kolommen
filtered_min_cluster <- df_with_cluster[df_with_cluster$Cluster %in% names(counts[counts == min_count]), ]
filtered_max_cluster <- df_with_cluster[df_with_cluster$Cluster %in% names(counts[counts == max_count]), ]

# Weergeef de filtered data
print(filtered_min_cluster)
print(filtered_max_cluster)

# # Vind rijen die bij 'Cluster' 2 horen
# rows_with_cluster_2 <- df_with_cluster[df_with_cluster$Cluster == 2, ]
# 
# # Weergeef de rijen
# print(rows_with_cluster_2)

# Creeër een subset df voor iedere cluster (1:10)
cluster_subsets <- lapply(1:10, function(cluster_num) {
  subset(df_with_cluster, Cluster == cluster_num, select = c("Cluster"))
})

# Print de subsets van de clusters
for (cluster_num in 1:10) {    
  cat("Cluster", cluster_num, ":\n")
  print(cluster_subsets[[cluster_num]])
  cat("\n")
}

# Creeër een lijst om subsets van de df op te slaan 
cluster_subsets <- list()

# Creeër en sla de subset dataframes op voor de clusters 1:10
for (cluster_num in 1:10) {
  subset_df <- subset(df_with_cluster, Cluster == cluster_num, select = c( "Cluster"))
  cluster_subsets[[cluster_num]] <- subset_df
  colnames(subset_df)[0] <- "Gemeente"
  
  # Print en sla de subset op
  cat("Cluster", cluster_num, ":\n")
  print(subset_df)
  cat("\n")
  
  # Sla de subset op als CSV- bestand
  write.csv(subset_df, file = file.path("data", paste0("KMeans_Cluster_", cluster_num, "_subset.csv")), row.names = TRUE)
}

# PCA om dimensionaliteit te verminderen tot 3 componenten
pca <- prcomp(new_df_scaled, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca$x[, 1:3])  # Keep the first three components

# Voeg clusterinfo toe
pca_data$Cluster <- as.factor(km.out$cluster)

# Creeër 3D scatterplot
plot_ly(data = pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, type = "scatter3d", mode = "markers+text", text = rownames(pca_data), textposition = 'top center') %>%
  layout(scene = list(title = "3D Scatterplot with Clusters (PCA)"))

# Filter the data for a specific cluster (e.g., Cluster 1)
cluster_number <- 3
filtered_data <- pca_data[pca_data$Cluster == cluster_number, ]

# Create a 3D scatterplot for the filtered data
plot_ly(data = filtered_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, type = "scatter3d", mode = "markers+text", text = rownames(filtered_data), textposition = 'top center') %>%
  layout(scene = list(title = "3D Scatterplot for Cluster 1 (PCA)"))


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

# # Top 10 kortste afstanden om te selecteren
# top_n <- 10
# 
# # Zet het 'euclidean_dist'-object om naar een matrix
# euclidean_mat <- as.matrix(euclidean_dist)
# 
# # Haal de rijnamen op
# row_names <- row.names(euclidean_mat)
# 
# # Een lijst om de topafstanden en hun overeenkomstige rijnamen op te slaan
# top_distances_with_names <- vector("list", length = nrow(euclidean_mat))
# 
# # Loop door elke rij om de topafstanden te vinden
# for (i in 1:nrow(euclidean_mat)) {
#   # Haal de Euclidische afstanden op voor de huidige rij
#   row_distances <- euclidean_mat[i, ]
#   
#   # Sluit de afstand tot de huidige rij uit (nul afstand)
#   row_distances <- row_distances[-i]
#   
#   # Vind de indices van de topafstanden
#   top_indices <- order(row_distances)[1:top_n]
#   
#   # Haal de overeenkomstige rijnamen op voor de topafstanden
#   top_row_names <- row_names[top_indices]
#   
#   # Sla de topafstanden en hun overeenkomstige rijnamen op in de lijst
#   top_distances_with_names[[i]] <- data.frame(RowName = top_row_names, Distance = row_distances[top_indices])
# }


# Manhattan distance

# # Calculate Manhattan distance between each vector in the matrix
# manhattan_distances <- as.matrix(dist(new_df, method = "manhattan"))
# 
# # Number of closest neighbors to find
# num_closest <- 10
# 
# # Initialize a list to store the results
# top_closest_per_row <- vector("list", length = nrow(manhattan_distances))
# 
# # Loop through each row and find the top closest rows
# for (i in 1:nrow(manhattan_distances)) {
#   row_distances <- manhattan_distances[i, ]
#   sorted_indices <- order(row_distances)
#   top_indices <- sorted_indices[1:num_closest]
#   top_closest_per_row[[i]] <- data.frame(RowName = rownames(new_df)[top_indices], Distance = row_distances[top_indices])
# }
# 
# # Print the top 10 closest distances for each row with row names
# for (i in 1:nrow(manhattan_distances)) {
#   cat("Top 10 closest distances for", rownames(new_df)[i], ":\n")
#   print(top_closest_per_row[[i]])
#   cat("\n")
# }


# Functie om de dichtsbijzijnde gemeentes te vinden voor een specifieke gemeente (rij)
print_top_10_dichtstbijzijnde_manhattan_afstanden <- function(row_name) {
  # Bereken Manhattan afstand voor iedere vector in de matrix
  manhattan_distances <- as.matrix(dist(new_df, method = "manhattan"))
  
  # Aantal dichtsbijzijnde gemeenten
  num_closest <- 344
  
  # Vind de rijne index voor een specifieke rij
  row_index <- which(rownames(new_df) == row_name)
  
  if (length(row_index) == 0) {
    cat("Rij naam is niet gevonden:", row_name, "\n")
  } else {
    # Initialiseer een lijst om de resultaten op te slaan
    top_closest_per_row <- vector("list", length = nrow(manhattan_distances))
    
    # Loop door iedere rij en vind de dichtsbijzijnde rijen
    for (i in 1:nrow(manhattan_distances)) {
      row_distances <- manhattan_distances[i, ]
      sorted_indices <- order(row_distances)
      top_indices <- sorted_indices[1:(num_closest + 1)]  # Inclusief één extra voor zichzelf
      top_indices <- top_indices[top_indices != i]  # Uitsluiten van zichzelf
      top_closest_per_row[[i]] <- data.frame(Gemeente = rownames(new_df)[top_indices], Afstand = row_distances[top_indices])
    }
    
    # Print de top 5 
    cat("Top 5 vergelijkbare gemeenten voor de gemeente", row_name, ":\n")
    print(top_closest_per_row[[row_index]])
    cat("\n")
  }
}

# Zoek de bijbehorende gemeentes voor een gemeente X
print_top_10_dichtstbijzijnde_manhattan_afstanden("Haarlem")

# Functie om de dichtsbijzijnde gemeentes te vinden voor een specifieke gemeente (rij)
print_manhattan_afstanden_percentages <- function(row_name) {
  # Bereken Manhattan afstand voor iedere vector in de matrix
  manhattan_distances <- as.matrix(dist(new_df, method = "manhattan"))

  # Aantal dichtsbijzijnde gemeenten
  num_closest <- 344

  # Vind de rijne index voor een specifieke rij
  row_index <- which(rownames(new_df) == row_name)

  if (length(row_index) == 0) {
    cat("Rij naam is niet gevonden:", row_name, "\n")
  } else {
    # Initialiseer een lijst om de resultaten op te slaan
    top_closest_per_row <- vector("list", length = nrow(manhattan_distances))

    # Loop door iedere rij en vind de dichtsbijzijnde rijen
    for (i in 1:nrow(manhattan_distances)) {
      row_distances <- manhattan_distances[i, ]
      sorted_indices <- order(row_distances)
      top_indices <- sorted_indices[1:(num_closest + 1)]  # Inclusief één extra voor zichzelf
      top_indices <- top_indices[top_indices != i]  # Uitsluiten van zichzelf

      # Calculate percentages based on Manhattan distances
      min_distance <- min(row_distances[top_indices])
      max_distance <- max(row_distances[top_indices])
      distances <- row_distances[top_indices]
      percentages <- 100 - (distances - min_distance) / (max_distance - min_distance) * 100

      top_closest_per_row[[i]] <- data.frame(
        Gemeente = rownames(new_df)[top_indices],
        Afstand = row_distances[top_indices],
        Percentage = percentages
      )
    }

    # Print the top 5
    cat("Top 5 vergelijkbare gemeenten voor de gemeente", row_name, ":\n")
    print(top_closest_per_row[[row_index]])
    cat("\n")
  }
}

# Zoek de bijbehorende gemeentes voor een gemeente X
print_manhattan_afstanden_percentages("Haarlem")



# print_verste_gemeente <- function(row_name) {
#   # Bereken Manhattan afstand voor iedere vector in de matrix
#   manhattan_distances <- as.matrix(dist(new_df, method = "manhattan"))
#   
#   # Vind de rij-index voor een specifieke rij
#   row_index <- which(rownames(new_df) == row_name)
#   
#   if (length(row_index) == 0) {
#     cat("Rij naam is niet gevonden:", row_name, "\n")
#   } else {
#     # Haal de hoogste Manhattan-afstand op
#     max_distance <- max(manhattan_distances[row_index, ])
#     
#     # Vind de gemeente(s) met de hoogste Manhattan-afstand
#     farthest_communities <- rownames(new_df)[manhattan_distances[row_index, ] == max_distance]
#     
#     # Print de verste gemeente(s)
#     cat("De verste gemeente(s) van", row_name, "met een Manhattan-afstand van", max_distance, "zijn:")
#     cat(farthest_communities, sep = ", ")
#     cat("\n")
#   }
# }
# 
# print_verste_gemeente("Haarlem")

# Function to categorize percentages into specified categories
# categorize_percentage <- function(percentage) {
#   if (percentage >= 1 && percentage <= 14) {
#     return("Categorie 1")
#   } else if (percentage >= 15 && percentage <= 28) {
#     return("Categorie 2")
#   } else if (percentage >= 29 && percentage <= 42) {
#     return("Categorie 3")
#   } else if (percentage >= 43 && percentage <= 56) {
#     return("Categorie 4")
#   } else if (percentage >= 57 && percentage <= 70) {
#     return("Categorie 5")
#   } else if (percentage >= 71 && percentage <= 84) {
#     return("Categorie 6")
#   } else if (percentage >= 85 && percentage <= 100) {
#     return("Categorie 7")
#   }
# }
# 
# # Function to find the farthest community for a specific row
# print_verste_gemeente <- function(row_name) {
#   # Calculate Manhattan distance for each vector in the matrix
#   manhattan_distances <- as.matrix(dist(new_df, method = "manhattan"))
#   
#   # Find the row index for a specific row
#   row_index <- which(rownames(new_df) == row_name)
#   
#   if (length(row_index) == 0) {
#     cat("Rij naam is niet gevonden:", row_name, "\n")
#   } else {
#     # Get the maximum Manhattan distance
#     max_distance <- max(manhattan_distances[row_index, ])
#     
#     # Find the community(ies) with the maximum Manhattan distance
#     farthest_communities <- rownames(new_df)[manhattan_distances[row_index, ] == max_distance]
#     
#     # Calculate the percentage based on the maximum distance
#     percentage <- (max_distance / max(manhattan_distances)) * 100
#     categorie <- categorize_percentage(percentage)
#     
#     # Print the farthest community(ies) and the categorized percentage
#     cat("De verste gemeente(s) van", row_name, "met een Manhattan-afstand van", max_distance, "zijn:\n")
#     cat(farthest_communities, sep = ", ")
#     cat("\n")
#     cat("De gecategoriseerde percentage is:", percentage, "en behoort tot", categorie, "\n")
#   }
# }
# 
# # Call the function to find the farthest community
# print_verste_gemeente("Haarlem")


# Euclidean
# Definieer de functie om de top 10 dichtstbijzijnde Euclidische afstanden te vinden en af te drukken voor een specifieke rijnaam
print_top_10_dichtstbijzijnde_euclidische_afstanden <- function(rij_naam) {
  # Bereken de Euclidische afstand tussen elke vector in de matrix
  euclidische_afstanden <- as.matrix(dist(new_df, method = "euclidean"))
  
  # Aantal dichtstbijzijnde buren om te vinden
  num_dichtstbijzijnde <- 5
  
  # Vind de rij-index voor de opgegeven rijnaam
  rij_index <- which(rownames(new_df) == rij_naam)
  
  if (length(rij_index) == 0) {
    cat("Rijnaam niet gevonden:", rij_naam, "\n")
  } else {
    # Initialiseer een lijst om de resultaten op te slaan
    top_dichtstbijzijnde_per_rij <- vector("list", length = nrow(euclidische_afstanden))
    
    # Loop door elke rij en vind de top dichtstbijzijnde rijen
    for (i in 1:nrow(euclidische_afstanden)) {
      rij_afstanden <- euclidische_afstanden[i, ]
      gesorteerde_indices <- order(rij_afstanden)
      top_indices <- gesorteerde_indices[1:(num_dichtstbijzijnde + 1)] # Inclusief één extra voor zichzelf
      top_indices <- top_indices[top_indices != i]  # Uitsluiten van zichzelf
      top_dichtstbijzijnde_per_rij[[i]] <- data.frame(RijNaam = rownames(new_df)[top_indices], Afstand = rij_afstanden[top_indices])
    }
    
    # Druk de top 10 dichtstbijzijnde Euclidische afstanden af voor de opgegeven rijnaam
    cat("Top 5 dichtstbijzijnde Euclidische afstanden voor", rij_naam, ":\n")
    print(top_dichtstbijzijnde_per_rij[[rij_index]])
    cat("\n")
  }
}

# Roep de functie aan met de specifieke rijnaam "Amsterdam" voor Euclidische afstand
print_top_10_dichtstbijzijnde_euclidische_afstanden("Haarlem")






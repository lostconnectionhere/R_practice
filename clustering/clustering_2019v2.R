# Benodigde libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(factoextra)
library(plotly)

# Data inlezen https://opendata.cbs.nl/statline/#/CBS/nl/dataset/60039fvw/table?ts=1698238503560 - Maatstaven Financiële-verhoudingswet (Fvw)
df1 <- read.csv2('data/FVW_2019.csv', sep=";")

# Data analyseren
head(df1) 

# Kolomnamen wijzigen voor leesbaarheid
colnames(df1)[colnames(df1) == "X"] <- "Gemeente"
colnames(df1)[colnames(df1) == "Inwoners.naar.geslacht.en.leeftijd.Totaal.mannen.en.vrouwen.Totaal.mannen.en.vrouwen"] <- "Inwoners.Totaal"

# Verwijder de laatste 4 rijen, die staan voor irrelevante data bijv. Buitenland
df1 <- head(df1, n = nrow(df1) - 4)

# Verwijder "(..)" van de rijnamen
# df1$Gemeente <- sub("\\s?\\(.*\\)", "", df1$Gemeente)
df1$Gemeente <- sub(" \\(gemeente\\)$", "", df1$Gemeente)
print(df1$Gemeente)

# Gemeentenaam kolom verplaatsen naar rijnummer
row.names(df1) <- df1$Gemeente

# Verwijder alle gemeenten die NA hebben voor aantal inwoners
na_rows <- is.na(df1$Inwoners.Totaal)
print(df1[na_rows,])
df1 <- df1[!na_rows, ]

# Hoeveel missende waarden per kolom?
missing_values <- colSums(is.na(df1))
print(missing_values)

# Verwijder de kolommen die leeg zijn
columns_to_delete <- c("Gemeente", "Oppervlakten.Bedrijfsterreinen", "Banen.van.werknemers", "Periodieke.bijstandsuitkeringen.Adreslozen")
new_df <- df1[, !(names(df1) %in% columns_to_delete)]
names(new_df)

# Functie om leading en trailing spaties te verwijderen voor character- of factor-kolommen
trim_character_or_factor <- function(new_df) {
  new_df[] <- lapply(new_df, function(col) {
    if (is.character(col) || is.factor(col)) {
      return(trimws(as.character(col)))
    } else {
      return(col)
    }
  })
  return(new_df)
}

# Pas de functie toe op je dataframes df1, df2, en df3
new_df <- trim_character_or_factor(new_df)

# Hoeveel punten bij iedere kolom aanwezig
apply(new_df, 2,  function(col) sum(col == '.'))

# Alle punten omzetten in NA's
new_df <- new_df %>% mutate(across(.cols = everything(), ~replace(., . == '.', NA)))

# Hoeveel missende waarden per kolom
sapply(new_df, function(x) sum(is.na(x)))

# Welke class hebben alle kolommen
sapply(new_df, class)

# voor reproduceerbaarheid seed 123 gekozen
set.seed(123)

# Data scaling, dit kan op meerdere manieren
library(caret)

# Min-Max scaling (Normalization)
min_max_scaled_data <- as.data.frame(scale(new_df, center = FALSE, scale = apply(new_df, 2, max) - apply(new_df, 2, min)))

# Standardization (Z-score scaling)
standardized_data <- as.data.frame(scale(new_df))

# Normalization (0 to 1)
normalized_data <- predict(preProcess(new_df, method = "range"), newdata = new_df)
# normalized_data <- preProcess(new_df, method = c("range"))

#KMeans
# Max clusters op 10
n_clusters <- 10

# Sum of squares initialiseren
wss <- numeric(n_clusters)

# De verschillende aantallen clusters analyseren - min_max_scaled_data
for (i in 1:n_clusters) {
  # fit het model: km.out_minmax
  km.out_minmax <- kmeans(min_max_scaled_data, centers = i, nstart = 20)
  wss[i] <- km.out_minmax$tot.withinss
}

# De verschillende aantallen clusters analyseren - standardized_data
for (i in 1:n_clusters) {
  # fit het model: km.out_standardized
  km.out_standardized <- kmeans(standardized_data, centers = i, nstart = 20)
  wss[i] <- km.out_standardized$tot.withinss
}

# De verschillende aantallen clusters analyseren - normalized_data
for (i in 1:n_clusters) {
  # fit het model: km.out_standardized
  km.out_normalized <- kmeans(normalized_data, centers = i, nstart = 20)
  wss[i] <- km.out_normalized$tot.withinss
}

# Scree plot minmax
wss_df_minmax <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot_minmax <- ggplot(wss_df_minmax, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')

scree_plot_minmax

# Add a horizontal dashed line for the WSS values
scree_plot_minmax +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000', n_clusters - 1), '#FF0000')
  )

# Scree plot standardized_data
wss_df_standardized <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot_standardized <- ggplot(wss_df_standardized, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')

scree_plot_minmax

# Add a horizontal dashed line for the WSS values
scree_plot_standardized +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000', n_clusters - 1), '#FF0000')
  )

# Plot the 10 clusters -> smooth ellipse.type = "norm" - min_max_scaled_data
cluster_plot_ <- fviz_cluster(km.out_minmax, data = min_max_scaled_data)
cluster_plot_minmax

# Plot the 10 clusters -> smooth ellipse.type = "norm" - standardized_data
cluster_plot_standardized <- fviz_cluster(km.out_standardized, data = standardized_data)
cluster_plot_standardized

# Plot the 10 clusters -> smooth ellipse.type = "norm" - normalized_data
cluster_plot_normalized <- fviz_cluster(km.out_standardized, data = normalized_data)
cluster_plot_normalized

# Cluster plot(s) opslaan 
ggsave(file.path("visualisations","cluster_plot_minmax.png"), plot = cluster_plot_minmax, width = 23, height = 15, dpi = 300)
ggsave(file.path("visualisations","cluster_plot_standardized.png"), plot = cluster_plot_standardized, width = 23, height = 15, dpi = 300)
ggsave(file.path("visualisations","cluster_plot_normalized.png"), plot = cluster_plot_normalized, width = 23, height = 15, dpi = 300)


# Voeg de clusternummers toe aan een nieuwe kolom
# min_max_scaled_data_with_cluster <- new_df
new_df$Cluster_minmax <- km.out_minmax$cluster
new_df$Cluster_standardized <-km.out_standardized$cluster
new_df$Cluster_normalized <- km.out_normalized$cluster

# MinMax clusters
# Creeër een subset df voor iedere cluster (1:10)
cluster_subsets <- lapply(1:10, function(cluster_num) {
  subset(new_df, Cluster_minmax == cluster_num, select = c("Cluster_minmax"))
})

# Print de subsets van de clusters
for (cluster_num in 1:10) {    
  cat("Cluster_minmax", cluster_num, ":\n")
  print(cluster_subsets[[cluster_num]])
  cat("\n")
}

# Creeër een lijst om subsets van de df op te slaan 
cluster_subsets <- list()

# Creeër en sla de subset dataframes op voor de clusters 1:10
for (cluster_num in 1:10) {
  subset_df <- subset(new_df, Cluster_minmax == cluster_num, select = c( "Cluster_minmax"))
  cluster_subsets[[cluster_num]] <- subset_df
  colnames(subset_df)[0] <- "Gemeente"
  
  # Print en sla de subset op
  cat("Cluster", cluster_num, ":\n")
  print(subset_df)
  cat("\n")
  
  # Sla de subset op als CSV- bestand
  write.csv(subset_df, file = file.path("data", "cluster_numbers", "minmax", paste0("minmax_cluster", cluster_num, "_subset.csv")), row.names = TRUE)
}

# Standardized
# Creeër een subset df voor iedere cluster (1:10)
cluster_subsets <- lapply(1:10, function(cluster_num) {
  subset(new_df, Cluster_standardized == cluster_num, select = c("Cluster_standardized"))
})

# Print de subsets van de clusters
for (cluster_num in 1:10) {    
  cat("Cluster_standardized", cluster_num, ":\n")
  print(cluster_subsets[[cluster_num]])
  cat("\n")
}

# Creeër een lijst om subsets van de df op te slaan 
cluster_subsets <- list()

# Creeër en sla de subset dataframes op voor de clusters 1:10
for (cluster_num in 1:10) {
  subset_df <- subset(new_df, Cluster_standardized == cluster_num, select = c( "Cluster_standardized"))
  cluster_subsets[[cluster_num]] <- subset_df
  colnames(subset_df)[0] <- "Gemeente"
  
  # Print en sla de subset op
  cat("Cluster", cluster_num, ":\n")
  print(subset_df)
  cat("\n")
  
  # Sla de subset op als CSV- bestand
  write.csv(subset_df, file = file.path("data", "cluster_numbers", "standardized", paste0("standardized_cluster", cluster_num, "_subset.csv")), row.names = TRUE)
}

# Normalized
# Creeër een subset df voor iedere cluster (1:10)
cluster_subsets <- lapply(1:10, function(cluster_num) {
  subset(new_df, Cluster_normalized == cluster_num, select = c("Cluster_normalized"))
})

# Print de subsets van de clusters
for (cluster_num in 1:10) {    
  cat("Cluster_normalized", cluster_num, ":\n")
  print(cluster_subsets[[cluster_num]])
  cat("\n")
}

# Creeër een lijst om subsets van de df op te slaan 
cluster_subsets <- list()

# Creeër en sla de subset dataframes op voor de clusters 1:10
for (cluster_num in 1:10) {
  subset_df <- subset(new_df, Cluster_normalized == cluster_num, select = c( "Cluster_normalized"))
  cluster_subsets[[cluster_num]] <- subset_df
  colnames(subset_df)[0] <- "Gemeente"
  
  # Print en sla de subset op
  cat("Cluster", cluster_num, ":\n")
  print(subset_df)
  cat("\n")
  
  # Sla de subset op als CSV- bestand
  write.csv(subset_df, file = file.path("data", "cluster_numbers", "normalized", paste0("normalized_cluster", cluster_num, "_subset.csv")), row.names = TRUE)
}

names(new_df)

write.csv(new_df, "df.csv")

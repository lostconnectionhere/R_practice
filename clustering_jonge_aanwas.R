#Benodigde libraries
library(dplyr)
library(plyr)

#Inlezen dataset + analyseren
data_sub1 <- read.csv2('data/Dataset_subset1.csv', sep=";")
data_sub2 <- read.csv2('data/Dataset_subset2.csv', sep=";")
perc_risico_jongeren <- read.csv2('data/Percentage_hoog_risicojongeren.csv', sep=";")

print(data_sub1)
print(data_sub2)
print(perc_risico_jongeren)

#aggregrate op basis van waarde in de kolom Gemeentenaam
# aggregate(~Gemeentenaam_1, data = data_sub1, FUN=sum)
data_sub1_agg <- ddply(data_sub1, "Gemeentenaam_1", numcolwise(sum))
print(data_sub1_agg)

data_sub2_agg <- ddply(data_sub2, "Gemeentenaam_1", numcolwise(sum))
print(data_sub2_agg)

#Kolommen verwijderen
data_sub1_agg <- data_sub1_agg[, !names(data_sub1_agg) %in% c("ID")]
data_sub2_agg <- data_sub2_agg[, !names(data_sub2_agg) %in% c("ID")]
names(data_sub1_agg)
names(data_sub2_agg)

# Voeg de dataframes aan elkaar toe op basis vaan de gemeentenaam
df <- merge(data_sub1_agg, data_sub2_agg, by="Gemeentenaam_1")
# data_sub1_agg %>% mutate(Gemeentenaam_1 = Gemeentenaam_1$data_sub2_agg[match(Gemeentenaam_1$data_sub1_agg, gemeenten_namen$GemeentecodeGM)])
# print(df)
# colnames(df)

#Onderzoek of er lege rijen zijn
print("Totaal aantal missende waarden in de dataframe - ")
sum(is.na(df))
colSums(is.na(df))

# Gemeentenaam kolom verplaatsen naar rijnummer
row.names(df) <- df$Gemeentenaam_1

# Gemeentenaam kolom verwijdered
df <- df[, !names(df) %in% c("Gemeentenaam_1")]

# Controleren of dataframe alleen uit numerieke waarden bestaat
summary(df)

# voor reproduceerbaarheid seed 123 gekozen
set.seed(123)

# data scalen --> zodat clusteren niet afhankelijk is van een variabele
df_scaled <- scale(df)
head(df_scaled)


km.out <- kmeans(df_scaled, centers = 5, nstart = 20)
km.out

# Max clusters op 10
n_clusters <- 10

# Sum of squares initialiseren
wss <- numeric(n_clusters)

# De verschillende aantallen clusters analyseren
for (i in 1:n) {
  # fit het model: km.out
  km.out <- kmeans(df_scaled, centers = i, nstart = 20)
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
fviz_cluster(km.out, df_scaled, ellipse.type = "norm")



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
print(df1$Gemeente)

# Verwijder de laatste 4 rijen, die staan voor irrelevante data bijv. Buitenland
df1 <- head(df1, n = nrow(df1) - 4)
print(df1$Gemeente)

# Verwijder alle gemeenten die NA hebben voor aantal inwoners
na_rows <- is.na(df1$Inwoners.Totaal)
print(df1[na_rows,])
df1 <- df1[!na_rows, ]

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

# Hoeveel punten bij iedere kolom aanwezig
apply(df1, 2,  function(col) sum(col == '.'))

# Alle punten omzetten in NA's
df1 <- df1 %>% mutate(across(.cols = everything(), ~replace(., . == '.', NA)))

# Hoeveel missende waarden per kolom
sapply(df1, function(x) sum(is.na(x)))

# Welke class hebben alle kolommen
sapply(df1, class)

# Gemeentenaam kolom verplaatsen naar rijnummer
row.names(df1) <- df1$Gemeente

# Gemeentenaam kolom verwijdered
new_df <- df1[, !names(df1) %in% c("Gemeente")]
names(new_df)

# Data scaling, dit kan op meerdere manieren
library(caret)

# Min-Max scaling (Normalization)
min_max_scaled_data <- as.data.frame(scale(new_df, center = FALSE, scale = apply(new_df, 2, max) - apply(new_df, 2, min)))

# Standardization (Z-score scaling)
standardized_data <- as.data.frame(scale(new_df))

# Normalization (0 to 1)
normalized_data <- preProcess(new_df, method = c("range"))

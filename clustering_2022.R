#Benodigde libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(factoextra)

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

# voeg de dataframes samen met een inner join
# innerJoinDf <- inner_join(df1, df2, df3,by="ID")
# view(innerJoinDf)

# You can join df1 and df2 first
innerJoinDf1 <- inner_join(df1, df2, by = "ID")

# Then, join the result with df3
innerJoinDf <- inner_join(innerJoinDf1, df3, by = "ID")

# View the resulting inner-joined data frame
View(innerJoinDf)


# verwijder alle rijen die geen Gemeente zijn, behalve de rij met waardes voor NL
# De rijen filteren gebaserd op de gekozen regel
df_filtered <- subset(innerJoinDf, grepl("^GM", WijkenEnBuurten.x)| WijkenEnBuurten.x == "NL00")

# Print the filtered data frame
print(df_filtered)

#verwijder kolommen
new_df <- df_filtered[, !names(df_filtered) %in% c("ID", "WijkenEnBuurten.y", "Gemeentenaam_1.y", "WijkenEnBuurten", "Gemeentenaam_1","AfstandTotSchool_109", "ScholenBinnen3Km_110", "PersonenPerSoortUitkeringWW_85", 
                                                   "PersonenPerSoortUitkeringAOW_86", "GemGestandaardiseerdInkomenVanHuish_75", "PersonenPerSoortUitkeringBijstand_83", "PersonenPerSoortUitkeringAO_84", "OpleidingsniveauMiddelbaar_65",
                                                   "OpleidingsniveauMiddelbaar_65", "OpleidingsniveauHoog_66", "Nettoarbeidsparticipatie_67", "PercentageWerknemers_68", "PercentageZelfstandigen_69", "OpleidingsniveauLaag_64")]
names(new_df)
print(new_df)

#Benodigde libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(factoextra)

#Inlezen dataset + analyseren uit 2022
df1 <- read.csv2('data/algemeen_gemeenten.csv', sep=";")
df2 <- read.csv2('data/algemeen_gemeenten_statline.csv', sep=";")

names(df1)
summary(df1)
names(df2)
summary(df2)

#verwijder kolom "ID" en "WijkenEnBuurten"
df2 <- df2[, !names(df2) %in% c("ID", "WijkenEnBuurten")]
names(df2)

# #Kolom gemeentenaam tijdelijk opschuiven
# # Gemeentenaam kolom verplaatsen naar rijnummer
# row.names(df2) <- df2$Gemeentenaam_1
# 
# # Gemeentenaam kolom verwijdered
# df2 <- df2[, !names(df2) %in% c("Gemeentenaam_1")]

# Convert all character columns to numeric
# Excluding the 'Name' column from conversion
df2_char_to_num <- df2  # Create a copy of the original data frame

# Identify the column you want to exclude (e.g., "Name")
column_to_exclude <- "Gemeentenaam_1"

# Convert character columns to numeric except for the excluded column
for (col_name in colnames(df2_char_to_num)) {
  if (col_name != column_to_exclude && is.character(df2_char_to_num[[col_name]])) {
    df2_char_to_num[[col_name]] <- as.numeric(as.character(df2_char_to_num[[col_name]]))
  }
}

# Print the resulting data frame
print(df2_char_to_num)

# Check the result
print(df2_char_to_num)
summary(df2_char_to_num)
df2 <- df2_char_to_num

summary(df2)

df2$Gemeenten <- df2$Gemeentenaam_1
df2$Gemeentenaam_1 <- NULL

#Controleer op null-waarden
print("Totaal aantal missende waarden in de dataframe - ")
sum(is.na(df1))
sum(is.na(df2))

#voeg df1 aan df2 op basis van gemeentenaam
merged_df <- merge(df2, df1, by="Gemeenten", all.x=TRUE)

na.omit(df)
sum(is.na(df))

unique(df1$Gemeenten)
unique(df2$Gemeenten)



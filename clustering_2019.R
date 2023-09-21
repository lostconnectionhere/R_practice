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

# Create a logical condition to check if values in "Gemeentenaam_1" start with "GM"
condition <- grepl("^GM", df2$WijkenEnBuurten)

# Subset the data frame based on the condition
result_df2 <- df2[condition, ]

# Print the resulting data frame
print(result_df2)

#verwijder kolom "ID" en "WijkenEnBuurten"
df2 <- result_df2[, !names(df2) %in% c("ID", "WijkenEnBuurten")]
names(df2)

# Convert all character columns to numeric
df2_char_to_num <- sapply(df2, function(x) as.numeric(as.character(x)))

# Check the result
print(df2_char_to_num)
summary(df2_char_to_num)
df2 <- df2_char_to_num

#Controleer op null-waarden
print("Totaal aantal missende waarden in de dataframe - ")
sum(is.na(df1))
sum(is.na(df2))

#voeg df1 aan df2 op basis van gemeentenaam
df <- merge(df1, df2, by.x="Gemeenten", by.y="Gemeentenaam_1", all.x=TRUE)
na.omit(df)
sum(is.na(df))


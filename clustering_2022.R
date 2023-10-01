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

# Kopie van df maken als test
df1_char_to_num <- df1  

# Identify the column you want to exclude (e.g., "Name")
columns_to_exclude <- c("Gemeentenaam_1", "ID", "WijkenEnBuurten")

# # Vervang character kolommen voor numerieke, behalve de kolommen die hierboven staan
# for (col_name in colnames(df1_char_to_num)) {
#   if (col_name %in% columns_to_exclude) {
#     next
#   }
#   
#   if (is.character(df1_char_to_num[[col_name]])) {
#     df1_char_to_num[[col_name]] <- as.numeric(df1_char_to_num[[col_name]])
#   }
# }
# 
# summary(df1_char_to_num)


# Functie die character kolommen veranders naar numerieke, behalve een aantal kolommen
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

# Apply the function to each data frame
df1 <- convert_and_exclude(df1, columns_to_exclude)
df2 <- convert_and_exclude(df2, columns_to_exclude)
df3 <- convert_and_exclude(df3, columns_to_exclude)

# Print the modified data frames
print(df1)
print(df2)
print(df3)

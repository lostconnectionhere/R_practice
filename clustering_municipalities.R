library(tidyverse)
library(tidyr)

#Inlezen dataset
kerncijfers <- read.csv2('data/regionale_kerncijfers_subset.csv', sep=";")
print(kerncijfers)

colnames(kerncijfers) 

# kerncijfers[, c('Perioden')]

#Clusters op basis van data uit het jaar 2022
kerncijfers_2022 <- kerncijfers[kerncijfers$Perioden %in% '2022JJ00',]

#Controleren of in de kolom Perioden alleen 2022 staat
# dim(kerncijfers_2022)
kerncijfers_2022[, c('Perioden')]

#Onderzoek of er lege rijen zijn
print("Totaal aantal missende waarden - ")
sum(is.na(kerncijfers_2022))

colSums(is.na(kerncijfers_2022))
kerncijfers_2022 <- na.omit(kerncijfers_2022)
colSums(is.na(kerncijfers_2022))

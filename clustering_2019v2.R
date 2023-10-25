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

#Get Missing Plants List
## Written by Mia Akbar, Apr 2023

library(dplyr)

Data <- read.csv("Data/Clean/semiclean_heightdat.csv")

MissingData <- Data %>% 
  dplyr::filter(is.na(stem_length) & is.na(bud_length) & is.na(flower_length) & is.na(fruit_length))

MissingPlants <- unique(MissingData$ind)

write.csv(MissingPlants, "Data/MissingPlants.csv", row.names = FALSE)



#Get missing plants 2020
#Written by Mia Akbar, Feb 2023 
library(dplyr)

Data <- read.csv("Data/2020_Lythrum_Weights.csv")
Data <- Data[-grep("^M", Data$Block), ] #remove M blocks
unique(Data$Missing)

Data <- Data %>% 
  mutate(Ind = paste(Block, Pos, sep = "-")) %>% 
  select(Ind, everything())


MissingPlants <- Data[Data$Missing == 1, "Ind", drop=FALSE]

write.csv(MissingPlants, "Data/MissingPlants.csv", row.names = FALSE)

# Cleaning Weights 
# Written by Mia Akbar, 2023

library(tidyverse)

dat <- read.csv("Data/LythrumWeights2021_Data.csv")

dat <- dat %>% 
  mutate(Ind = as.factor(paste(Block, Pos, sep = "-")))

#find mean empty bag weight
emptybag <- dat %>% 
  dplyr::filter(Missing==1)
emptybag$Weight <- as.numeric(emptybag$Weight)
summary(emptybag)
meanbag <- sum(emptybag$Weight, na.rm = T)/34

dat <- dat %>% 
  select(Block, Pos, Ind, Weight)  %>% 
  rename(rawWeight = Weight) # only ones we care about right now are block, pos, and weight (fitness)

datc <- dat %>% 
  mutate(Weight = rawWeight - meanbag)
datc$Weight <- ifelse(datc$Weight < 0, 0, datc$Weight)

WeightNA <- dat %>% 
  filter(is.na(rawWeight)) %>% 
  filter(Block == "E3" | Block == "E6" | Block == "E10") # n = 0 ; this means the only NAs are likely due to no harvestable fruit. This makes life easy. 

datc <- datc %>% 
  mutate(Weight = ifelse(is.na(Weight), 0, Weight))

write.csv(datc, "Data/Clean/2021FitnessClean.csv", row.names=F)

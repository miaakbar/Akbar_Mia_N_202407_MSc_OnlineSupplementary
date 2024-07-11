# Cleaning Weights 
# Written by Mia Akbar, 2024

library(tidyverse)

dat <- read.csv("Data/LythrumWeights2023.csv")

unique(dat$Missing)

dat <- dat %>% 
  mutate(Ind = paste(Block, Pos, sep = "-")) %>% 
  select(Ind, everything())

#find mean empty bag weight
emptybag <- dat %>% 
  dplyr::filter(Missing==1)
emptybag$Weight <- as.numeric(emptybag$Weight)
summary(emptybag)

#remove individuals that had problems in `NOTES`
dat <- dat[!dat$Ind == "E3-49",]
dat <- dat[!dat$Ind == "E3-50",]
dat <- dat[!dat$Ind == "E6-20",]
dat <- dat[!dat$Ind == "E6-18",]

#summarize empty bag weight
emptybag <- dat %>% 
  dplyr::filter(Missing==1)
emptybag$Weight <- as.numeric(emptybag$Weight)
summary(emptybag)

meanbag <- sum(emptybag$Weight, na.rm = T)/37

dat <- dat %>% 
  dplyr::select(Block, Pos, Ind, Weight) # only ones we care about right now are block, pos, and weight (fitness)


datc <- dat %>% 
  mutate(Weight = Weight - meanbag)
datc$Weight <- ifelse(datc$Weight < 0, 0, datc$Weight)

WeightNA <- datc %>% 
  filter(is.na(Weight)) %>% 
  filter(Block == "E3" | Block == "E6" | Block == "E10") # n = 0 ; this means the only NAs are likely due to no harvestable fruit. This makes life easy. 

datc$Weight[is.na(datc$Weight)] <- 0 #remove this when u add the proper meanbagweight for 2023


write.csv(datc, "Data/Clean/2023FitnessClean.csv", row.names=F)

# Cleaning Weights 
# Written by Mia Akbar, 2023

library(tidyverse)

dat <- read.csv("Data/LythrumWeights2022.csv")

dat <- dat %>% 
  rename(Weight=Weight..g.) %>% 
  mutate(Ind = as.factor(paste(Block, Pos, sep = "-")))

#find mean empty bag weight
emptybag <- dat %>% 
  dplyr::filter(Missing==1)
emptybag$Weight <- as.numeric(emptybag$Weight)
summary(emptybag)

#temporarily remove individuals that dont make any sense 
dat <- dat[!dat$Ind == "E6-39",]
emptybag <- dat %>% 
  dplyr::filter(Missing==1)
emptybag$Weight <- as.numeric(emptybag$Weight)
summary(emptybag)

meanbag <- sum(emptybag$Weight, na.rm = T)/118

dat <- dat %>% 
  dplyr::select(Block, Pos, Weight)  %>% 
  rename(rawWeight = Weight) # only ones we care about right now are block, pos, and weight (fitness)

datc <- dat %>% 
  mutate(Weight = rawWeight - meanbag)
datc$Weight <- ifelse(datc$Weight < 0, 0, datc$Weight)

WeightNA <- dat %>% 
  filter(is.na(rawWeight)) # n = 0 which makes life very easy. 

write.csv(datc, "Data/Clean/2022FitnessClean.csv", row.names=F)

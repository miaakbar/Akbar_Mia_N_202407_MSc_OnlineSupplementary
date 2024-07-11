# Cleaning Weights 
# Written by Mia Akbar, 2023

library(tidyverse)

dat <- read.csv("Data/Raw/2018_Lythrum_Weights_E.csv")

#find mean empty bag weight
emptybag <- dat %>% 
  dplyr::filter(Missing==1)
emptybag$Weight <- as.numeric(emptybag$Weight)
summary(emptybag)
meanbag <- sum(emptybag$Weight, na.rm = T)/115

dat <- dat %>% 
  select(Block, Pos, Weight)  %>% 
  rename(rawWeight = Weight) # only ones we care about right now are block, pos, and weight (fitness)
View(dat)
dat$Block <- as.factor(dat$Block)
dat$Pos <- as.factor(dat$Pos)
dat$rawWeight <- as.numeric(dat$rawWeight)

summary(dat)

dat <- dat %>% 
  mutate(Ind = as.factor(paste(Block, Pos, sep = "-")))

# Read in the missing plants
missing <- read.csv("Data/MissingPlants.csv") 
nrow(missing)
# Create "ind" column key in missing plants dataframe
missing <- missing %>% 
  rename(Ind=x)

# Take out the missing plants
datc <- anti_join(dat, missing, by="Ind") # datc = "dat complete"
nrow(dat)
nrow(missing)
nrow(datc)

# Subtract the mean bag weight
datc <- datc %>% 
  mutate(Weight = rawWeight - meanbag)
nrow(datc)
datc$Weight <- ifelse(datc$Weight < 0, 0, datc$Weight) #negative values = zero

write.csv(datc, "Data/Clean/2018FitnessClean.csv", row.names=F)

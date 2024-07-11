# Cleaning Weights 
# Written by Mia Akbar, 2023

library(tidyverse)

dat <- read.csv("Data/2020_Lythrum_Weights.csv")

dat <- dat[-grep("^M", dat$Block), ] #remove M block

dat <- dat %>% 
  mutate(Ind = paste(Block, Pos, sep = "-"))

#find mean empty bag weight for heavy bags 
emptybagheavy <- dat %>% 
  dplyr::filter(Missing==1) %>% 
  dplyr::filter(Block=="E1"| Block=="E6"|Block=="E8"|Block=="E10"|Block=="E12" & Pos >=179)

emptybagheavy$Weight <- as.numeric(emptybagheavy$Weight)
summary(emptybagheavy)
meanbagheavy <- sum(emptybagheavy$Weight, na.rm = T)/26

#find mean empty bag weight for light bags 
emptybaglight <- dat %>% 
  dplyr::filter(Missing==1) %>% 
  dplyr::filter(Block=="E3" | Block=="E12" & Pos < 179)

emptybaglight$Weight <- as.numeric(emptybaglight$Weight)
summary(emptybaglight)
#temporarily remove individuals that dont make any sense 
dat <- dat[!dat$Ind == "E3-89",]
dat <- dat[!dat$Ind == "E3-91",]

emptybaglight <- dat %>% 
  dplyr::filter(Missing==1) %>% 
  dplyr::filter(Block=="E3" | Block=="E12" & Pos < 179)

emptybaglight$Weight <- as.numeric(emptybaglight$Weight)
summary(emptybaglight)
meanbaglight <- sum(emptybaglight$Weight, na.rm = T)/12

dat <- dat %>% 
  select(Block, Pos, Weight)  %>% 
  rename(rawWeight = Weight) # only ones we care about right now are block, pos, and weight (fitness)


datlight <- dat %>% 
  dplyr::filter(Block=="E3" | Block=="E12" & Pos < 179) %>% 
  mutate(Weight = rawWeight - meanbaglight)
  
datheavy <- dat %>% 
  dplyr::filter(Block=="E1"| Block=="E6"|Block=="E8"|Block=="E10"|Block=="E12" & Pos >=179) %>% 
  mutate(Weight = rawWeight - meanbagheavy)

datnone <- dat %>% 
  dplyr::filter(Block=="E2"| Block=="E4"|Block=="E5"|Block=="E7"|Block=="E9"|Block=="E11") %>% 
  mutate(Weight = 0)

datc <- rbind(datlight, datheavy,datnone)

datc$Weight <- ifelse(datc$Weight < 0, 0, datc$Weight)

WeightNA <- datc %>% 
  filter(is.na(Weight))

write.csv(datc, "Data/Clean/2020FitnessClean.csv", row.names=F)

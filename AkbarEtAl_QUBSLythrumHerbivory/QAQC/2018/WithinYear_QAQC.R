# QA/QC 
# Written by Mia Akbar, Apr 2023

library(tidyverse)


## Does the last measure of fruit correlate to inflorescence biomass? 
heightdat <- read.csv("Data/Clean/semiclean_heightdat.csv")
fitness <- read.csv("Data/Clean/2018FitnessClean.csv")

finalfruit <- heightdat %>% 
  dplyr::filter(round==9) %>% 
  dplyr::select(ind, block, individual, date, fruit_length)

finalfruit$Ind <- finalfruit$ind

fitfruit <- left_join(finalfruit,fitness, by="Ind")

cor(fitfruit$fruit_length, fitfruit$Weight, use="pairwise.complete.obs")

ggplot(data=fitfruit, aes(x=log(fruit_length), y=log(Weight))) + geom_point() + theme_classic()


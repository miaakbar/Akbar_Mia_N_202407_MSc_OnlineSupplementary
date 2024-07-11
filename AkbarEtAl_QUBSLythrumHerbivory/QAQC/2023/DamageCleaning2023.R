#DamageCleaning2023
#Script written by Claire Smith for 2019 data 
#Modified by Mia Akbar, Mar 2024

library(dplyr)
library(lubridate)

Dam <- read.csv("Data/Lythrum2023_Damage.csv")
str(Dam)
missing <- read.csv("Data/MissingPlants.csv")
Dam <- Dam %>% 
  mutate(Ind = paste(Block, Pos, sep = "-")) %>% 
  select(Ind, everything())
nrow(Dam)

summary(Dam$Egg)


Dam <- anti_join(Dam, missing, by="Ind") # all values in x where there aren't 
# values in y

summary(Dam)
unique(Dam$Block) #looks like there are a few empty cells at the end of this .csv 
Dam <- Dam %>% 
  filter(Block != "")

Dam$Block <- as.factor(Dam$Block)
Dam$Ind <- as.factor(Dam$Ind)
Dam$Pos <- as.factor(Dam$Pos)
Dam$Egg <- as.logical(Dam$Egg)
Dam$Larv <- as.logical(Dam$Larv)
Dam$Adt <- as.logical(Dam$Adt)
Dam$U <- as.numeric(Dam$U)
Dam$D <- as.numeric(Dam$D)

unique(Dam$Date) #not messy at all! phew! 

Dam$Date <- paste(Dam$Date, "-2023", sep = "")
Dam$Date <- dmy(Dam$Date)

Dam$PropDam <- Dam$D/(Dam$D + Dam$U)

##check for unrealistic values
summary(Dam)
Dam[which(Dam$D > 20),]
Dam[which(Dam$U > 20),]



Dam <- Dam %>%
  mutate(Treatment = case_when(Block == "E1" | Block == "E3" | Block == "E6" | Block == "E8" | Block == "E10" | Block == "E12" ~ "Sprayed",
                               TRUE ~ "Unsprayed"))


write.csv(Dam, "Data/Clean/2023_Dam_Clean.csv", row.names=F)

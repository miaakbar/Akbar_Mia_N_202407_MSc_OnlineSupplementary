# DamageCleaning
## Written by Mia Akbar, 2023

library(tidyverse)

geDam <- read.csv("Data/Raw/Field data2018_damage assessment.csv")
str(Dam)
missing <- read.csv("Data/MissingPlants.csv")
missing$Ind <- missing$x
Dam$Ind <- paste(Dam$Plot, Dam$Individual, sep="-")
nrow(Dam)
Dam <- Dam[-grep("^M", Dam$Plot), ] #remove M blocks 


Dam <- anti_join(Dam, missing, by="Ind") # all values in x where there aren't 
# values in y

Dam$Plot <- as.factor(Dam$Plot)
Dam$Ind <- as.factor(Dam$Ind)
Dam$Individual <- as.factor(Dam$Individual)
Dam$Num_primary_stems <- as.numeric(Dam$Num_primary_stems)
Dam$Num_stems_dmg <- as.numeric(Dam$Num_stems_dmg)

##check for unrealistic values
summary(Dam)
Dam[which(Dam$Num_primary_stems > 20),]
Dam[which(Dam$Num_stems_dmg > 20),]
Dam[which(Dam$Num_stems_dmg > Dam$Num_primary_stems),]

##fix errors after checking field notebooks
Dam[Dam$Date=="16/8/2018" & Dam$Ind == "E5-12",]$Num_stems_dmg <- 1 #3
Dam[Dam$Date=="16/8/2018" & Dam$Ind == "E5-12",]$Num_primary_stems <- 3 #1


##Make new column for proportion of damage
Dam$Prop_Dam <- Dam$Num_stems_dmg/(Dam$Num_primary_stems)

Sprayed <- c("E1","E3","E6","E8","E10","E12")
Unsprayed <- c("E2","E4","E5","E7","E9","E11")

Dam$Treatment <- character(nrow(Dam))
for (i in 1:nrow(Dam)){
  if(Dam$Plot[i] %in% Sprayed){
    Dam$Treatment[i] <- "Sprayed"
  } else if(Dam$Plot[i] %in% Unsprayed) {
    Dam$Treatment[i] <- "Unsprayed"
  }
}

write.csv(Dam, "Data/Clean/2018DamClean.csv", row.names=F)

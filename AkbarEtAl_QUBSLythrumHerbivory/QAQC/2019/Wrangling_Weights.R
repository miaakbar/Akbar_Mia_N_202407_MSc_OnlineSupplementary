## Wrangling Weights
## A script to create a "clean" csv of the Lythrum 2019 common garden plant weights for blocks E1-12

library(dplyr)

## A. Read in data and make sure columns in proper formats:

dat<-read.csv("Data/LythrumWeights2019.csv")
summary(dat)
str(dat)
dat <- dat %>% 
  select(Block, Pos, Weight)  %>% 
  rename(rawWeight = Weight) # only ones we care about right now are block, pos, and weight (fitness)
View(dat)
# Make sure all blocks have capital E's
dat$Block <- as.factor(gsub("([e])","\\U\\1", dat$Block, perl=TRUE))
#Make sure Pos are factors
dat$Pos <- as.factor(dat$Pos)

## 

## B. Take out all missing plants:

# First, create "ind" column that will work as key between the missing plants dataframe and dat
dat <- dat %>% 
  mutate(ind = as.factor(paste(Block, Pos, sep = "-")))

# Read in the missing plants
missing <- read.csv("Data/Missing_Plants_List.csv") 
nrow(missing)
# Create "ind" column key in missing plants dataframe
missing <- missing %>% 
  rename(ind=x)

# Take out the missing plants
datc <- anti_join(dat, missing, by="ind") # datc = "dat complete"
nrow(dat)
nrow(missing)
nrow(datc)
# missing[grepl("E11",missing$ind),]
# datc[datc$Block == "E11" & datc$Pos == "51",] # E11-51 isn't in populationinfo
##

# Subtract the mean bag weight
datc <- datc %>% 
  mutate(Weight = rawWeight - 29.04583)
nrow(datc)
datc$Weight <- ifelse(datc$Weight < 0, 0, datc$Weight) #negative values = zero

WeightNA <- datc %>% 
  filter(is.na(Weight))

write.csv(datc, "Data/Clean/2019_Fitness_Clean.csv", row.names=F)

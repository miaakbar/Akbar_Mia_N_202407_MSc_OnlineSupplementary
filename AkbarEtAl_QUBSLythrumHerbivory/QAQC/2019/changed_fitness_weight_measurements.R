library(dplyr)

dat<-read.csv("Data/LythrumWeights2019.csv")
summary(dat)
str(dat)
dat <- dat %>% 
  select(Block, Pos, Weight)

fakedata <- dat
# Make sure all blocks have capital E's
fakedata$Block <- as.factor(gsub("([e])","\\U\\1", fakedata$Block, perl=TRUE))


# Take out all missing plants
fakedata <- fakedata %>% 
  mutate(ind = as.factor(paste(Block, Pos, sep = "-")))

missing_plants<-read.csv("Data/Missing_Plants_List.csv")
summary(missing_plants)
missing_plants <- missing_plants %>% 
  rename(ind=x)

newnew <- anti_join(fakedata, missing_plants, by="ind")
fakedata <- fakedata %>% 
  mutate(weight_without_bag =  Weight - 29.04583)

summary(fakedata)
fakedata[which(fakedata$weight_without_bag<0),]
nrow(fakedata)

nrow(newnew)
summary(newnew)
realdata<-newnew
realdata<- realdata %>% 
  mutate(weight_without_bag = Weight - 29.04583)
realdata[which(realdata$weight_without_bag<(-1)),]

Dat <- realdata

odd_weights <- which(realdata$weight_without_bag<(-1))

Dat_OKweights <- Dat[-odd_weights,]

Dat_renamed <- Dat_OKweights %>%
  rename(raw_weight = Weight,
         weight = weight_without_bag)

Dat_export <- Dat_renamed

write.csv(Dat_export, "Data/Clean/2019_Fitness_Clean.csv", row.names=F)

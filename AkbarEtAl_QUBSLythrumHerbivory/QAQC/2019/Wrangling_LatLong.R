
library(dplyr)

dat <- read.csv("Data/populationinfo.csv")
str(dat)

dat$ID <- as.factor(dat$ID)
dat$POP <- as.factor(dat$POP)
dat$FAM <- as.factor(dat$FAM)
dat$IND <- as.factor(dat$IND)

dat <- dat %>% 
  mutate(ind = paste(dat$BLOCK, dat$ID, sep = "-"),
         pop = POP,
         fam = FAM,
         lat = LAT, 
         lon = LON) %>% 
  select(ind, pop, fam, lat, lon)

dat_export <- dat

write.csv(dat_export, "Data/Clean/2019_LatLong_Clean.csv", row.names=F)

         
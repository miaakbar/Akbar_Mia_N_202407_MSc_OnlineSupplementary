Dam <- read.csv("Data/QUBS_Lythrum_Damage_Data_2019_corr.csv")
str(Dam)
missing <- read.csv("Data/Missing_Plants_List.csv")
Dam$ind <- paste(Dam$block, Dam$pos, sep="-")
nrow(Dam)
missing <- rename(missing, ind=x)

summary(Dam$dmg.score)


Dam <- anti_join(Dam, missing, by="ind") # all values in x where there aren't 
# values in y

summary(Dam)
Dam$block <- as.factor(Dam$block)
Dam$ind <- as.factor(Dam$ind)
Dam$pos <- as.factor(Dam$pos)
Dam$egg <- as.logical(Dam$egg)
Dam$larvae <- as.logical(Dam$larvae)
Dam$adult <- as.logical(Dam$adult)
Dam$undamaged.stems <- as.numeric(Dam$undamaged.stems)
Dam$damaged.stems <- as.numeric(Dam$damaged.stems)

Dams <- Dam %>% 
  group_by(ind) %>% 
  summarize(n=n())
nrow(Dams[which(Dams$n<=2),])
nrow(Dams[-which(Dams$n<=2),])

nrow(Dams)

Dam$prop_dam <- Dam$damaged.stems/(Dam$damaged.stems + Dam$undamaged.stems)

round1 <- c("18-Jun-19","19-Jun-19","20-Jun-19")
round2 <- c("18-Jul-19","19-Jul-19","20-Jul-19")
round3 <- c("14-Aug-19", "15-Aug-19","16-Aug-19")

pest <- c("E1","E3","E6","E8","E10","E12")
no_pest <- c("E2","E4","E5","E7","E9","E11")

Dam$treatment <- character(nrow(Dam))
for (i in 1:nrow(Dam)){
  if(Dam$block[i] %in% pest){
    Dam$treatment[i] <- "pest"
  } else if(Dam$block[i] %in% no_pest) {
    Dam$treatment[i] <- "control"
  }
}

write.csv(Dam, "Data/Clean/2019_Dam_Clean.csv", row.names=F)

unique(QUBS_Lythrum_Damage_Data_2019$date)
unique(X2019_Dam_Clean$date)

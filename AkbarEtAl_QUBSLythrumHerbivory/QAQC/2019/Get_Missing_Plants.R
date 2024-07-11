library(dplyr)

Data <- read.csv("Data/Modified/QUBS_Lythrum_Field_Data_2019_edited.csv")

######### Take out missing plants #############

heights <- c("veg", "bud", "flw", "frt", "abt")

# Find which rows have all NAs
missingData <- data.frame()

for(i in 1:nrow(Data)){
  if(sum(!is.na(Data[i,][heights])) == 0){
    missingData <- rbind(missingData, Data[i,])
  }
}

timesMissing <- missingData %>% 
  group_by(ind) %>%
  summarize(times_missing=n())

timesMissing <- as.data.frame(timesMissing)

missingList <- data.frame()
for (i in 1:nrow(timesMissing)){
  if(timesMissing[i,2] >= 10){
    missingList <- rbind(missingList, timesMissing[i,])
  }
}

missingListInds <- missingList$ind

# Take out missing individuals
Data <- Data[!(Data$ind %in% missingListInds),]

names(missingListInds) <- "ind"

# Write out file with list missing inds
write.csv(missingListInds, "Data/Missing_Plants_List.csv", row.names = F)

# Write new working file
write.csv(Data, "Data/LythrumFieldData1.csv", row.names = F)


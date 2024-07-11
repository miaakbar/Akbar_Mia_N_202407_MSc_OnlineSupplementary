#Get Day of First Flower for 2020
#Modified by Mia Akbar Feb 2023

library(tidyverse)

LythrumFieldData <- read.csv("Data/Clean/2020_SemiClean_Lythrum_Heights.csv")
summary(LythrumFieldData)
LythrumFieldData$Block <- as.factor(LythrumFieldData$Block)
LythrumFieldData$Pos <- as.factor(LythrumFieldData$Pos)


# Read in Lythrum data for date, flower, fruit, and abort length and individual identifier only
LythrumFlowerDays <- LythrumFieldData %>%
  replace_na(list(Flr = 0, Fert = 0, Abt = 0)) %>%
  # Replace NAs with 0s so that values can be easily added up by row
  mutate(DAYS = as.integer(difftime(Date, as.Date("2020-05-31", "%Y-%m-%d"), units = "days")),
         # This replaces DATE with DAYS based on difference from first measure date
         # You may want to replace the date provided (2019-05-22) with the planting or seeding date
         # because this will be more biologically relevant
         FLOWER = case_when(Flr + Fert + Abt > 0 ~ 1,
                            TRUE ~ 0),
         # Check for whether or not flowering or post stages were observed on a certain day
         FFDAYS = case_when(FLOWER == 1 ~ DAYS,
                            TRUE ~ NA_integer_)) %>%
  # Create new column with either the day this was observed or NA if no flowering observed yet
  group_by(Ind) %>%
  summarize(FFDAYS = min(FFDAYS, na.rm = TRUE)) %>%
  # Summarize by individual, taking the lowest date (and removing NAs)
  mutate(FFDAYS = ifelse(FFDAYS == Inf, NA_integer_, FFDAYS),
         FLOWER = as.logical(ifelse(is.na(FFDAYS) == TRUE, 0, 1)))
# The previous function puts in Inf if there is no flowering date observed
# I replace these with NA and then include a binary 0/1 column for whether flowering was observed during the season


summary(LythrumFlowerDays)

write.csv(LythrumFlowerDays,"Data/Clean/2020_Flower_Clean.csv", row.names = F)

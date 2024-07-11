#HeightCleaning2021
#Script written by Claire Smith for 2019 data 
#Modified by Mia Akbar, Aug 2023

#Prep heights for error checking 

library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

heightdat <- read.csv("Data/2021Heights.csv")
summary(heightdat)

heightdat$Pos <- as.factor(heightdat$Pos)
heightdat$Veg <- as.numeric(heightdat$Veg)
heightdat$Bud <- as.numeric(heightdat$Bud)
heightdat$Flr <- as.numeric(heightdat$Flr)
heightdat$Fert <- as.numeric(heightdat$Fert)
heightdat$Abt <- as.numeric(heightdat$Abt)

unique(heightdat$Block) #looks like "E9" is sometimes coded as "E9 "
heightdat <- heightdat %>% 
  mutate(Block = case_when(
    Block %in% c("E9", "E9 ") ~ "E9",TRUE ~ Block
  ))

unique(heightdat$Block) #looks like "E10" is sometimes coded as "E10 "
heightdat <- heightdat %>% 
  mutate(Block = case_when(
    Block %in% c("E10", "E10 ") ~ "E10",TRUE ~ Block
  ))
heightdat$Block <- as.factor(heightdat$Block)

unique(heightdat$Date) #not messy at all! phew! 

heightdat$Date <- paste(heightdat$Date, "-2021", sep = "")
heightdat$Date <- dmy(heightdat$Date)


##make new column for ease of combining later
heightdat <- heightdat %>% 
  mutate(Ind = paste(Block, Pos, sep = "-")) %>% 
  select(Ind, everything())


##check for unrealistic values
summary(heightdat)
heightdat[which(heightdat$Veg > 250),]
heightdat[which(heightdat$Bud > 250),]
heightdat[which(heightdat$Flr > 250),]
heightdat[which(heightdat$Fert > 250),]
heightdat[which(heightdat$Abt > 250),]


##fix Veg based on field notebooks 
heightdat[heightdat$Date=="2021-07-03" & heightdat$Ind == "E5-31",]$Veg <- 32 #3232 


# Look for between "round" measurement issues 

##Divide measurement dates into "rounds"
### important to note that this is only based on speculation, not field notes. Should confirm in field notebooks later that this was the order things were put into rounds in. 
date_check <- heightdat %>% 
  group_by(Date) %>% 
  summarise(block= unique(Block))

#round1 <- "2021-06-02"
#round2 <- "2021-06-07"
#round3 <- "2021-06-10"
#round4 <- "2021-06-15"
#round5 <- "2021-06-17"
#round6 <- "2021-06-22"
#round7 <- "2021-06-24"
#round8 <- "2021-06-28"
#round9 <- "2021-07-03"
#round10 <- "2021-07-05"
#round11 <- c("2021-07-09","2021-07-10")
#round12 <- "2021-07-12"
#round13 <- "2021-07-15"
#round14 <- c("2021-07-19","2021-07-20")
#round15 <- "2021-07-22"
#round16 <- "2021-07-26"
#round17 <- c("2021-07-29","2021-07-30")
#round18 <- "2021-08-02"
#round19 <- "2021-08-05"
#round20 <- c("2021-08-09", "2021-08-10")
#round21 <- c("2021-08-12","2021-08-13")
#round22 <- "2021-08-16"
#round23 <- "2021-08-19"
#round24 <- "2021-08-23"
#round25 <- c("2021-08-26","2021-08-27")
#round26 <- "2021-08-30"



# Define the date rounds
round_dates <- list(
  "2021-06-02", "2021-06-07", "2021-06-10", "2021-06-15", "2021-06-17",
  "2021-06-22", "2021-06-24", "2021-06-28", "2021-07-03", "2021-07-05",
  c("2021-07-09", "2021-07-10"), "2021-07-12", "2021-07-15",
  c("2021-07-19", "2021-07-20"), "2021-07-22", "2021-07-26",
  c("2021-07-29", "2021-07-30"), "2021-08-02", "2021-08-05",
  c("2021-08-09", "2021-08-10"), c("2021-08-12", "2021-08-13"),
  "2021-08-16", "2021-08-19", "2021-08-23",
  c("2021-08-26", "2021-08-27"), "2021-08-30"
)

# Convert date strings to Date objects
round_dates <- lapply(round_dates, as.Date)

# Create a vector of corresponding round numbers
round_numbers <- seq_along(round_dates)

# Match each date to its round number
heightdat$round <- sapply(heightdat$Date, function(date) {
  for (i in seq_along(round_dates)) {
    if (date %in% round_dates[[i]]) {
      return(round_numbers[i])
    }
  }
  return(NA)
})


heightdat$round <- as.factor(heightdat$round)

heightdat$ind_round <- factor(paste(heightdat$Ind, heightdat$round, sep = "_"))

##write semiclean file for prelim analysis 

write.csv(heightdat, "Data/Clean/2021_SemiClean_Lythrum_Heights.csv", row.names = FALSE)

##### Within round issues #####

# Check for illogical measurements for each plant in a given round

# Make function to check heights of each plant over the summer
heightOK <- function(x){
  
  # Consider heights in a given row
  testVec=c(x["Bud"], x["Flr"], x["Fert"], x["Abt"], x["Veg"])
  
  # Order the heights in row from biggest to smallest, ignoring NAs
  # (1 is biggest, 2 is 2nd biggest, ...)
  orderVec <- order(testVec, decreasing=T, na.last=NA)
  
  # Does the order make sense?
  # We want: bud>flw>frt>abt>veg
  # So orderVec should go 1 2 3 4 5
  # If so the differences between terms should be positive (1 1 1 1) 
  # If not, at least one will be negative
  # (e.g. 1 2 4 3 5 the differences will be 1 2 -1 2)
  # TRUE if good to go = heights OK
  
  all(diff(orderVec) >= 0) 
}

# Check if height trajectories make sense
heightOKDat <- apply(heightdat, 1, heightOK)

length(heightOKDat)-sum(heightOKDat)

issuesDat <- heightdat[!heightOKDat,]
issuesDat <- issuesDat %>% arrange(Date,Block, Pos)
issuesDat <- issuesDat[, cols_init]

# View(issuesDat)
if (nrow(issuesDat)==0) {
  print("No within round issues :)")
} else {
  write.csv(issuesDat, "Data/Issues/issuesLythrum2021_withinrounds.csv", row.names = F)
  message = paste("There were", nrow(issuesDat), "within round issues found :'(")
  print(message)
  print("See Data/Issues/issuesLythrum2021_withinrounds.csv for details")
}

##### Issues between rounds #####

maxDiff <- 10 # the max a previous veg can be above the next round's veg

issues_bw_l <- heightdat %>% 
  arrange(Ind, round) %>% 
  group_by(Ind) %>% 
  mutate(Height_Diff = c(NA, diff(Veg)))

# Which individuals have issues between rounds
issues_bw_ind <- issues_bw_l %>% 
  filter(!is.na(Height_Diff) & Height_Diff < -10)

problem_ind <- unique(issues_bw_ind$Ind)
problem_ind <- as.data.frame(problem_ind) %>% 
  rename(Ind=problem_ind)

#left join with original dataframe
issues_bw_ind <-left_join(problem_ind, heightdat, by="Ind", multiple="all")


# Make a new dataframe with ind (each individual plant's key) and its veg measurements for each round
issues_bw_wide <- pivot_wider(issues_bw_ind, id_cols = Ind, names_from = round, values_from = Veg)


# View(issues_bw_Data)
# View(issues_bw_spread)
if (nrow(issues_bw_wide)==0) {
  print("No within round issues :)")
} else {
  write.csv(issues_bw_wide, "Data/Issues/issuesLythrum2021_betweenrounds.csv", row.names = F)
  message = paste("There were", nrow(issues_bw_wide), "between round issues found :'(")
  print(message)
  print("See Data/Issues/issuesLythrum2021_betweenrounds.csv for details")
}


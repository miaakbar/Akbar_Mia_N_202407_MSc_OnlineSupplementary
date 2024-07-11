#HeightCleaning2023
#Script written by Claire Smith for 2019 data 
#Modified by Mia Akbar, Mar 2024

#Prep heights for error checking 

library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

heightdat <- read.csv("Data/Lythrum2023_Heights.csv")
summary(heightdat)

heightdat$Position <- as.factor(heightdat$Position)
heightdat$Veg <- as.numeric(heightdat$Veg)
heightdat$Bud <- as.numeric(heightdat$Bud)
heightdat$Flr <- as.numeric(heightdat$Flr)
heightdat$Frt <- as.numeric(heightdat$Frt)
heightdat$Abt <- as.numeric(heightdat$Abt)


unique(heightdat$Block) 
heightdat$Block <- as.factor(heightdat$Block)

unique(heightdat$Date) #not messy at all! phew! 

heightdat$Date <- paste(heightdat$Date, "-2023", sep = "")
heightdat$Date <- dmy(heightdat$Date)


##make new column for ease of combining later
heightdat <- heightdat %>% 
  mutate(Ind = paste(Block, Position, sep = "-")) %>% 
  select(Ind, everything())


##check for unrealistic values
summary(heightdat)
heightdat[which(heightdat$Veg > 250),]
heightdat[which(heightdat$Bud > 250),]
heightdat[which(heightdat$Flr > 250),]
heightdat[which(heightdat$Fert > 250),]
heightdat[which(heightdat$Abt > 250),]

### Cleanest year ever, lets gooooooo

##fix Bud based on field notebooks 
heightdat[heightdat$Date=="2023-07-18" & heightdat$Ind == "E3-172",]$Bud <- 118 #1118


##fix Veg based on notebooks 
heightdat[heightdat$Date=="2023-06-20" & heightdat$Ind == "E10-97",]$Veg <- 89.5 #895


# Look for between "round" measurement issues 

##Divide measurement dates into "rounds"

# Define the date rounds
round_dates <- list("2023-06-07",
  c("2023-06-12", "2023-06-13", "2023-06-14"), c("2023-06-19", "2023-06-20"), c("2023-06-27","2023-06-28"), c("2023-07-04" , "2023-07-06"), c("2023-07-12", "2023-07-11"), c("2023-07-18","2023-07-17"), c("2023-07-27", "2023-07-24"), c("2023-08-01","2023-07-31"), c("2023-08-09"), c("2023-08-14", "2023-08-15"), c("2023-08-23", "2023-08-22","2023-08-21"), c("2023-08-29", "2023-08-28"), "2023-09-04" ,"2023-09-11" ,"2023-09-18")

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

write.csv(heightdat, "Data/Clean/2023_SemiClean_Lythrum_Heights.csv", row.names = FALSE)

##### Within round issues #####

# Check for illogical measurements for each plant in a given round

# Make function to check heights of each plant over the summer
heightOK <- function(x){
  
  # Consider heights in a given row
  testVec=c(x["Bud"], x["Flr"], x["Frt"], x["Abt"], x["Veg"])
  
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
  write.csv(issuesDat, "Data/Issues/issuesLythrum2023_withinrounds.csv", row.names = F)
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
  write.csv(issues_bw_wide, "Data/Issues/issuesLythrum2023_betweenrounds.csv", row.names = F)
  message = paste("There were", nrow(issues_bw_wide), "between round issues found :'(")
  print(message)
  print("See Data/Issues/issuesLythrum2021_betweenrounds.csv for details")
}


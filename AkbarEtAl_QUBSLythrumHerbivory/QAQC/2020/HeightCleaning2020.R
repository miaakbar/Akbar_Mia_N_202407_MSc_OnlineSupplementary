#HeightCleaning2020
#Script written by Claire Smith for 2019 data 
#Modified by Mia Akbar, Feb 2023 

#Prep heights for error checking 

library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

heightdat <- read.csv("Data/2020Heights.csv")
summary(heightdat)

heightdat$Pos <- as.factor(heightdat$Pos)
heightdat$Veg <- as.numeric(heightdat$Veg)
heightdat$Flr <- as.numeric(heightdat$Flr)
heightdat$Fert <- as.numeric(heightdat$Fert)
heightdat$Abt <- as.numeric(heightdat$Abt)

unique(heightdat$Block) #looks like "E7" is sometimes coded as "E7 "
heightdat <- heightdat %>% 
  mutate(Block = case_when(
    Block %in% c("E7", "E7 ") ~ "E7",TRUE ~ Block
  ))
heightdat$Block <- as.factor(heightdat$Block)

unique(heightdat$Date) #not messy at all! phew! 

heightdat$Date <- paste(heightdat$Date, "-2020", sep = "")
heightdat$Date <- dmy(heightdat$Date)

#Condense to 6 current blocks because there is no point in cleaning the other ones
##heightdat <- heightdat %>% 
  ##filter(Block %in% c("E3", "E4", "E5", "E6", "E9", "E10"))
  

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


##fix Bud based on field notebooks 
heightdat[heightdat$Date=="2020-07-28" & heightdat$Ind == "E3-113",]$Bud <- 118 #1118 
heightdat[heightdat$Date=="2020-07-28" & heightdat$Ind == "E12-8",]$Bud <- 94 #944

# Look for between "round" measurement issues 

##Divide measurement dates into "rounds"
### important to note that this is only based on speculation, not field notes. Should confirm in field notebooks later that this was the order things were put into rounds in. 
date_check <- heightdat %>% 
  group_by(Date) %>% 
  summarise(block= unique(Block))

round1 <- "2020-05-31"
round2 <- "2020-06-08"
round3 <- "2020-06-15"
round4 <- "2020-06-25"
round5 <- "2020-06-30"
round6 <- c("2020-07-06", "2020-07-07")
round7 <- "2020-07-15"
round8 <- c("2020-07-20", "2020-07-21")
round9 <- "2020-07-28"
round10 <- "2020-08-07"
round11 <- c("2020-08-13","2020-08-14","2020-08-15")
round12 <- "2020-08-20"
round13 <- "2020-08-26"

heightdat$cdate <- as.character(heightdat$Date)
heightdat$round <- ifelse(heightdat$cdate %in% round1, 1, 
                     ifelse(heightdat$cdate %in% round2, 2,
                            ifelse(heightdat$cdate %in% round3, 3,
                                   ifelse(heightdat$cdate %in% round4, 4,
                                          ifelse(heightdat$cdate %in% round5, 5,
                                                 ifelse(heightdat$cdate %in% round6, 6,
                                                        ifelse(heightdat$cdate %in% round7, 7,
                                                               ifelse(heightdat$cdate %in% round8, 8,
                                                                      ifelse(heightdat$cdate %in% round9, 9,
                                                                             ifelse(heightdat$cdate %in% round10, 10,
                                                                                    ifelse(heightdat$cdate %in% round11, 11,
                                                                                           ifelse(heightdat$cdate %in% round12, 12,13))))))))))))

heightdat$round <- as.factor(heightdat$round)

heightdat$ind_round <- factor(paste(heightdat$Ind, heightdat$round, sep = "_"))

##write semiclean file for prelim analysis 

write.csv(heightdat, "Data/Clean/2020_SemiClean_Lythrum_Heights.csv", row.names = FALSE)

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
  write.csv(issuesDat, "Data/Issues/issuesLythrum2020_withinrounds.csv", row.names = F)
  message = paste("There were", nrow(issuesDat), "within round issues found :'(")
  print(message)
  print("See Data/Issues/issuesLythrum2020_withinrounds.csv for details")
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
  write.csv(issues_bw_wide, "Data/Issues/issuesLythrum2020_betweenrounds.csv", row.names = F)
  message = paste("There were", nrow(issues_bw_wide), "between round issues found :'(")
  print(message)
  print("See Data/Issues/issuesLythrum2020_betweenrounds.csv for details")
}


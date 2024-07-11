library(dplyr)
library(tidyr)

##### Read in and prep data for error-checking #####

# Data <- read.csv("Data/QUBS_Lythrum_Field_Data_2019.csv")
Data <- read.csv("2019 Field Data/Data/Modified/QUBS_Lythrum_Field_Data_2019_edited.csv")
# View(Data)
cols_init <- colnames(Data)

Data$date <- as.Date(Data$date)

Data$pos <- as.factor(Data$pos)
Data$mer <- as.factor(Data$mer)
Data$sec <- as.factor(Data$sec)
Data$new <- as.factor(Data$new)

Data$edited <- as.factor(Data$edited)
Data$deleted <- as.factor(Data$deleted)
Data$missed_N <- as.factor(Data$missed_N)

Data$ind <- factor(paste(Data$block, Data$pos, sep = "-"))

## Make new column "round" to separate measurements into rounds by date
round1 <- c("2019-05-22", "2019-05-21")
round2 <- c("2019-05-29")
round3 <- c("2019-06-05", "2019-06-06")
round4 <- c("2019-06-10", "2019-06-11")
round5 <- c("2019-06-17")
round6 <- c("2019-06-24")
round7 <- c("2019-06-27", "2019-06-28")
round8 <- c("2019-07-02", "2019-07-03")
round9 <- c("2019-07-04", "2019-07-05")
round10 <- c("2019-07-08", "2019-07-09")
round11 <- c("2019-07-15", "2019-07-16")
round12 <- c("2019-07-23", "2019-07-24", "2019-07-25")
round13 <- c("2019-07-29", "2019-07-30", "2019-07-31")
round14 <- c("2019-08-06", "2019-08-08", "2019-08-07")
round15 <- c("2019-08-12", "2019-08-13")
round16 <- c("2019-08-21", "2019-08-22", "2019-08-23", "2019-08-20")
round17 <- c("2019-09-29")

Data$cdate <- as.character(Data$date)
Data$round <- ifelse(Data$cdate %in% round1, 1, 
                     ifelse(Data$cdate %in% round2, 2,
                            ifelse(Data$cdate %in% round3, 3,
                                   ifelse(Data$cdate %in% round4, 4,
                                          ifelse(Data$cdate %in% round5, 5,
                                                 ifelse(Data$cdate %in% round6, 6,
                                                        ifelse(Data$cdate %in% round7, 7,
                                                               ifelse(Data$cdate %in% round8, 8,
                                                                      ifelse(Data$cdate %in% round9, 9,
                                                                             ifelse(Data$cdate %in% round10, 10,
                                                                                    ifelse(Data$cdate %in% round11, 11,
                                                                                           ifelse(Data$cdate %in% round12, 12,
                                                                                                  ifelse(Data$cdate %in% round13, 13,
                                                                                                         ifelse(Data$cdate %in% round14, 14,
                                                                                                                ifelse(Data$cdate %in% round15, 15,
                                                                                                                       ifelse(Data$cdate %in% round16, 16,17))))))))))))))))
Data$round <- as.factor(Data$round)
str(Data)
Data$ind_round <- factor(paste(Data$ind, Data$round, sep = "_"))

##### More obvious issues #####

# Check for unrealistic values
summary(Data)
Data[which(Data$veg > 250),]
Data[which(Data$bud > 250),]
Data[which(Data$flw > 250),]
Data[which(Data$frt > 250),]
Data[which(Data$abt > 250),]

#fix veg
Data[Data$date=="2019-06-06" & Data$block=="E3" & Data$pos==149,]$veg <- 30 #4530 should be 30
Data[Data$date=="2019-06-24" & Data$block=="E2" & Data$pos==109,]$veg <- 103.5 #1035 should be 103.5
Data[Data$date=="2019-07-08" & Data$block=="E1" & Data$pos==68,]$veg <- 116.5 #1165 should be 116.5
#fix bud
Data[Data$date=="2019-06-27" & Data$block=="E3" & Data$pos==57,]$bud <- 62.5 #625 should be 62.5
Data[Data$date=="2019-08-13" & Data$block=="E10" & Data$pos==38,]$bud <- 153 #1253 should be 153

#fix frt
Data[Data$date=="2019-09-29" & Data$block=="E11" & Data$pos==118,]$frt <- 146.5 #625 should be 62.5


# # Output temporary "cleaner" data file for prelim analysis
# write.csv(Data, "Data/LythrumFieldData1.csv", row.names=F)

##### Within round issues #####

# Check for illogical measurements for each plant in a given round

# Make function to check heights of each plant over the summer
heightOK <- function(x){
  
  # Consider heights in a given row
  testVec=c(x["bud"], x["flw"], x["frt"], x["abt"], x["veg"])
  
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
heightOKDat <- apply(Data, 1, heightOK)

#length(heightOKDat)-sum(heightOKDat)

issuesDat <- Data[!heightOKDat,]
issuesDat <- issuesDat %>% arrange(date,block, pos)
issuesDat <- issuesDat[, cols_init]

# View(issuesDat)
if (nrow(issuesDat)==0) {
  print("No within round issues :)")
} else {
  write.csv(issuesDat, "Data/Issues/issuesLythrum2019_withinrounds.csv", row.names = F)
  message = paste("There were", nrow(issuesDat), "within round issues found :'(")
  print(message)
  print("See Data/Issues/issuesLythrum2019_withinrounds.csv for details")
}

##### Issues between rounds #####

maxDiff <- 10 # the max a previous veg can be above the next round's veg

issues_bw_l <- Data %>% 
  select(ind, veg) %>%
  filter(!is.na(veg)) %>% 
  arrange(ind) %>% 
  group_by(ind) %>%
  summarize(issue=!all(abs(diff(veg) >= -maxDiff))) %>% 
  as.data.frame()

# Which individuals have issues between rounds
issues_bw_ind <- issues_bw_l$ind[issues_bw_l$issue]

# Get the entries from the main dataframe for these individuals
issues_bw_Data <- Data[as.character(Data$ind) %in% as.character(issues_bw_ind),]

# Make a new dataframe with ind (each individual plant's key) and its veg measurements for each round
issues_bw_spread <- issues_bw_Data %>%
  select(date, block, pos, round, veg) %>%
  filter(!is.na(veg)) %>% 
  arrange(date, block, pos) %>% 
  spread(key=round, value=veg)


# View(issues_bw_Data)
# View(issues_bw_spread)
if (nrow(issues_bw_spread)==0) {
  print("No within round issues :)")
} else {
  write.csv(issues_bw_spread, "Data/Issues/issuesLythrum2019_betweenrounds.csv", row.names = F)
  message = paste("There were", nrow(issues_bw_spread), "between round issues found :'(")
  print(message)
  print("See Data/Issues/issuesLythrum2019_betweenrounds.csv for details")
}

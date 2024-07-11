#HeightsCleaning2018
#Mia Akbar
#Updated Apr 2023

## Before loading into R, I deleted 804 rows of data that were entered as duplicate measurements E4,E7,E8,E11 (2018-06-27) I also entered data for E12 (2017-07-04) E4 (2018-07-10) and E12 (2018-07-12) that were missing from original data transcription for some reason. The original csv still exists in the TEAMS folder, but I will use this modified one for cleaning and subsequent analysis!

heightdat <- read.csv("Data/Raw/FieldData_Height_HARDCOPY_Modified2023.csv") 
summary(heightdat)

# Prep Heights for error checking 

heightdat <- heightdat[-grep("^M", heightdat$block), ] 
heightdat <- heightdat[-grep("^J", heightdat$block), ] #remove M/J blocks 

#Condense to 6 current blocks because there is no point in cleaning the other ones
#heightdat <- heightdat %>% 
  #filter(block %in% c("E3", "E4", "E5", "E6", "E9", "E10"))

unique(heightdat$block)
unique(heightdat$date) #big mess!

## recode dates so that everything is y-m-d
### this could probably be more efficient but I am scared that i will confuse lubridate and convert august dates to july dates or june dates. 
library(dplyr)
library(tidyverse)
library(lubridate)
heightdat <- heightdat %>% 
  mutate(date = case_when(
    date == "20/6/2018" ~ "2018-06-20",
    date %in% c("21/6/2018", "21/06/2018") ~ "2018-06-21",
    date %in% c("27/6/2018", "27/06/2018") ~ "2018-06-27",
    date %in% c("28/6/2018", "28/06/2018") ~ "2018-06-28",
    date %in% c("03-07-2018", "2018-03-07") ~ "2018-07-03",
    date %in% c("2018-04-07", "04-07-2018") ~ "2018-07-04",
    date == "2018-05-07" ~ "2018-07-05",
    date == "2018-09-07" ~ "2018-07-09",
    date == "2018-10-07" ~ "2018-07-10",
    date == "2018-11-07" ~ "2018-07-11",
    date == "2018-12-07" ~ "2018-07-12",
    date == "16/7/2018" ~ "2018-07-16", 
    date == "17/7/2018" ~ "2018-07-17",
    date == "19/7/2018" ~ "2018-07-19",
    date == "31/7/2018" ~ "2018-07-31",
    date == "30/7/2018" ~ "2018-07-30",
    date == "2018-01-08" ~ "2018-08-01",
    date == "2018-06-08" ~ "2018-08-06",
    date == "2018-07-08" ~ "2018-08-07",
    date == "2018-10-08" ~ "2018-08-10",
    date == "2018-12-08" ~ "2018-08-12",
    date == "13/8/2018" ~ "2018-08-13",
    date == "14/8/2018" ~ "2018-08-14",
    date == "15/8/2018" ~ "2018-08-15",
    date == "18/8/2018" ~ "2018-08-18",
    date == "20/8/2018" ~ "2018-08-20",
    date == "21/8/2018" ~ "2018-08-21",
    date == "22/8/2018" ~ "2018-08-22",
    TRUE ~ date
  ))

unique(heightdat$date) #that's better :) 


##make new column for ease of combining later
heightdat <- heightdat %>% 
  mutate(ind = paste(block, individual, sep = "-")) %>% 
  dplyr::select(ind, everything())

##fix variable classifications
heightdat$date <- as.Date(heightdat$date)
heightdat$ind <- as.factor(heightdat$ind)
heightdat$block <- as.factor(heightdat$block)
heightdat$stem_length <- as.numeric(heightdat$stem_length)
heightdat$bud_length <- as.numeric(heightdat$bud_length)
heightdat$flower_length <- as.numeric(heightdat$flower_length)
heightdat$fruit_length <- as.numeric(heightdat$fruit_length)

##There were cases where four blocks were entered into the spreadsheet twice for 18-06-27 ((804 individuals from E11, E4, E7, E8) have duplicate ID on 18-06-27).However, due to transcription errors, not all duplicates matched eachother exactly. After consulting the raw datafiles, I will make hard recodes to the data 
###fix problems based on field notebooks
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E11-55",]$stem_length <- 84
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E11-55",]$bud_length <- 93.5
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E11-55",]$flower_length <- 86
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E11-56",]$stem_length <- 84.5
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E11-56",]$bud_length <- 88
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E11-144",]$stem_length <- 79
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E11-144",]$bud_length <- 84.5
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E4-98",]$stem_length <- 92
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E4-98",]$bud_length <- 97
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E4-116",]$stem_length <- 62.5
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E4-165",]$stem_length <- 80.5
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E4-173",]$stem_length <- 81
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E4-173",]$bud_length <- 84
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E7-22",]$stem_length <- 48.5
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E7-135",]$stem_length <- 60
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-148",]$stem_length <- NA
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-148",]$bud_length <- NA
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-149",]$stem_length <- 13.5
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-149",]$bud_length <- 34
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-150",]$stem_length <- 60
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-151",]$stem_length <- 71.5
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-151",]$bud_length <- 72.5
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-152",]$stem_length <- 35
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-152",]$bud_length <- NA
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-153",]$stem_length <- 73
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-153",]$bud_length <- 74
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-154",]$stem_length <- 54.5
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-154",]$bud_length <- NA
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E8-165",]$stem_length <- 62



##check for unrealistic values
summary(heightdat)
heightdat[which(heightdat$stem_length > 250),]
heightdat[which(heightdat$bud_length > 250),]
heightdat[which(heightdat$flower_length > 250),]
heightdat[which(heightdat$fruit_length > 250),]

##fix stem_length after checking field notebooks
heightdat[heightdat$date=="2018-06-20" & heightdat$ind == "E2-151",]$stem_length <- 95 #959
heightdat[heightdat$date=="2018-06-27" & heightdat$ind == "E3-135",]$stem_length <- 91.5 #9135
heightdat[heightdat$date=="2018-06-28" & heightdat$ind == "E6-42",]$stem_length <- 100 #1000

##fix flower_length after checking field notebooks 
heightdat[heightdat$date=="2018-07-11" & heightdat$ind == "E9-63",]$flower_length <- 119 #1119
heightdat[heightdat$date=="2018-08-08" & heightdat$ind == "E8-187",]$flower_length <- 153.5 #1535


# Look for between "round" measurement issues 

##Divide measurement dates into "rounds" 
round1 <- c("2018-06-20", "2018-06-21")
round2 <- c("2018-06-27", "2018-06-28")
round3 <- c("2018-07-04", "2018-07-03", "2018-07-05") 
round4 <- c("2018-07-09", "2018-07-10", "2018-07-11", "2018-07-12") 
round5 <- c("2018-07-16", "2018-07-17", "2018-07-19") 
round6 <- c("2018-07-30", "2018-07-31", "2018-08-01")
round7 <- c("2018-08-06", "2018-08-07", "2018-08-08", "2018-08-10")
round8 <- c("2018-08-12", "2018-08-13","2018-08-14","2018-08-15","2018-08-18")
round9 <- c("2018-08-20", "2018-08-21","2018-08-22")

heightdat$cdate <- as.character(heightdat$date)
heightdat$round <- ifelse(heightdat$cdate %in% round1, 1, 
                     ifelse(heightdat$cdate %in% round2, 2,
                            ifelse(heightdat$cdate %in% round3, 3,
                                   ifelse(heightdat$cdate %in% round4, 4,
                                          ifelse(heightdat$cdate %in% round5, 5,
                                                 ifelse(heightdat$cdate %in% round6, 6,
                                                        ifelse(heightdat$cdate %in% round7, 7,
                                                               ifelse(heightdat$cdate %in% round8, 8,9))))))))

heightdat$round <- as.factor(heightdat$round)
                                             
##Perhaps this is a good time to see how many observations each block has per a certain round 
obs_freq <- heightdat %>% 
  group_by(round) %>% 
  summarise(block = length(unique(block)),
            obs = length(stem_length)) 

obsfreq <- heightdat %>% 
  group_by(date) %>% 
  summarise(block = length(unique(block)),
            obs = length(stem_length))
            
## Check for illogical measurements for each plant in a given round 

### make function to check heights of each plant over season 

heightOK <- function(x) {
  
  #consider heights in a given row
  testVec = c(x["bud_length"], x["flower_length"], x["fruit_length"], x["stem_length"])
  
  #order heights in row from biggest to smallest, ignoring NAs
  # (1 is biggest, 2 is second largest)
  orderVec <- order(testVec, decreasing = T, na.last=NA)
  # Does the order make sense?
  # We want: bud>flw>frt>abt>veg
  # So orderVec should go 1 2 3 4 5
  # If so the differences between terms should be positive (1 1 1 1) 
  # If not, at least one will be negative
  # (e.g. 1 2 4 3 5 the differences will be 1 2 -1 2)
  # TRUE if good to go = heights OK
  
  all(diff(orderVec) >= 0) 
}


###Check if height trajectories make sense 

heightOKDat <- apply(heightdat, 1, heightOK)

length(heightOKDat)-sum(heightOKDat)

issuesDat <- heightdat[!heightOKDat,]
issuesDat <- issuesDat %>% arrange(date,block,individual)
cleanIssuesDat <- issuesDat %>%
  filter(flower_length != stem_length) #removing entries where flower/veg are equal
cleanIssuesDat <- cleanIssuesDat %>%
  arrange(date, block, individual)

write.csv(cleanIssuesDat, "Data/Issues/issuesLythrum2018_withinrounds.csv", row.names = F)


## Check between round issues 
maxDiff <- 10 # the max a previous veg can be above the next round's veg

issues_bw_l <- heightdat %>% 
  select(ind, stem_length) %>%
  filter(!is.na(stem_length)) %>% 
  arrange(ind) %>% 
  group_by(ind) %>%
  summarize(issue=!all(abs(diff(stem_length) >= -maxDiff))) %>% 
  as.data.frame()

# Which individuals have issues between rounds
issues_bw_ind <- issues_bw_l$ind[issues_bw_l$issue]
length(issues_bw_ind)

# Get the entries from the main dataframe for these individuals
issues_bw_Data <- heightdat[as.character(heightdat$ind) %in% as.character(issues_bw_ind),]

# Make a new dataframe with ind (each individual plant's key) and its veg measurements for each round
library(tidyr)
issues_bw_spread <- issues_bw_Data %>%
  select(block, individual, round, stem_length) %>%
  filter(!is.na(stem_length)) %>% 
  arrange(block, individual) %>% 
  spread(key=round, value=stem_length)


write.csv(issues_bw_spread, "Data/Issues/issuesLythrum2018_betweenrounds.csv", row.names = F)

##Fix within round issues after checking paper copies for problems with transcribing 

heightdat[heightdat$date=="2018-06-28" & heightdat$ind == "E1-1",]$flower_length <- NA #78
heightdat[heightdat$date=="2018-06-28" & heightdat$ind == "E1-2",]$stem_length <- 78 #NA

heightdat[heightdat$date=="2018-06-28" & heightdat$ind == "E6-84",]$flower_length <- 122 #118

heightdat[heightdat$date=="2018-07-04" & heightdat$ind == "E12-92",]$flower_length <- 126.5 #136.6

heightdat[heightdat$date=="2018-07-11" & heightdat$ind == "E8-100",]$flower_length <- NA #116.5
heightdat[heightdat$date=="2018-07-11" & heightdat$ind == "E8-101",]$flower_length <- 116.5 #123
heightdat[heightdat$date=="2018-07-11" & heightdat$ind == "E8-102",]$flower_length <- 123 #NA
heightdat[heightdat$date=="2018-07-11" & heightdat$ind == "E8-103",]$flower_length <- NA #107
heightdat[heightdat$date=="2018-07-11" & heightdat$ind == "E8-104",]$flower_length <- 107 #NA

heightdat[heightdat$date=="2018-07-12" & heightdat$ind == "E12-17",]$bud_length <- 109.5 #101.5

heightdat[heightdat$date=="2018-07-16" & heightdat$ind == "E2-70",]$bud_length <- 90.5 #30.5

heightdat[heightdat$date=="2018-07-17" & heightdat$ind == "E5-155",]$flower_length <- 109.5 #104.5

heightdat[heightdat$date=="2018-07-30" & heightdat$ind == "E5-155",]$bud_length <- 139.5 #131.5

heightdat[heightdat$date=="2018-07-31" & heightdat$ind == "E10-51",]$fruit_length <- 146 #116

heightdat[heightdat$date=="2018-07-31" & heightdat$ind == "E5-186",]$flower_length <- 129 #121

heightdat[heightdat$date=="2018-07-31" & heightdat$ind == "E6-94",]$flower_length <- 136 #139

heightdat[heightdat$date=="2018-08-06" & heightdat$ind == "E1-171",]$flower_length <- 96 #90

heightdat[heightdat$date=="2018-08-07" & heightdat$ind == "E2-34",]$flower_length <- 122 #155

heightdat[heightdat$date=="2018-08-07" & heightdat$ind == "E7-117",]$flower_length <- 105.5 #150.5

heightdat[heightdat$date=="2018-08-08" & heightdat$ind == "E10-170",]$fruit_length <- 121 #129

heightdat[heightdat$date=="2018-08-08" & heightdat$ind == "E9-94",]$flower_length <- 166 #106

heightdat[heightdat$date=="2018-08-10" & heightdat$ind == "E12-22",]$flower_length <- 168 #158

heightdat[heightdat$date=="2018-08-12" & heightdat$ind == "E2-81",]$fruit_length <- 144 #104

heightdat[heightdat$date=="2018-08-15" & heightdat$ind == "E12-62",]$fruit_length <- 167 #169

heightdat[heightdat$date=="2018-08-20" & heightdat$ind == "E2-35",]$stem_length <- 110 #117

heightdat[heightdat$date=="2018-08-20" & heightdat$ind == "E2-139",]$bud_length <- 98 #96

heightdat[heightdat$date=="2018-08-20" & heightdat$ind == "E4-119",]$flower_length <- 164.5 #134.5

heightdat[heightdat$date=="2018-08-20" & heightdat$ind == "E4-169",]$fruit_length <- 140.5 #190.5

heightdat[heightdat$date=="2018-08-21" & heightdat$ind == "E10-138",]$fruit_length <- 150.5 #160.5

heightdat[heightdat$date=="2018-08-21" & heightdat$ind == "E11-148",]$flower_length <- 196.5 #116.5

heightdat[heightdat$date=="2018-08-21" & heightdat$ind == "E8-2",]$fruit_length <- NA #156.5

# make a new dataframe for logistic growth curve analysis 
growthmoddat <- heightdat %>% 
  filter(block %in% c("E3", "E6", "E10")) %>% 
  recode()
  select(ind,date,stem_length) %>% 



write.csv(heightdat, "Data/Clean/semiclean_heightdat.csv", row.names = FALSE)













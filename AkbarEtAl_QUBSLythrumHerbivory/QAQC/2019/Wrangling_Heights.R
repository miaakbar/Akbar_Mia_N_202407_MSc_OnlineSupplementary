library(dplyr)

####################################
# Take out missing individuals
Heights <- read.csv("Data/Modified/QUBS_Lythrum_Field_Data_2019_edited.csv")
missing <- read.csv("Data/Missing_Plants_List.csv")

cols_init <- colnames(Heights)

Heights$date <- as.Date(Heights$date)

Heights$pos <- as.factor(Heights$pos)
Heights$mer <- as.factor(Heights$mer)
Heights$sec <- as.factor(Heights$sec)
Heights$new <- as.factor(Heights$new)

Heights$edited <- as.factor(Heights$edited)
Heights$deleted <- as.factor(Heights$deleted)
Heights$missed_N <- as.factor(Heights$missed_N)

Heights$ind <- factor(paste(Heights$block, Heights$pos, sep = "-"))

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

Heights$cdate <- as.character(Heights$date)
Heights$round <- ifelse(Heights$cdate %in% round1, 1, 
                     ifelse(Heights$cdate %in% round2, 2,
                            ifelse(Heights$cdate %in% round3, 3,
                                   ifelse(Heights$cdate %in% round4, 4,
                                          ifelse(Heights$cdate %in% round5, 5,
                                                 ifelse(Heights$cdate %in% round6, 6,
                                                        ifelse(Heights$cdate %in% round7, 7,
                                                               ifelse(Heights$cdate %in% round8, 8,
                                                                      ifelse(Heights$cdate %in% round9, 9,
                                                                             ifelse(Heights$cdate %in% round10, 10,
                                                                                    ifelse(Heights$cdate %in% round11, 11,
                                                                                           ifelse(Heights$cdate %in% round12, 12,
                                                                                                  ifelse(Heights$cdate %in% round13, 13,
                                                                                                         ifelse(Heights$cdate %in% round14, 14,
                                                                                                                ifelse(Heights$cdate %in% round15, 15,
                                                                                                                       ifelse(Heights$cdate %in% round16, 16,17))))))))))))))))
Heights$round <- as.factor(Heights$round)
str(Heights)
Heights$ind_round <- factor(paste(Heights$ind, Heights$round, sep = "_"))

##### More obvious issues #####

# Check for unrealistic values
summary(Heights)
Heights[which(Heights$veg > 250),]
Heights[which(Heights$bud > 250),]
Heights[which(Heights$flw > 250),]
Heights[which(Heights$frt > 250),]
Heights[which(Heights$abt > 250),]

#fix veg
Heights[Heights$date=="2019-06-06" & Heights$block=="E3" & Heights$pos==149,]$veg <- 30 #4530 should be 30
Heights[Heights$date=="2019-06-24" & Heights$block=="E2" & Heights$pos==109,]$veg <- 103.5 #1035 should be 103.5
Heights[Heights$date=="2019-07-08" & Heights$block=="E1" & Heights$pos==68,]$veg <- 116.5 #1165 should be 116.5
#fix bud
Heights[Heights$date=="2019-06-27" & Heights$block=="E3" & Heights$pos==57,]$bud <- 62.5 #625 should be 62.5
Heights[Heights$date=="2019-08-13" & Heights$block=="E10" & Heights$pos==38,]$bud <- 153 #1253 should be 153

#fix frt
Heights[Heights$date=="2019-09-29" & Heights$block=="E11" & Heights$pos==118,]$frt <- 146.5 #625 should be 62.5


Heights$date <- as.Date(Heights$date)

Heights$pos <- as.factor(Heights$pos)
Heights$mer <- as.factor(Heights$mer)
Heights$sec <- as.factor(Heights$sec)
Heights$new <- as.factor(Heights$new)

Heights$edited <- as.factor(Heights$edited)
Heights$deleted <- as.factor(Heights$deleted)
Heights$missed_N <- as.factor(Heights$missed_N)

Heights$ind <- factor(paste(Heights$block, Heights$pos, sep = "-"))

missing <- rename(missing, ind=x)

Heights2 <- anti_join(Heights, missing, by="ind") # all values in x where there aren't 


write.csv(Heights2, "Data/Clean/2019_Heights_Clean.csv", row.names=F)

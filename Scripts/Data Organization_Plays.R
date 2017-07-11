###########################################
##  ASA NFL DFS book                     ##
##  Data Oganization: Play-by-Play Data  ##
##  Stewart Gibson                       ##
##  7/10/17                              ##
###########################################

## Import data
plays.2016 <- read.csv("data/2016_play_by_play.csv")

## Remove plays that aren't offensive plays ($OffenseTeam == "")
plays.2016 <- plays.2016[which(plays.2016$OffenseTeam != ""),]

## Reomve PAT, 2-pt conversions, kickoffs, etc ($Down == 0)
plays.2016 <- plays.2016[which(plays.2016$Down != 0),]

## Keep only plays that are of $PlayType FUMBLES, PASS, RUSH, SACK, SCRAMBLE
plays.2016 <- plays.2016[which(plays.2016$PlayType %in% 
                                 c("FUMBLES", "PASS", "RUSH", "SACK", "SCRAMBLE")),]

## Convert teams to lowercase, change team abbreviations to match that of other data
plays.2016$OffenseTeam <- tolower(plays.2016$OffenseTeam)
plays.2016$OffenseTeam[which(plays.2016$OffenseTeam == 'gb')] <- 'gnb'
plays.2016$OffenseTeam[which(plays.2016$OffenseTeam == 'jax')] <- 'jac'
plays.2016$OffenseTeam[which(plays.2016$OffenseTeam == 'kc')] <- 'kan'
plays.2016$OffenseTeam[which(plays.2016$OffenseTeam == 'la')] <- 'lar'
plays.2016$OffenseTeam[which(plays.2016$OffenseTeam == 'ne')] <- 'nwe'
plays.2016$OffenseTeam[which(plays.2016$OffenseTeam == 'no')] <- 'nor'
plays.2016$OffenseTeam[which(plays.2016$OffenseTeam == 'sd')] <- 'sdg'
plays.2016$OffenseTeam[which(plays.2016$OffenseTeam == 'sf')] <- 'sfo'
plays.2016$OffenseTeam[which(plays.2016$OffenseTeam == 'tb')] <- 'tam'

## Create a $Week variable, input appropriate week for each row
plays.2016$Week <- NA
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-09-08","2016-09-11","2016-09-12"))] <- 1
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-09-15","2016-09-18","2016-09-19"))] <- 2
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-09-22","2016-09-25","2016-09-26"))] <- 3
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-09-29","2016-10-02","2016-10-03"))] <- 4
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-10-06","2016-10-09","2016-10-10"))] <- 5
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-10-13","2016-10-16","2016-10-17"))] <- 6
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-10-20","2016-10-23","2016-10-24"))] <- 7
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-10-27","2016-10-30","2016-10-31"))] <- 8
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-11-03","2016-11-06","2016-11-07"))] <- 9
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-11-10","2016-11-13","2016-11-14"))] <- 10
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-11-17","2016-11-20","2016-11-21"))] <- 11
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-11-24","2016-11-27","2016-11-28"))] <- 12
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-12-01","2016-12-04","2016-12-05"))] <- 13
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-12-08","2016-12-11","2016-12-12"))] <- 14
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-12-15","2016-12-17","2016-12-18","2016-12-19"))] <- 15
plays.2016$Week[which(plays.2016$GameDate %in% c("2016-12-22","2016-12-24","2016-12-25","2016-12-26"))] <- 16
plays.2016$Week[which(plays.2016$GameDate %in% c("2017-01-01"))] <- 17

## Save data environment for future use
save.image("data/clean_data_play_by_play.RData")


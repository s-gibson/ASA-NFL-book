################################################################### 
##  ASA NFL DFS book                                             ##
##  Ch. 2: How many fantasy points are scored per actual point?  ##
##  Stewart Gibson                                               ##
##  6/10/17                                                      ##
###################################################################

## Load packages
require(ggplot2)

## Import data
OU.2016 <- read.csv("~/Documents/ASA/NFL_book/data/2016_DonBest_VegasData_NFL_Week.csv")
OU.2012_2015 <- read.csv("~/Documents/ASA/NFL_book/data/2012_2015_Final_DonbestData_NFL.csv")

## Keep only regular season games
OU.2016 <- OU.2016[which(OU.2016$Regular.Season == 1),]

# Create variables for year and week
OU.2016$Year <- 2016
OU.2012_2015$Year <- substr(OU.2012_2015$Date, start = 7, stop = 10)
OU.2016$Week <- NA
OU.2012_2015$Week <- NA
OU.2016$Information <- as.character(OU.2016$Information)
OU.2012_2015$Information <- as.character(OU.2012_2015$Information)
for (i in c(1:17)) {
  OU.2016$Week[grep(paste('WEEK ', i, " ", sep = ''), OU.2016$Information)] <- i
  OU.2012_2015$Week[grep(paste('WEEK ', i, " ", sep = ''), OU.2012_2015$Information)] <- i
}

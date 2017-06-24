########################
##  ASA NFL DFS book  ##
##  Data Oganization  ##
##  Stewart Gibson    ##
##  6/10/17           ##
########################

## Import data of weekly scores, O/U's
OU.2016 <- read.csv("data/2016_DonBest_VegasData_NFL_Week.csv")
OU.2012_2015 <- read.csv("data/2012_2015_Final_DonbestData_NFL.csv")

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
rm(i)

# Create data frame of only 2015 data
OU.2015 <- OU.2012_2015[which(OU.2012_2015$Year == 2015),]

# Remove Preseaston/Regular Season/Postseason columns from OU.2016
OU.2016 <- OU.2016[,c(1:11,15:17)]

# Import players' weekly fantasy points data
Fantasy.2016 <- read.csv("data/2016_NFL_Fantasy_Points.csv")
Fantasy.2016$First.Name <- as.character(Fantasy.2016$First.Name)
Fantasy.2016$Last.Name <- as.character(Fantasy.2016$Last.Name)

# Remove [space] in front of players' first and last names
Fantasy.2016$First.Name <- gsub(" ", "", Fantasy.2016$First.Name)
Fantasy.2016$Last.Name <- gsub(" ", "", Fantasy.2016$Last.Name)

# Change class of Team Name (Home, Away) columns (OU.2015, OU.2016) to "character"
OU.2015$Home.Team <- as.character(OU.2015$Home.Team)
OU.2015$Away.Team <- as.character(OU.2015$Away.Team)
OU.2016$Home.Team <- as.character(OU.2016$Home.Team)
OU.2016$Away.Team <- as.character(OU.2016$Away.Team)

# Manually fill in missing Home Teams from OU.2016
OU.2016$Home.Team[which(is.na(OU.2016$Home.Team))] <- c("Atlanta Falcons", "Arizona Cardinals",
                                              "Arizona Cardinals", "Atlanta Falcons",
                                              "Arizona Cardinals", "Arizona Cardinals",
                                              "Atlanta Falcons", "Arizona Cardinals", 
                                              "Atlanta Falcons", "Arizona Cardinals",
                                              "Atlanta Falcons", "Atlanta Falcons",
                                              "Arizona Cardinals", "Arizona Cardinals",
                                              "Atlanta Falcons", "Atlanta Falcons")


# Replace elongated teams names in OU.2015_2016 with abbreviations (i.e. "Arizona Cardinals" ->
# "ari")
full.names <- unique(OU.2016$Home.Team)[order(unique(OU.2016$Home.Team))][c(1:19, 21, 20, 22:27, 29, 28, 30:32)]
for (i in c(1:32)) {
  OU.2016$Home.Team[which(OU.2016$Home.Team == 
                                 full.names[i])] <- as.character(levels(Fantasy.2016$Team)[i])
  OU.2016$Away.Team[which(OU.2016$Away.Team == 
                            full.names[i])] <- as.character(levels(Fantasy.2016$Team)[i])
  
  OU.2015$Home.Team[which(OU.2015$Home.Team == 
                            full.names[i])] <- as.character(levels(Fantasy.2016$Team)[i])
  OU.2015$Away.Team[which(OU.2015$Away.Team == 
                            full.names[i])] <- as.character(levels(Fantasy.2016$Team)[i])
  
}

rm(i, full.names)

## NOTE: Still need to do some work on "St. Louis Rams".  Wait to see if 2015 player fantasy data
## notes Rams' players as "stl" or "lar".

## Change Christine Michael's information in weeks 1-10 to reflect SEA games.  Substitute
## Russell Wilson's Team, H/A, Oppt info
Fantasy.2016$Team[which(Fantasy.2016$First.Name == "Christine" & 
                          Fantasy.2016$Week <= 10)] <- 'sea'
Fantasy.2016$h.a[which(Fantasy.2016$First.Name == "Christine" & 
                         Fantasy.2016$Week <= 10)] <- 
  Fantasy.2016$h.a[which(Fantasy.2016$First.Name == "Russell" & 
                           Fantasy.2016$Last.Name == "Wilson" &
                           Fantasy.2016$Week <= 10)]
Fantasy.2016$Oppt[which(Fantasy.2016$First.Name == "Christine" & 
                          Fantasy.2016$Week <= 10)] <-
  Fantasy.2016$Oppt[which(Fantasy.2016$First.Name == "Russell" & 
                           Fantasy.2016$Last.Name == "Wilson" &
                           Fantasy.2016$Week <= 10)]

## Change Josh Huff's information in weeks 1-8 to reflect PHI games.  Substitute Carson Wentz'
## Team, H/A, Oppt info
Fantasy.2016$Team[which(Fantasy.2016$First.Name == "Josh" &
                          Fantasy.2016$Last.Name == "Huff" &
                          Fantasy.2016$Week <= 8)] <- 'phi'
Fantasy.2016$h.a[which(Fantasy.2016$First.Name == "Josh" &
                         Fantasy.2016$Last.Name == "Huff" &
                         Fantasy.2016$Week <= 8)] <-
  Fantasy.2016$h.a[which(Fantasy.2016$First.Name == "Carson" & 
                           Fantasy.2016$Last.Name == "Wentz" &
                           Fantasy.2016$Week <= 8)]
Fantasy.2016$Oppt[which(Fantasy.2016$First.Name == "Josh" &
                          Fantasy.2016$Last.Name == "Huff" &
                          Fantasy.2016$Week <= 8)] <-
  Fantasy.2016$Oppt[which(Fantasy.2016$First.Name == "Carson" & 
                           Fantasy.2016$Last.Name == "Wentz" &
                           Fantasy.2016$Week <= 8)]

## Change Griff Whalen's team to 'sdg'
Fantasy.2016$Team[which(Fantasy.2016$Last.Name == 'Whalen')] <- 'sdg'

## Change Ronnie Hillman's information in weeks 1-11 to reflect MIN games.  Substitute with Matt
## Asiata's Team, H/A, Oppt information.
Fantasy.2016$Team[which(Fantasy.2016$First.Name == "Ronnie" &
                          Fantasy.2016$Last.Name == "Hillman" &
                          Fantasy.2016$Week <= 11)] <- 'min'
Fantasy.2016$h.a[which(Fantasy.2016$First.Name == "Ronnie" &
                         Fantasy.2016$Last.Name == "Hillman" &
                         Fantasy.2016$Week <= 11)] <-
  Fantasy.2016$h.a[which(Fantasy.2016$First.Name == "Matt" & 
                           Fantasy.2016$Last.Name == "Asiata" &
                           Fantasy.2016$Week <= 11 &
                           Fantasy.2016$Week >= 7)]
Fantasy.2016$Oppt[which(Fantasy.2016$First.Name == "Ronnie" &
                          Fantasy.2016$Last.Name == "Hillman" &
                          Fantasy.2016$Week <= 11)] <-
  Fantasy.2016$Oppt[which(Fantasy.2016$First.Name == "Matt" & 
                            Fantasy.2016$Last.Name == "Asiata" &
                            Fantasy.2016$Week <= 11 &
                            Fantasy.2016$Week >= 7)]


## Remove John Phillips - Wk 5
Fantasy.2016 <- Fantasy.2016[-1958,]

## REMOVE THIS CODE ONCE MASON PROVIDES REST OF WEEK 14 SCORES
## Remove Wk 14 Fantasy information
Fantasy.2016 <- Fantasy.2016[which(Fantasy.2016$Week != 14),]

## Add columns to Fantasy.2016 that show players' teams' actual points and actual spread
Fantasy.2016$Actual.Points <- NA
Fantasy.2016$Actual.Spread <- NA
Fantasy.2016$Team <- as.character(Fantasy.2016$Team)
for (i in c(1:nrow(Fantasy.2016))) {
  if (Fantasy.2016$h.a[i] == 'h') {
    Fantasy.2016$Actual.Points[i] <- OU.2016$Home.Team.Score[which(
      OU.2016$Home.Team == Fantasy.2016$Team[i] &
        OU.2016$Week == Fantasy.2016$Week[i])]
    
    Fantasy.2016$Actual.Spread[i] <- OU.2016$Actual.Spread..Relative.to.Home.Team.[which(
      OU.2016$Home.Team == Fantasy.2016$Team[i] &
        OU.2016$Week == Fantasy.2016$Week[i])]
  } else {
    Fantasy.2016$Actual.Points[i] <- OU.2016$Away.Team.Score[which(
      OU.2016$Away.Team == Fantasy.2016$Team[i] &
        OU.2016$Week == Fantasy.2016$Week[i])]
    
    Fantasy.2016$Actual.Spread[i] <- -1*OU.2016$Actual.Spread..Relative.to.Home.Team.[which(
      OU.2016$Away.Team == Fantasy.2016$Team[i] &
        OU.2016$Week == Fantasy.2016$Week[i])]
  }
}
rm(i)

## Create columns that store players' names as their full name (First Last) and as their first
## initial + last name
Fantasy.2016$First.Last <- paste(Fantasy.2016$First.Name, Fantasy.2016$Last.Name)
Fantasy.2016$Initial.Last <- paste(substr(Fantasy.2016$First.Last, start = 1, stop = 1),
                                   ". ", Fantasy.2016$Last.Name, sep = "")

## Save data environment for future use
save.image("~/Documents/ASA/ASA NFL book/data/clean_data.RData")

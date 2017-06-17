######################################################## 
##  ASA NFL DFS book                                  ##
##  Ch. 3.1: How do fantasy points vary with spread?  ##
##  Stewart Gibson                                    ##
##  6/10/17                                           ##
########################################################

## Load packages
require(ggplot2)

## Load data
load("data/clean_data.RData")

## Start by removing all Team Defenses from "Fantasy.2016".  Although defenses do contribute actual
## points to their teams' totals, a large portion of their fantasy production is tied to non-
## offensive outcomes.
Fantasy.2016 <- Fantasy.2016[which(Fantasy.2016$Pos != "Def"),] ## Should we remove D/ST from this?

## Create a dataframe of each teams' weekly spread and offensive fantasy point totals
Team.Fantasy.totals.2016 <- unique(Fantasy.2016[c("Week", "Team")])
Team.Fantasy.totals.2016$Spread <- NA
Team.Fantasy.totals.2016$Offensive.Fantasy.Points <- NA

for (i in c(1:nrow(Team.Fantasy.totals.2016))) {
  Team.Fantasy.totals.2016$Spread[i] <- Fantasy.2016$Actual.Spread[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i])][1]
  
  Team.Fantasy.totals.2016$Offensive.Fantasy.Points[i] <- sum(Fantasy.2016$DK.points[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i])], na.rm = T)
}

## Visualization: Scatter plot (with regression) of Offensive Fantasy Points vs. Spread for
## all teams.  Also include regression equation in plot.
Fantasy.Actual.reg <- lm(data = Team.Fantasy.totals.2016, formula = Offensive.Fantasy.Points ~
                           Spread)
summary(Fantasy.Actual.reg)
reg.eq <- paste("Y = ", round(Fantasy.Actual.reg$coefficients[1], 2), " - ", 
                abs(round(Fantasy.Actual.reg$coefficients[2], 2)), " * X", sep = '')

ggplot(data = Team.Fantasy.totals.2016, aes(x = Spread, y = Offensive.Fantasy.Points)) +
  geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ x) +
  ggtitle("[TBD] All Teams") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Spread") +
  ylab("Total Offensive Fantasy Points") +
  geom_text(x = 20, y = max(Team.Fantasy.totals.2016$Offensive.Fantasy.Points), label = reg.eq,
            color = 'blue', size = 5) +
  scale_x_continuous(limits = c(min(Team.Fantasy.totals.2016$Spread), 
                                max(Team.Fantasy.totals.2016$Spread) + 3)) +
  scale_y_continuous(limits = c(min(Team.Fantasy.totals.2016$Offensive.Fantasy.Points - 5),
                                max(Team.Fantasy.totals.2016$Offensive.Fantasy.Points + 5)))
ggsave("Visualizations/Ch_3.1_spread/Spread.vs.Fantasy.Points_ALL.png")

## Create the same visualization as above, but for one for each of all 32 teams
uniq.teams <- sort(unique(Team.Fantasy.totals.2016$Team))
for (i in c(1:32)) {
  dat <- Team.Fantasy.totals.2016[which(Team.Fantasy.totals.2016$Team == uniq.teams[i]),]
  Fantasy.Spread.reg <- lm(data = dat, formula = Offensive.Fantasy.Points ~
                             Spread)
  reg.eq <- paste("Y = ", round(Fantasy.Spread.reg$coefficients[1], 2), " - ", 
                  abs(round(Fantasy.Spread.reg$coefficients[2], 2)), " * X", sep = '')
  
  ggplot(data = dat, aes(x = Spread, y = Offensive.Fantasy.Points)) +
    geom_point() + 
    geom_smooth(method = 'lm', formula = y ~ x) +
    ggtitle(paste("[TBD]", toupper(uniq.teams[i]), sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Spread") +
    ylab("Total Offensive Fantasy Points") +
    geom_text(x = 20, y = max(Team.Fantasy.totals.2016$Offensive.Fantasy.Points), label = reg.eq,
              color = 'blue', size = 5) +
    scale_x_continuous(limits = c(min(Team.Fantasy.totals.2016$Spread), 
                                  max(Team.Fantasy.totals.2016$Spread) + 3)) +
    scale_y_continuous(limits = c(min(Team.Fantasy.totals.2016$Offensive.Fantasy.Points - 5),
                                  max(Team.Fantasy.totals.2016$Offensive.Fantasy.Points + 5)))
  
  ggsave(paste("Visualizations/Ch_3.1_spread/Team viz/Spread.vs.Fantasy.Points_",
               toupper(uniq.teams[i]), ".png", sep =''))
}


## Create a dataframe of each teams' weekly actual spread and offensive fantasy point totals
## BY POSITONAL UNIT (QB, RB, WR/TE)
Team.Fantasy.totals.Pos.2016 <- unique(Fantasy.2016[c("Week", "Team", "Pos")])
Team.Fantasy.totals.Pos.2016$Spread <- NA
Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points <- NA

Team.Fantasy.totals.Pos.2016 <- Team.Fantasy.totals.Pos.2016[
  which(Team.Fantasy.totals.Pos.2016$Pos == "QB" |
          Team.Fantasy.totals.Pos.2016$Pos == "RB" |
          Team.Fantasy.totals.Pos.2016$Pos == "WR")
  ,]

Team.Fantasy.totals.Pos.2016$Pos <- as.character(Team.Fantasy.totals.Pos.2016$Pos)
Team.Fantasy.totals.Pos.2016$Pos[which(
  Team.Fantasy.totals.Pos.2016$Pos %in% c('WR','TE'))] <- "WR/TE"

for (i in c(1:nrow(Team.Fantasy.totals.Pos.2016))) {
  if (Team.Fantasy.totals.Pos.2016$Pos[i] == "QB") {
    Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points[i] <- sum(Fantasy.2016$DK.points[which(
      Fantasy.2016$Team == Team.Fantasy.totals.Pos.2016$Team[i] &
        Fantasy.2016$Week == Team.Fantasy.totals.Pos.2016$Week[i] &
        Fantasy.2016$Pos == 'QB')], na.rm = T)} else if (
          Team.Fantasy.totals.Pos.2016$Pos[i] == "RB") {
          Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points[i] <- 
            sum(Fantasy.2016$DK.points[which(
              Fantasy.2016$Team == Team.Fantasy.totals.Pos.2016$Team[i] &
                Fantasy.2016$Week == Team.Fantasy.totals.Pos.2016$Week[i] &
                Fantasy.2016$Pos == 'RB')], na.rm = T)} else {
                  Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points[i] <- 
                    sum(Fantasy.2016$DK.points[which(
                      Fantasy.2016$Team == Team.Fantasy.totals.Pos.2016$Team[i] &
                        Fantasy.2016$Week == Team.Fantasy.totals.Pos.2016$Week[i] &
                        Fantasy.2016$Pos %in% c("WR","TE"))], na.rm = T)}
  
  Team.Fantasy.totals.Pos.2016$Spread[i] <- Fantasy.2016$Actual.Spread[which(
    Fantasy.2016$Team == Team.Fantasy.totals.Pos.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.Pos.2016$Week[i])][1]
}

## Visualization: Scatter plot (with regression) of Offensive Fantasy Points vs. Spread for
## all teams.  Also include regression equation in plot.
#Fantasy.Actual.reg <- lm(data = Team.Fantasy.totals.Pos.2016, formula = Offensive.Fantasy.Points ~
 #                          Spread)
#summary(Fantasy.Actual.reg)
#reg.eq <- paste("Y = ", round(Fantasy.Actual.reg$coefficients[1], 2), " - ", 
 #               abs(round(Fantasy.Actual.reg$coefficients[2], 2)), " * X", sep = '')

ggplot(data = Team.Fantasy.totals.Pos.2016, aes(x = Spread, y = Offensive.Fantasy.Points)) +
  geom_point(aes(group = Pos, color = Pos), size = 0.5) + 
  geom_smooth(aes(group = Pos, color = Pos), method = 'lm', formula = y ~ x) +
  ggtitle("[TBD] All Teams") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Spread") +
  ylab("Total Offensive Fantasy Points") +
  #geom_text(x = 20, y = max(Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points), label = reg.eq,
   #         color = 'blue', size = 5) +
  scale_x_continuous(limits = c(min(Team.Fantasy.totals.Pos.2016$Spread), 
                                max(Team.Fantasy.totals.Pos.2016$Spread) + 3)) +
  scale_y_continuous(limits = c(min(Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points - 5),
                                max(Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points + 5)))
ggsave("Visualizations/Ch_3.1_spread/By Position/Spread.vs.Fantasy.Points_ALL.png")

## Create the same visualization as above, but for one for each of all 32 teams
uniq.teams <- sort(unique(Team.Fantasy.totals.Pos.2016$Team))
for (i in c(1:32)) {
  dat <- Team.Fantasy.totals.Pos.2016[which(Team.Fantasy.totals.Pos.2016$Team == uniq.teams[i]),]
  #Fantasy.Spread.reg <- lm(data = dat, formula = Offensive.Fantasy.Points ~
   #                          Spread)
  #reg.eq <- paste("Y = ", round(Fantasy.Spread.reg$coefficients[1], 2), " - ", 
   #               abs(round(Fantasy.Spread.reg$coefficients[2], 2)), " * X", sep = '')
  
  ggplot(data = dat, aes(x = Spread, y = Offensive.Fantasy.Points)) +
    geom_point(aes(group = Pos, color = Pos)) + 
    geom_smooth(aes(group = Pos, color = Pos), method = 'lm', formula = y ~ x) +
    ggtitle(paste("[TBD]", toupper(uniq.teams[i]), sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Spread") +
    ylab("Total Offensive Fantasy Points") +
    #geom_text(x = 20, y = max(Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points), label = reg.eq,
     #         color = 'blue', size = 5) +
    scale_x_continuous(limits = c(min(Team.Fantasy.totals.Pos.2016$Spread), 
                                  max(Team.Fantasy.totals.Pos.2016$Spread) + 3)) +
    scale_y_continuous(limits = c(min(Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points - 5),
                                  max(Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points + 5)))
  
  ggsave(paste("Visualizations/Ch_3.1_spread/By Position/Team viz/Spread.vs.Fantasy.Points_",
               toupper(uniq.teams[i]), ".png", sep =''))
}



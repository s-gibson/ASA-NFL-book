############################################################################### 
##  ASA NFL DFS book                                                         ##
##  Ch. 3.2: How does injury affect fantasy points scored per actual point?  ##
##  Stewart Gibson                                                           ##
##  6/10/17                                                                  ##
###############################################################################

## Load packages
require(ggplot2)

## Load data
load("data/clean_data.RData")

## Start by removing all Team Defenses from "Fantasy.2016".  Although defenses do contribute actual
## points to their teams' totals, a large portion of their fantasy production is tied to non-
## offensive outcomes.
Fantasy.2016 <- Fantasy.2016[which(Fantasy.2016$Pos != "Def"),] 

## Create a dataframe of each teams' weekly actual point and offensive fantasy point totals
Team.Fantasy.totals.2016 <- unique(Fantasy.2016[c("Week", "Team")])
Team.Fantasy.totals.2016$Actual.Points <- NA
Team.Fantasy.totals.2016$Offensive.Fantasy.Points <- NA

for (i in c(1:nrow(Team.Fantasy.totals.2016))) {
  Team.Fantasy.totals.2016$Actual.Points[i] <- Fantasy.2016$Actual.Points[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i])][1]
  
  Team.Fantasy.totals.2016$Offensive.Fantasy.Points[i] <- sum(Fantasy.2016$DK.points[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i])], na.rm = T)
}

## Add column to dataframe indicating whether each team was missing a key player due to injury in 
## each week.
Team.Fantasy.totals.2016$Injury <- NA
for (i in 1:nrow(Team.Fantasy.totals.2016)) {
  Team.Fantasy.totals.2016$Injury[i] <- as.numeric(sum(Injuries.2016[which(
    Injuries.2016$Team == Team.Fantasy.totals.2016$Team[i]
  ),(Team.Fantasy.totals.2016$Week[i] + 3)] %in% c("D", "IR", "O", "Q")) > 0)
}

Team.Fantasy.totals.2016$Injury[which(Team.Fantasy.totals.2016$Injury == 1)] <- "Yes"
Team.Fantasy.totals.2016$Injury[which(Team.Fantasy.totals.2016$Injury == 0)] <- "No"

## Create plot of team total fantasy points vs. actual points, for team-weeks where there was a
## significant injury and for weeks where there was not a significant injury.
ggplot(data = Team.Fantasy.totals.2016, aes(x = Actual.Points, y = Offensive.Fantasy.Points,
                                            color = Injury, group = Injury)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  ggtitle("Impact of Key Player Injury on FP/AP Ratio") +
  ylab("Total Offensive Fantasy Points") +
  xlab("Actual Points") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Visualizations/Ch_3.2_injuries/FP_vs_AP_Injury.png")
  
ggplot(data = Team.Fantasy.totals.2016, aes(x = Offensive.Fantasy.Points,
                                            color = Injury, fill = Injury)) +
  geom_density(alpha = 0.5) +
  xlab("Offensive Fantasy Points") +
  ggtitle("Offensive Fantasy Points Distribution by Injury") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Visualizations/Ch_3.2_injuries/FP Distributions_Injury.png")
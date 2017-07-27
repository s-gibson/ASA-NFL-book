################################################################################ 
##  ASA NFL DFS book                                                          ##
##  Ch. 3.3: How do teams' fantasy point totals vary in home and away games?  ##
##  Stewart Gibson                                                            ##
##  7/26/17                                                                   ##
################################################################################

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
Team.Fantasy.totals.2016$H.A <- NA

for (i in c(1:nrow(Team.Fantasy.totals.2016))) {
  Team.Fantasy.totals.2016$Spread[i] <- Fantasy.2016$Actual.Spread[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i])][1]
  
  Team.Fantasy.totals.2016$Offensive.Fantasy.Points[i] <- sum(Fantasy.2016$DK.points[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i])], na.rm = T)
  
  Team.Fantasy.totals.2016$H.A[i] <- as.character(Fantasy.2016$h.a[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i])])[1]
}
Team.Fantasy.totals.2016$H.A[which(Team.Fantasy.totals.2016$H.A == 'h')] <- "Home"
Team.Fantasy.totals.2016$H.A[which(Team.Fantasy.totals.2016$H.A == 'a')] <- "Away"

## Create fantasy point distributions split by home/away
uniq.teams <- sort(unique(Team.Fantasy.totals.2016$Team))

for (i in 1:length(uniq.teams)) {
  ggplot(data = Team.Fantasy.totals.2016[which(Team.Fantasy.totals.2016$Team == uniq.teams[i]),],
         aes(x=Offensive.Fantasy.Points, group=H.A, color=H.A, fill = H.A)) +
    geom_density(alpha=0.5) +
    scale_x_continuous(limits = c(0,220)) +
    scale_y_continuous(limits = c(0, 0.055)) +
    xlab("Offensive Fantasy Points") +
    scale_fill_discrete(name = "Home/Away") +
    scale_color_discrete(name = "Home/Away") +
    ggtitle(paste(toupper(uniq.teams[i]), "Total Fantasy Points Home/Away Splits")) +
    theme(plot.title = element_text(hjust = 0.5))
    
  ggsave(paste("Visualizations/Ch_3.3_H.A/",uniq.teams[i],".png",sep = ""))
  }
  
  
  
############################################################## 
##  ASA NFL DFS book                                        ##
##  Player Fantasy point, salary, and FP/$1K distributions  ##
##  Stewart Gibson                                          ##
##  7/10/17                                                  ##
##############################################################

## Load packages
require(ggplot2)

## Load data
load("data/clean_data.RData")
load("data/clean_data_play_by_play.RData")

## Create dataframe for each team's games.  Include points scored, spread, run play count
## and pass play count
playcounts <- data.frame(Team = unique(Fantasy.2016[c("Team", "Week")])[1],
                         Week = unique(Fantasy.2016[c("Team", "Week")])[2],
                         Point.Total = NA, Spread = NA, Run.Plays = NA, Pass.Plays = NA)

for (i in 1:nrow(playcounts)) { 
  playcounts$Point.Total[i] <- Fantasy.2016$Actual.Points[which(
    Fantasy.2016$Team == playcounts$Team[i] & Fantasy.2016$Week == playcounts$Week[i])][1]
  playcounts$Spread[i] <- Fantasy.2016$Actual.Spread[which(
    Fantasy.2016$Team == playcounts$Team[i] & Fantasy.2016$Week == playcounts$Week[i])][1]
  playcounts$Run.Plays[i] <- length(which(
    plays.2016$OffenseTeam == playcounts$Team[i] & plays.2016$Week == playcounts$Week[i] &
      plays.2016$IsRush == 1))[1]
  playcounts$Pass.Plays[i] <- length(which(
    plays.2016$OffenseTeam == playcounts$Team[i] & plays.2016$Week == playcounts$Week[i] &
      (plays.2016$IsPass == 1 | plays.2016$PlayType == "SACK")))[1]
}

## Plot Run/Pass plays vs. spread and point total
ggplot(data = playcounts) +
  geom_point(aes(x = Spread, y = Run.Plays, color = 'red'), size = 0.5, alpha = 0.5) +
  geom_smooth(aes(x = Spread, y = Run.Plays), color = 'red', method = 'lm', formula = y~x) +
  geom_point(aes(x = Spread, y = Pass.Plays, color = 'blue'), size = 0.5, alpha = 0.5) +
  geom_smooth(aes(x = Spread, y = Pass.Plays), color = 'blue', method = 'lm', formula = y~x) +
  ylab("Play Counts") +
  xlab("Spread") +
  ggtitle("Run/Pass Play Counts vs. Spread") +
  scale_color_manual(labels = c("Pass","Run"), values = c("red" = "red", "blue" = "blue")) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file = "Visualizations/Ch_6/Playcounts vs Spread.png")
  
ggplot(data = playcounts) +
  geom_point(aes(x = Point.Total, y = Run.Plays, color = 'red'), size = 0.5, alpha = 0.5) +
  geom_smooth(aes(x = Point.Total, y = Run.Plays), color = 'red', method = 'lm', formula = y~x) +
  geom_point(aes(x = Point.Total, y = Pass.Plays, color = 'blue'), size = 0.5, alpha = 0.5) +
  geom_smooth(aes(x = Point.Total, y = Pass.Plays), color = 'blue', method = 'lm', formula = y~x) +
  ylab("Play Counts") +
  xlab("Point Total") +
  ggtitle("Run/Pass Play Counts vs. Point Total") +
  scale_color_manual(labels = c("Pass","Run"), values = c("red" = "red", "blue" = "blue")) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file = "Visualizations/Ch_6/Playcounts vs Point Total.png")

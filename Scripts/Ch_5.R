###################################################### 
##  ASA NFL DFS book                                ##
##  Ch. 5: How do players and positions correlate?  ##
##  Stewart Gibson                                  ##
##  6/24/17                                         ##
######################################################

## Load packages
require(ggplot2)

## Start by removing all Team Defenses from "Fantasy.2016".  Although defenses do contribute actual
## points to their teams' totals, a large portion of their fantasy production is tied to non-
## offensive outcomes.
Fantasy.2016 <- Fantasy.2016[which(Fantasy.2016$Pos != "Def"),] ## Should we remove D/ST from this?

## Create separate dataframes for each positional unit: QB, RB, WR/TE
QB.2016 <- Fantasy.2016[which(Fantasy.2016$Pos == 'QB'),]
RB.2016 <- Fantasy.2016[which(Fantasy.2016$Pos == 'RB'),]
WR_TE.2016 <- Fantasy.2016[which(Fantasy.2016$Pos %in% c('WR','TE')),]

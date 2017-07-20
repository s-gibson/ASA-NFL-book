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

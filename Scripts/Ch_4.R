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

## Create separate dataframes for each positional unit: QB, RB, WR/TE
QB.2016 <- Fantasy.2016[which(Fantasy.2016$Pos == 'QB'),]
RB.2016 <- Fantasy.2016[which(Fantasy.2016$Pos == 'RB'),]
WR_TE.2016 <- Fantasy.2016[which(Fantasy.2016$Pos %in% c('WR','TE')),]

uniq.teams <- sort(unique(Fantasy.2016$Team))
for (i in 1:length(uniq.teams)) {
  dat.QB <- QB.2016[which(QB.2016$Team == uniq.teams[i]),]
  dat.RB <- RB.2016[which(RB.2016$Team == uniq.teams[i]),]
  dat.WR_TE <- WR_TE.2016[which(WR_TE.2016$Team == uniq.teams[i]),]
  
  # Plot QB trendline
  ggplot(data = dat.QB, aes(x = Week, y = DK.points, group = Last.Name, color = Last.Name)) +
    geom_line() +
    xlab('Week') +
    ylab('Fantasy Points') +
    ggtitle(paste(toupper(uniq.teams[i]), "QB Fantasy Trendline", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("Visualizations/Ch_4/QB/QB_", uniq.teams[i],".png", sep = "") )
  
  # Plot RB trendline
  ggplot(data = dat.RB, aes(x = Week, y = DK.points, group = Last.Name, color = Last.Name)) +
    geom_line() +
    xlab('Week') +
    ylab('Fantasy Points') +
    ggtitle(paste(toupper(uniq.teams[i]), "RB Fantasy Trendline", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
    
  ggsave(paste("Visualizations/Ch_4/RB/RB_", uniq.teams[i],".png", sep = "") )
  
  # Plot WR/TE trendline
  ggplot(data = dat.WR_TE, aes(x = Week, y = DK.points, group = Last.Name, color = Last.Name)) +
    geom_line() +
    xlab('Week') +
    ylab('Fantasy Points') +
    ggtitle(paste(toupper(uniq.teams[i]), "WR/TE Fantasy Trendline", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
    
  ggsave(paste("Visualizations/Ch_4/WR_TE/WR_TE_", uniq.teams[i],".png", sep = "") )
  
}


########################################################## 
##  ASA NFL DFS book                                    ##
##  Ch. 4: How do share positional fantasy production?  ##
##  Stewart Gibson                                      ##
##  6/24/17                                             ##
##########################################################

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

## Subset players who are above cutoff for total fantasy points in 2016
# Establish cutoffs
QB.cutoff <- 20
RB.cutoff <- 45
WR_TE.cutoff <- 30

# QB
QB.2016.totals <- data.frame(First.Last = unique(QB.2016$First.Last), Total.DKP = NA)
for (i in 1:nrow(QB.2016.totals)) {
  QB.2016.totals$Total.DKP[i] <- sum(QB.2016$DK.points[which(QB.2016$First.Last == 
                                                     QB.2016.totals$First.Last[i])], na.rm = T)
}
QB.list <- QB.2016.totals$First.Last[which(QB.2016.totals$Total.DKP >= QB.cutoff)]

# RB
RB.2016.totals <- data.frame(First.Last = unique(RB.2016$First.Last), Total.DKP = NA)
for (i in 1:nrow(RB.2016.totals)) {
  RB.2016.totals$Total.DKP[i] <- sum(RB.2016$DK.points[which(RB.2016$First.Last == 
                                                               RB.2016.totals$First.Last[i])], na.rm = T)
}
RB.list <- RB.2016.totals$First.Last[which(RB.2016.totals$Total.DKP >= QB.cutoff)]

# WR/TE
WR_TE.2016.totals <- data.frame(First.Last = unique(WR_TE.2016$First.Last), Total.DKP = NA)
for (i in 1:nrow(WR_TE.2016.totals)) {
  WR_TE.2016.totals$Total.DKP[i] <- sum(WR_TE.2016$DK.points[which(WR_TE.2016$First.Last == 
                                                                     WR_TE.2016.totals$First.Last[i])], na.rm = T)
}
WR_TE.list <- WR_TE.2016.totals$First.Last[which(WR_TE.2016.totals$Total.DKP >= WR_TE.cutoff)]

## Create trendlines for all teams and their positional units, only charting players who are 
## above positional cutoff threshold
uniq.teams <- sort(unique(Fantasy.2016$Team))

for (i in 1:length(uniq.teams)) {
  dat.QB <- QB.2016[which(QB.2016$Team == uniq.teams[i] & 
                      QB.2016$First.Last %in% QB.list),]
  dat.RB <- RB.2016[which(RB.2016$Team == uniq.teams[i] & 
                            RB.2016$First.Last %in% RB.list),]
  dat.WR_TE <- WR_TE.2016[which(WR_TE.2016$Team == uniq.teams[i] & 
                                  WR_TE.2016$First.Last %in% WR_TE.list),]
  
  # Plot QB trendline
  ggplot(data = dat.QB, aes(x = Week, y = DK.points, group = Initial.Last, 
                            color = Initial.Last)) +
    geom_line() +
    geom_point() +
    xlab('Week') +
    ylab('Fantasy Points') +
    ggtitle(paste(toupper(uniq.teams[i]), "QB Fantasy Trendline", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1:17)) +
    scale_color_discrete(name = "Player")
  
  ggsave(paste("Visualizations/Ch_4/Restrictive/QB/QB_", uniq.teams[i],".png", sep = "") )
  
  # Plot RB trendline
  ggplot(data = dat.RB, aes(x = Week, y = DK.points, group = Initial.Last, 
                            color = Initial.Last)) +
    geom_line() +
    geom_point() +
    xlab('Week') +
    ylab('Fantasy Points') +
    ggtitle(paste(toupper(uniq.teams[i]), "RB Fantasy Trendline", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1:17)) +
    scale_color_discrete(name = "Player")
  
  ggsave(paste("Visualizations/Ch_4/Restrictive/RB/RB_", uniq.teams[i],".png", sep = "") )
  
  # Plot WR/TE trendline
  ggplot(data = dat.WR_TE, aes(x = Week, y = DK.points, group = Initial.Last, 
                               color = Initial.Last)) +
    geom_line() +
    geom_point() +
    xlab('Week') +
    ylab('Fantasy Points') +
    ggtitle(paste(toupper(uniq.teams[i]), "WR/TE Fantasy Trendline", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1:17)) +
    scale_color_discrete(name = "Player")
    
  ggsave(paste("Visualizations/Ch_4/Restrictive/WR_TE/WR_TE_", uniq.teams[i],".png", sep = "") )
  
}


## Create trendlines for all teams and their positional units, NOT RESTRICTING BY TOTAL DKP
## CUTOFF.  Can possibly include these charts in appendix
for (i in 1:length(uniq.teams)) {
  dat.QB <- QB.2016[which(QB.2016$Team == uniq.teams[i]),]
  dat.RB <- RB.2016[which(RB.2016$Team == uniq.teams[i]),]
  dat.WR_TE <- WR_TE.2016[which(WR_TE.2016$Team == uniq.teams[i]),]
  
  # Plot QB trendline
  ggplot(data = dat.QB, aes(x = Week, y = DK.points, group = Initial.Last, 
                            color = Initial.Last)) +
    geom_line() +
    geom_point() +
    xlab('Week') +
    ylab('Fantasy Points') +
    ggtitle(paste(toupper(uniq.teams[i]), "QB Fantasy Trendline", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1:17)) +
    scale_color_discrete(name = "Player")
  
  ggsave(paste("Visualizations/Ch_4/Non_Restrictive/QB/QB_", uniq.teams[i],".png", sep = "") )
  
  # Plot RB trendline
  ggplot(data = dat.RB, aes(x = Week, y = DK.points, group = Initial.Last, 
                            color = Initial.Last)) +
    geom_line() +
    geom_point() +
    xlab('Week') +
    ylab('Fantasy Points') +
    ggtitle(paste(toupper(uniq.teams[i]), "RB Fantasy Trendline", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1:17)) +
    scale_color_discrete(name = "Player")
  
  ggsave(paste("Visualizations/Ch_4/Non_Restrictive/RB/RB_", uniq.teams[i],".png", sep = "") )
  
  # Plot WR/TE trendline
  ggplot(data = dat.WR_TE, aes(x = Week, y = DK.points, group = Initial.Last, 
                               color = Initial.Last)) +
    geom_line() +
    geom_point() +
    xlab('Week') +
    ylab('Fantasy Points') +
    ggtitle(paste(toupper(uniq.teams[i]), "WR/TE Fantasy Trendline", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1:17)) +
    scale_color_discrete(name = "Player")
  
  ggsave(paste("Visualizations/Ch_4/Non_Restrictive/WR_TE/WR_TE_", uniq.teams[i],".png", sep = "") )
  
}


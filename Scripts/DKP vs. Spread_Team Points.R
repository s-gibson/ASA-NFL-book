############################################################# 
##  ASA NFL DFS book                                       ##
##  Player Fantasy points vs. spread and team point total  ##
##  Stewart Gibson                                         ##
##  7/9/17                                                 ##
#############################################################

## Load packages
require(ggplot2)

## Load data
load("data/clean_data.RData")

## Create dataframe of each players season point totals, only include players who scored above
## positional cutoff levels.
season.totals <- data.frame(First.Last = unique(Fantasy.2016$First.Last), pos = NA, 
                            DK.points.total = NA, Games = NA)
for  (i in 1:nrow(season.totals)) {
  season.totals$pos[i] <- as.character(Fantasy.2016$Pos[which(Fantasy.2016$First.Last == 
                                                                season.totals$First.Last[i])][1])
  season.totals$DK.points.total[i] <- sum(Fantasy.2016$DK.points[which(
    Fantasy.2016$First.Last == season.totals$First.Last[i])])
  season.totals$Games[i] <- length(which(Fantasy.2016$First.Last == 
                                           season.totals$First.Last[i] & 
                                           !is.na(Fantasy.2016$DK.points) &
                                           !is.na(Fantasy.2016$Actual.Spread) &
                                           !is.na(Fantasy.2016$Actual.Points)))
}

QB.cutoff <- 20
RB.cutoff <- 45
WR_TE.cutoff <- 30

## Set unique players list (only considering players who scored above their positional cutoff)
uniq.players <- unique(season.totals$First.Last[which(
  ((season.totals$pos == 'QB' & season.totals$DK.points.total >= QB.cutoff) |
    (season.totals$pos == 'RB' & season.totals$DK.points.total >= RB.cutoff) |
    (season.totals$pos == 'WR' & season.totals$DK.points.total >= WR_TE.cutoff) |
    (season.totals$pos == 'TE' & season.totals$DK.points.total >= WR_TE.cutoff)) & 
    season.totals$Games > 3)])

## Subset Fantasy.2016 to only include players above positional cutoffs
Fantasy.2016.sub <- Fantasy.2016[which(Fantasy.2016$First.Last %in% uniq.players),]

## Create charts
uniq.teams <- sort(unique(Fantasy.2016$Team))

for (i in 1:length(uniq.teams)) {
  dat <- Fantasy.2016[which(Fantasy.2016$Team == uniq.teams[i]),]
  
  season.totals <- data.frame(First.Last = unique(dat$First.Last), pos = NA, 
                              DK.points.total = NA, Games = NA)
  for  (j in 1:nrow(season.totals)) {
    season.totals$pos[j] <- as.character(dat$Pos[which(dat$First.Last == 
                                                                  season.totals$First.Last[j])][1])
    season.totals$DK.points.total[j] <- sum(dat$DK.points[which(
      dat$First.Last == season.totals$First.Last[j])])
    season.totals$Games[j] <- length(which(dat$First.Last == 
                                             season.totals$First.Last[j] & 
                                             !is.na(dat$DK.points) &
                                             !is.na(dat$Actual.Spread) &
                                             !is.na(dat$Actual.Points)))
  }
  
  considerable.players <- unique(season.totals$First.Last[which(
    ((season.totals$pos == 'QB' & season.totals$DK.points.total >= QB.cutoff) |
       (season.totals$pos == 'RB' & season.totals$DK.points.total >= RB.cutoff) |
       (season.totals$pos == 'WR' & season.totals$DK.points.total >= WR_TE.cutoff) |
       (season.totals$pos == 'TE' & season.totals$DK.points.total >= WR_TE.cutoff)) & 
      season.totals$Games > 3)])
  
  dat <- dat[which(dat$First.Last %in% considerable.players),]
  dat <- dat[which(!(dat$First.Last %in% c("Matt Barkley", "Kevin White", "Ladarius Green",
                                           "Kevin Hogan"))),]
  
  ggplot(data = dat, aes(x = Actual.Points, y = DK.points, color = Initial.Last, group = First.Last)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ poly(x,3), se = F) +
    ylab("Fantasy Points") +
    xlab("Team Point Total") +
    ggtitle(paste(toupper(uniq.teams[i]), "Player Fantasy Points vs. Team Point Total", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_discrete(name = "Player")
    
  ggsave(paste("Visualizations/Fantasy Point Comp Charts/Vs. Point Total/",uniq.teams[i],
               ".png", sep = ""))
  
  ggplot(data = dat, aes(x = Actual.Spread, y = DK.points, color = Initial.Last, group = First.Last)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ poly(x,3), se = F) +
    ylab("Fantasy Points") +
    xlab("Game Spread") +
    ggtitle(paste(toupper(uniq.teams[i]), "Player Fantasy Points vs. Game Spread", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_discrete(name = "Player")
  
  ggsave(paste("Visualizations/Fantasy Point Comp Charts/Vs. Spread/",uniq.teams[i],
               ".png", sep = ""))
  }
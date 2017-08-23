############################################################## 
##  ASA NFL DFS book                                        ##
##  Player Fantasy point, salary, and FP/$1K distributions  ##
##  Stewart Gibson                                          ##
##  7/8/17                                                  ##
##############################################################

## Load packages
require(ggplot2)

## Load data
load("data/clean_data.RData")

## Create dataframe of each players season point totals, only include players who scored above
## positional cutoff levels.
season.totals <- cbind(unique(Fantasy.2016[,c("First.Last","Team")]), 
                       data.frame(pos = NA, DK.points.total = NA))

for  (i in 1:nrow(season.totals)) {
  season.totals$pos[i] <- as.character(Fantasy.2016$Pos[which(
    Fantasy.2016$First.Last == season.totals$First.Last[i] &
      Fantasy.2016$Team == season.totals$Team[i])][1])
  season.totals$DK.points.total[i] <- sum(Fantasy.2016$DK.points[which(
    Fantasy.2016$First.Last == season.totals$First.Last[i] &
      Fantasy.2016$Team == season.totals$Team[i])])
}

QB.cutoff <- 20
RB.cutoff <- 45
WR_TE.cutoff <- 30

## Set unique players list (only considering players who scored above their positional cutoff)
uniq.players <- season.totals[which(
  (season.totals$pos == 'QB' & season.totals$DK.points.total >= QB.cutoff)|
    (season.totals$pos == 'RB' & season.totals$DK.points.total >= RB.cutoff)|
    (season.totals$pos == 'WR' & season.totals$DK.points.total >= WR_TE.cutoff) |
    (season.totals$pos == 'TE' & season.totals$DK.points.total >= WR_TE.cutoff)), c(1,2,3)]

for (i in 1:nrow(uniq.players)) {
  dat <- Fantasy.2016[which(Fantasy.2016$First.Last == uniq.players$First.Last[i] &
                              Fantasy.2016$Team == uniq.players$Team[i] &
                              Fantasy.2016$Pos == uniq.players$pos[i]),]
  
  # set color scheme
  if (dat$Team[length(dat$Team)] == "ari") {col.set = c("darkred","white")
  } else if (dat$Team[length(dat$Team)] == "atl") {col.set = c("black","red")
  } else if (dat$Team[length(dat$Team)] == "bal") {col.set = c("darkorchid4","goldenrod")
  } else if (dat$Team[length(dat$Team)] == "buf") {col.set = c("mediumblue","white")
  } else if (dat$Team[length(dat$Team)] == "car") {col.set = c("black","deepskyblue")
  } else if (dat$Team[length(dat$Team)] == "chi") {col.set = c("darkblue","orange")
  } else if (dat$Team[length(dat$Team)] == "cin") {col.set = c("darkorange","black")
  } else if (dat$Team[length(dat$Team)] == "cle") {col.set = c("darkorange2","chocolate4")
  } else if (dat$Team[length(dat$Team)] == "dal") {col.set = c("gray60","midnightblue")
  } else if (dat$Team[length(dat$Team)] == "den") {col.set = c("navy","darkorange2")
  } else if (dat$Team[length(dat$Team)] == "det") {col.set = c("dodgerblue","grey")
  } else if (dat$Team[length(dat$Team)] == "gnb") {col.set = c("green4","yellow")
  } else if (dat$Team[length(dat$Team)] == "hou") {col.set = c("red3","royalblue4")
  } else if (dat$Team[length(dat$Team)] == "ind") {col.set = c("white","blue3")
  } else if (dat$Team[length(dat$Team)] == "jac") {col.set = c("turquoise","khaki")
  } else if (dat$Team[length(dat$Team)] == "kan") {col.set = c("red","gold")
  } else if (dat$Team[length(dat$Team)] == "lar") {col.set = c("navy","lightgoldenrod")
  } else if (dat$Team[length(dat$Team)] == "mia") {col.set = c("turquoise","orange")
  } else if (dat$Team[length(dat$Team)] == "min") {col.set = c("purple","gold")
  } else if (dat$Team[length(dat$Team)] == "nor") {col.set = c("lightgoldenrod","black")
  } else if (dat$Team[length(dat$Team)] == "nwe") {col.set = c("royalblue4","red4")
  } else if (dat$Team[length(dat$Team)] == "nyg") {col.set = c("blue","red")
  } else if (dat$Team[length(dat$Team)] == "nyj") {col.set = c("darkgreen","white")
  } else if (dat$Team[length(dat$Team)] == "oak") {col.set = c("grey","black")
  } else if (dat$Team[length(dat$Team)] == "phi") {col.set = c("seagreen4","black")
  } else if (dat$Team[length(dat$Team)] == "pit") {col.set = c("black", "yellow")
  } else if (dat$Team[length(dat$Team)] == "sdg") {col.set = c("deepskyblue","yellow")
  } else if (dat$Team[length(dat$Team)] == "sea") {col.set = c("navy","chartreuse")
  } else if (dat$Team[length(dat$Team)] == "sfo") {col.set = c("red","darkgoldenrod")
  } else if (dat$Team[length(dat$Team)] == "tam") {col.set = c("red3","burlywood4")
  } else if (dat$Team[length(dat$Team)] == "ten") {col.set = c("steelblue1","steelblue4")
  } else {col.set = c("orangered4","gold")}
  
  # Plot distributions
  ggplot(data = dat) +
    stat_density(aes(DK.points), adjust = 0.5, color = col.set[2], fill = col.set[1], size = 1) +
    xlab("DK Fantasy Points") +
    scale_x_continuous(limits = c(-10,60)) +
    scale_y_continuous(limits = c(0, .2)) +
    ggtitle(paste(dat$First.Last[1], "DK Points Distribution (2016)", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste('Visualizations/Distributions/DK Points/',dat$First.Last[1],'.png', sep = ''))
  ggsave(paste('Visualizations/Distributions/By Position/DK Points/',
               dat$Pos[1],"/", dat$First.Last[1],'.png', sep = ''))
  ggsave(paste('Visualizations/Distributions/By Team/DK Points/',
               dat$Team[1], dat$First.Last[1],'.png', sep = ''))
  
  ggplot(data = dat[which(dat$DK.salary > 0),]) +
    stat_density(aes(DK.salary), adjust = 0.5, color = col.set[2], fill = col.set[1], size = 1) +
    xlab("DK Salary") +
    scale_x_continuous(limits = c(1000, 12000)) +
    scale_y_continuous(limits = c(0, 0.0015)) +
    ggtitle(paste(dat$First.Last[1], "DK Salary Distribution (2016)", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste('Visualizations/Distributions/Salary/',dat$First.Last[1],'.png', sep = ''))
  ggsave(paste('Visualizations/Distributions/By Position/DKP per $1K/',
               dat$Pos[1],"/", dat$First.Last[1],'.png', sep = ''))
  ggsave(paste('Visualizations/Distributions/By Team/DKP per $1K/',
               dat$Team[1], dat$First.Last[1],'.png', sep = ''))
  
  ggplot(data = dat[which(dat$DK.salary > 0),]) +
    stat_density(aes(DK.points/(DK.salary/1000)), adjust = 0.5, color = col.set[2], fill = col.set[1], size = 1) +
    xlab("DK Points/$1K") +
    scale_x_continuous(limits = c(-1, 12)) +
    scale_y_continuous(limits = c(0, .75)) +
    ggtitle(paste(dat$First.Last[1], "DK Fantasy Points/$1K (2016)", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste('Visualizations/Distributions/DKP Per $1K/',dat$First.Last[1],'.png', sep = ''))
  ggsave(paste('Visualizations/Distributions/By Position/Salary/',
               dat$Pos[1],"/", dat$First.Last[1],'.png', sep = ''))
  ggsave(paste('Visualizations/Distributions/By Team/Salary/',
               dat$Team[1], dat$First.Last[1],'.png', sep = ''))
  
}
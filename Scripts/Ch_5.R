######################################## 
##  ASA NFL DFS book                  ##
##  Ch. 5: How do players correlate?  ##
##  Stewart Gibson                    ##
##  6/24/17                           ##
########################################

## Load packages
require(corrplot)

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

## Create second version of Fantasy.2016 so that below-cut players are kept in a full dataset
Fantasy.2016.full <- Fantasy.2016

## Subset Fantasy.2016 to only include players in QB.list, RB.list, WR_TE.list
Fantasy.2016 <- Fantasy.2016[which(as.character(Fantasy.2016$First.Last) %in% 
                                     c(as.character(QB.list), 
                                       as.character(RB.list), 
                                       as.character(WR_TE.list))),]

## Create correlation matrix for each team.
uniq.teams <- sort(unique(Fantasy.2016$Team))

for (i in 1:length(uniq.teams)) {
  dat <- Fantasy.2016[which(Fantasy.2016$Team == uniq.teams[i]),]
  uniq.players <- c(unique(dat$Initial.Last[which(dat$Pos == 'QB')]),
                    unique(dat$Initial.Last[which(dat$Pos == 'RB')]),
                    unique(dat$Initial.Last[which(dat$Pos == 'WR')]),
                    unique(dat$Initial.Last[which(dat$Pos == 'TE')]))
  
  # build matrix for correlation plot
  corr.mat <- matrix(NA, ncol = length(uniq.players), nrow = 17)
  colnames(corr.mat) <- uniq.players
  for (r in 1:nrow(corr.mat)) {
    for (c in 1:ncol(corr.mat)) {
      corr.mat[r,c] <- max(dat$DK.points[which(dat$Initial.Last == colnames(corr.mat)[c] &
                                             dat$Week == r)], 0)
      
    }
  }
  # create correlation plot
  f1 <- paste('Visualizations/Ch_5/All Games/', 
              uniq.teams[i], sep="")
  f2 <- paste(f1, '.png', sep = "")
  png(f2, height = 450, width = 550, pointsize = 22-ncol(corr.mat))
  corrplot(cor(corr.mat), method = 'number', type = 'lower', 
           col = colorRampPalette(c("red","gray90","green"))(100),
           title = paste(toupper(uniq.teams[i]),"Player Correlation", sep = " "),
           mar=c(0,0,1,0))
  dev.off()
}

## Create dataset for each team/week combination.  Fill in QB1, RB1, RB2, WR1, WR2, WR3, TE1
## to be top, second, third fantasy point scorer at each position for each week.  This will
## create a correlation matrix slightly different from the one above, allowing for flexibility
## in which player fills each depth spot on a given week.
Weekly.depth <- unique(Fantasy.2016.full[c("Week","Team")])
Weekly.depth <- cbind(Weekly.depth, matrix(NA, ncol = 8, nrow = nrow(Weekly.depth)))
colnames(Weekly.depth)[3:10] <- c("QB1","RB1","RB2","WR1","WR2","WR3","TE1", "Actual.Points")

for (i in 1:nrow(Weekly.depth)) {
  dat <- Fantasy.2016.full[which(Fantasy.2016.full$Week == Weekly.depth$Week[i] &
                                   Fantasy.2016.full$Team == Weekly.depth$Team[i]),]
  # Fill out matrix
  Weekly.depth$QB1[i] <- sort(dat$DK.points[which(dat$Pos == "QB")], decreasing = T)[1]
  Weekly.depth$RB1[i] <- sort(dat$DK.points[which(dat$Pos == "RB")], decreasing = T)[1]
  Weekly.depth$RB2[i] <- sort(dat$DK.points[which(dat$Pos == "RB")], decreasing = T)[2]
  Weekly.depth$WR1[i] <- sort(dat$DK.points[which(dat$Pos == "WR")], decreasing = T)[1]
  Weekly.depth$WR2[i] <- sort(dat$DK.points[which(dat$Pos == "WR")], decreasing = T)[2]
  Weekly.depth$WR3[i] <- sort(dat$DK.points[which(dat$Pos == "WR")], decreasing = T)[3]
  Weekly.depth$TE1[i] <- sort(dat$DK.points[which(dat$Pos == "TE")], decreasing = T)[1]
  Weekly.depth$Actual.Points[i] <- dat$Actual.Points[1]
}
Weekly.depth$RB2[which(is.na(Weekly.depth$RB2))] <- 0

for (i in 1:length(uniq.teams)) {
  corr.mat <- Weekly.depth[which(Weekly.depth$Team == uniq.teams[i]),]
  f1 <- paste('Visualizations/Ch_5/By Depth/', 
              uniq.teams[i], sep="")
  f2 <- paste(f1, '.png', sep = "")
  png(f2, height = 450, width = 550, pointsize = 22-ncol(corr.mat))
  corrplot(cor(as.matrix(corr.mat[,3:9])), method = 'number', type = 'lower', 
           col = colorRampPalette(c("red","gray90","green"))(100),
           title = paste(toupper(uniq.teams[i]), "Correlation by Depth", sep = " "),
           mar=c(0,0,1,0))
  dev.off()
}

## Create above corrplot, but only consider games in which actual point totals are in the upper
## 10% of point totals
png('Visualizations/Ch_5/High-Scoring Games/High-Scoring Games.png', 
    height = 450, width = 550, pointsize = 22-ncol(corr.mat))
corrplot(cor(as.matrix(Weekly.depth[which(
  Weekly.depth$Actual.Points >=  quantile(Weekly.depth$Actual.Points, probs = .9)),3:9])), 
  method = 'number', type = 'lower', 
  col = colorRampPalette(c("red","gray90","green"))(100),
  title = "All Teams Player Correlation by Position Depth ( > 34 Points)",
  mar=c(0,0,1,0))
dev.off()

## Create above corrplot for all games by all teams.
png('Visualizations/Ch_5/By Depth/ALL Games ALL Teams.png', 
    height = 450, width = 550, pointsize = 22-ncol(corr.mat))
corrplot(cor(as.matrix(Weekly.depth[,3:9])), 
  method = 'number', type = 'lower', 
  col = colorRampPalette(c("red","gray90","green"))(100),
  title = "All Teams Player Correlation by Position Depth",
  mar=c(0,0,1,0))
dev.off()


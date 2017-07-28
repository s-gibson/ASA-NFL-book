############################################################## 
##  ASA NFL DFS book                                        ##
##  Ch. 7: How how does matchup affect fantasy production?  ##
##  Stewart Gibson                                          ##
##  7/13/17                                                  ##
##############################################################

## Load packages
require(ggplot2)

## Load data
load("data/clean_data.RData")

## Remove defenses
Fantasy.2016 <- Fantasy.2016[which(Fantasy.2016$Pos != "Def"),]

## Create dataframe of defenses and how many total offensive, QB, RB, and WR/TE fantasy points
## they allow per game
Defense.FP <- data.frame(Oppt = sort(unique(Fantasy.2016$Team)),
                         Offense.FP = NA, QB.FP = NA, RB.FP = NA, WR_TE.FP = NA)

uniq.teams <- sort(unique(Fantasy.2016$Team))

for (i in 1:length(uniq.teams)) {
  dat <- Fantasy.2016[which(Fantasy.2016$Oppt == uniq.teams[i]),]
  Defense.FP$Offense.FP[i] <- mean(tapply(dat$DK.points, dat$Week, sum), na.rm = T)
  Defense.FP$QB.FP[i] <- mean(tapply(dat$DK.points[which(dat$Pos == "QB")], 
                                     dat$Week[which(dat$Pos == "QB")], sum), na.rm = T)
  Defense.FP$RB.FP[i] <- mean(tapply(dat$DK.points[which(dat$Pos == "RB")], 
                                     dat$Week[which(dat$Pos == "RB")], sum), na.rm = T)
  Defense.FP$WR_TE.FP[i] <- mean(tapply(dat$DK.points[which(dat$Pos %in% c("WR","TE"))], 
                                     dat$Week[which(dat$Pos %in% c("WR","TE"))], sum), na.rm = T)
    }

## Create dataframe with the sum of each teams' total and weekly positional fantasy production and
## the average fantasy production allowed by the opponent.
Fantasy.vs.Oppt <- unique(Fantasy.2016[,c("Team","Week","Pos")])
Fantasy.vs.Oppt$Pos <- as.character(Fantasy.vs.Oppt$Pos)
Fantasy.vs.Oppt$Pos[which(Fantasy.vs.Oppt$Pos == "WR")] <- "WR/TE"
Fantasy.vs.Oppt$Pos[which(Fantasy.vs.Oppt$Pos == "TE")] <- "All"
Fantasy.vs.Oppt$Oppt <- NA
Fantasy.vs.Oppt$Oppt.FP.allowed <- NA
Fantasy.vs.Oppt$Total.FP <- NA

for (i in 1:nrow(Fantasy.vs.Oppt)) {
  Fantasy.vs.Oppt$Oppt[i] <- as.character(Fantasy.2016$Oppt)[which(
    Fantasy.2016$Team == Fantasy.vs.Oppt$Team[i] &
      Fantasy.2016$Week == Fantasy.vs.Oppt$Week[i])][1]
  if (Fantasy.vs.Oppt$Pos[i] == "QB") {
    Fantasy.vs.Oppt$Oppt.FP.allowed[i] <- Defense.FP$QB.FP[which(
      Defense.FP$Oppt == Fantasy.vs.Oppt$Oppt[i])]
    Fantasy.vs.Oppt$Total.FP[i] <- sum(Fantasy.2016$DK.points[which(
      Fantasy.2016$Week == Fantasy.vs.Oppt$Week[i] &
        Fantasy.2016$Team == Fantasy.vs.Oppt$Team[i] &
        as.character(Fantasy.2016$Pos) == "QB")], na.rm = T)
  } else if (Fantasy.vs.Oppt$Pos[i] == "RB") {
    Fantasy.vs.Oppt$Oppt.FP.allowed[i] <- Defense.FP$RB.FP[which(
      Defense.FP$Oppt == Fantasy.vs.Oppt$Oppt[i])]
    Fantasy.vs.Oppt$Total.FP[i] <- sum(Fantasy.2016$DK.points[which(
      Fantasy.2016$Week == Fantasy.vs.Oppt$Week[i] &
        Fantasy.2016$Team == Fantasy.vs.Oppt$Team[i] &
        as.character(Fantasy.2016$Pos) == "RB")], na.rm = T)
  } else if (Fantasy.vs.Oppt$Pos[i] == "WR/TE") {
    Fantasy.vs.Oppt$Oppt.FP.allowed[i] <- Defense.FP$WR_TE.FP[which(
      Defense.FP$Oppt == Fantasy.vs.Oppt$Oppt[i])]
    Fantasy.vs.Oppt$Total.FP[i] <- sum(Fantasy.2016$DK.points[which(
      Fantasy.2016$Week == Fantasy.vs.Oppt$Week[i] &
        Fantasy.2016$Team == Fantasy.vs.Oppt$Team[i] &
        as.character(Fantasy.2016$Pos) %in% c("WR","TE"))], na.rm = T)
  } else {Fantasy.vs.Oppt$Oppt.FP.allowed[i] <- Defense.FP$Offense.FP[which(
        Defense.FP$Oppt == Fantasy.vs.Oppt$Oppt[i])]
      Fantasy.vs.Oppt$Total.FP[i] <- sum(Fantasy.2016$DK.points[which(
        Fantasy.2016$Week == Fantasy.vs.Oppt$Week[i] &
          Fantasy.2016$Team == Fantasy.vs.Oppt$Team[i])], na.rm = T)
    }
}

## Plot each teams' fantasy point total (total offense and positional units) vs. the average
## point total allowed by each opponent
for (i in 1:length(uniq.teams)) {
  dat <- Fantasy.vs.Oppt[which(Fantasy.vs.Oppt$Team == uniq.teams[i]),]
  
  FP.total.reg <- lm(dat$Total.FP[which(dat$Pos == "All")] ~ 
                      dat$Oppt.FP.allowed[which(dat$Pos == "All")])
  reg.eq <- paste("Y = ", round(FP.total.reg$coefficients[1], 2), " + ", 
                  round(FP.total.reg$coefficients[2], 2), " * X", sep = '')
  
  # Total offense
  ggplot(data = dat[which(dat$Pos == "All"),], aes (x = Oppt.FP.allowed, 
                                                    y = Total.FP)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~x) +
    ggtitle(paste(toupper(uniq.teams[i]), 
                          "Total Fantasy Points vs. Opponent Average FP/Game", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Opponent Average FP/Game") +
    ylab("Total Offensive Fantasy Points") +
    geom_text(x = 85, y = max(Fantasy.vs.Oppt$Total.FP), label = reg.eq,
              color = 'blue', size = 5) +
    scale_x_continuous(limits = c(min(Fantasy.vs.Oppt$Oppt.FP.allowed[which(
      Fantasy.vs.Oppt$Pos == "All")]), max(Fantasy.vs.Oppt$Oppt.FP.allowed[which(
      Fantasy.vs.Oppt$Pos == "All")]) + 3)) +
    scale_y_continuous(limits = c(min(Fantasy.vs.Oppt$Total.FP[which(
      Fantasy.vs.Oppt$Pos == "All")] - 5),
                                  max(Fantasy.vs.Oppt$Total.FP[which(
                                    Fantasy.vs.Oppt$Pos == "All")] + 5)))
  
  ggsave(paste("Visualizations/Ch_7/Total Offense/",uniq.teams[i],".png", sep = ""))
  
  # Positional units
  ggplot(data = dat[which(dat$Pos != "All"),], aes (x = Oppt.FP.allowed, 
                                                    y = Total.FP, group = Pos, color = Pos)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~x) +
    ggtitle(paste(toupper(uniq.teams[i]), 
                  "Total Fantasy Points vs. Opponent Average FP/Game", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Opponent Average FP/Game") +
    ylab("Total Positiona Unit Fantasy Points") #+

  ggsave(paste("Visualizations/Ch_7/Positional Units/",uniq.teams[i],".png", sep = ""))
  
}

## Plot teams' fantasy point total (total offense and positional units) vs. the average
## point total allowed by each opponent at the league-aggregate level.
FP.total.reg <- lm(Fantasy.vs.Oppt$Total.FP[which(Fantasy.vs.Oppt$Pos == "All")] ~ 
                     Fantasy.vs.Oppt$Oppt.FP.allowed[which(Fantasy.vs.Oppt$Pos == "All")])
reg.eq <- paste("Y = ", round(FP.total.reg$coefficients[1], 2), " + ", 
                round(FP.total.reg$coefficients[2], 2), " * X", sep = '')

# Total offense
ggplot(data = Fantasy.vs.Oppt[which(Fantasy.vs.Oppt$Pos == "All"),], aes (x = Oppt.FP.allowed, 
                                                  y = Total.FP)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~x) +
  ggtitle("Total Fantasy Points vs. Opponent Average FP/Game") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Opponent Average FP/Game") +
  ylab("Total Offensive Fantasy Points") +
  geom_text(x = 85, y = max(Fantasy.vs.Oppt$Total.FP), label = reg.eq,
            color = 'blue', size = 5) +
  scale_x_continuous(limits = c(min(Fantasy.vs.Oppt$Oppt.FP.allowed[which(
    Fantasy.vs.Oppt$Pos == "All")]), max(Fantasy.vs.Oppt$Oppt.FP.allowed[which(
      Fantasy.vs.Oppt$Pos == "All")]) + 3)) +
  scale_y_continuous(limits = c(min(Fantasy.vs.Oppt$Total.FP[which(
    Fantasy.vs.Oppt$Pos == "All")] - 5),
    max(Fantasy.vs.Oppt$Total.FP[which(
      Fantasy.vs.Oppt$Pos == "All")] + 5)))

ggsave("Visualizations/Ch_7/League-Aggregate/Total Offense.png")

# Positional units
ggplot(data = Fantasy.vs.Oppt[which(Fantasy.vs.Oppt$Pos != "All"),], aes (x = Oppt.FP.allowed, 
                                                  y = Total.FP, group = Pos, color = Pos)) +
  geom_point(size = 0.4, alpha = 0.4) +
  geom_smooth(method = 'lm', formula = y ~x) +
  ggtitle("Total Fantasy Points vs. Opponent Average FP/Game") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Opponent Average FP/Game") +
  ylab("Total Positiona Unit Fantasy Points") #+

ggsave("Visualizations/Ch_7/League-Aggregate/Positional Units.png")



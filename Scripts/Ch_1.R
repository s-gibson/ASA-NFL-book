######################################################## 
##  ASA NFL DFS book                                  ##
##  Ch. 1: How Many Points will be scored in a game?  ##
##  Stewart Gibson                                    ##
##  6/7/17                                            ##
########################################################

## Load packages
require(ggplot2)

## Import data
OU.2016 <- read.csv("data/2016_DonBest_VegasData_NFL_Week.csv")
OU.2012_2015 <- read.csv("data/2012_2015_Final_DonbestData_NFL.csv")

## Keep only regular season games
OU.2016 <- OU.2016[which(OU.2016$Regular.Season == 1),]

# Create variables for year and week
OU.2016$Year <- 2016
OU.2012_2015$Year <- substr(OU.2012_2015$Date, start = 7, stop = 10)
OU.2016$Week <- NA
OU.2012_2015$Week <- NA
OU.2016$Information <- as.character(OU.2016$Information)
OU.2012_2015$Information <- as.character(OU.2012_2015$Information)
for (i in c(1:17)) {
  OU.2016$Week[grep(paste('WEEK ', i, " ", sep = ''), OU.2016$Information)] <- i
  OU.2012_2015$Week[grep(paste('WEEK ', i, " ", sep = ''), OU.2012_2015$Information)] <- i
}

## Create dataset of only Over/Under and actual point totals
viz.dat <- data.frame(Over.Under = c(OU.2016$Vegas.Over.Under, OU.2012_2015$Vegas.Over.Under),
                      Actual.Points = c(OU.2016$Actual.Total.Points, OU.2012_2015$Actual.Total.Points))

## Create vizualization: O/U (x) vs. Actual Point Total (y)
## 3rd order polynomial fit
ggplot(data = viz.dat) +
  geom_point(aes(x = Over.Under, y = Actual.Points), size = 0.1) +
  geom_smooth(aes(x = Over.Under, y = Actual.Points), method = 'lm',
              formula = y ~ poly(x,3)) +
  ggtitle("Actual Point Total vs. Over/Under") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Over/Under") +
  ylab("Actual Points Scored") #+ 
  #geom_abline(intercept = seq(-50, 80,10), slope = 1, color = 'red', size = 0.5, alpha = 0.1)


## 5th order polynomial fit
ggplot(data = viz.dat) +
  geom_point(aes(x = Over.Under, y = Actual.Points), size = 0.1) +
  geom_smooth(aes(x = Over.Under, y = Actual.Points), method = 'lm',
              formula = y ~ poly(x,5)) +
  ggtitle("Actual Point Total vs. Over/Under") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Over/Under") +
  ylab("Actual Points Scored") #+ 
  #geom_abline(intercept = seq(-50, 80,10), slope = 1, color = 'red', size = 0.5, alpha = 0.1)

## Loess fit
ggplot(data = viz.dat) +
  geom_point(aes(x = Over.Under, y = Actual.Points), size = 0.1) +
  geom_smooth(aes(x = Over.Under, y = Actual.Points), method = 'loess') +
  ggtitle("Actual Point Total vs. Over/Under") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Over/Under") +
  ylab("Actual Points Scored") + 
  geom_abline(intercept = seq(-50, 80,10), slope = 1, color = 'red', size = 0.5, alpha = 0.1)





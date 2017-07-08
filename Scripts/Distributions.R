############################################################## 
##  ASA NFL DFS book                                        ##
##  Player Fantasy point, salary, and FP/$1K distributions  ##
##  Stewart Gibson                                          ##
##  6/24/17                                                 ##
##############################################################

## Load packages
require(ggplot2)

## Load data
load("data/clean_data.RData")

## Set unique players list
uniq.players <- unique(Fantasy.2016$First.Last)

for (i in 1:length(uniq.players)) {
  dat <- Fantasy.2016[which(Fantasy.2016$First.Last == uniq.players[i]),]
  
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
  
}
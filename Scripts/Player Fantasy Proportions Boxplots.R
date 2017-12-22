################################################ 
##  ASA NBA Site Content                      ##
##  Player Fantasy Point Proportion Boxplots  ##
##  Stewart Gibson                            ##
##  11/25/17                                 ##
################################################

## Load packages
require(ggplot2)

## Load data
load("data/NBA_2017.RData")

## Consideration cutoff: How many DKP must a player score to be considered for correlation?
cutoff <- 200

## Loop through all teams
uniq.teams <- sort(unique(NBA_17$Team))

for (i in 1:length(uniq.teams)) {
  dat <- NBA_17[which(NBA_17$Team == uniq.teams[i] & !is.na(NBA_17$DKP)),]
  
  # Add column with players' "<first intial>. <last name>" and columns with total DKP scored
  # for each specific game
  for (j in 1:nrow(dat)) {
    dat$Initial.Last[j] <- paste(substr(unlist(strsplit(
      as.character(dat$Last..First[j]), split = ","))[2], start = 2, stop = 2),". ",
      unlist(strsplit(
        as.character(dat$Last..First[j]), split = ","))[1], sep = "")
    
    dat$Total.DKP[j] <- sum(dat$DKP[which(dat$GameID == dat$GameID[j])], na.rm = T)
    
    dat$Player.DKP[j] <- sum(NBA_17$DKP[which(
      as.character(NBA_17$Last..First) == as.character(dat$Last..First[j]))],na.rm = T)
  }
  
  dat <- dat[which(dat$Player.DKP > cutoff),]
  
  # set color scheme
  if (dat$Team[length(dat$Team)] == "atl") {col.set = c("red","yellow")
  } else if (dat$Team[length(dat$Team)] == "bkn") {col.set = c("black","grey")
  } else if (dat$Team[length(dat$Team)] == "bos") {col.set = c("green4","white")
  } else if (dat$Team[length(dat$Team)] == "cha") {col.set = c("darkslateblue","darkcyan")
  } else if (dat$Team[length(dat$Team)] == "chi") {col.set = c("red","black")
  } else if (dat$Team[length(dat$Team)] == "cle") {col.set = c("darkred","gold")
  } else if (dat$Team[length(dat$Team)] == "dal") {col.set = c("blue","navy")
  } else if (dat$Team[length(dat$Team)] == "den") {col.set = c("steelblue1","gold")
  } else if (dat$Team[length(dat$Team)] == "det") {col.set = c("red","blue")
  } else if (dat$Team[length(dat$Team)] == "gsw") {col.set = c("dodgerblue","gold")
  } else if (dat$Team[length(dat$Team)] == "hou") {col.set = c("red","grey")
  } else if (dat$Team[length(dat$Team)] == "ind") {col.set = c("navy","gold")
  } else if (dat$Team[length(dat$Team)] == "lac") {col.set = c("blue","red")
  } else if (dat$Team[length(dat$Team)] == "lal") {col.set = c("purple3","gold")
  } else if (dat$Team[length(dat$Team)] == "mem") {col.set = c("royalblue4","steelblue")
  } else if (dat$Team[length(dat$Team)] == "mia") {col.set = c("orangered4","black")
  } else if (dat$Team[length(dat$Team)] == "mil") {col.set = c("forestgreen","wheat")
  } else if (dat$Team[length(dat$Team)] == "min") {col.set = c("royalblue4","forestgreen")
  } else if (dat$Team[length(dat$Team)] == "nor") {col.set = c("royalblue4","gold3")
  } else if (dat$Team[length(dat$Team)] == "nyk") {col.set = c("blue","orange")
  } else if (dat$Team[length(dat$Team)] == "okc") {col.set = c("steelblue3","orange")
  } else if (dat$Team[length(dat$Team)] == "orl") {col.set = c("dodgerblue","black")
  } else if (dat$Team[length(dat$Team)] == "phi") {col.set = c("blue","white")
  } else if (dat$Team[length(dat$Team)] == "pho") {col.set = c("blueviolet","orange")
  } else if (dat$Team[length(dat$Team)] == "por") {col.set = c("black","red")
  } else if (dat$Team[length(dat$Team)] == "sac") {col.set = c("darkviolet","grey")
  } else if (dat$Team[length(dat$Team)] == "sas") {col.set = c("black","grey")
  } else if (dat$Team[length(dat$Team)] == "tor") {col.set = c("darkviolet","red")
  } else if (dat$Team[length(dat$Team)] == "uta") {col.set = c("midnightblue","seagreen")
  } else {col.set = c("blue4","red")}
  
  # Team Offense
  ggplot(data = dat[which(dat$active == 1),]) +
    geom_boxplot(aes(x = factor(Initial.Last, 
                                levels=levels(factor(dat$Initial.Last))[
                                  order(tapply(dat$DKP/dat$Total.DKP, factor(dat$Initial.Last), 
                                               median), 
                                        decreasing = T)]), y = DKP/Total.DKP), 
                 outlier.alpha = 0, coef = 0, color = col.set[2], fill = col.set[1]) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    xlab("") +
    ylab("% of Team Fantasy Points") +
    ggtitle(paste(toupper(dat$Team[i]), "Proportion of Team Fantasy Points", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("Visualizations/Player Fantasy Proportions/",
               uniq.teams[i],".png", sep = ""))
  
}


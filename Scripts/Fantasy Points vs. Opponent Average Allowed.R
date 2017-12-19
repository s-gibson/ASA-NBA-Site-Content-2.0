###################################################### 
##  ASA NBA Site Content                            ##
##  Player Fantasy Production vs. Opponent Average  ##
##  Stewart Gibson                                  ##
##  11/21/17                                        ##
######################################################

### Load packages
require(ggplot2)

### Load data
load("data/NBA_2017.RData")

## Consideration cutoff: How many DKP must a player score to be considered for correlation?
cutoff <- 50

### First calculate teams' average fantasy points allowed to each position per game.  
### This is the average sum of fantasy points scored by players who qualify for each position.
### Because some players are eligbile for multpile positions, some players' fantasy production
### will be double counted.
avg.DKP <- data.frame(Opp = sort(unique(NBA_17$Opp)),
                      Opp.Avg.PG = 0,
                      Opp.Avg.SG = 0,
                      Opp.Avg.SF = 0,
                      Opp.Avg.PF = 0,
                      Opp.Avg.C = 0)

for (i in 1:nrow(avg.DKP)) {
  dat <- NBA_17[which(NBA_17$Opp == avg.DKP$Opp[i]),]
  
  avg.DKP$Opp.Avg.PG[i] <- sum(dat$DKP[which(dat$DK_PG == 1)], na.rm = T)/length(unique(dat$GameID))
  avg.DKP$Opp.Avg.SG[i] <- sum(dat$DKP[which(dat$DK_SG == 1)], na.rm = T)/length(unique(dat$GameID))
  avg.DKP$Opp.Avg.SF[i] <- sum(dat$DKP[which(dat$DK_SF == 1)], na.rm = T)/length(unique(dat$GameID))
  avg.DKP$Opp.Avg.PF[i] <- sum(dat$DKP[which(dat$DK_PF == 1)], na.rm = T)/length(unique(dat$GameID))
  avg.DKP$Opp.Avg.C[i] <- sum(dat$DKP[which(dat$DK_C == 1)], na.rm = T)/length(unique(dat$GameID))
}


## Plot each teams' fantasy point total (total offense and positional units) vs. the average
## point total allowed by each opponent
uniq.teams <- sort(unique(NBA_17$Team))
for (i in 1:length(uniq.teams)) {
  # Determine which players are to be included based on total fantasy points for the season.
  dat <- NBA_17[which(NBA_17$Team == uniq.teams[i]),]
  uniq.players <- unique(as.character(dat$Last..First))
  player.points <- data.frame(Player = uniq.players,
                              Initial.Last = "Fill",
                              Total.DKP = 0,
                              stringsAsFactors = F)
  for (j in 1:length(uniq.players)) {
    player.points$Initial.Last[j] <- paste(substr(unlist(strsplit(
      as.character(player.points$Player[j]), split = ","))[2], start = 2, stop = 2),". ",
      unlist(strsplit(
        as.character(player.points$Player[j]), split = ","))[1], sep = "")
    
    player.points$Total.DKP[j] <- sum(NBA_17$DKP[which(
      as.character(NBA_17$Last..First) == as.character(player.points$Player[j]))],na.rm = T)
  }
  
  player.points <- player.points[order(player.points$Total.DKP, decreasing = T),]
  player.points <- player.points[which(player.points$Total.DKP > cutoff),]
  
  dat <- dat[which(dat$Last..First %in% player.points$Player),]
  colnames(player.points)[1] <- "Last..First"
  dat <- merge(dat, player.points, by = "Last..First")
  dat <- merge(dat, avg.DKP, by = "Opp")
  
  # Set 'opponent averages' of positions that don't apply to each player to NA
  for (p in 1:nrow(dat)) {
    dat[p,which(dat[p, c(36:40)] == 0) + 41] <- NA
  }

  dat$Opp.Avg.Tot <- rowMeans(dat[,c(42:46)], na.rm = T)
  
  ggplot(data = dat[which(!is.na(dat$DKP) & dat$active == 1),], aes(x = Opp.Avg.Tot, y = DKP, color = Initial.Last, group = Initial.Last)) +
    geom_point() +
    geom_smooth(method = 'lm',formula = y ~ log(x), se = F) +
    xlab("Opponent Position DKP Allowed") +
    ylab("Fantasy Points") +
    ggtitle(paste(toupper(uniq.teams[i]), "Fantasy Points vs. Opponent Average Allowed")) +
    scale_color_discrete(name = "Player") +
    scale_x_continuous(limits = c(min(avg.DKP[,-1]),max(avg.DKP[,-1])))
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("Visualizations/Fantasy Points vs. Opponent Average Allowed/",uniq.teams[i],
               ".png", sep = ""))
}


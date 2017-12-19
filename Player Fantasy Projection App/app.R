##################################### 
##  ASA NBA Site Content           ##
##  Player Fantasy Projection App  ##
##  Stewart Gibson                 ##
##  9/1/17                         ##
#####################################

### Set working directory
# setwd("~/Documents/ASA/ASA Site Content/NBA/Player Fantasy Projection App")

### Load data and packages
load("NBA_2017.RData")
require(rsconnect)
require(shiny)
require(ggplot2)

NBA_17$Last..First <- as.character(NBA_17$Last..First)
levels(NBA_17$H.A) <- c("Away","Home")
iter <- 1000

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


################# The App ########################
ui <- fluidPage(
  fluidRow(
    titlePanel("Advanced Sports Analytics NBA Player Fantasy Projection App")),
  # Player 1
  fluidRow(
    wellPanel(
      column(3, selectInput(inputId = "Player1", label = "Player 1",
                            choices = c("",sort(unique(NBA_17$Last..First))), multiple = F)),
      column(2, numericInput(inputId = "Over.Under1", label = "Over/Under 1", step = 0.5, value = 210)),
      column(2, numericInput(inputId = "Spread1", label = "Spread 1", step = 0.5, value  = 0)),
      column(2, selectInput(inputId = "Opponent1", label = "Opponent 1",
                            choices = c("",sort(toupper(unique(NBA_17$Opp)))), multiple = F)),
      column(2,selectInput(inputId = "H.A1", label = "Home/Away 1", choices = c("Home","Away"), multiple = F)))
    ),
  # Player 2
  fluidRow(
    wellPanel(
      column(3, selectInput(inputId = "Player2", label = "Player 2",
                            choices = c("",sort(unique(NBA_17$Last..First))), multiple = F)),
      column(2, numericInput(inputId = "Over.Under2", label = "Over/Under 2", step = 0.5, value = 210)),
      column(2, numericInput(inputId = "Spread2", label = "Spread 2", step = 0.5, value  = 0)),
      column(2, selectInput(inputId = "Opponent2", label = "Opponent 2",
                            choices = c("",sort(toupper(unique(NBA_17$Opp)))), multiple = F)),
      column(2,selectInput(inputId = "H.A2", label = "Home/Away 2", choices = c("Home","Away"), multiple = F)))
  ),
  # Player 3
  fluidRow(
    wellPanel(
      column(3, selectInput(inputId = "Player3", label = "Player 3",
                            choices = c("",sort(unique(NBA_17$Last..First))), multiple = F)),
      column(2, numericInput(inputId = "Over.Under3", label = "Over/Under 3", step = 0.5, value = 210)),
      column(2, numericInput(inputId = "Spread3", label = "Spread 3", step = 0.5, value  = 0)),
      column(2, selectInput(inputId = "Opponent3", label = "Opponent 3",
                            choices = c("",sort(toupper(unique(NBA_17$Opp)))), multiple = F)),
      column(2,selectInput(inputId = "H.A3", label = "Home/Away 3", choices = c("Home","Away"), multiple = F)))
  ),
  
  submitButton("Submit"),
  
  plotOutput(outputId = "chart")
)
        
server <- function(input, output) {
  output$chart <- renderPlot({ 
    if (input$Player1 != "" & input$Player2 == "" & input$Player3 == "") {
    dat.player1 <- NBA_17[which(NBA_17$Last..First == input$Player1 &
                                  !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    dat.team1 <- NBA_17[which(NBA_17$Team == dat.player1$Team[nrow(dat.player1)] &
                                !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    dat.opp1 <- NBA_17[which(NBA_17$Opp == tolower(input$Opponent1) &
                               !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    
    Projections_Player1 <- data.frame(Project_1 = rep(NA,iter),
                                      Project_2 = rep(NA,iter),
                                      Project_3 = rep(NA,iter),
                                      Project_4 = rep(NA,iter),
                                      Project_5 = rep(NA,iter))
    
    ### General distribution
    player1.dens <- density(dat.player1$DKP)
    Projections_Player1$Project_1 <- sample(player1.dens$x, iter, replace = T, 
                                            prob = player1.dens$y)
    
    ### Point total regression
    player1.Project_2.reg <- lm(data = dat.player1, formula = DKP ~ poly(Team.pts, 3))
    Projections_Player1$Project_2 <- rep(predict.lm(object = player1.Project_2.reg, 
                                                    newdata = data.frame(Team.pts = input$Over.Under1/2 - input$Spread1/2)),iter)
    Projections_Player1$Project_2 <- Projections_Player1$Project_2 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player1.Project_2.reg, 
                            newdata = data.frame(Team.pts = input$Over.Under1/2 - input$Spread1/2),
                            se.fit = T)$se.fit)
    
    ### Spread regression
    dat.player1$Spread <- dat.player1$Opp.pts - dat.player1$Team.pts
    player1.Project_3.reg <- lm(data = dat.player1, formula = DKP ~ poly(Spread, 3))
    Projections_Player1$Project_3 <- rep(predict.lm(object = player1.Project_3.reg, 
                                                    newdata = data.frame(Spread = 
                                                                           input$Spread1)),iter)
    Projections_Player1$Project_3 <- Projections_Player1$Project_3 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player1.Project_3.reg, 
                            newdata = data.frame(Spread = input$Spread1),
                            se.fit = T)$se.fit)
    
    ### Opponent average DKP allowed regression
    dat.player1 <- merge(dat.player1, 
                         avg.DKP[,c(1,as.numeric(which(colMeans(dat.player1[,36:40]) > 0.75)+1))], by = "Opp")
    dat.player1$Opp.Avg.All <- if (length(which(grepl("Opp.Avg", colnames(dat.player1)))) > 1) {
      rowMeans(dat.player1[,which(grepl("Opp.Avg", colnames(dat.player1)))])
    } else (dat.player1[,which(grepl("Opp.Avg", colnames(dat.player1)))])
    
    player1.Project_4.reg <- lm(data = dat.player1, formula = DKP ~ poly(Opp.Avg.All, 3))
    Projections_Player1$Project_4 <- 
      rep(predict.lm(object = player1.Project_4.reg,newdata = data.frame(
        Opp.Avg.All = mean(as.numeric(avg.DKP[which(avg.DKP$Opp == tolower(input$Opponent1)),
                                              as.numeric(which(colMeans(dat.player1[,36:40]) > 0.75)+1)])))),iter)
    
    Projections_Player1$Project_4 <- Projections_Player1$Project_4 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player1.Project_4.reg,newdata = data.frame(
              Opp.Avg.All = mean(
                as.numeric(avg.DKP[which(avg.DKP$Opp == tolower(input$Opponent1)),
                                   as.numeric(which(colMeans(dat.player1[,36:40]) > 0.75)+1)]))),
              se.fit = T)$se.fit)
    
    ### Player proportion x Team regression
    for (i in 1:nrow(dat.player1)) {
      dat.player1$Team.DKP[i] <- sum(dat.team1$DKP[which(
        dat.team1$GameID == dat.player1$GameID[i])],na.rm = T)
      dat.player1$DKP.Prop[i] <- dat.player1$DKP[i]/dat.player1$Team.DKP[i]
    }
    
    player1.Project_5.reg <- lm(data = dat.player1, formula = Team.DKP ~ Spread + Team.pts + H.A)
    Projections_Player1$Project_5 <- rep(predict.lm(object = player1.Project_5.reg, 
                                                    newdata = data.frame(Spread = input$Spread1,
                                                                         Team.pts = input$Over.Under1/2 - input$Spread1/2,
                                                                         H.A = input$H.A1)),iter)
    Projections_Player1$Project_5 <- Projections_Player1$Project_5 +
      rnorm(iter, mean = 0, 
            sd = summary(player1.Project_5.reg)$sigma)
    
    Projections_Player1$Project_5 <- Projections_Player1$Project_5 *
      sample(x = density(dat.player1$DKP.Prop)$x, size = iter, replace = T, prob = density(dat.player1$DKP.Prop)$y)
    
    ### Produce distribution of projections
    ggplot() +
      geom_density(aes(x = rowMeans(Projections_Player1[,1:5]), #as.numeric(unlist(Projections_Player1[,1:5]))
                       color = paste(dat.player1$Last..First[1], ": 100%", sep = ""), 
                       fill = paste(dat.player1$Last..First[1], ": 100%", sep = "")),
                   alpha = 0.5) +
      xlab("Projected DKP") +
      ylab("Density") +
      scale_color_discrete(name = "Player") +
      scale_fill_discrete(name = "Player") +
      ggtitle("Player DKP Projection") +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else if (input$Player1 != "" & input$Player2 != "" & input$Player3 == "") {
    
    ### Data subset
    # Player 1
    dat.player1 <- NBA_17[which(NBA_17$Last..First == input$Player1 &
                                  !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    dat.team1 <- NBA_17[which(NBA_17$Team == dat.player1$Team[nrow(dat.player1)] &
                                !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    dat.opp1 <- NBA_17[which(NBA_17$Opp == tolower(input$Opponent1) &
                               !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    
    Projections_Player1 <- data.frame(Project_1 = rep(NA,iter),
                                      Project_2 = rep(NA,iter),
                                      Project_3 = rep(NA,iter),
                                      Project_4 = rep(NA,iter),
                                      Project_5 = rep(NA,iter))
    # Player 2
    dat.player2 <- NBA_17[which(NBA_17$Last..First == input$Player2 &
                                  !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    dat.team2 <- NBA_17[which(NBA_17$Team == dat.player2$Team[nrow(dat.player2)] &
                                !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    dat.opp2 <- NBA_17[which(NBA_17$Opp == tolower(input$Opponent2) &
                               !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    
    Projections_Player2 <- data.frame(Project_1 = rep(NA,iter),
                                      Project_2 = rep(NA,iter),
                                      Project_3 = rep(NA,iter),
                                      Project_4 = rep(NA,iter),
                                      Project_5 = rep(NA,iter))
    
    ### General distribution
    # Player 1
    player1.dens <- density(dat.player1$DKP)
    Projections_Player1$Project_1 <- sample(player1.dens$x, iter, replace = T, 
                                            prob = player1.dens$y)
    
    # Player 2
    player2.dens <- density(dat.player2$DKP)
    Projections_Player2$Project_1 <- sample(player2.dens$x, iter, replace = T, 
                                            prob = player2.dens$y)
    
    ### Point total regression
    # Player 1
    player1.Project_2.reg <- lm(data = dat.player1, formula = DKP ~ poly(Team.pts, 3))
    Projections_Player1$Project_2 <- rep(predict.lm(object = player1.Project_2.reg, 
                                                    newdata = data.frame(Team.pts = input$Over.Under1/2 - input$Spread1/2)),iter)
    Projections_Player1$Project_2 <- Projections_Player1$Project_2 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player1.Project_2.reg, 
                            newdata = data.frame(Team.pts = input$Over.Under1/2 - input$Spread1/2),
                            se.fit = T)$se.fit)
    
    # Player 2
    player2.Project_2.reg <- lm(data = dat.player2, formula = DKP ~ poly(Team.pts, 3))
    Projections_Player2$Project_2 <- rep(predict.lm(object = player2.Project_2.reg, 
                                                    newdata = data.frame(Team.pts = input$Over.Under2/2 - input$Spread2/2)),iter)
    Projections_Player2$Project_2 <- Projections_Player2$Project_2 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player2.Project_2.reg, 
                            newdata = data.frame(Team.pts = input$Over.Under2/2 - input$Spread2/2),
                            se.fit = T)$se.fit)
    
    ### Spread regression
    # Player 1
    dat.player1$Spread <- dat.player1$Opp.pts - dat.player1$Team.pts
    player1.Project_3.reg <- lm(data = dat.player1, formula = DKP ~ poly(Spread, 3))
    Projections_Player1$Project_3 <- rep(predict.lm(object = player1.Project_3.reg, 
                                                    newdata = data.frame(Spread = 
                                                                           input$Spread1)),iter)
    Projections_Player1$Project_3 <- Projections_Player1$Project_3 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player1.Project_3.reg, 
                            newdata = data.frame(Spread = input$Spread1),
                            se.fit = T)$se.fit)
    
    # Player 2
    dat.player2$Spread <- dat.player2$Opp.pts - dat.player2$Team.pts
    player2.Project_3.reg <- lm(data = dat.player2, formula = DKP ~ poly(Spread, 3))
    Projections_Player2$Project_3 <- rep(predict.lm(object = player2.Project_3.reg, 
                                                    newdata = data.frame(Spread = 
                                                                           input$Spread2)),iter)
    Projections_Player2$Project_3 <- Projections_Player2$Project_3 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player2.Project_3.reg, 
                            newdata = data.frame(Spread = input$Spread2),
                            se.fit = T)$se.fit)
    
    ### Opponent average DKP allowed regression
    # Player 1
    dat.player1 <- merge(dat.player1, 
                         avg.DKP[,c(1,as.numeric(which(colMeans(dat.player1[,36:40]) > 0.75)+1))], by = "Opp")
    dat.player1$Opp.Avg.All <- if (length(which(grepl("Opp.Avg", colnames(dat.player1)))) > 1) {
      rowMeans(dat.player1[,which(grepl("Opp.Avg", colnames(dat.player1)))])
    } else (dat.player1[,which(grepl("Opp.Avg", colnames(dat.player1)))])
    
    player1.Project_4.reg <- lm(data = dat.player1, formula = DKP ~ poly(Opp.Avg.All, 3))
    Projections_Player1$Project_4 <- 
      rep(predict.lm(object = player1.Project_4.reg,newdata = data.frame(
        Opp.Avg.All = mean(as.numeric(avg.DKP[which(avg.DKP$Opp == tolower(input$Opponent1)),
                                              as.numeric(which(colMeans(dat.player1[,36:40]) > 0.75)+1)])))),iter)
    
    Projections_Player1$Project_4 <- Projections_Player1$Project_4 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player1.Project_4.reg,newdata = data.frame(
              Opp.Avg.All = mean(
                as.numeric(avg.DKP[which(avg.DKP$Opp == tolower(input$Opponent1)),
                                   as.numeric(which(colMeans(dat.player1[,36:40]) > 0.75)+1)]))),
              se.fit = T)$se.fit)
    
    # Player 2
    dat.player2 <- merge(dat.player2, 
                         avg.DKP[,c(1,as.numeric(which(colMeans(dat.player2[,36:40]) > 0.75)+1))], by = "Opp")
    dat.player2$Opp.Avg.All <- if (length(which(grepl("Opp.Avg", colnames(dat.player2)))) > 1) {
      rowMeans(dat.player2[,which(grepl("Opp.Avg", colnames(dat.player2)))])
    } else (dat.player2[,which(grepl("Opp.Avg", colnames(dat.player2)))])
      
    player2.Project_4.reg <- lm(data = dat.player2, formula = DKP ~ poly(Opp.Avg.All, 3))
    Projections_Player2$Project_4 <- 
      rep(predict.lm(object = player2.Project_4.reg,newdata = data.frame(
        Opp.Avg.All = mean(as.numeric(avg.DKP[which(avg.DKP$Opp == tolower(input$Opponent2)),
                                              as.numeric(which(colMeans(dat.player2[,36:40]) > 0.75)+1)])))),iter)
    
    Projections_Player2$Project_4 <- Projections_Player2$Project_4 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player2.Project_4.reg,newdata = data.frame(
              Opp.Avg.All = mean(
                as.numeric(avg.DKP[which(avg.DKP$Opp == tolower(input$Opponent2)),
                                   as.numeric(which(colMeans(dat.player2[,36:40]) > 0.75)+1)]))),
              se.fit = T)$se.fit)
    
    ### Player proportion x Team regression
    # Player 1
    for (i in 1:nrow(dat.player1)) {
      dat.player1$Team.DKP[i] <- sum(dat.team1$DKP[which(
        dat.team1$GameID == dat.player1$GameID[i])],na.rm = T)
      dat.player1$DKP.Prop[i] <- dat.player1$DKP[i]/dat.player1$Team.DKP[i]
    }
    
    player1.Project_5.reg <- lm(data = dat.player1, formula = Team.DKP ~ Spread + Team.pts + H.A)
    Projections_Player1$Project_5 <- rep(predict.lm(object = player1.Project_5.reg, 
                                                    newdata = data.frame(Spread = input$Spread1,
                                                                         Team.pts = input$Over.Under1/2 - input$Spread1/2,
                                                                         H.A = input$H.A1)),iter)
    Projections_Player1$Project_5 <- Projections_Player1$Project_5 +
      rnorm(iter, mean = 0, 
            sd = summary(player1.Project_5.reg)$sigma)
    
    Projections_Player1$Project_5 <- Projections_Player1$Project_5 *
      sample(x = density(dat.player1$DKP.Prop)$x, size = iter, replace = T, prob = density(dat.player1$DKP.Prop)$y)
    
    # Player 2
    for (j in 1:nrow(dat.player2)) {
      dat.player2$Team.DKP[j] <- sum(dat.team2$DKP[which(
        dat.team2$GameID == dat.player2$GameID[j])],na.rm = T)
      dat.player2$DKP.Prop[j] <- dat.player2$DKP[j]/dat.player2$Team.DKP[j]
    }
    
    player2.Project_5.reg <- lm(data = dat.player2, formula = Team.DKP ~ Spread + Team.pts + H.A)
    Projections_Player2$Project_5 <- rep(predict.lm(object = player2.Project_5.reg, 
                                                    newdata = data.frame(Spread = input$Spread2,
                                                                         Team.pts = input$Over.Under2/2 - input$Spread2/2,
                                                                         H.A = input$H.A2)),iter)
    Projections_Player2$Project_5 <- Projections_Player2$Project_5 +
      rnorm(iter, mean = 0, 
            sd = summary(player2.Project_5.reg)$sigma)
    
    Projections_Player2$Project_5 <- Projections_Player2$Project_5 *
      sample(x = density(dat.player2$DKP.Prop)$x, size = iter, replace = T, prob = density(dat.player2$DKP.Prop)$y)
    
    ### Get TPPs
    TPP_draws <- data.frame(Player1 = sample(x = density(unlist(as.list(Projections_Player1[,1:5])))$x,
                                             size = iter,
                                             replace = T,
                                             prob = density(unlist(as.list(Projections_Player1[,1:5])))$y),
                            Player2 = sample(x = density(unlist(as.list(Projections_Player2[,1:5])))$x,
                                             size = iter,
                                             replace = T,
                                             prob = density(unlist(as.list(Projections_Player2[,1:5])))$y))
    
    TPP_table <- data.frame(Player1 = length(which(apply(TPP_draws, 1, FUN = "which.max") == 1))/iter,
                            Player2 = length(which(apply(TPP_draws, 1, FUN = "which.max") == 2))/iter)
    
    ### Produce distribution of projections
    ggplot() +
      geom_density(aes(x = rowMeans(Projections_Player1[,1:5]), #as.numeric(unlist(Projections_Player1[,1:5]))
                       color = paste(dat.player1$Last..First[1], ": ",TPP_table[,1]*100,"%", sep = ""), 
                       fill = paste(dat.player1$Last..First[1], ": ",TPP_table[,1]*100,"%", sep = "")),
                   alpha = 0.5) +
      geom_density(aes(x = rowMeans(Projections_Player2[,1:5]), #as.numeric(unlist(Projections_Player1[,1:5]))
                       color = paste(dat.player2$Last..First[1], ": ",TPP_table[,2]*100,"%", sep = ""), 
                       fill = paste(dat.player2$Last..First[1], ": ",TPP_table[,2]*100,"%", sep = "")),
                   alpha = 0.5) +
      xlab("Projected DKP") +
      ylab("Density") +
      scale_color_discrete(name = "Player") +
      scale_fill_discrete(name = "Player") +
      ggtitle("Player DKP Projection") +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else if (input$Player1 != "" & input$Player2 != "" & input$Player3 != "") {
    ### Data subset
    # Player 1
    dat.player1 <- NBA_17[which(NBA_17$Last..First == input$Player1 &
                                  !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    dat.team1 <- NBA_17[which(NBA_17$Team == dat.player1$Team[nrow(dat.player1)] &
                                !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    dat.opp1 <- NBA_17[which(NBA_17$Opp == tolower(input$Opponent1) &
                               !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    
    Projections_Player1 <- data.frame(Project_1 = rep(NA,iter),
                                      Project_2 = rep(NA,iter),
                                      Project_3 = rep(NA,iter),
                                      Project_4 = rep(NA,iter),
                                      Project_5 = rep(NA,iter))
    # Player 2
    dat.player2 <- NBA_17[which(NBA_17$Last..First == input$Player2 &
                                  !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    dat.team2 <- NBA_17[which(NBA_17$Team == dat.player2$Team[nrow(dat.player2)] &
                                !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    dat.opp2 <- NBA_17[which(NBA_17$Opp == tolower(input$Opponent2) &
                               !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    
    Projections_Player2 <- data.frame(Project_1 = rep(NA,iter),
                                      Project_2 = rep(NA,iter),
                                      Project_3 = rep(NA,iter),
                                      Project_4 = rep(NA,iter),
                                      Project_5 = rep(NA,iter))
    
    # Player 3
    dat.player3 <- NBA_17[which(NBA_17$Last..First == input$Player3 &
                                  !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    dat.team3 <- NBA_17[which(NBA_17$Team == dat.player3$Team[nrow(dat.player3)] &
                                !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    dat.opp3 <- NBA_17[which(NBA_17$Opp == tolower(input$Opponent3) &
                               !is.na(NBA_17$DKP) & NBA_17$active == 1),]
    
    Projections_Player3 <- data.frame(Project_1 = rep(NA,iter),
                                      Project_2 = rep(NA,iter),
                                      Project_3 = rep(NA,iter),
                                      Project_4 = rep(NA,iter),
                                      Project_5 = rep(NA,iter))
    
    ### General distribution
    # Player 1
    player1.dens <- density(dat.player1$DKP)
    Projections_Player1$Project_1 <- sample(player1.dens$x, iter, replace = T, 
                                            prob = player1.dens$y)
    
    # Player 2
    player2.dens <- density(dat.player2$DKP)
    Projections_Player2$Project_1 <- sample(player2.dens$x, iter, replace = T, 
                                            prob = player2.dens$y)
    
    # Player 3
    player3.dens <- density(dat.player3$DKP)
    Projections_Player3$Project_1 <- sample(player3.dens$x, iter, replace = T, 
                                            prob = player3.dens$y)
    
    ### Point total regression
    # Player 1
    player1.Project_2.reg <- lm(data = dat.player1, formula = DKP ~ poly(Team.pts, 3))
    Projections_Player1$Project_2 <- rep(predict.lm(object = player1.Project_2.reg, 
                                                    newdata = data.frame(Team.pts = input$Over.Under1/2 - input$Spread1/2)),iter)
    Projections_Player1$Project_2 <- Projections_Player1$Project_2 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player1.Project_2.reg, 
                            newdata = data.frame(Team.pts = input$Over.Under1/2 - input$Spread1/2),
                            se.fit = T)$se.fit)
    
    # Player 2
    player2.Project_2.reg <- lm(data = dat.player2, formula = DKP ~ poly(Team.pts, 3))
    Projections_Player2$Project_2 <- rep(predict.lm(object = player2.Project_2.reg, 
                                                    newdata = data.frame(Team.pts = input$Over.Under2/2 - input$Spread2/2)),iter)
    Projections_Player2$Project_2 <- Projections_Player2$Project_2 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player2.Project_2.reg, 
                            newdata = data.frame(Team.pts = input$Over.Under2/2 - input$Spread2/2),
                            se.fit = T)$se.fit)
    
    # Player 3
    player3.Project_2.reg <- lm(data = dat.player3, formula = DKP ~ poly(Team.pts, 3))
    Projections_Player3$Project_2 <- rep(predict.lm(object = player3.Project_2.reg, 
                                                    newdata = data.frame(Team.pts = input$Over.Under3/2 - input$Spread3/2)),iter)
    Projections_Player3$Project_2 <- Projections_Player3$Project_2 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player3.Project_2.reg, 
                            newdata = data.frame(Team.pts = input$Over.Under3/2 - input$Spread3/2),
                            se.fit = T)$se.fit)
    
    ### Spread regression
    # Player 1
    dat.player1$Spread <- dat.player1$Opp.pts - dat.player1$Team.pts
    player1.Project_3.reg <- lm(data = dat.player1, formula = DKP ~ poly(Spread, 3))
    Projections_Player1$Project_3 <- rep(predict.lm(object = player1.Project_3.reg, 
                                                    newdata = data.frame(Spread = 
                                                                           input$Spread1)),iter)
    Projections_Player1$Project_3 <- Projections_Player1$Project_3 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player1.Project_3.reg, 
                            newdata = data.frame(Spread = input$Spread1),
                            se.fit = T)$se.fit)
    
    # Player 2
    dat.player2$Spread <- dat.player2$Opp.pts - dat.player2$Team.pts
    player2.Project_3.reg <- lm(data = dat.player2, formula = DKP ~ poly(Spread, 3))
    Projections_Player2$Project_3 <- rep(predict.lm(object = player2.Project_3.reg, 
                                                    newdata = data.frame(Spread = 
                                                                           input$Spread2)),iter)
    Projections_Player2$Project_3 <- Projections_Player2$Project_3 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player2.Project_3.reg, 
                            newdata = data.frame(Spread = input$Spread2),
                            se.fit = T)$se.fit)
    
    # Player 3
    dat.player3$Spread <- dat.player3$Opp.pts - dat.player3$Team.pts
    player3.Project_3.reg <- lm(data = dat.player3, formula = DKP ~ poly(Spread, 3))
    Projections_Player3$Project_3 <- rep(predict.lm(object = player3.Project_3.reg, 
                                                    newdata = data.frame(Spread = 
                                                                           input$Spread3)),iter)
    Projections_Player3$Project_3 <- Projections_Player3$Project_3 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player3.Project_3.reg, 
                            newdata = data.frame(Spread = input$Spread3),
                            se.fit = T)$se.fit)
    
    ### Opponent average DKP allowed regression
    # Player 1
    dat.player1 <- merge(dat.player1, 
                         avg.DKP[,c(1,as.numeric(which(colMeans(dat.player1[,36:40]) > 0.75)+1))], by = "Opp")
    dat.player1$Opp.Avg.All <- if (length(which(grepl("Opp.Avg", colnames(dat.player1)))) > 1) {
      rowMeans(dat.player1[,which(grepl("Opp.Avg", colnames(dat.player1)))])
    } else (dat.player1[,which(grepl("Opp.Avg", colnames(dat.player1)))])
    
    player1.Project_4.reg <- lm(data = dat.player1, formula = DKP ~ poly(Opp.Avg.All, 3))
    Projections_Player1$Project_4 <- 
      rep(predict.lm(object = player1.Project_4.reg,newdata = data.frame(
        Opp.Avg.All = mean(as.numeric(avg.DKP[which(avg.DKP$Opp == tolower(input$Opponent1)),
                                              as.numeric(which(colMeans(dat.player1[,36:40]) > 0.75)+1)])))),iter)
    
    Projections_Player1$Project_4 <- Projections_Player1$Project_4 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player1.Project_4.reg,newdata = data.frame(
              Opp.Avg.All = mean(
                as.numeric(avg.DKP[which(avg.DKP$Opp == tolower(input$Opponent1)),
                                   as.numeric(which(colMeans(dat.player1[,36:40]) > 0.75)+1)]))),
              se.fit = T)$se.fit)
    
    # Player 2
    dat.player2 <- merge(dat.player2, 
                         avg.DKP[,c(1,as.numeric(which(colMeans(dat.player2[,36:40]) > 0.75)+1))], by = "Opp")
    dat.player2$Opp.Avg.All <- if (length(which(grepl("Opp.Avg", colnames(dat.player2)))) > 1) {
      rowMeans(dat.player2[,which(grepl("Opp.Avg", colnames(dat.player2)))])
    } else (dat.player2[,which(grepl("Opp.Avg", colnames(dat.player2)))])
    
    player2.Project_4.reg <- lm(data = dat.player2, formula = DKP ~ poly(Opp.Avg.All, 3))
    Projections_Player2$Project_4 <- 
      rep(predict.lm(object = player2.Project_4.reg,newdata = data.frame(
        Opp.Avg.All = mean(as.numeric(avg.DKP[which(avg.DKP$Opp == tolower(input$Opponent2)),
                                              as.numeric(which(colMeans(dat.player2[,36:40]) > 0.75)+1)])))),iter)
    
    Projections_Player2$Project_4 <- Projections_Player2$Project_4 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player2.Project_4.reg,newdata = data.frame(
              Opp.Avg.All = mean(
                as.numeric(avg.DKP[which(avg.DKP$Opp == tolower(input$Opponent2)),
                                   as.numeric(which(colMeans(dat.player2[,36:40]) > 0.75)+1)]))),
              se.fit = T)$se.fit)
    
    # Player 3
    dat.player3 <- merge(dat.player3, 
                         avg.DKP[,c(1,as.numeric(which(colMeans(dat.player3[,36:40]) > 0.75)+1))], by = "Opp")
    dat.player3$Opp.Avg.All <- if (length(which(grepl("Opp.Avg", colnames(dat.player3)))) > 1) {
      rowMeans(dat.player3[,which(grepl("Opp.Avg", colnames(dat.player3)))])
    } else (dat.player3[,which(grepl("Opp.Avg", colnames(dat.player3)))])
    
    player3.Project_4.reg <- lm(data = dat.player3, formula = DKP ~ poly(Opp.Avg.All, 3))
    Projections_Player3$Project_4 <- 
      rep(predict.lm(object = player3.Project_4.reg,newdata = data.frame(
        Opp.Avg.All = mean(as.numeric(avg.DKP[which(avg.DKP$Opp == tolower(input$Opponent3)),
                                              as.numeric(which(colMeans(dat.player3[,36:40]) > 0.75)+1)])))),iter)
    
    Projections_Player3$Project_4 <- Projections_Player3$Project_4 +
      rnorm(iter, mean = 0, 
            sd = predict.lm(object = player3.Project_4.reg,newdata = data.frame(
              Opp.Avg.All = mean(
                as.numeric(avg.DKP[which(avg.DKP$Opp == tolower(input$Opponent3)),
                                   as.numeric(which(colMeans(dat.player3[,36:40]) > 0.75)+1)]))),
              se.fit = T)$se.fit)
    
    ### Player proportion x Team regression
    # Player 1
    for (i in 1:nrow(dat.player1)) {
      dat.player1$Team.DKP[i] <- sum(dat.team1$DKP[which(
        dat.team1$GameID == dat.player1$GameID[i])],na.rm = T)
      dat.player1$DKP.Prop[i] <- dat.player1$DKP[i]/dat.player1$Team.DKP[i]
    }
    
    player1.Project_5.reg <- lm(data = dat.player1, formula = Team.DKP ~ Spread + Team.pts + H.A)
    Projections_Player1$Project_5 <- rep(predict.lm(object = player1.Project_5.reg, 
                                                    newdata = data.frame(Spread = input$Spread1,
                                                                         Team.pts = input$Over.Under1/2 - input$Spread1/2,
                                                                         H.A = input$H.A1)),iter)
    Projections_Player1$Project_5 <- Projections_Player1$Project_5 +
      rnorm(iter, mean = 0, 
            sd = summary(player1.Project_5.reg)$sigma)
    
    Projections_Player1$Project_5 <- Projections_Player1$Project_5 *
      sample(x = density(dat.player1$DKP.Prop)$x, size = iter, replace = T, prob = density(dat.player1$DKP.Prop)$y)
    
    # Player 2
    for (j in 1:nrow(dat.player2)) {
      dat.player2$Team.DKP[j] <- sum(dat.team2$DKP[which(
        dat.team2$GameID == dat.player2$GameID[j])],na.rm = T)
      dat.player2$DKP.Prop[j] <- dat.player2$DKP[j]/dat.player2$Team.DKP[j]
    }
    
    player2.Project_5.reg <- lm(data = dat.player2, formula = Team.DKP ~ Spread + Team.pts + H.A)
    Projections_Player2$Project_5 <- rep(predict.lm(object = player2.Project_5.reg, 
                                                    newdata = data.frame(Spread = input$Spread2,
                                                                         Team.pts = input$Over.Under2/2 - input$Spread2/2,
                                                                         H.A = input$H.A2)),iter)
    Projections_Player2$Project_5 <- Projections_Player2$Project_5 +
      rnorm(iter, mean = 0, 
            sd = summary(player2.Project_5.reg)$sigma)
    
    Projections_Player2$Project_5 <- Projections_Player2$Project_5 *
      sample(x = density(dat.player2$DKP.Prop)$x, size = iter, replace = T, prob = density(dat.player2$DKP.Prop)$y)
    
    # Player 1
    for (k in 1:nrow(dat.player3)) {
      dat.player3$Team.DKP[k] <- sum(dat.team3$DKP[which(
        dat.team3$GameID == dat.player3$GameID[k])],na.rm = T)
      dat.player3$DKP.Prop[k] <- dat.player3$DKP[k]/dat.player3$Team.DKP[k]
    }
    
    player3.Project_5.reg <- lm(data = dat.player3, formula = Team.DKP ~ Spread + Team.pts + H.A)
    Projections_Player3$Project_5 <- rep(predict.lm(object = player3.Project_5.reg, 
                                                    newdata = data.frame(Spread = input$Spread3,
                                                                         Team.pts = input$Over.Under3/2 - input$Spread3/2,
                                                                         H.A = input$H.A3)),iter)
    Projections_Player3$Project_5 <- Projections_Player3$Project_5 +
      rnorm(iter, mean = 0, 
            sd = summary(player3.Project_5.reg)$sigma)
    
    Projections_Player3$Project_5 <- Projections_Player3$Project_5 *
      sample(x = density(dat.player3$DKP.Prop)$x, size = iter, replace = T, prob = density(dat.player3$DKP.Prop)$y)
    
    ### Get TPPs
    TPP_draws <- data.frame(Player1 = sample(x = density(unlist(as.list(Projections_Player1[,1:5])))$x,
                                             size = iter,
                                             replace = T,
                                             prob = density(unlist(as.list(Projections_Player1[,1:5])))$y),
                            Player2 = sample(x = density(unlist(as.list(Projections_Player2[,1:5])))$x,
                                             size = iter,
                                             replace = T,
                                             prob = density(unlist(as.list(Projections_Player2[,1:5])))$y),
                            Player3 = sample(x = density(unlist(as.list(Projections_Player3[,1:5])))$x,
                                             size = iter,
                                             replace = T,
                                             prob = density(unlist(as.list(Projections_Player3[,1:5])))$y))
    
    TPP_table <- data.frame(Player1 = length(which(apply(TPP_draws, 1, FUN = "which.max") == 1))/iter,
                            Player2 = length(which(apply(TPP_draws, 1, FUN = "which.max") == 2))/iter,
                            Player3 = length(which(apply(TPP_draws, 1, FUN = "which.max") == 3))/iter)
    
    ### Produce distribution of projections
    ggplot() +
      geom_density(aes(x = rowMeans(Projections_Player1[,1:5]), #as.numeric(unlist(Projections_Player1[,1:5]))
                       color = paste(dat.player1$Last..First[1], ": ",TPP_table[,1]*100,"%", sep = ""), 
                       fill = paste(dat.player1$Last..First[1], ": ",TPP_table[,1]*100,"%", sep = "")),
                   alpha = 0.5) +
      geom_density(aes(x = rowMeans(Projections_Player2[,1:5]), #as.numeric(unlist(Projections_Player1[,1:5]))
                       color = paste(dat.player2$Last..First[1], ": ",TPP_table[,2]*100,"%", sep = ""), 
                       fill = paste(dat.player2$Last..First[1], ": ",TPP_table[,2]*100,"%", sep = "")),
                   alpha = 0.5) +
      geom_density(aes(x = rowMeans(Projections_Player3[,1:5]), #as.numeric(unlist(Projections_Player1[,1:5]))
                       color = paste(dat.player3$Last..First[1], ": ",TPP_table[,3]*100,"%", sep = ""), 
                       fill = paste(dat.player3$Last..First[1], ": ",TPP_table[,3]*100,"%", sep = "")),
                   alpha = 0.5) +
      xlab("Projected DKP") +
      ylab("Density") +
      scale_color_discrete(name = "Player") +
      scale_fill_discrete(name = "Player") +
      ggtitle("Player DKP Projection") +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else {}
    
    
  })
  }

shinyApp(ui = ui, server = server)

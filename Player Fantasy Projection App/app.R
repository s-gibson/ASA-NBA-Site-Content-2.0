##################################### 
##  ASA NBA Site Content           ##
##  Player Fantasy Projection App  ##
##  Stewart Gibson                 ##
##  9/1/17                         ##
#####################################

### Set working directory
#setwd("~/Documents/ASA/ASA Site Content/NBA/Player Fantasy Projection App")

### Load data and packages
load("NBA_2017.RData")
require(rsconnect)
require(shiny)
require(ggplot2)

NBA_17$Last..First <- as.character(NBA_17$Last..First)
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
    titlePanel("Advanced Sports Analytics NBA Player Fantasy Projection App"),
    
    column(2,wellPanel(
      selectInput(inputId = "Player1", label = "Player 1",
                  choices = c("",sort(unique(NBA_17$Last..First))), multiple = F),
      numericInput(inputId = "Over.Under1", label = "Over/Under 1", step = 0.5, value = 210),
      numericInput(inputId = "Spread1", label = "Spread 1", step = 0.5, value  = 0),
      # sliderInput(inputId = "Over.Under1", label = "Over/Under 1", min = 180, max = 240, step = 0.5,
      #             value = 210),
      # sliderInput(inputId = "Spread1", label = "Spread 1", min = -25, max = 25, step = 0.5, value = 0),
      selectInput(inputId = "Opponent1", label = "Opponent 1",
                  choices = c("",sort(toupper(unique(NBA_17$Opp)))), multiple = F),
      selectInput(inputId = "H.A", label = "Home/Away 1", choices = c("Home","Away"), multiple = F),
      submitButton("Submit")
      
    )),
    column(8,  plotOutput(outputId = "chart"))
  )
  
)

server <- function(input, output) {
  output$chart <- renderPlot({
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
    dat.player1$Opp.Avg.All <- 
      rowMeans(dat.player1[,which(grepl("Opp.Avg", colnames(dat.player1)))])
    
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
    
    ### Produce distribution of projections
    ggplot() +
      geom_density(aes(x = rowMeans(Projections_Player1[,1:4]), #as.numeric(unlist(Projections_Player1[,1:4]))
                       color = dat.player1$Last..First[1], fill = dat.player1$Last..First[1]),
                   alpha = 0.5) +
      xlab("Projected DKP") +
      ylab("Density") +
      scale_color_discrete(name = "Player") +
      scale_fill_discrete(name = "Player") +
      ggtitle("Player DKP Projection") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  }

shinyApp(ui = ui, server = server)

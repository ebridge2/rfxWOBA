#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(lubridate)
library(readr)
library(tidyverse)
library(lubridate)

dat <- read_rds("../../data/shiny/player_dat.rds")

id.dat <- read_rds("../../data/shiny/player_id.rds")

rfx <- read_rds('../../data/projection/rfxwOBA_1000.rds')

dat$rfxwOBA <- rowMeans(rfx)

get.wOBA.denom <- function(Y) {
  wOBA.denom <- rep(1, length(Y))
  wOBA.denom[Y=="Sac bunt"] <- 0
  return(wOBA.denom)
}

get.wOBA.value <- function(Y) {
  wOBA.value <- rep(NA, length(Y))
  wOBA.value[Y=="Strikeout" | Y=="Out" | Y=="Sac bunt"] <- 0
  wOBA.value[Y=="Walk" | Y=="Hit by pitch"] <- 0.7
  wOBA.value[Y=="Single"] <- 0.9
  wOBA.value[Y=="Double"] <- 1.25
  wOBA.value[Y=="Triple"] <- 1.6
  wOBA.value[Y=="Home run"] <- 2
  return(wOBA.value)
}

get.rfxwOBA.dist <- function(dat, player.name, id.dat, yr, type, rfx) {
  id <- id.dat$mlb_id[id.dat$mlb_name==player.name]
  if (type=="batter") {
    idx <- which(dat$batter==id & year(dat$game_date)==yr)
  } else if (type=="pitcher") {
    idx <- which(dat$pitcher==id & year(dat$game_date)==yr)
  }
  player.df <- dat[idx,] %>% select(woba_value, woba_denom, xwOBA, rfxwOBA)
  rfx.df <- rfx[idx,] %>% 
    colSums() %>%
    as.data.frame() 
  colnames(rfx.df) <- "rfxwOBA"
  PA <- nrow(player.df)
  rfx.df %>% ggplot(aes(x=rfxwOBA/sum(player.df$woba_denom))) + 
    geom_density() + 
    geom_vline(aes(xintercept=sum(player.df$woba_value)/sum(player.df$woba_denom), color="Actual wOBA")) +
    geom_vline(aes(xintercept=sum(player.df$rfxwOBA)/sum(player.df$woba_denom), color="Mean rfxwOBA")) +
    geom_vline(aes(xintercept=sum(player.df$xwOBA, na.rm=T)/sum(player.df$woba_denom), color="bsxwOBA")) +
    labs(title=paste0(player.name, " rfxwOBA Disribution - ", yr, " (PA = ", PA, ")"),
         x="rfxwOBA", color="Statistic")
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  observeEvent(input$randomPlayer, {
    player.name <- id.dat$mlb_name[id.dat$mlb_id==sample(dat$batter, size=1)]
    updateTextInput(session, "player.name", value=player.name)
  })
  
  output$rfxwOBA.Plots <- renderPlot({
    validate(need(input$player.name, "Player name cannot be blank"))

  
    player.name <- input$player.name
    
    get.rfxwOBA.dist(dat, player.name, id.dat, 2017, "batter", rfx)
    
    
  })
  
  
})

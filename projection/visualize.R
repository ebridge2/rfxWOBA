library(readr)
library(tidyverse)
library(lubridate)

#load the clean data
dat1 <- read_csv("../data/cleaned/model_data.csv")

# load the raw data
dat2 <- read_csv("../data/raw/baseball_data.csv") 


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

# join the batter id, pitcher id, game_date, and woba values from raw data

dat <- data.frame(outcome=dat$outcome) %>%
  mutate(batter=dat2$batter,
         pitcher=dat2$pitcher,
         xwOBA=as.numeric(dat2$estimated_woba_using_speedangle),
         game_date=as.Date(dat2$game_date),
         woba_value=get.wOBA.value(outcome),
         woba_denom=get.wOBA.denom(outcome),
         des=dat2$des) 

dat[is.na(dat$xwOBA),]$xwOBA <- dat[is.na(dat$xwOBA),]$woba_value
  
rm(dat1)
rm(dat2)

rfx <- readRDS('../data/projection/rfxwOBA_1000.rds')


dat$rfxwOBA <- rowMeans(rfx)

# player id data

id.dat <- read_csv("../data/raw/player_id.csv") %>%
  select(mlb_id, mlb_name)

# function to generate distribtion of rfxwOBA for a given player in a given year

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
  rfx.df %>% ggplot(aes(x=rfxwOBA/sum(player.df$woba_denom))) + 
    geom_density() + 
    geom_vline(aes(xintercept=sum(player.df$woba_value)/sum(player.df$woba_denom), color="Actual wOBA")) +
    geom_vline(aes(xintercept=sum(player.df$rfxwOBA)/sum(player.df$woba_denom), color="Mean rfxwOBA")) +
    geom_vline(aes(xintercept=sum(player.df$xwOBA, na.rm=T)/sum(player.df$woba_denom), color="Baseball Savant xwOBA")) +
    labs(title=paste0(player.name, " rfxwOBA Disribution - ", yr),
         x="rfxwOBA", color="Statistic")
}

# function for 3 year rolling wOBA vs estimates

rolling.rfxwOBA <- function(dat, player.name, id.dat, type) {
  id <- id.dat$mlb_id[id.dat$mlb_name==player.name]
  if (type=="batter") {
    idx <- which(dat$batter==id)
  } else if (type=="pitcher") {
    idx <- which(dat$pitcher==id)
  }
  player.df <- dat[idx,] %>% 
    select(woba_value, woba_denom, xwOBA, rfxwOBA, game_date) %>%
    arrange(game_date)
  player.df %>% ggplot(aes(x=game_date)) +
    geom_line(aes(y=cumsum(woba_value)/cumsum(woba_denom), color="Actual wOBA")) +
    geom_line(aes(y=cumsum(rfxwOBA)/cumsum(woba_denom), color="Mean rfxwOBA")) +
    geom_line(aes(y=cumsum(xwOBA)/cumsum(woba_denom), color="Baseball Savant xwOBA")) +
    labs(title=paste0(player.name, " rolling wOBA vs. estimates: 2017-2019"),
         x="Date", y="Value", color="Statistic")
}

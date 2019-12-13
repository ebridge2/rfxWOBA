
library(shiny)
library(tidyverse)
library(readr)
library(lubridate)
library(readr)
library(tidyverse)
library(lubridate)
library(data.table)
library(mltools)

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

#load the clean data
dat1 <- read_csv("../data/cleaned/model_data.csv")

# load the raw data
dat2 <- read_csv("../data/raw/baseball_data.csv")

# join the batter id, pitcher id, game_date, and woba values from raw data

dat <- data.frame(outcome=dat1$outcome) %>%
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

# player id data

id.dat <- read_csv("../data/raw/player_id.csv") %>%
  select(mlb_id, mlb_name)

write_rds(dat, "../data/shiny/player_dat.rds")

write_rds(id.dat, "../data/shiny/player_id.rds")

dat <- read_csv("../data/cleaned/model_data.csv")

descriptions <- read_csv("../data/raw/baseball_data.csv") %>%
  select(des)


# select numeric features
X.num <- dat %>%
  select(-year, -outcome, -pitch_name, -stadium, -if_fielding_alignment, -of_fielding_alignment) %>%
  mutate(stand=as.numeric(stand == "R"), p_throws=as.numeric(p_throws == "R"),
         inning_topbot=as.numeric(inning_topbot == "Top"), on_1b_bool=as.numeric(on_1b_bool),
         on_2b_bool=as.numeric(on_2b_bool), on_3b_bool=as.numeric(on_3b_bool)) %>%
  rename(batterRighty=stand, pitcherRighty=p_throws, inningTopHalf=inning_topbot, RO1b=on_1b_bool,
         RO2b=on_2b_bool, RO3b=on_3b_bool)

# one-hot encode categorical features
X.cat <- dat %>%
  select(pitch_name, stadium, if_fielding_alignment, of_fielding_alignment) %>%
  mutate(pitch_name=factor(pitch_name, levels=unique(pitch_name)),
         stadium=factor(stadium, levels=unique(stadium)),
         if_fielding_alignment=factor(if_fielding_alignment, levels=unique(if_fielding_alignment)),
         of_fielding_alignment=factor(of_fielding_alignment, levels=unique(of_fielding_alignment))) %>%
  # rename with periods so that we can grab the categorical features later since they are OHE'd with _s
  rename(if.fielding.alignment=if_fielding_alignment,
         of.fielding.alignment=of_fielding_alignment) %>%
  as.data.table() %>%
  # one-hot encode the entire table
  one_hot()

# paste em together with categoricals all after numerics
X <- cbind(X.num, X.cat)

write_rds(X, "../data/shiny/model_dat.rds")

write_rds(descriptions, "../data/shiny/descriptions.rds")

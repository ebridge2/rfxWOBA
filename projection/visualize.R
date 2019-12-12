library(readr)
library(tidyverse)

#load the clean data
dat <- read_csv("../data/cleaned/model_data.csv")

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
}

# join the batter id, pitcher id, game_date, and woba values from raw data

dat <- dat %>%
  mutate(batter=dat2$batter,
         pitcher=dat2$pitcher,
         game_date=dat2$game_date,
         woba_value=get.wOBA.value(outcome),
         woba_denom=get.wOBA.denom(outcome))

rm(dat2)

dat$rfxwOBA <- rowMeans(rfxwOBA)
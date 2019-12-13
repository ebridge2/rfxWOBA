
library(shiny)
library(tidyverse)
library(readr)
library(lubridate)
library(readr)
library(tidyverse)
library(lubridate)

#load the clean data
dat1 <- read_csv("../../data/cleaned/model_data.csv")

# load the raw data
dat2 <- read_csv("../../data/raw/baseball_data.csv")

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

# player id data

id.dat <- read_csv("../../data/raw/player_id.csv") %>%
  select(mlb_id, mlb_name)

write_rds(dat, "../../data/shiny/player_dat.rds")

write_rds(id.dat, "../../data/shiny/player_id.rds")

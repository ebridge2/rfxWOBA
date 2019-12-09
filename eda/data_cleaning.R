library(readr)
library(tidyverse)

dat <- read_csv("baseball_data.csv")

dat <- dat %>% mutate(launch_angle=as.numeric(launch_angle),
                      launch_speed=as.numeric(launch_speed),
                      release_spin_rate=as.numeric(release_spin_rate),
                      hc_x=as.numeric(hc_x),
                      hc_y=as.numeric(hc_y),
                      stadium=home_team,
                      player_id=batter, 
                      year=game_year) %>%
  mutate(spray_angle=atan((hc_x-125.42)/(198.27-hc_y))*180/pi*0.75,
         outcome=ifelse(events=="single", "Single",
                        ifelse(events=="double", "Double",
                               ifelse(events=="triple", "Triple",
                                      ifelse(events=="home_run", "Home run",
                                             ifelse(events %in% c("strikeout", 
                                                                  "strikeout_double_play"), 
                                                    "Strikeout",
                                                    ifelse(events %in% c("sac_bunt",
                                                                         "sac_bunt_double_play"),
                                                           "Sac bunt", 
                                                           ifelse(events=="walk", "Walk", 
                                                                  ifelse(events=="hit_by_pitch", "Hit by pitch", "Out"))))))))) 

speed.2017 <- read_csv('sprint_speed_2017.csv')
speed.2017$year <- 2017
speed.2018 <- read_csv('sprint_speed_2018.csv')
speed.2018$year <- 2018
speed.2019 <- read_csv('sprint_speed_2019.csv')
speed.2019$year <- 2019
speed <- rbind(speed.2017, speed.2018, speed.2019)

def.2017 <- read_csv('defense_2017.csv')
def.2017$year <- 2017
def.2018 <- read_csv('defense_2018.csv')
def.2018$year <- 2018
def.2019 <- read_csv('defense_2019.csv')
def.2019$year <- 2019
def <- rbind(def.2017, def.2018, def.2019) %>%
  mutate(fg_id=playerid)

dat <- dat %>% left_join(speed, by=c("year", "player_id"))

player_ids <- read_csv('player_id.csv') %>%
  mutate(player_id=mlb_id,
         fg_id=as.numeric(fg_id)) %>%
  select(player_id, fg_id)


def <- def %>% left_join(player_ids, by="fg_id") %>%
  mutate(Inn=floor(Inn)+(Inn-floor(Inn))*10/3)

defP <- def %>% filter(Pos=="P") %>% 
  rename(pitcher=player_id) %>%
  mutate(def1=(DRS/Inn)*1000) %>%
  select(year, pitcher, def1)
dat <- dat %>% left_join(defP, by=c("year", "pitcher"))
defC <- def %>% filter(Pos=="C") %>% 
  rename(fielder_2=player_id) %>%
  mutate(def2=(DRS/Inn)*1000) %>%
  select(year, fielder_2, def2)
dat <- dat %>% left_join(defC, by=c("year", "fielder_2"))
def1B <- def %>% filter(Pos=="1B") %>% 
  rename(fielder_3=player_id) %>%
  mutate(def3=(DRS/Inn)*1000) %>%
  select(year, fielder_3, def3)
dat <- dat %>% left_join(def1B, by=c("year", "fielder_3"))
def2B <- def %>% filter(Pos=="2B") %>% 
  rename(fielder_4=player_id) %>%
  mutate(def4=(DRS/Inn)*1000) %>%
  select(year, fielder_4, def4)
dat <- dat %>% left_join(def2B, by=c("year", "fielder_4"))
def3B <- def %>% filter(Pos=="3B") %>% 
  rename(fielder_5=player_id) %>%
  mutate(def5=(DRS/Inn)*1000) %>%
  select(year, fielder_5, def5)
dat <- dat %>% left_join(def3B, by=c("year", "fielder_5"))
defSS <- def %>% filter(Pos=="SS") %>% 
  rename(fielder_6=player_id) %>%
  mutate(def6=(DRS/Inn)*1000) %>%
  select(year, fielder_6, def6)
dat <- dat %>% left_join(defSS, by=c("year", "fielder_6"))
defLF <- def %>% filter(Pos=="LF") %>% 
  rename(fielder_7=player_id) %>%
  mutate(def7=(DRS/Inn)*1000) %>%
  select(year, fielder_7, def7)
dat <- dat %>% left_join(defLF, by=c("year", "fielder_7"))
defCF <- def %>% filter(Pos=="CF") %>% 
  rename(fielder_8=player_id) %>%
  mutate(def8=(DRS/Inn)*1000) %>%
  select(year, fielder_8, def8)
dat <- dat %>% left_join(defCF, by=c("year", "fielder_8"))
defRF <- def %>% filter(Pos=="RF") %>% 
  rename(fielder_9=player_id) %>%
  mutate(def9=(DRS/Inn)*1000) %>%
  select(year, fielder_9, def9)
dat <- dat %>% left_join(defRF, by=c("year", "fielder_9"))

dat <- dat %>% mutate(on_1b_bool=ifelse(on_1b=="null", FALSE, TRUE))
dat <- dat %>% mutate(on_2b_bool=ifelse(on_2b=="null", FALSE, TRUE))
dat <- dat %>% mutate(on_3b_bool=ifelse(on_3b=="null", FALSE, TRUE))

dat.batted <- dat %>% filter(outcome %in% c("Single",
                                            "Double",
                                            "Triple",
                                            "Home run",
                                            "Out")) %>%
  filter(!is.na(woba_denom))

dat.pred <- dat %>% select(release_speed, release_pos_x, release_pos_y, release_pos_z,
                           zone, stand, p_throws, pfx_x, pfx_z, plate_x, plate_z, 
                           on_1b_bool, on_2b_bool, on_3b_bool, vx0, vy0, vz0,
                           ax, ay, az, balls, strikes, sz_top, sz_bot, launch_speed, 
                           launch_angle, spray_angle, effective_speed, release_spin_rate, 
                           release_extension, pitch_number, pitch_name, def1, def2, def3,
                           def4, def5, def6, def7, def8, def9, if_fielding_alignment, 
                           of_fielding_alignment, stadium, outs_when_up, 
                           inning, inning_topbot, sprint_speed, outcome)

dat.pred.batted <- dat.batted %>% 
  select(release_speed, release_pos_x, release_pos_y, release_pos_z,
         zone, stand, p_throws, pfx_x, pfx_z, plate_x, plate_z, 
         on_1b_bool, on_2b_bool, on_3b_bool, vx0, vy0, vz0,
         ax, ay, az, balls, strikes, sz_top, sz_bot, launch_speed, 
         launch_angle, spray_angle, effective_speed, release_spin_rate, 
         release_extension, pitch_number, pitch_name, def1, def2, def3,
         def4, def5, def6, def7, def8, def9, if_fielding_alignment, 
         of_fielding_alignment, stadium, outs_when_up, 
         inning, inning_topbot, sprint_speed, outcome)

write_csv(dat.pred, "model_data.csv")
write_csv(dat.pred.batted, "model_data_batted.csv")

png("BattedBalls.png")
dat.batted %>% ggplot(aes(x=-(launch_angle+90), y=launch_speed, color=factor(outcome,
                                                                             levels=c("Out",
                                                                                      "Single",
                                                                                      "Double",
                                                                                      "Triple",
                                                                                      "Home run")))) +
  geom_point(alpha=0.025) +
  coord_polar(theta="x") +
  scale_x_continuous(limits = c(-180, 180),
                     breaks = c(-180, -135, -90, -45, 0),
                     labels = c(90, 45, 0, -45, -90)) +
  labs(title="2017-19 Batted Balls",
       x="Launch Angle",
       y="Launch Speed",
       color="Events") +
  guides(color=guide_legend(override.aes=list(alpha=1)))
dev.off()
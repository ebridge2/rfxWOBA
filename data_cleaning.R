library(readr)
library(tidyverse)

dat <- read_csv("baseball_data.csv")

dat <- dat %>% mutate(launch_angle=as.numeric(launch_angle),
                      launch_speed=as.numeric(launch_speed),
                      hc_x=as.numeric(hc_x),
                      hc_y=as.numeric(hc_y)) %>%
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

dat.batted <- dat %>% filter(outcome %in% c("Single",
                                            "Double",
                                            "Triple",
                                            "Home run",
                                            "Out")) %>%
  filter(!is.na(woba_denom))
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
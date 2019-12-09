library(tidyverse)
library(readr)
library(scales)

## load data

dat <- read_csv("./data/cleaned/model_data_batted.csv")

## batter-related qualities
## launch_angle + launch_speed -> outcome
png("BattedBalls.png")
dat %>% ggplot(aes(x=-(launch_angle+90), y=launch_speed, color=factor(outcome,
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

## strikes in count -> outcome
png("strikes.png")
dat %>% ggplot(aes(x = strikes, 
                   group = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), 
                   fill = factor(..x..))) + 
  geom_bar(aes(y= ..prop..), stat = "count") +
  geom_text(stat = "count", aes(label = scales::percent(..prop..), y= ..prop..), vjust = -0.5)+
  facet_grid(~factor(outcome, levels=c("Out","Single","Double","Triple","Home run")))+
  labs(y = "Percent", fill="Number of strikes", x = "Number of strikes")+
  scale_fill_brewer(palette="Greens")+
  ggtitle("Distribution of the number of strikes")
dev.off()



























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


## defence-related qualities
## def2
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def2)) + 
  geom_boxplot()+
  ylim(-50, 50)+
  labs(y = "Def2", x = "Outcome")+
  ggtitle("Distribution of Defense variable 2")
## def3
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def3)) + 
  geom_boxplot()+
  ylim(-20, 20)+
  labs(y = "Def3", x = "Outcome")+
  ggtitle("Distribution of Defense variable 3")
## def4
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def4)) + 
  geom_boxplot()+
  ylim(-25, 25)+
  labs(y = "Def4", x = "Outcome")+
  ggtitle("Distribution of Defense variable 4")
## def5
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def5)) + 
  geom_boxplot()+
  ylim(-25, 25)+
  labs(y = "Def5", x = "Outcome")+
  ggtitle("Distribution of Defense variable 5")
## def6
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def6)) + 
  geom_boxplot()+
  ylim(-30, 30)+
  labs(y = "Def6", x = "Outcome")+
  ggtitle("Distribution of Defense variable 6")
## def7
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def7)) + 
  geom_boxplot()+
  ylim(-35, 35)+
  labs(y = "Def7", x = "Outcome")+
  ggtitle("Distribution of Defense variable 7")
## def8
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def8)) + 
  geom_boxplot()+
  ylim(-35, 35)+
  labs(y = "Def8", x = "Outcome")+
  ggtitle("Distribution of Defense variable 8")
## def9
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def9)) + 
  geom_boxplot()+
  ylim(-35, 35)+
  labs(y = "Def9", x = "Outcome")+
  ggtitle("Distribution of Defense variable 9")
## sum(def1-9)
dat$sum_def <- rowSums(dat[, paste0("def", 1:9)])
dat %>%
  ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = sum_def)) + 
  geom_boxplot()+
  labs(y = "Sum of the defense variables", x = "Outcome")+
  ylim(-150, 150)+
  ggtitle("Distribution of the sum of Defense variables 1-9")
## sd(def1-9)
dat$sd_def <- apply(dat[, paste0("def", 1:9)], 1, function(x) sd(x))
dat %>%
  ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = sd_def)) + 
  geom_boxplot()+
  labs(y = "SD of the defense variables", x = "Outcome")+
  ylim(0, 25)+
  ggtitle("Distribution of the sum of Defense variables 1-9")















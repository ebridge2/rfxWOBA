library(tidyverse)
library(readr)
library(scales)
select <- dplyr::select
## load data

dat <- read_csv("../data/cleaned/model_data_batted.csv")

## batter-related qualities
## launch_angle + launch_speed -> outcome
png("./figs/hits/BattedBalls.png")
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
foul.lines <- data.frame(lf=c(315,315),
                         rf=c(45,45),
                         launch_speed=c(0,120))
dat <- dat %>% mutate(spray_angle2=ifelse(spray_angle<0, 360+spray_angle, spray_angle))
png("./figs/hits/BattedBalls2.png")
dat %>% ggplot(aes(x=spray_angle2, y=launch_speed, color=factor(outcome,
                                                                       levels=c("Out",
                                                                                "Single",
                                                                                "Double",
                                                                                "Triple",
                                                                                "Home run")))) +
  geom_line(data=foul.lines, aes(x=lf, y=launch_speed), color="black") +
  geom_line(data=foul.lines, aes(x=rf, y=launch_speed), color="black") +
  geom_point(alpha=0.1) + scale_x_continuous(limits = c(0, 360), breaks=c(0, 45, 315), labels=c(0, 45, -45)) +
  coord_polar(theta="x") +
  labs(title="2017-19 Batted Balls",
       x="Spray Angle",
       y="Launch Speed",
       color="Events") +
  guides(color=guide_legend(override.aes=list(alpha=1)))
dev.off()

## strikes in count -> outcome
png("./figs/hits/strikes.png")
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

## sprint_speed -> outcome
png("./figs/hits/Sprint_speed.png")
dat %>% ggplot(aes(x = sprint_speed, group = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), fill = factor(outcome, levels=c("Out","Single","Double","Triple","Home run"))))+
  geom_density(alpha = 0.3)+
  scale_fill_discrete(name = "Outcome")+
  labs(y = "Density", fill="Outcome", x = "Sprint Speed")+
  ggtitle("Distribution of sprint speed")
dev.off()


## defence-related qualities
## def1
png("./figs/Def/Def1.png")
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def1)) + 
  geom_boxplot()+
  ylim(-50, 50)+
  labs(y = "Defensive Runs Saved/1000 Innings", x = "Outcome")+
  ggtitle("Distributions of Pitcher Defense by Outcome")
dev.off()
## def2
png("./figs/Def/Def2.png")
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def2)) + 
  geom_boxplot()+
  ylim(-50, 50)+
  labs(y = "Defensive Runs Saved/1000 Innings", x = "Outcome")+
  ggtitle("Distributions of Catcher Defense by Outcome")
dev.off()
## def3
png("./figs/Def/Def3.png")
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def3)) + 
  geom_boxplot()+
  ylim(-20, 20)+
  labs(y = "Defensive Runs Saved/1000 Innings", x = "Outcome")+
  ggtitle("Distributions of First Base Defense by Outcome")
dev.off()
## def4
png("./figs/Def/Def4.png")
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def4)) + 
  geom_boxplot()+
  ylim(-25, 25)+
  labs(y = "Defensive Runs Saved/1000 Innings", x = "Outcome")+
  ggtitle("Distributions of Second Base Defense by Outcome")
dev.off()
## def5
png("./figs/Def/Def5.png")
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def5)) + 
  geom_boxplot()+
  ylim(-25, 25)+
  labs(y = "Defensive Runs Saved/1000 Innings", x = "Outcome")+
  ggtitle("Distributions of Third Base Defense by Outcome")
dev.off()
## def6
png("./figs/Def/Def6.png")
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def6)) + 
  geom_boxplot()+
  ylim(-30, 30)+
  labs(y = "Defensive Runs Saved/1000 Innings", x = "Outcome")+
  ggtitle("Distributions of Shortstop Defense by Outcome")
dev.off()
## def7
png("./figs/Def/Def7.png")
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def7)) + 
  geom_boxplot()+
  ylim(-35, 35)+
  labs(y = "Defensive Runs Saved/1000 Innings", x = "Outcome")+
  ggtitle("Distributions of Left Field Defense by Outcome")
dev.off()
## def8
png("./figs/Def/Def8.png")
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def8)) + 
  geom_boxplot()+
  ylim(-35, 35)+
  labs(y = "Defensive Runs Saved/1000 Innings", x = "Outcome")+
  ggtitle("Distributions of Center Field Defense by Outcome")
dev.off()
## def9
png("./figs/Def/Def9.png")
dat %>% ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = def9)) + 
  geom_boxplot()+
  ylim(-35, 35)+
  labs(y = "Defensive Runs Saved/1000 Innings", x = "Outcome")+
  ggtitle("Distributions of Right Field Defense by Outcome")
dev.off()
## sum(def1-9)
dat$sum_def <- rowSums(dat[, paste0("def", 1:9)])
png("./figs/Def/Sum_def.png")
dat %>%
  ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = sum_def)) + 
  geom_boxplot()+
  labs(y = "Sum of Defensive Runs Saved/1000 Innings", x = "Outcome")+
  ylim(-150, 150)+
  ggtitle("Distributions of Full Team Defense by Outcome")
dev.off()
## sd(def1-9)
dat$sd_def <- apply(dat[, paste0("def", 1:9)], 1, function(x) sd(x))
png("./figs/Def/SD_def.png")
dat %>%
  ggplot(aes(x = factor(outcome, levels=c("Out","Single","Double","Triple","Home run")), y = sd_def)) + 
  geom_boxplot()+
  labs(y = "SD Defensive Runs Saved/1000 Innings", x = "Outcome")+
  ylim(0, 25)+
  ggtitle("Distributions of SD of Full Team Defense by Outcome")
dev.off()



#Types of hits in stadiums
png("./figs/Stadium/Singles.png", width=720)
dat %>% filter(outcome=="Single") %>%
  ggplot(aes(x=factor(stadium), y=..count..)) + 
  geom_bar() +
  labs(x="Stadium",
       title="Number of Singles in each Stadium 2017-2019") 
dev.off()

png("./figs/Stadium/Doubles.png", width=720)
dat %>% filter(outcome=="Double") %>%
  ggplot(aes(x=factor(stadium), y=..count..)) + 
  geom_bar() +
  labs(x="Stadium",
       title="Number of Doubles in each Stadium 2017-2019") 
dev.off()

png("./figs/Stadium/Triples.png", width=720)
dat %>% filter(outcome=="Triple") %>%
  ggplot(aes(x=factor(stadium), y=..count..)) + 
  geom_bar() +
  labs(x="Stadium",
       title="Number of Triples in each Stadium 2017-2019") 
dev.off()

png("./figs/Stadium/HR.png", width=720)
dat %>% filter(outcome=="Home run") %>%
  ggplot(aes(x=factor(stadium), y=..count..)) + 
  geom_bar() +
  labs(x="Stadium",
       title="Number of Home runs in each Stadium 2017-2019") 
dev.off()










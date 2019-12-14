library(readr)
library(tidyverse)
library(lubridate)
library(psychometric)
library(kableExtra)

select <- dplyr::select

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

dat <- data.frame(outcome=dat1$outcome) %>%
  mutate(batter=dat2$batter,
         pitcher=dat2$pitcher,
         bsxwOBA=as.numeric(dat2$estimated_woba_using_speedangle),
         game_date=as.Date(dat2$game_date),
         woba_value=get.wOBA.value(outcome),
         woba_denom=get.wOBA.denom(outcome),
         des=dat2$des) 

dat[is.na(dat$bsxwOBA),]$bsxwOBA <- dat[is.na(dat$bsxwOBA),]$woba_value
  
rm(dat1)
rm(dat2)


# player id data

id.dat <- read_csv("../data/raw/player_id.csv") %>%
  dplyr::select(mlb_id, mlb_name)


##### Using the model trained on all data

rfx <- readRDS('../data/projection/rfxwOBA.rds')


dat$rfxwOBA <- rowMeans(rfx)





# function to generate distribtion of rfxwOBA for a given player in a given year

get.rfxwOBA.dist <- function(dat, player.name, id.dat, yr, type, rfx) {
  id <- id.dat$mlb_id[id.dat$mlb_name==player.name]
  if (type=="batter") {
    idx <- which(dat$batter==id & year(dat$game_date)==yr)
  } else if (type=="pitcher") {
    idx <- which(dat$pitcher==id & year(dat$game_date)==yr)
  }
  player.df <- dat[idx,] %>% dplyr::select(woba_value, woba_denom, bsxwOBA, rfxwOBA)
  rfx.df <- rfx[idx,] %>% 
    colSums() %>%
    as.data.frame() 
  colnames(rfx.df) <- "rfxwOBA"
  PA <- nrow(player.df)
  rfx.df %>% ggplot(aes(x=rfxwOBA/sum(player.df$woba_denom))) + 
    geom_density() + 
    geom_vline(aes(xintercept=sum(player.df$woba_value)/sum(player.df$woba_denom), color="Actual wOBA")) +
    geom_vline(aes(xintercept=sum(player.df$rfxwOBA)/sum(player.df$woba_denom), color="Mean rfxwOBA")) +
    geom_vline(aes(xintercept=sum(player.df$bsxwOBA, na.rm=T)/sum(player.df$woba_denom), color="bsxwOBA")) +
    labs(title=paste0(player.name, " rfxwOBA Disribution - ", yr, " (PA = ", PA, ")"),
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
    dplyr::select(woba_value, woba_denom, bsxwOBA, rfxwOBA, game_date) %>%
    arrange(game_date)
  player.df %>% ggplot(aes(x=game_date)) +
    geom_line(aes(y=cumsum(woba_value)/cumsum(woba_denom), color="Actual wOBA")) +
    geom_line(aes(y=cumsum(rfxwOBA)/cumsum(woba_denom), color="Mean rfxwOBA")) +
    geom_line(aes(y=cumsum(bsxwOBA)/cumsum(woba_denom), color="bsxwOBA")) +
    labs(title=paste0(player.name, " rolling wOBA vs. estimates: 2017-2019"),
         x="Date", y="Value", color="Statistic")
}

# select woba values and predictions

woba.dat <- dat %>%
  dplyr::select(woba_value, woba_denom, bsxwOBA, rfxwOBA, game_date, batter, pitcher) %>%
  mutate(Year=year(game_date))


# get season-long woba values and predictions for each player

woba.dat.batter <- woba.dat %>% 
  group_by(batter, Year) %>%
  summarize(PA=n(), 
            wOBA=sum(woba_value)/sum(woba_denom),
            bsxwOBA=sum(bsxwOBA)/sum(woba_denom),
            rfxwOBA=sum(rfxwOBA)/sum(woba_denom)) %>%
  gather(key="measure", value="value", -(batter:Year)) %>%
  unite(temp, measure, Year) %>%
  spread(temp, value)

attach(woba.dat.batter)

cor.dat <- data.frame(
  batter=rep(batter, 2),
  PA=c(woba.dat.batter$PA_2017, woba.dat.batter$PA_2018),
  PA.next=c(PA_2018, PA_2019),
  wOBA=c(wOBA_2017, wOBA_2018),
  wOBA.next=c(wOBA_2018, wOBA_2019),
  rfxwOBA=c(rfxwOBA_2017, rfxwOBA_2018),
  rfxwOBA.next=c(rfxwOBA_2018, rfxwOBA_2019),
  bsxwOBA=c(bsxwOBA_2017, bsxwOBA_2018),
  bsxwOBA.next=c(bsxwOBA_2018, bsxwOBA_2019)
)

detach(woba.dat.batter)

# correlate woba expectations to next year real values

cors <- cor.dat %>% 
  filter(PA >= 100 & PA.next >= 100) %>%
  summarise(wOBA2wOBA.next=cor(wOBA, wOBA.next),
            rfx2wOBA.next=cor(rfxwOBA, wOBA.next),
            bsx2wOBA.next=cor(bsxwOBA, wOBA.next),
            rfx2rfx.next=cor(rfxwOBA, rfxwOBA.next),
            bsx2bsx.next=cor(bsxwOBA, bsxwOBA.next))


N <- cor.dat %>%
  filter(PA >= 100 & PA.next >= 100) %>%
  nrow()

CI.1 <- CIr(cors$wOBA2wOBA.next, N)
CI.2 <- CIr(cors$rfx2wOBA.next, N)
CI.3 <- CIr(cors$bsx2wOBA.next, N)
CI.4 <- CIr(cors$rfx2rfx.next, N)
CI.5 <- CIr(cors$bsx2bsx.next, N)

cor.dat <- cor.dat %>%
  left_join(id.dat %>% rename(batter=mlb_id))

tbl1.full <- cor.dat %>% filter(PA >= 100 & PA.next >= 100) %>%
  dplyr::select(mlb_name, PA, PA.next, 
         wOBA, wOBA.next, 
         rfxwOBA, rfxwOBA.next,
         bsxwOBA, bsxwOBA.next) %>%
  top_n(10) %>%
  kable(col.names=c("Batter Name",
                    "PA", "PA Next Season",
                    "wOBA", "wOBA Next Season",
                    "rfxwOBA", "rfxwOBA Next Season",
                    "bsxwOBA", "bsxwOBA Next Season"),
        format="html", digits=3) %>%
  kable_styling("striped") 

tbl2.full <- data.frame(
  Metric=c("wOBA", "rfxwOBA", "bsxwOBA"),
  cor=round(c(cors$wOBA2wOBA.next, cors$rfx2wOBA.next, cors$bsx2wOBA.next), 3),
  CI=c(paste0("[", round(CI.1[1], 3), ", ", round(CI.1[2], 3), "]"),
       paste0("[", round(CI.2[1], 3), ", ", round(CI.2[2], 3), "]"),
       paste0("[", round(CI.3[1], 3), ", ", round(CI.3[2], 3), "]"))
) %>%
  kable(col.names=c("Metric", "Correlation to Next Season wOBA", "95% CI"),
        format="html") %>%
  kable_styling("striped")

tbl3.full <- data.frame(
  Metric=c("wOBA", "rfxwOBA", "bsxwOBA"),
  cor=round(c(cors$wOBA2wOBA.next, cors$rfx2rfx.next, cors$bsx2bsx.next), 3),
  CI=c(paste0("[", round(CI.1[1], 3), ", ", round(CI.1[2], 3), "]"),
       paste0("[", round(CI.4[1], 3), ", ", round(CI.4[2], 3), "]"),
       paste0("[", round(CI.5[1], 3), ", ", round(CI.5[2], 3), "]"))
) %>%
  kable(col.names=c("Metric", "Correlation to Itself Next Season", "95% CI"),
        format="html") %>%
  kable_styling("striped")

png("./figs/Mike_Trout_2019_full.png")
get.rfxwOBA.dist(dat, "Mike Trout", id.dat, 2019, "batter", rfx)
dev.off()

png("./figs/Mike_Trout_rolling_full.png")
rolling.rfxwOBA(dat, "Mike Trout", id.dat, "batter")
dev.off()

png("./figs/Chris_Davis_2019_full.png")
get.rfxwOBA.dist(dat, "Chris Davis", id.dat, 2019, "batter", rfx)
dev.off()

png("./figs/Chris_Davis_rolling_full.png")
rolling.rfxwOBA(dat, "Chris Davis", id.dat, "batter")
dev.off()

png("./figs/rfxwOBA_vs_next_wOBA_full.png")
cor.dat %>% filter(PA >= 100 & PA.next >= 100) %>%
  ggplot(aes(x=rfxwOBA, y=wOBA.next)) +
  geom_point() +
  labs(title="rfxwOBA vs. next season wOBA",
       x="rfxwOBA", y="next season wOBA")
dev.off()

png("./figs/wOBA_vs_next_wOBA_full.png")
cor.dat %>% filter(PA >= 100 & PA.next >= 100) %>%
  ggplot(aes(x=wOBA, y=wOBA.next)) +
  geom_point() +
  labs(title="wOBA vs. next season wOBA",
       x="rfxwOBA", y="next season wOBA")
dev.off()

png("./figs/Marcell_Ozuna_2019_full.png")
get.rfxwOBA.dist(dat, "Marcell Ozuna", id.dat, 2019, "batter", rfx)
dev.off()

#### using separate models holding out one year at a time from training set

rfx.2019 <- readRDS('../data/projection/rfxwOBA_2019.rds')
rfx.2018 <- readRDS('../data/projection/rfxwOBA_2018.rds')
rfx.2017 <- readRDS('../data/projection/rfxwOBA_2017.rds')

rfx <- rbind(rfx.2019, rfx.2018, rfx.2017)

rm(rfx.2019, rfx.2018, rfx.2017)

dat$rfxwOBA <- rowMeans(rfx)


# select woba values and predictions

woba.dat <- dat %>%
  dplyr::select(woba_value, woba_denom, bsxwOBA, rfxwOBA, game_date, batter, pitcher) %>%
  mutate(Year=year(game_date))


# get season-long woba values and predictions for each player

woba.dat.batter <- woba.dat %>% 
  group_by(batter, Year) %>%
  summarize(PA=n(), 
            wOBA=sum(woba_value)/sum(woba_denom),
            bsxwOBA=sum(bsxwOBA)/sum(woba_denom),
            rfxwOBA=sum(rfxwOBA)/sum(woba_denom)) %>%
  gather(key="measure", value="value", -(batter:Year)) %>%
  unite(temp, measure, Year) %>%
  spread(temp, value)

attach(woba.dat.batter)

cor.dat <- data.frame(
  batter=rep(batter, 2),
  PA=c(woba.dat.batter$PA_2017, woba.dat.batter$PA_2018),
  PA.next=c(PA_2018, PA_2019),
  wOBA=c(wOBA_2017, wOBA_2018),
  wOBA.next=c(wOBA_2018, wOBA_2019),
  rfxwOBA=c(rfxwOBA_2017, rfxwOBA_2018),
  rfxwOBA.next=c(rfxwOBA_2018, rfxwOBA_2019),
  bsxwOBA=c(bsxwOBA_2017, bsxwOBA_2018),
  bsxwOBA.next=c(bsxwOBA_2018, bsxwOBA_2019)
)

detach(woba.dat.batter)

# correlate woba expectations to next year real values

cors <- cor.dat %>% 
  filter(PA >= 100 & PA.next >= 100) %>%
  summarise(wOBA2wOBA.next=cor(wOBA, wOBA.next),
            rfx2wOBA.next=cor(rfxwOBA, wOBA.next),
            bsx2wOBA.next=cor(bsxwOBA, wOBA.next),
            rfx2rfx.next=cor(rfxwOBA, rfxwOBA.next),
            bsx2bsx.next=cor(bsxwOBA, bsxwOBA.next))

N <- cor.dat %>%
  filter(PA >= 100 & PA.next >= 100) %>%
  nrow()

CI.1 <- CIr(cors$wOBA2wOBA.next, N)
CI.2 <- CIr(cors$rfx2wOBA.next, N)
CI.3 <- CIr(cors$bsx2wOBA.next, N)
CI.4 <- CIr(cors$rfx2rfx.next, N)
CI.5 <- CIr(cors$bsx2bsx.next, N)

cor.dat <- cor.dat %>%
  left_join(id.dat %>% rename(batter=mlb_id))

tbl1.hold <- cor.dat %>% filter(PA >= 100 & PA.next >= 100) %>%
  dplyr::select(mlb_name, PA, PA.next, 
                wOBA, wOBA.next, 
                rfxwOBA, rfxwOBA.next,
                bsxwOBA, bsxwOBA.next) %>%
  top_n(10) %>%
  kable(col.names=c("Batter Name",
                    "PA", "PA Next Season",
                    "wOBA", "wOBA Next Season",
                    "rfxwOBA", "rfxwOBA Next Season",
                    "bsxwOBA", "bsxwOBA Next Season"),
        format="html", digits=3) %>%
  kable_styling("striped") 

tbl2.hold <- data.frame(
  Metric=c("wOBA", "rfxwOBA", "bsxwOBA"),
  cor=round(c(cors$wOBA2wOBA.next, cors$rfx2wOBA.next, cors$bsx2wOBA.next), 3),
  CI=c(paste0("[", round(CI.1[1], 3), ", ", round(CI.1[2], 3), "]"),
       paste0("[", round(CI.2[1], 3), ", ", round(CI.2[2], 3), "]"),
       paste0("[", round(CI.3[1], 3), ", ", round(CI.3[2], 3), "]"))
) %>%
  kable(col.names=c("Metric", "Correlation to Next Season wOBA", "95% CI"),
        format="html") %>%
  kable_styling("striped")

tbl3.hold <- data.frame(
  Metric=c("wOBA", "rfxwOBA", "bsxwOBA"),
  cor=round(c(cors$wOBA2wOBA.next, cors$rfx2rfx.next, cors$bsx2bsx.next), 3),
  CI=c(paste0("[", round(CI.1[1], 3), ", ", round(CI.1[2], 3), "]"),
       paste0("[", round(CI.4[1], 3), ", ", round(CI.4[2], 3), "]"),
       paste0("[", round(CI.5[1], 3), ", ", round(CI.5[2], 3), "]"))
) %>%
  kable(col.names=c("Metric", "Correlation to Itself Next Season", "95% CI"),
        format="html") %>%
  kable_styling("striped")

sink("./figs/tbl1_full.txt")
tbl1.full
sink()

sink("./figs/tbl2_full.txt")
tbl2.full
sink()

sink("./figs/tbl3_full.txt")
tbl3.full
sink()

sink("./figs/tbl1_hold.txt")
tbl1.hold
sink()

sink("./figs/tbl2_hold.txt")
tbl2.hold
sink()

sink("./figs/tbl3_hold.txt")
tbl3.hold
sink()

png("./figs/Mike_Trout_2019_hold.png")
get.rfxwOBA.dist(dat, "Mike Trout", id.dat, 2019, "batter", rfx)
dev.off()

png("./figs/Mike_Trout_rolling_hold.png")
rolling.rfxwOBA(dat, "Mike Trout", id.dat, "batter")
dev.off()

png("./figs/Chris_Davis_2019_hold.png")
get.rfxwOBA.dist(dat, "Chris Davis", id.dat, 2019, "batter", rfx)
dev.off()

png("./figs/Chris_Davis_rolling_hold.png")
rolling.rfxwOBA(dat, "Chris Davis", id.dat, "batter")
dev.off()

png("./figs/rfxwOBA_vs_next_wOBA_hold.png")
cor.dat %>% filter(PA >= 100 & PA.next >= 100) %>%
  ggplot(aes(x=rfxwOBA, y=wOBA.next)) +
  geom_point() +
  labs(title="rfxwOBA vs. next season wOBA",
       x="rfxwOBA", y="next season wOBA")
dev.off()

png("./figs/wOBA_vs_next_wOBA_hold.png")
cor.dat %>% filter(PA >= 100 & PA.next >= 100) %>%
  ggplot(aes(x=wOBA, y=wOBA.next)) +
  geom_point() +
  labs(title="wOBA vs. next season wOBA",
       x="rfxwOBA", y="next season wOBA")
dev.off()

png("./figs/Marcell_Ozuna_2019_hold.png")
get.rfxwOBA.dist(dat, "Marcell Ozuna", id.dat, 2019, "batter", rfx)
dev.off()



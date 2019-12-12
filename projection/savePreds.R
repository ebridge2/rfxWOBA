library(tidyverse)
library(readr)
library(gridExtra)
library(rerf)
library(data.table)
library(mltools)
library(kableExtra)
library(parallel)


fit <- readRDS("../data/rf/fit_model.rds")
dat <- read_csv("../data/cleaned/model_data.csv")

predictions <- matrix(rep(NA*5), ncol=5)

nCores <- detectCores() - 1

# select numeric features
X.num <- dat %>%
  select(-outcome, -pitch_name, -stadium, -if_fielding_alignment, -of_fielding_alignment) %>%
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

# obtain the outcome variable
Y <- dat %>%
  pull(outcome)

idx <- complete.cases(X)

preds <- Predict(X[idx,], fit, num.cores=nCores, aggregate.output=FALSE)
p.out <- rowSums(preds=="Out")/ncol(preds)
p.single <- rowSums(preds=="Single")/ncol(preds)
p.double <- rowSums(preds=="Double")/ncol(preds)
p.triple <- rowSums(preds=="Triple")/ncol(preds)
p.hr <- rowSums(preds=="Home run")/ncol(preds)
p <- cbind(p.out, p.single, p.double, p.triple, p.hr)

predictions[idx,] <- p

saveRDS(predictions, "../data/projection/predictions.rds")


#### Code to get 100 rfxwOBA samples in serial

library(readr)
library(tidyverse)
library(rerf)
library(parallel)
library(data.table)
library(mltools)

select <- dplyr::select

dat <- read_csv("../data/cleaned/model_data.csv")
fit <- readRDS("../data/rf/fit_model.rds")


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

# obtain the outcome variable
Y <- dat %>%
  pull(outcome)



randImpute <- function(X) {
  X.cc <- X[complete.cases(X),]
  for (col in colnames(X)) {
    n <- sum(is.na(X[[col]]))
    imp <- sample(X.cc[[col]], size=n)
    X[[col]][is.na(X[[col]])] <- imp
  }
  return(X)
}

rfxwOBA.marginal <- function(X, Y, const, nSim) {
  res <- matrix(rep(NA, nrow(X)*nSim), ncol=nSim)
  wobacon <- c(0, 0.9, 1.25, 1.6, 2)
  nCores <- detectCores() - 1
  idxBB <- Y=="Walk" | Y=="Hit by pitch"
  idxK <- Y=="Strikeout" | Y=="Sac bunt"
  res[idxBB,] <- 0.7
  res[idxK,] <- 0
  idxBIP <- !idxBB&!idxK
  X.bip <- randImpute(X[idxBIP,])
  for (i in 1:nSim) {
    print(i)
    idx <- sample(nrow(X.bip))
    X.new <- X.bip[idx,]
    for (val in const) {
      X.new[[val]] <- X.bip[[val]]
    }
    preds <- Predict(X.new, fit, num.cores=nCores, aggregate.output=FALSE)
    p.out <- rowSums(preds=="Out")/ncol(preds)
    p.single <- rowSums(preds=="Single")/ncol(preds)
    p.double <- rowSums(preds=="Double")/ncol(preds)
    p.triple <- rowSums(preds=="Triple")/ncol(preds)
    p.hr <- rowSums(preds=="Home run")/ncol(preds)
    p <- cbind(p.out, p.single, p.double, p.triple, p.hr)
    res[idxBIP,i] <- t(wobacon%*%t(p))
  }
  return(res)
}

set.seed(12345)

const <- c("launch_angle", "launch_speed", "sprint_speed", "spray_angle", "batterRighty")
nSim <- 100

rfxwOBA <- rfxwOBA.marginal(X, Y, const, nSim)
saveRDS(rfxwOBA, file="../data/projection/rfxwOBA.rds")




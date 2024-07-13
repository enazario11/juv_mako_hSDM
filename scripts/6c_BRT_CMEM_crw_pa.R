### libraries ####
library(tidyverse)
library(gbm)
library(dismo)
library(here);here <- here::here
library(ggBRT)

set.seed(1004)

### load data ####
#### CRW daily data ####
dat_base_d <- readRDS(here("data/locs_brts/crw_pas/dat_base.rds")) %>% mutate(tag = as.factor(tag))
dat_do_d <- readRDS(here("data/locs_brts/crw_pas/dat_do.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_d <- readRDS(here("data/locs_brts/crw_pas/dat_agi.rds")) %>% mutate(tag = as.factor(tag))

#### CRW seasonal data ####
dat_base_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_base_seas.rds")) %>% mutate(tag = as.factor(tag))
dat_do_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_do_seas.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_agi_seas.rds")) %>% mutate(tag = as.factor(tag))

#### CRW annual data ####
dat_base_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_base_ann.rds")) %>% mutate(tag = as.factor(tag))
dat_do_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_do_ann.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_agi_ann.rds")) %>% mutate(tag = as.factor(tag))

#### Add random variable for predictor selection ####
pred_var <- rnorm(31084, mean = 50, sd = 10)

dat_base_d$pred_var <- pred_var
dat_do_d$pred_var <- pred_var
dat_agi_d$pred_var <- pred_var

dat_base_a$pred_var <- pred_var
dat_base_s$pred_var <- pred_var

#### Add seasonal and annual data to daily data df ####
dat_do <- cbind(dat_do_d, dat_do_s$o2_mean_0m, dat_do_s$o2_mean_60m, dat_do_s$o2_mean_250m, dat_do_a$o2_mean_0m, dat_do_a$o2_mean_60m, dat_do_a$o2_mean_250m)
dat_do <- dat_do %>%
  rename("o2_mean_0m_seas" = "dat_do_s$o2_mean_0m", 
         "o2_mean_60m_seas" = "dat_do_s$o2_mean_60m", 
         "o2_mean_250m_seas" = "dat_do_s$o2_mean_250m", 
         "o2_mean_0m_ann" = "dat_do_a$o2_mean_0m", 
         "o2_mean_60m_ann" = "dat_do_a$o2_mean_60m", 
        "o2_mean_250m_ann" = "dat_do_a$o2_mean_250m")

dat_agi <- cbind(dat_agi_d, dat_agi_s$AGI_0m, dat_agi_s$AGI_60m, dat_agi_s$AGI_250m, dat_agi_a$AGI_0m, dat_agi_a$AGI_60m, dat_agi_a$AGI_250m)
dat_agi <- dat_agi %>%
  rename("AGI_0m_seas" = "dat_agi_s$AGI_0m",
         "AGI_60m_seas" = "dat_agi_s$AGI_60m", 
         "AGI_250m_seas" = "dat_agi_s$AGI_250m", 
         "AGI_0m_ann" = "dat_agi_a$AGI_0m", 
         "AGI_60m_ann" = "dat_agi_a$AGI_60m", 
         "AGI_250m_ann" = "dat_agi_a$AGI_250m")

#### Split into test and train ####
#base
dat_train_base_d <- dat_base_d %>% sample_frac(0.75) #daily
dat_test_base_d <- dat_base_d %>% sample_frac(0.25)

dat_train_base_s <- dat_base_s %>% sample_frac(0.75) #seasonal
dat_test_base_s <- dat_base_s %>% sample_frac(0.25)

dat_train_base_a <- dat_base_a %>% sample_frac(0.75) #annual
dat_test_base_a <- dat_base_a %>% sample_frac(0.25)

#do
dat_train_do <- dat_do %>% sample_frac(0.75)
dat_test_do <- dat_do %>% sample_frac(0.25)

#agi
dat_train_agi <- dat_agi %>% sample_frac(0.75)
dat_test_agi <- dat_agi %>% sample_frac(0.25)

### Run BRT ####
#### base ####
#base w/o spatial predictors, w/ tag id predictor and covars only at the surface (no DO or AGI)
try(brt_base_0m_Nspat_Ytag <- dismo::gbm.step(
                              data = dat_train_base, 
                              gbm.x = c(1, 8:18, 20), 
                              gbm.y = 5,
                              family = "bernoulli", 
                              tree.complexity = 3,
                              learning.rate = 0.05, 
                              bag.fraction = 0.75, 
                              silent = TRUE, 
                              plot.main = TRUE
                              )
    )
saveRDS(brt_base_0m_Nspat_Ytag, here("data/brt/mod_outputs/brt_base_0m_Nspat_Ytag.rds"))

#base w/o spatial predictors, w/o tag id predictor and covars only at the surface (no DO or AGI)
try(brt_base_0m_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_base, 
  gbm.x = c(8:18, 20), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_base_0m_Nspat_Ntag, here("data/brt/mod_outputs/brt_base_0m_Nspat_Ntag.rds"))

#base w/ spatial predictors, w/ tag id predictor and covars only at the surface (no DO or AGI)
try(brt_base_0m_Yspat_Ytag <- dismo::gbm.step(
  data = dat_train_base, 
  gbm.x = c(1, 4, 8:19, 20), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_base_0m_Yspat_Ytag, here("data/brt/mod_outputs/brt_base_0m_Yspat_Ytag.rds"))

#### do ####
#do w/ spatial predictors, w/ tag id predictors, and DO covar at the surface
try(brt_do_0m_Yspat_Ytag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(1, 4, 8:20, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_Yspat_Ytag, here("data/brt/mod_outputs/brt_do_0m_Yspat_Ytag.rds"))

#do w/o spatial predictors, w/ tag id predictors, and DO covar at the surface
try(brt_do_0m_Nspat_Ytag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(1, 8:19, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_Nspat_Ytag, here("data/brt/mod_outputs/brt_do_0m_Nspat_Ytag.rds"))

#do w/o spatial predictors, w/ tag id predictors, and DO covar at the surface and at 60m 
try(brt_do_0m_60m_Nspat_Ytag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(1, 8:19, 21, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_Nspat_Ytag, here("data/brt/mod_outputs/brt_do_0m_60m_Nspat_Ytag.rds"))

#do w/o spatial predictors, w/ tag id predictors, and DO covar at the surface and at 250m 
try(brt_do_0m_250m_Nspat_Ytag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(1, 8:19, 22, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_250m_Nspat_Ytag, here("data/brt/mod_outputs/brt_do_0m_250m_Nspat_Ytag.rds"))

#do w/o spatial predictors, w/ tag id predictors, and DO covar at the surface and at 60m and 250m 
try(brt_do_0m_60m_250m_Nspat_Ytag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(1, 8:19, 21, 22, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_Nspat_Ytag, here("data/brt/mod_outputs/brt_do_0m_60m_250m_Nspat_Ytag.rds"))

#do w/ spatial predictors, w/ tag id predictors, and DO covar at the surface and at 60m and 250m
try(brt_do_0m_60m_250m_Yspat_Ytag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(1, 4, 8:22, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_Yspat_Ytag, here("data/brt/mod_outputs/brt_do_0m_60m_250m_Yspat_Ytag.rds"))

#### agi ####
#agi w/ spatial predictors, w/ tag id predictors, and agi covar at the surface
try(brt_agi_0m_Yspat_Ytag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(1, 4, 8:20, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_Yspat_Ytag, here("data/brt/mod_outputs/brt_agi_0m_Yspat_Ytag.rds"))

#agi w/o spatial predictors, w/ tag id predictors, and agi covar at the surface
try(brt_agi_0m_Nspat_Ytag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(1, 8:19, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_Nspat_Ytag, here("data/brt/mod_outputs/brt_agi_0m_Nspat_Ytag.rds"))

#agi w/o spatial predictors, w/ tag id predictors, and agi covar at the surface and at 60m 
try(brt_agi_0m_60m_Nspat_Ytag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(1, 8:19, 21, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_Nspat_Ytag, here("data/brt/mod_outputs/brt_agi_0m_60m_Nspat_Ytag.rds"))

#agi w/o spatial predictors, w/ tag id predictors, and agi covar at the surface and at 250m 
try(brt_agi_0m_250m_Nspat_Ytag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(1, 8:19, 22, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_250m_Nspat_Ytag, here("data/brt/mod_outputs/brt_agi_0m_250m_Nspat_Ytag.rds"))

#agi w/o spatial predictors, w/ tag id predictors, and agi covar at the surface and at 60m and 250m 
try(brt_agi_0m_60m_250m_Nspat_Ytag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(1, 8:19, 21, 22, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_Nspat_Ytag, here("data/brt/mod_outputs/brt_agi_0m_60m_250m_Nspat_Ytag.rds"))

#agi w/ spatial predictors, w/ tag id predictors, and agi covar at the surface and at 60m and 250m
try(brt_agi_0m_60m_250m_Yspat_Ytag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(1, 4, 8:22, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_Yspat_Ytag, here("data/brt/mod_outputs/brt_agi_0m_60m_250m_Yspat_Ytag.rds"))

### Run BRT w/o tag ID ####
#### do ####
#do w/ spatial predictors, and DO covar at the surface
try(brt_do_0m_Yspat_Ntag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(4, 8:20, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_Yspat_Ntag, here("data/brt/mod_outputs/crw/no_tag/brt_do_0m_Yspat_Ntag.rds"))

#do w/o spatial predictors and DO covar at the surface
try(brt_do_0m_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(8:19, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_Nspat_Ntag, here("data/brt/mod_outputs/crw/no_tag/brt_do_0m_Nspat_Ntag.rds"))

#do w/o spatial predictors and DO covar at the surface and at 60m 
try(brt_do_0m_60m_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(8:19, 21, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_Nspat_Ntag, here("data/brt/mod_outputs/crw/no_tag/brt_do_0m_60m_Nspat_Ntag.rds"))

#do w/o spatial predictors and DO covar at the surface and at 250m 
try(brt_do_0m_250m_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(8:19, 22, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_250m_Nspat_Ntag, here("data/brt/mod_outputs/crw/no_tag/brt_do_0m_250m_Nspat_Ntag.rds"))

#do w/o spatial predictors and DO covar at the surface and at 60m and 250m 
try(brt_do_0m_60m_250m_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(8:19, 21, 22, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_Nspat_Ntag, here("data/brt/mod_outputs/crw/no_tag/brt_do_0m_60m_250m_Nspat_Ntag.rds"))

#do w/ spatial predictors and DO covar at the surface and at 60m and 250m
try(brt_do_0m_60m_250m_Yspat_Ntag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(4, 8:22, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_Yspat_Ntag, here("data/brt/mod_outputs/crw/no_tag/brt_do_0m_60m_250m_Yspat_Ntag.rds"))

#### agi ####
#agi w/ spatial predictors and agi covar at the surface
try(brt_agi_0m_Yspat_Ntag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(4, 8:20, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_Yspat_Ntag, here("data/brt/mod_outputs/crw/no_tag/brt_agi_0m_Yspat_Ntag.rds"))

#agi w/o spatial predictors and agi covar at the surface
try(brt_agi_0m_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(8:19, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_Nspat_Ntag, here("data/brt/mod_outputs/crw/no_tag/brt_agi_0m_Nspat_Ntag.rds"))

#agi w/o spatial predictors and agi covar at the surface and at 60m 
try(brt_agi_0m_60m_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(8:19, 21, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_Nspat_Ntag, here("data/brt/mod_outputs/crw/no_tag/brt_agi_0m_60m_Nspat_Ntag.rds"))

#agi w/o spatial predictors and agi covar at the surface and at 250m 
try(brt_agi_0m_250m_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(8:19, 22, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_250m_Nspat_Ntag, here("data/brt/mod_outputs/crw/no_tag/brt_agi_0m_250m_Nspat_Ntag.rds"))

#agi w/o spatial predictors and agi covar at the surface and at 60m and 250m 
try(brt_agi_0m_60m_250m_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(8:19, 21, 22, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_Nspat_Ntag, here("data/brt/mod_outputs/crw/no_tag/brt_agi_0m_60m_250m_Nspat_Ntag.rds"))

#agi w/ spatial predictors and agi covar at the surface and at 60m and 250m
try(brt_agi_0m_60m_250m_Yspat_Ntag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(4, 8:22, 23), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_Yspat_Ntag, here("data/brt/mod_outputs/crw/no_tag/brt_agi_0m_60m_250m_Yspat_Ntag.rds"))

# Base seasonal and annual models ####
try(brt_base_0m_seas_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_base_s, 
  gbm.x = c(9:19, 21), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.01, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_base_0m_seas_Nspat_Ntag, here("data/brt/mod_outputs/crw/seasonal/brt_base_0m_seas_Nspat_Ntag.rds"))

try(brt_base_0m_ann_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_base_a, 
  gbm.x = c(9:19, 21), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_base_0m_ann_Nspat_Ntag, here("data/brt/mod_outputs/crw/annual/brt_base_0m_ann_Nspat_Ntag.rds"))

# DO seasonal and annual ####
#### seasonal only all depths ####
#do w/o spatial predictors and DO covar at the surface and at 60m and 250m 
try(brt_do_0m_60m_250m_seas_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(9:19, 23:26), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.01, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_seas_Nspat_Ntag, here("data/brt/mod_outputs/crw/seasonal/brt_do_0m_60m_250m_seas_Nspat_Ntag.rds"))

#do w/ spatial predictors and DO covar at the surface and at 60m and 250m
try(brt_do_0m_60m_250m_seas_Yspat_Ntag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(4, 9:20, 23:26), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.01, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_seas_Yspat_Ntag, here("data/brt/mod_outputs/crw/seasonal/brt_do_0m_60m_250m_seas_Yspat_Ntag.rds"))

#### annual only all depths ####
#do w/o spatial predictors and DO covar at the surface and at 60m and 250m 
try(brt_do_0m_60m_250m_ann_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(9:19, 23, 27:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_ann_Nspat_Ntag, here("data/brt/mod_outputs/crw/annual/brt_do_0m_60m_250m_ann_Nspat_Ntag.rds"))

#do w/ spatial predictors and DO covar at the surface and at 60m and 250m
try(brt_do_0m_60m_250m_ann_Yspat_Ntag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(4, 9:20, 23, 27:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_ann_Yspat_Ntag, here("data/brt/mod_outputs/crw/annual/brt_do_0m_60m_250m_ann_Yspat_Ntag.rds"))

#### all temp res all depths ####
#do w/o spatial predictors and DO covar at the surface and at 60m and 250m 
try(brt_do_0m_60m_250m_dail_seas_ann_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(8:19, 21:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_dail_seas_ann_Nspat_Ntag, here("data/brt/mod_outputs/crw/annual/brt_do_0m_60m_250m_dail_seas_ann_Nspat_Ntag.rds"))

#do w/ spatial predictors and DO covar at the surface and at 60m and 250m
try(brt_do_0m_60m_250m_dail_seas_ann_Yspat_Ntag <- dismo::gbm.step(
  data = dat_train_do, 
  gbm.x = c(4, 8:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_dail_seas_ann_Yspat_Ntag, here("data/brt/mod_outputs/crw/annual/brt_do_0m_60m_250m_dail_seas_ann_Yspat_Ntag.rds"))

# AGI seasonal and annual ####
#### seasonal only all depths ####
#AGI w/o spatial predictors and AGI covar at the surface and at 60m and 250m 
try(brt_agi_0m_60m_250m_seas_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(8:18, 23:26), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.01, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_seas_Nspat_Ntag, here("data/brt/mod_outputs/crw/seasonal/brt_agi_0m_60m_250m_seas_Nspat_Ntag.rds"))

#agi w/ spatial predictors and agi covar at the surface and at 60m and 250m
try(brt_agi_0m_60m_250m_seas_Yspat_Ntag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(4, 8:18, 20, 23:26), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.01, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_seas_Yspat_Ntag, here("data/brt/mod_outputs/crw/seasonal/brt_agi_0m_60m_250m_seas_Yspat_Ntag.rds"))

#### annual only all depths ####
#agi w/o spatial predictors and agi covar at the surface and at 60m and 250m 
try(brt_agi_0m_60m_250m_ann_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(8:18, 23, 27:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_ann_Nspat_Ntag, here("data/brt/mod_outputs/crw/annual/brt_agi_0m_60m_250m_ann_Nspat_Ntag.rds"))

#agi w/ spatial predictors and agi covar at the surface and at 60m and 250m
try(brt_agi_0m_60m_250m_ann_Yspat_Ntag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(4, 8:18, 20, 23, 27:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_ann_Yspat_Ntag, here("data/brt/mod_outputs/crw/annual/brt_agi_0m_60m_250m_ann_Yspat_Ntag.rds"))

#### all temp res all depths ####
#agi w/o spatial predictors and agi covar at the surface and at 60m and 250m 
try(brt_agi_0m_60m_250m_dail_seas_ann_Nspat_Ntag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(8:19, 21:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_dail_seas_ann_Nspat_Ntag, here("data/brt/mod_outputs/crw/annual/brt_agi_0m_60m_250m_dail_seas_ann_Nspat_Ntag.rds"))

#agi w/ spatial predictors and agi covar at the surface and at 60m and 250m
try(brt_agi_0m_60m_250m_dail_seas_ann_Yspat_Ntag <- dismo::gbm.step(
  data = dat_train_agi, 
  gbm.x = c(4, 8:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_dail_seas_ann_Yspat_Ntag, here("data/brt/mod_outputs/crw/annual/brt_agi_0m_60m_250m_dail_seas_ann_Yspat_Ntag.rds"))

### explore outputs -- will more thoroughly do in quarto doc ####
ggBRT::ggPerformance(test)

gbm.plot(test, nplots = 13, plot.layout = c(3,4), write.title = FALSE)

#relative influence of predictors
ggBRT::ggInfluence(test)

#find the 5 most important pairwise interactions
AGI_int <- gbm.interactions(test)
AGI_int$rank.list

#plot some interactions of interest
gbm.perspec(test, 12, 1)
gbm.perspec(test, 12, 2)

#predictions
preds <- predict.gbm(test, dat_test0,
                     n.trees = test$gbm.call$best.trees,
                     type = "response")

calc.deviance(obs = dat_test0$PA, preds) #get % deviance

dat_pred0 <- cbind(dat_test0$PA, preds)
pres0 <- dat_pred0[dat_pred0[,1] == 1, 2]
abs0 <- dat_pred0[dat_pred0[,1] == 0, 2]

#evaluate (AUC, TSS, TPR)
e = evaluate(p = pres0, a = abs0)
plot(e, 'TPR')
plot(e, 'TNR')
plot(e, 'ROC')
boxplot(e)
density(e)
mean(e@TPR) #TPR
max(e@TPR + e@TNR -1) #TSS

dev_eval_brt=function(model_object){ #provides % deviance for model selection -- refer to PA paper for other model eval functoins for dismo pacakge model object
  null <- model_object$self.statistics$null
  res <- model_object$self.statistics$resid
  dev=((null - res)/null)*100
  return(dev)
}










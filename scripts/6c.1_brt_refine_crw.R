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
dat_do_all <- cbind(dat_do_d, dat_do_s$o2_mean_0m, dat_do_s$o2_mean_60m, dat_do_s$o2_mean_250m, dat_do_a$o2_mean_0m, dat_do_a$o2_mean_60m, dat_do_a$o2_mean_250m)
dat_do_all <- dat_do_all %>%
  rename("o2_mean_0m_seas" = "dat_do_s$o2_mean_0m", 
         "o2_mean_60m_seas" = "dat_do_s$o2_mean_60m", 
         "o2_mean_250m_seas" = "dat_do_s$o2_mean_250m", 
         "o2_mean_0m_ann" = "dat_do_a$o2_mean_0m", 
         "o2_mean_60m_ann" = "dat_do_a$o2_mean_60m", 
         "o2_mean_250m_ann" = "dat_do_a$o2_mean_250m")

dat_agi_all <- cbind(dat_agi_d, dat_agi_s$AGI_0m, dat_agi_s$AGI_60m, dat_agi_s$AGI_250m, dat_agi_a$AGI_0m, dat_agi_a$AGI_60m, dat_agi_a$AGI_250m)
dat_agi_all <- dat_agi_all %>%
  rename("AGI_0m_seas" = "dat_agi_s$AGI_0m",
         "AGI_60m_seas" = "dat_agi_s$AGI_60m", 
         "AGI_250m_seas" = "dat_agi_s$AGI_250m", 
         "AGI_0m_ann" = "dat_agi_a$AGI_0m", 
         "AGI_60m_ann" = "dat_agi_a$AGI_60m", 
         "AGI_250m_ann" = "dat_agi_a$AGI_250m")

#### Split into test and train daily data #####
#base
dat_base_temp <- floor((nrow(dat_base_d)/4)*3) #define % of training and test set
dat_train_base_d <- dat_base_d[sample(nrow(dat_base_d),dat_base_temp),]
dat_test_base_d <- dat_base_d[sample(nrow(dat_base_d),nrow(dat_base_d)-dat_base_temp),]
#saveRDS(dat_test_base_d, here("data/brt/mod_eval/base_test_daily.rds"))

#do
dat_do_temp <- floor((nrow(dat_do_d)/4)*3) #define % of training and test set
dat_train_do_d <- dat_do_d[sample(nrow(dat_do_d),dat_do_temp),]
dat_test_do_d <- dat_do_d[sample(nrow(dat_do_d),nrow(dat_do_d)-dat_do_temp),]
#saveRDS(dat_test_do_d, here("data/brt/mod_eval/do_test_daily.rds"))

#agi
dat_agi_temp <- floor((nrow(dat_agi_d)/4)*3) #define % of training and test set
dat_train_agi_d <- dat_agi_d[sample(nrow(dat_agi_d),dat_agi_temp),]
dat_test_agi_d <- dat_agi_d[sample(nrow(dat_agi_d),nrow(dat_agi_d)-dat_agi_temp),]
#saveRDS(dat_test_agi_d, here("data/brt/mod_eval/agi_test_daily.rds"))

#### Split into test and train seasonal/annual data####
dat_base_temp_s <- floor((nrow(dat_base_s)/4)*3)
dat_train_base_s <- dat_base_s[sample(nrow(dat_base_s),dat_base_temp_s),] #seasonal
dat_test_base_s <- dat_base_s[sample(nrow(dat_base_s),nrow(dat_base_s)-dat_base_temp_s),]
#saveRDS(dat_test_base_s, here("data/brt/mod_eval/base_test_seasonal.rds"))

dat_base_temp_a <- floor((nrow(dat_base_a)/4)*3)
dat_train_base_a <- dat_base_a[sample(nrow(dat_base_a),dat_base_temp_a),] #annual
dat_test_base_a <- dat_base_a[sample(nrow(dat_base_a),nrow(dat_base_a)-dat_base_temp_a),]
#saveRDS(dat_test_base_a, here("data/brt/mod_eval/base_test_annual.rds"))

#do
dat_do_temp_all <- floor((nrow(dat_do_all)/4)*3) #define % of training and test set
dat_train_do_all <- dat_do_all[sample(nrow(dat_do_all),dat_do_temp_all),]
dat_test_do_all <- dat_do_all[sample(nrow(dat_do_all),nrow(dat_do_all)-dat_do_temp_all),]
#saveRDS(dat_test_do_all, here("data/brt/mod_eval/do_test_daily_seasonal_annual.rds"))

#agi
dat_agi_temp_all <- floor((nrow(dat_agi_all)/4)*3) #define % of training and test set
dat_train_agi_all <- dat_agi_all[sample(nrow(dat_agi_all),dat_agi_temp_all),]
dat_test_agi_all <- dat_agi_all[sample(nrow(dat_agi_all),nrow(dat_agi_all)-dat_agi_temp_all),]
#saveRDS(dat_test_agi_all, here("data/brt/mod_eval/agi_test_daily_seasonal_annual.rds"))

#do and agi 
dat_train_do_agi_all <- cbind(dat_train_agi_all, dat_train_do_all$o2_mean_0m, dat_train_do_all$o2_mean_0m_seas, dat_train_do_all$o2_mean_0m_ann)
dat_train_do_agi_all <- dat_train_do_agi_all %>%
  rename("o2_mean_0m" = "dat_train_do_all$o2_mean_0m", 
         "o2_mean_0m_seas" = "dat_train_do_all$o2_mean_0m_seas",
         "o2_mean_0m_ann" = "dat_train_do_all$o2_mean_0m_ann")

dat_test_do_agi_all <- cbind(dat_test_agi_all, dat_test_do_all$o2_mean_0m, dat_test_do_all$o2_mean_0m_seas, dat_test_do_all$o2_mean_0m_ann)
dat_test_do_agi_all <- dat_test_do_agi_all %>%
  rename("o2_mean_0m" = "dat_test_do_all$o2_mean_0m", 
         "o2_mean_0m_seas" = "dat_test_do_all$o2_mean_0m_seas",
         "o2_mean_0m_ann" = "dat_test_do_all$o2_mean_0m_ann")
#saveRDS(dat_test_do_agi_all, here("data/brt/mod_eval/agi_do_test_daily_seasonal_annual.rds"))

### Remove wind vars ####
# base model w/o wind vars
try(brt_base_0m_dail_no_wind <- dismo::gbm.step(
  data = dat_train_base_d, 
  gbm.x = c(8:10, 15:18, 20), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_base_0m_dail_no_wind, here("data/brt/mod_outputs/crw/refined/brt_base_0m_dail_no_wind.rds"))

#do reference model w/o wind vars
try(brt_do_0m_60m_250m_dail_seas_ann_no_wind <- dismo::gbm.step(
  data = dat_train_do_all, 
  gbm.x = c(8:11, 16:19, 21:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_dail_seas_ann_no_wind, here("data/brt/mod_outputs/crw/refined/brt_do_0m_60m_250m_dail_seas_ann_no_wind.rds"))

#agi reference model w/o wind vars
try(brt_agi_0m_60m_250m_dail_seas_ann_no_wind <- dismo::gbm.step(
  data = dat_train_agi_all, 
  gbm.x = c(8:11, 16:19, 21:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_dail_seas_ann_no_wind, here("data/brt/mod_outputs/crw/refined/brt_agi_0m_60m_250m_dail_seas_ann_no_wind.rds"))

### Combo models of DO and AGI #### 
#agi at 250 m at all temp res and do at 0 m at all temp res
try(brt_agi_250_DO_0_dail_seas_ann <- dismo::gbm.step(
  data = dat_train_do_agi_all, 
  gbm.x = c(8:10, 15:18, 22:23, 26, 29, 30:32), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_250_DO_0_dail_seas_ann, here("data/brt/mod_outputs/crw/refined/brt_agi_250_DO_0_dail_seas_ann.rds"))

### Modified depth and temporal res. ####
#DO removed 60m from reference model w/ all info
try(brt_do_0m_250m_dail_seas_ann <- dismo::gbm.step(
  data = dat_train_do_all, 
  gbm.x = c(8:11, 16:19, 22:24, 26, 27, 29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_250m_dail_seas_ann, here("data/brt/mod_outputs/crw/refined/brt_do_0m_250m_dail_seas_ann.rds"))

#DO removed seasonal res from reference model w/ all info
try(brt_do_0m_60m_250m_dail_ann <- dismo::gbm.step(
  data = dat_train_do_all, 
  gbm.x = c(8:11, 16:19, 21:23, 27:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_dail_ann, here("data/brt/mod_outputs/crw/refined/brt_do_0m_60m_250m_dail_ann.rds"))

#DO removed daily res from reference model w/ all info
try(brt_do_0m_60m_250m_seas_ann <- dismo::gbm.step(
  data = dat_train_do_all, 
  gbm.x = c(9:11, 16:19, 23, 24:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_seas_ann, here("data/brt/mod_outputs/crw/refined/brt_do_0m_60m_250m_seas_ann.rds"))

#DO remove seasonal and 60m from reference model w/ all other info
try(brt_do_0m_250m_daily_ann <- dismo::gbm.step(
  data = dat_train_do_all, 
  gbm.x = c(8:11, 16:19, 22, 23, 27, 29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_250m_daily_ann, here("data/brt/mod_outputs/crw/refined/brt_do_0m_250m_daily_ann.rds"))

#DO remove seasonal and 60m from reference model and remove DO 250m daily and 0m annual w/ all other info
try(brt_do_0m_250m_daily_ann_refined <- dismo::gbm.step(
  data = dat_train_do_all, 
  gbm.x = c(8:11, 16:19, 23, 29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_250m_daily_ann_refined, here("data/brt/mod_outputs/crw/refined/brt_do_0m_250m_daily_ann_refined.rds"))

#agi removed 60m from reference model w/ all info
try(brt_agi_0m_250m_dail_seas_ann <- dismo::gbm.step(
  data = dat_train_agi_all, 
  gbm.x = c(8:11, 16:19, 22:24, 26, 27, 29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_250m_dail_seas_ann, here("data/brt/mod_outputs/crw/refined/brt_agi_0m_250m_dail_seas_ann.rds"))

#agi removed seasonal res from reference model w/ all info
try(brt_agi_0m_60m_250m_dail_ann <- dismo::gbm.step(
  data = dat_train_agi_all, 
  gbm.x = c(8:11, 16:19, 21:23, 27:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_dail_ann, here("data/brt/mod_outputs/crw/refined/brt_agi_0m_60m_250m_dail_ann.rds"))

#agi remove daily res from reference model w/ all info 
try(brt_agi_0m_60m_250m_seas_ann <- dismo::gbm.step(
  data = dat_train_agi_all, 
  gbm.x = c(9:11, 16:19, 23, 24:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_seas_ann, here("data/brt/mod_outputs/crw/refined/brt_agi_0m_60m_250m_seas_ann.rds"))

#agi remove seas res and 60m from reference model w/ all info 
try(brt_agi_0m_250m_daily_ann <- dismo::gbm.step(
  data = dat_train_agi_all, 
  gbm.x = c(8:10, 15:19, 22, 23, 27, 29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_250m_daily_ann, here("data/brt/mod_outputs/crw/refined/brt_agi_0m_250m_daily_ann.rds"))

#agi remove seas res and 60m from reference model, also w/o AGI 0m daily w/ all info 
try(brt_agi_0m_250m_daily_ann_refined <- dismo::gbm.step(
  data = dat_train_agi_all, 
  gbm.x = c(8:10, 15:19, 23, 29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_250m_daily_ann_refined, here("data/brt/mod_outputs/crw/refined/brt_agi_0m_250m_daily_ann_refined.rds"))


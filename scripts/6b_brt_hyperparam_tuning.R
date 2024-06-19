### libraries ####
library(tidyverse)
library(gbm)
library(dismo)
library(here);here <- here::here
library(ggBRT)
library(caret)
library(pROC)

set.seed(1004)

### load data ####
#CRW data
dat_base <- readRDS(here("data/locs_brts/crw_pas/dat_base.rds")) %>% subset(select = -c(rep))
dat_do <- readRDS(here("data/locs_brts/crw_pas/dat_do.rds")) %>% subset(select = -c(rep))
dat_agi <- readRDS(here("data/locs_brts/crw_pas/dat_agi.rds")) %>% subset(select = -c(rep))

#Bkg data
back_base <- readRDS(here("data/locs_brts/bckg_pas/dat_base_back.rds"))
back_do <- readRDS(here("data/locs_brts/bckg_pas/dat_do_back.rds"))
back_agi <- readRDS(here("data/locs_brts/bckg_pas/dat_agi_back.rds"))

### optimize hyperparameters (CRW PAs) ####
#split into test and train
#base
dat_base_nas <- na.omit(dat_base)
dat_train_base <- dat_base_nas %>% sample_frac(0.75)
dat_test_base <- dat_base_nas %>% sample_frac(0.25)

#do
dat_do_nas <- na.omit(dat_do)
dat_train_do <- dat_do_nas %>% sample_frac(0.75)
dat_test_do <- dat_do_nas %>% sample_frac(0.25)

#agi
dat_agi_nas <- na.omit(dat_agi)
dat_train_agi <- dat_agi_nas %>% sample_frac(0.75)
dat_test_agi <- dat_agi_nas %>% sample_frac(0.25)

# Set optimization options using caret
fitControl <- trainControl(method = "cv", number = 5) # Can be very slow with high "number"

# Set the range of options for each parameter: I'm varying interaction.depth (tree complexity) and shrinkage (learning rate). Range of values based on BRT paper from Elith et al (2008).
gbmGrid <- expand.grid(interaction.depth = seq(2, 5, by = 1), 
                       n.trees = 8000, #general number of trees models I ran for poster used
                       shrinkage = c(0.001, 0.01, 0.05, 0.1), 
                       n.minobsinnode = 10)

# Now test which combination of parameters works best. For some reason, the presence/absence variable must be a factor. Took ~30 min to run. 
#base model
gbmFit_base <- caret::train(factor(PA) ~ chl_mean + temp_mean + sal_mean + uo_mean + uostr_mean + vo_mean + vostr_mean + ssh_mean + mld_mean + bathy_mean + bathy_sd, 
                        data = dat_train_base, 
                        method = "gbm", 
                        trControl = fitControl, 
                        verbose = FALSE, 
                        tuneGrid = gbmGrid)
#saveRDS(gbmFit1, here("data/brt/hp_tuning/gbmFit_base.rds"))
plot(gbmFit_base)

#do model
gbmFit_do <- caret::train(factor(PA) ~ o2_mean_0m + o2_mean_60m + o2_mean_250m + chl_mean + temp_mean + sal_mean + uo_mean + uostr_mean + vo_mean + vostr_mean + ssh_mean + mld_mean + bathy_mean + bathy_sd, 
                          data = dat_train_do, 
                          method = "gbm", 
                          trControl = fitControl, 
                          verbose = FALSE, 
                          tuneGrid = gbmGrid)
#saveRDS(gbmFit_do, here("data/brt/hp_tuning/gbmFit_do.rds"))
plot(gbmFit_do)

#agi model
gbmFit_agi <- caret::train(factor(PA) ~ AGI_0m + AGI_60m + AGI_250m + chl_mean + temp_mean + sal_mean + uo_mean + uostr_mean + vo_mean + vostr_mean + ssh_mean + mld_mean + bathy_mean + bathy_sd, 
                           data = dat_train_agi, 
                           method = "gbm", 
                           trControl = fitControl, 
                           verbose = FALSE, 
                           tuneGrid = gbmGrid)

#saveRDS(gbmFit_agi, here("data/brt/hp_tuning/gbmFit_agi.rds"))
plot(gbmFit_agi)

# You can plot the results: ideally, a shrinkage value (learning rate) somewhere in the middle of the options you provided will be chosen, otherwise you may need to expand the range
plot(gbmFit1)

# Save the best values for learning rate and tree complexity. I will ultimately use gbm.step to select the best number of trees. 
lr.best_base <- gbmFit1$bestTune$shrinkage #0.05 (~ 0.80)
tc.best_base <- gbmFit1$bestTune$interaction.depth #either 3 or 4 -- within 0.2 accuracy reported by CV

lr.best_do <- gbmFit_do$bestTune$shrinkage #0.01 or 0.05 -- within 0.005 accuracy reported by CV (~ 0.81)
tc.best_do <- gbmFit_do$bestTune$interaction.depth # 3 or 4 trees doesn't make a difference -- within 0.005 accuracy reported by CV

lr.best_agi <- gbmFit_agi$bestTune$shrinkage #0.01 or 0.05, 0.05 somewhat better for 3 trees and 0.01 somewhat better for 4 trees. 
tc.best_agi <- gbmFit_agi$bestTune$interaction.depth #3 and 4 trees look about the same in terms of accuracy as reported by cv. All close to ~ 0.08

# Based on above results, for the CRW PA models, I should use a lr of 0.05, tree complexity of 3, bag fraction of 0.75, and model that selects the optimal number of trees rather than a set number. 

### optimize hyperparameters (Bkg PAs) ####
#split into test and train
#base
back_base_nas <- na.omit(back_base)
back_train_base <- back_base_nas %>% sample_frac(0.75)
back_test_base <- back_base_nas %>% sample_frac(0.25)

#do
back_do_nas <- na.omit(back_do)
back_train_do <- back_do_nas %>% sample_frac(0.75)
back_test_do <- back_do_nas %>% sample_frac(0.25)

#agi
back_agi_nas <- na.omit(back_agi)
back_train_agi <- back_agi_nas %>% sample_frac(0.75)
back_test_agi <- back_agi_nas %>% sample_frac(0.25)

# Now test which combination of parameters works best. For some reason, the presence/absence variable must be a factor. Took ~30 min to run. 
#base model
gbmFit_base_back <- caret::train(factor(PA) ~ chl_mean + temp_mean + sal_mean + uo_mean + uostr_mean + vo_mean + vostr_mean + ssh_mean + mld_mean + bathy_mean + bathy_sd, 
                            data = back_train_base, 
                            method = "gbm", 
                            trControl = fitControl, 
                            verbose = FALSE, 
                            tuneGrid = gbmGrid)
#saveRDS(gbmFit_base_back, here("data/brt/hp_tuning/gbmFit_base_back.rds"))
plot(gbmFit_base_back)

#do model
gbmFit_do_back <- caret::train(factor(PA) ~ o2_mean_0m + o2_mean_60m + o2_mean_250m + chl_mean + temp_mean + sal_mean + uo_mean + uostr_mean + vo_mean + vostr_mean + ssh_mean + mld_mean + bathy_mean + bathy_sd, 
                          data = back_train_do, 
                          method = "gbm", 
                          trControl = fitControl, 
                          verbose = FALSE, 
                          tuneGrid = gbmGrid)
#saveRDS(gbmFit_do_back, here("data/brt/hp_tuning/gbmFit_do_back.rds"))
plot(gbmFit_do_back)

#agi model
gbmFit_agi_back <- caret::train(factor(PA) ~ AGI_0m + AGI_60m + AGI_250m + chl_mean + temp_mean + sal_mean + uo_mean + uostr_mean + vo_mean + vostr_mean + ssh_mean + mld_mean + bathy_mean + bathy_sd, 
                           data = back_train_agi, 
                           method = "gbm", 
                           trControl = fitControl, 
                           verbose = FALSE, 
                           tuneGrid = gbmGrid)

#saveRDS(gbmFit_agi_back, here("data/brt/hp_tuning/gbmFit_agi_back.rds"))
plot(gbmFit_agi_back)

# Save the best values for learning rate and tree complexity. I will ultimately use gbm.step to select the best number of trees. 
gbmFit_base_back$bestTune$shrinkage #0.1 or 0.05 (~ 0.88) -- within 0.01 accuracy reported by CV
gbmFit_base_back$bestTune$interaction.depth #either 3 or 4 -- within 0.1 accuracy reported by CV

gbmFit_do_back$bestTune$shrinkage #0.1 or 0.05 (~0.90) -- within 0.01 accuracy reported by CV
gbmFit_do_back$bestTune$interaction.depth #All tree depths between 2-5 were within 0.01 accuracy. So could use any. 

gbmFit_agi_back$bestTune$shrinkage # 0.1 and 0.05 are both very close -- within 0.01 accuracy of each other
gbmFit_agi_back$bestTune$interaction.depth #tree depths of 3 and 4 resulted in similar accuracy (within 0.01)

# Based on above results, for the CRW PA models, I should use a lr of 0.05, tree complexity of 3, bag fraction of 0.75, and model that selects the optimal number of trees rather than a set number. 



### libraries ####
{library(tidyverse)
library(gbm)
library(dismo)
library(here);here <- here::here
library(ggBRT)
library(caret)
library(pROC)}

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

### optimize function ####
#load data 
#plot 75/25
#run caret 
#plot
#save plots 

# Set optimization options using caret
fitControl <- trainControl(method = "cv", number = 5) # Can be very slow with high "number"

# Set the range of options for each parameter: I'm varying interaction.depth (tree complexity) and shrinkage (learning rate). Range of values based on BRT paper from Elith et al (2008).
gbmGrid <- expand.grid(interaction.depth = seq(2, 5, by = 1), 
                       n.trees = 8000, #general number of trees models I ran for poster used
                       shrinkage = c(0.001, 0.01, 0.05, 0.1), 
                       n.minobsinnode = 10)

hyperparam_tune <- function(fitControl, gbmGrid, base_input, do_input, agi_input, hp_file_dest, res = c("ann", "seas"), pa = c("back", "crw")){
  
  #load dat -- need to add subset part back in for crw files
  base_dat <- readRDS(here(base_input)) #%>% subset(select = -c(rep))
  do_dat <- readRDS(here(do_input)) #%>% subset(select = -c(rep))
  agi_dat <- readRDS(here(agi_input)) #%>% subset(select = -c(rep))
  
  #split dat to train
  base_7525 <- na.omit(base_dat) %>% sample_frac(0.75)
  do_7525 <- na.omit(do_dat) %>% sample_frac(0.75)
  agi_7525 <- na.omit(agi_dat) %>% sample_frac(0.75)
  
  #optimize base HPs
  gbmFit_base <- caret::train(factor(PA) ~ chl_mean + temp_mean + sal_mean + uo_mean + uostr_mean + vo_mean + vostr_mean + ssh_mean + mld_mean + bathy_mean + bathy_sd, 
                              data = base_7525, 
                              method = "gbm", 
                              trControl = fitControl, 
                              verbose = FALSE, 
                              tuneGrid = gbmGrid)
  
  #save and plot base file
  saveRDS(gbmFit_base, here(paste0(hp_file_dest,"/gbmFit_base","_",res,"_", pa, ".rds")))
  plot(gbmFit_base)
  
  #optimize do HPs
  gbmFit_do <- caret::train(factor(PA) ~ o2_mean_0m + o2_mean_60m + o2_mean_250m + chl_mean + temp_mean + sal_mean + uo_mean + uostr_mean + vo_mean + vostr_mean + ssh_mean + mld_mean + bathy_mean + bathy_sd, 
                              data = do_7525, 
                              method = "gbm", 
                              trControl = fitControl, 
                              verbose = FALSE, 
                              tuneGrid = gbmGrid)
  
  #save and plot do file
  saveRDS(gbmFit_do, here(paste0(hp_file_dest,"/gbmFit_do","_",res,"_", pa, ".rds")))
  plot(gbmFit_do)
  
  #optimize agi HPs
  gbmFit_agi <- caret::train(factor(PA) ~ AGI_0m + AGI_60m + AGI_250m + chl_mean + temp_mean + sal_mean + uo_mean + uostr_mean + vo_mean + vostr_mean + ssh_mean + mld_mean + bathy_mean + bathy_sd, 
                            data = agi_7525, 
                            method = "gbm", 
                            trControl = fitControl, 
                            verbose = FALSE, 
                            tuneGrid = gbmGrid)
  
  #save and plot agi file
  saveRDS(gbmFit_agi, here(paste0(hp_file_dest,"/gbmFit_agi","_",res,"_", pa, ".rds")))
  plot(gbmFit_agi)

}

#### crw annual ####
hyperparam_tune(fitControl = fitControl, gbmGrid = gbmGrid, 
                base_input = "data/locs_brts/crw_pas_ann/dat_base_ann.rds", 
                do_input = "data/locs_brts/crw_pas_ann/dat_do_ann.rds",
                agi_input = "data/locs_brts/crw_pas_ann/dat_agi_ann.rds",
                hp_file_dest = "data/brt/hp_tuning/annual", 
                res = "ann", pa = "crw")

readRDS(here("data/brt/hp_tuning/annual/gbmFit_base_ann_crw.rds")) %>% plot()
readRDS(here("data/brt/hp_tuning/annual/gbmFit_do_ann_crw.rds")) %>% plot()
readRDS(here("data/brt/hp_tuning/annual/gbmFit_agi_ann_crw.rds")) %>% plot()

#lr notes: 0.05 marginally better than 0.01
#tc notees: 3 or 4

#### crw seasonal ####
hyperparam_tune(fitControl = fitControl, gbmGrid = gbmGrid, 
                base_input = "data/locs_brts/crw_pas_seas/dat_base_seas.rds", 
                do_input = "data/locs_brts/crw_pas_seas/dat_do_seas.rds",
                agi_input = "data/locs_brts/crw_pas_seas/dat_agi_seas.rds",
                hp_file_dest = "data/brt/hp_tuning/seasonal", 
                res = "seas", pa = "crw")

readRDS(here("data/brt/hp_tuning/seasonal/gbmFit_base_seas_crw.rds")) %>% plot()
readRDS(here("data/brt/hp_tuning/seasonal/gbmFit_do_seas_crw.rds")) %>% plot()
readRDS(here("data/brt/hp_tuning/seasonal/gbmFit_agi_seas_crw.rds")) %>% plot()

#lr notes: 0.01 marginally better than 0.05
#tc notees: 3 

#### back annual ####
hyperparam_tune(fitControl = fitControl, gbmGrid = gbmGrid, 
                base_input = "data/locs_brts/back_pas_ann/dat_base_ann.rds", 
                do_input = "data/locs_brts/back_pas_ann/dat_do_ann.rds",
                agi_input = "data/locs_brts/back_pas_ann/dat_agi_ann.rds",
                hp_file_dest = "data/brt/hp_tuning/annual", 
                res = "ann", pa = "back")

readRDS(here("data/brt/hp_tuning/annual/gbmFit_base_ann_back.rds")) %>% plot()
readRDS(here("data/brt/hp_tuning/annual/gbmFit_do_ann_back.rds")) %>% plot()
readRDS(here("data/brt/hp_tuning/annual/gbmFit_agi_ann_back.rds")) %>% plot()

#lr notes: 0.05 is top, marginally better than 0.1 and then 0.01
#tc notees: 3-5 marginally different

#### back seasonal ####
hyperparam_tune(fitControl = fitControl, gbmGrid = gbmGrid, 
                base_input = "data/locs_brts/back_pas_seas/dat_base_seas.rds", 
                do_input = "data/locs_brts/back_pas_seas/dat_do_seas.rds",
                agi_input = "data/locs_brts/back_pas_seas/dat_agi_seas.rds",
                hp_file_dest = "data/brt/hp_tuning/seasonal", 
                res = "seas", pa = "back")

readRDS(here("data/brt/hp_tuning/seasonal/gbmFit_base_seas_back.rds")) %>% plot()
readRDS(here("data/brt/hp_tuning/seasonal/gbmFit_do_seas_back.rds")) %>% plot()
readRDS(here("data/brt/hp_tuning/seasonal/gbmFit_agi_seas_back.rds")) %>% plot()

#lr notes: 0.05 is top, marginally better than 0,1 and then 0.01
#tc notees: 3 is better, accuracy tends to drop for 4



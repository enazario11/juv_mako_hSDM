### libraries ####
library(tidyverse)
library(gbm)
library(dismo)
library(here);here <- here::here
library(ggBRT)
library(caret)
library(pROC)
library(doParallel)

set.seed(1004)

### load data ####
#CRW data
dat_base <- readRDS(here("data/locs_brts/crw_pas/dat_base.rds")) %>% subset(select = -c(rep))
dat_do <- readRDS(here("data/locs_brts/crw_pas/dat_do.rds")) %>% subset(select = -c(rep))
dat_agi <- readRDS(here("data/locs_brts/crw_pas/dat_agi.rds")) %>% subset(select = -c(rep))

### optimize hyperparameters ####
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

#SCRATCH TO BE MOVED ####
#run BRT
try(test <- dismo::gbm.step(data = dat_train0, gbm.x = c(8:19, 23), gbm.y = 5,
                                              family = "bernoulli", tree.complexity = 5,
                                              learning.rate = 0.01, bag.fraction = 0.75, 
                            silent = TRUE, 
                            plot.main = TRUE))

#### explore outputs ####
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
plot(e, 'ROC') #closer to 1 is better -- also the AUC under the ROC curve (AUC and ROC is same)
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

### glm and gam test ####
m1 <- glm(PA ~ lat + thetao_mean + so_mean + mlotst_mean + lon + zos_mean + uo_mean + vo_mean + chl_mean + nppv_mean + o2_mean + bathy + bathy_sd + AGI_0m, data = dat)
summary(m1)

m2 <- gam(PA ~ s(lat) + s(thetao_mean) + s(so_mean) + s(mlotst_mean) + s(lon) + s(zos_mean) + s(uo_mean) + s(vo_mean) + s(chl_mean) + s(nppv_mean) + s(o2_mean) + s(bathy) + s(bathy_sd) + s(AGI_0m), random=~(1|tag),family=binomial, data=na.omit(dat), bs = "cs", method = "REML")
test_m <- gam(PA ~ s(AGI_0m) + s(thetao_mean), random=~(1|tag),family=binomial, data=na.omit(dat), bs = "cs", method = "REML")
test <- plot(test_m, residuals = TRUE)


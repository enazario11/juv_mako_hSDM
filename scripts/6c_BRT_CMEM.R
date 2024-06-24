### libraries ####
library(tidyverse)
library(gbm)
library(dismo)
library(here);here <- here::here
library(ggBRT)

set.seed(1004)

### load data ####
#CRW data
dat_base <- readRDS(here("data/locs_brts/crw_pas/dat_base.rds")) %>% mutate(tag = as.factor(tag))
dat_do <- readRDS(here("data/locs_brts/crw_pas/dat_do.rds")) %>% mutate(tag = as.factor(tag))
dat_agi <- readRDS(here("data/locs_brts/crw_pas/dat_agi.rds")) %>% mutate(tag = as.factor(tag))

### add random variable for predictor selection ####
pred_var <- rnorm(31084, mean = 50, sd = 10)

dat_base$pred_var <- pred_var
dat_do$pred_var <- pred_var
dat_agi$pred_var <- pred_var

#split into test and train
#base
dat_train_base <- dat_base %>% sample_frac(0.75)
dat_test_base <- dat_base %>% sample_frac(0.25)

#do
dat_train_do <- dat_do %>% sample_frac(0.75)
dat_test_do <- dat_do %>% sample_frac(0.25)

#agi
dat_train_agi <- dat_agi %>% sample_frac(0.75)
dat_test_agi <- dat_agi %>% sample_frac(0.25)

### run BRT ####
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

### glm and gam test ####
m1 <- glm(PA ~ lat + thetao_mean + so_mean + mlotst_mean + lon + zos_mean + uo_mean + vo_mean + chl_mean + nppv_mean + o2_mean + bathy + bathy_sd + AGI_0m, data = dat)
summary(m1)

m2 <- gam(PA ~ s(lat) + s(thetao_mean) + s(so_mean) + s(mlotst_mean) + s(lon) + s(zos_mean) + s(uo_mean) + s(vo_mean) + s(chl_mean) + s(nppv_mean) + s(o2_mean) + s(bathy) + s(bathy_sd) + s(AGI_0m), random=~(1|tag),family=binomial, data=na.omit(dat), bs = "cs", method = "REML")
test_m <- gam(PA ~ s(AGI_0m) + s(thetao_mean), random=~(1|tag),family=binomial, data=na.omit(dat), bs = "cs", method = "REML")
test <- plot(test_m, residuals = TRUE)


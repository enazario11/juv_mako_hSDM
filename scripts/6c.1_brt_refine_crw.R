### libraries ####
library(tidyverse)
library(gbm)
library(dismo)
library(here);here <- here::here
library(ggBRT)
library(foreach)
library(doParallel)

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
  dplyr::rename("o2_mean_0m_seas" = "dat_do_s$o2_mean_0m", 
         "o2_mean_60m_seas" = "dat_do_s$o2_mean_60m", 
         "o2_mean_250m_seas" = "dat_do_s$o2_mean_250m", 
         "o2_mean_0m_ann" = "dat_do_a$o2_mean_0m", 
         "o2_mean_60m_ann" = "dat_do_a$o2_mean_60m", 
         "o2_mean_250m_ann" = "dat_do_a$o2_mean_250m")

dat_agi_all <- cbind(dat_agi_d, dat_agi_s$AGI_0m, dat_agi_s$AGI_60m, dat_agi_s$AGI_250m, dat_agi_a$AGI_0m, dat_agi_a$AGI_60m, dat_agi_a$AGI_250m)
dat_agi_all <- dat_agi_all %>%
  dplyr::rename("AGI_0m_seas" = "dat_agi_s$AGI_0m",
         "AGI_60m_seas" = "dat_agi_s$AGI_60m", 
         "AGI_250m_seas" = "dat_agi_s$AGI_250m", 
         "AGI_0m_ann" = "dat_agi_a$AGI_0m", 
         "AGI_60m_ann" = "dat_agi_a$AGI_60m", 
         "AGI_250m_ann" = "dat_agi_a$AGI_250m")

### combined DO and AGI df ####
dat_do_agi_all <- cbind(dat_do_all, dat_agi_all[,c(19, 22, 24, 26, 27, 29)])

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
dat_do_agi_temp_all <- floor((nrow(dat_do_agi_all)/4)*3) #define % of training and test set
dat_train_do_agi_all <- dat_do_agi_all[sample(nrow(dat_do_agi_all),dat_do_agi_temp_all),]
dat_test_do_agi_all <- dat_do_agi_all[sample(nrow(dat_do_agi_all),nrow(dat_do_agi_all)-dat_do_agi_temp_all),]
#saveRDS(dat_test_do_agi_all, here("data/brt/mod_eval/agi_do_test_daily_seasonal_annual.rds"))

### Remove wind vars ####
# base model w/o wind vars
try(brt_base_0m_dail_no_wind <- dismo::gbm.step(
  data = dat_train_base_d, 
  gbm.x = c(8:10, 15:18), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_base_0m_dail_no_wind, here("data/brt/mod_outputs/final_mods/brt_base_0m_dail_no_wind.rds"))

#do reference model w/o wind vars
try(brt_do_0m_60m_250m_dail_seas_ann_no_wind <- dismo::gbm.step(
  data = dat_train_do_all, 
  gbm.x = c(8:11, 16:19, 21, 22, 24:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_60m_250m_dail_seas_ann_no_wind, here("data/brt/mod_outputs/final_mods/brt_do_0m_60m_250m_dail_seas_ann_no_wind.rds"))

#agi reference model w/o wind vars
try(brt_agi_0m_60m_250m_dail_seas_ann_no_wind <- dismo::gbm.step(
  data = dat_train_agi_all, 
  gbm.x = c(8:10, 15:19, 21, 22, 24:29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_60m_250m_dail_seas_ann_no_wind, here("data/brt/mod_outputs/final_mods/brt_agi_0m_60m_250m_dail_seas_ann_no_wind.rds"))

### Combo models of DO and AGI #### 
#agi at 250 m at all temp res and do at 0 m at all temp res
try(brt_agi_250_DO_0_dail_seas_ann <- dismo::gbm.step(
  data = dat_train_do_agi_all, 
  gbm.x = c(8:11, 16:19, 24, 27, 31,33, 35), 
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
  gbm.x = c(8:11, 16:19, 22, 24, 26, 27, 29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_0m_250m_dail_seas_ann, here("data/brt/mod_outputs/final_mods/brt_do_0m_250m_dail_seas_ann.rds"))

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
  gbm.x = c(8:10, 15:19, 22, 24, 26, 27, 29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_0m_250m_dail_seas_ann, here("data/brt/mod_outputs/final_mods/brt_agi_0m_250m_dail_seas_ann.rds"))

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

### ensemble model explore ####
#generate agi only model
try(brt_agi_only <- dismo::gbm.step(
  data = dat_train_do_agi_all, 
  gbm.x = c(30:35), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_agi_only, here("data/brt/mod_outputs/crw/ensemble/brt_agi_only.rds"))

#generate DO model 
try(brt_do_final <- dismo::gbm.step(
  data = dat_train_do_agi_all, 
  gbm.x = c(8:11, 16:19, 22, 24, 26, 27, 29), 
  gbm.y = 5,
  family = "bernoulli", 
  tree.complexity = 3,
  learning.rate = 0.05, 
  bag.fraction = 0.75, 
  silent = TRUE, 
  plot.main = TRUE
)
)
saveRDS(brt_do_final, here("data/brt/mod_outputs/crw/ensemble/brt_do_final.rds"))

#load do final model 
agi_mod <- readRDS(here("data/brt/mod_outputs/crw/ensemble/brt_agi_only.rds"))
do_mod <- readRDS(here("data/brt/mod_outputs/crw/ensemble/brt_do_final.rds"))

# make df of predictions from each model
test_df <- readRDS(here("data/brt/mod_eval/agi_do_test_daily_seasonal_annual.rds"))

pred_testdata <- data.frame(
  do = predict.gbm(do_mod, test_df,
                   n.trees = do_mod$gbm.call$best.trees,
                   type = "response"),
  agi = predict.gbm(agi_mod, test_df,
                    n.trees = agi_mod$gbm.call$best.trees,
                    type = "response")
)

summary(pred_testdata)

# Mean of probabilities
mean_prob <- rowMeans(pred_testdata)

# performance measures for "mean of probabilities"
(perf_mean_prob <- mecofun::evalSDM(test_df$PA, mean_prob))

### Leave one year out evaluation ####
#Subset one out (SOO) function originally developed by Heather and Nerea, modified by EN here
SOO_eval <- function(df, gbm.x, gbm.y, lr = 0.05, tc = 3, family = "bernoulli"){
  DataInput = df %>% mutate(Year=year(dt))
  
  Evaluations_LOO <- as.data.frame(matrix(data=0,nrow=length(unique(DataInput$Year)),ncol=10))
  colnames(Evaluations_LOO) <- c("Year","Deviance","AUC","TSS","TPR","TNR","AveragePredAbs","AveragePredPres","N. test points","% left out")
    
  counter=1
  for (y in min(DataInput$Year):max(DataInput$Year)) {
    print(y)
    
    DataInput_train <- DataInput[DataInput$Year!=y,]
    DataInput_test <- DataInput[DataInput$Year==y,]
    
    DataInput.loo <- gbm.step(data = DataInput_train, gbm.x = gbm.x, gbm.y = gbm.y, 
                                family = family, tree.complexity = tc,
                                learning.rate = lr, bag.fraction = 0.75)
    
    preds <- predict.gbm(DataInput.loo, DataInput_test,
                         n.trees = DataInput.loo$gbm.call$best.trees, type="response")
  
    null <- DataInput.loo$self.statistics$null
    res <- DataInput.loo$self.statistics$resid
    dev=((null - res)/null)*100 
    
    d <- cbind(DataInput_test$PA, preds)
    test = as.data.frame(d)
    colnames(test) <- c("PA", "preds")
    test= test %>% group_by(PA) %>% summarise(mean(preds))
    
    pres <- d[d[,1]==1,2]
    abs <- d[d[,1]==0,2]
    
    if(length(pres)>0 & length(abs)>0){
      e <- evaluate(p=pres, a=abs)
      thresh=threshold(e)$equal_sens_spec
      ind=which(abs(e@t-thresh)==min(abs(e@t-thresh)))
      Evaluations_LOO[counter,1] <- y
      Evaluations_LOO[counter,2] <- dev
      Evaluations_LOO[counter,3] <- e@auc
      Evaluations_LOO[counter,4] <- max(e@TPR + e@TNR-1)
      Evaluations_LOO[counter,5] <- e@TPR[ind]
      Evaluations_LOO[counter,6] <- e@TNR[ind]
      Evaluations_LOO[counter,7] <- test[1,2]
      Evaluations_LOO[counter,8] <- test[2,2]
      Evaluations_LOO[counter,9] <- nrow(DataInput_test)
      Evaluations_LOO[counter,10] <- nrow(DataInput_test)/nrow(DataInput)*100
      counter=counter+1
    }
  }
  return(Evaluations_LOO)}

#### base model ####
gbm_base = colnames(dat_base_d[, c(8:10, 15:18)])
soo_year_base <- SOO_eval(df = dat_base_d, gbm.x = gbm_base, gbm.y = "PA")
saveRDS(soo_year_base, here("data/brt/mod_outputs/crw/evaluation/soo_year_base.rds"))

#### do model ####
gbm_do = colnames(dat_do_all[, c(8:11, 16:19, 22, 24, 26, 27, 29)])
soo_year_do <- SOO_eval(df = dat_do_all, gbm.x = gbm_do, gbm.y = "PA")
saveRDS(soo_year_do, here("data/brt/mod_outputs/crw/evaluation/soo_year_do.rds"))

#### agi model ####
gbm_agi = colnames(dat_agi_all[, c(8:10, 15:19, 22, 24, 26, 27, 29)])
soo_year_agi <- SOO_eval(df = dat_agi_all, gbm.x = gbm_agi, gbm.y = "PA")
saveRDS(soo_year_agi, here("data/brt/mod_outputs/crw/evaluation/soo_year_agi.rds"))

#### do,agi combo model ####
gbm_do_agi = colnames(dat_do_agi_all[, c(8:11, 16:19, 24, 27, 31,33, 35)])
soo_year_combo <- SOO_eval(df = dat_do_agi_all, gbm.x = gbm_do_agi, gbm.y = "PA")
saveRDS(soo_year_combo, here("data/brt/mod_outputs/crw/evaluation/soo_year_combo.rds"))



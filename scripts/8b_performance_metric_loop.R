### libraries ####
{library(tidyverse)
 library(doParallel)
 library(foreach)
 library(here)
 library(gbm)
 library(dismo)
  set.seed(1004)}

### data ####
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

### combined DO and AGI df ####
dat_do_agi_all <- cbind(dat_do_all, dat_agi_all[,c(19, 22, 24, 26, 27, 29)])

### split into test and train daily data #####
#base
dat_base_temp <- floor((nrow(dat_base_d)/4)*3) #define % of training and test set
dat_train_base_d <- dat_base_d[sample(nrow(dat_base_d),dat_base_temp),]
dat_test_base_d <- dat_base_d[sample(nrow(dat_base_d),nrow(dat_base_d)-dat_base_temp),]

#do
dat_do_temp <- floor((nrow(dat_do_d)/4)*3) #define % of training and test set
dat_train_do_d <- dat_do_d[sample(nrow(dat_do_d),dat_do_temp),]
dat_test_do_d <- dat_do_d[sample(nrow(dat_do_d),nrow(dat_do_d)-dat_do_temp),]

#agi
dat_agi_temp <- floor((nrow(dat_agi_d)/4)*3) #define % of training and test set
dat_train_agi_d <- dat_agi_d[sample(nrow(dat_agi_d),dat_agi_temp),]
dat_test_agi_d <- dat_agi_d[sample(nrow(dat_agi_d),nrow(dat_agi_d)-dat_agi_temp),]

### split into test and train seasonal/annual data####
dat_base_temp_s <- floor((nrow(dat_base_s)/4)*3)
dat_train_base_s <- dat_base_s[sample(nrow(dat_base_s),dat_base_temp_s),] #seasonal
dat_test_base_s <- dat_base_s[sample(nrow(dat_base_s),nrow(dat_base_s)-dat_base_temp_s),]

dat_base_temp_a <- floor((nrow(dat_base_a)/4)*3)
dat_train_base_a <- dat_base_a[sample(nrow(dat_base_a),dat_base_temp_a),] #annual
dat_test_base_a <- dat_base_a[sample(nrow(dat_base_a),nrow(dat_base_a)-dat_base_temp_a),]

#do
dat_do_temp_all <- floor((nrow(dat_do_all)/4)*3) #define % of training and test set
dat_train_do_all <- dat_do_all[sample(nrow(dat_do_all),dat_do_temp_all),]
dat_test_do_all <- dat_do_all[sample(nrow(dat_do_all),nrow(dat_do_all)-dat_do_temp_all),]

#agi
dat_agi_temp_all <- floor((nrow(dat_agi_all)/4)*3) #define % of training and test set
dat_train_agi_all <- dat_agi_all[sample(nrow(dat_agi_all),dat_agi_temp_all),]
dat_test_agi_all <- dat_agi_all[sample(nrow(dat_agi_all),nrow(dat_agi_all)-dat_agi_temp_all),]

#do and agi 
dat_do_agi_temp_all <- floor((nrow(dat_do_agi_all)/4)*3) #define % of training and test set
dat_train_do_agi_all <- dat_do_agi_all[sample(nrow(dat_do_agi_all),dat_do_agi_temp_all),]
dat_test_do_agi_all <- dat_do_agi_all[sample(nrow(dat_do_agi_all),nrow(dat_do_agi_all)-dat_do_agi_temp_all),]

### function to run for loop across cores to run model simulations ####
n_cores <- detectCores()
cluster <- makeCluster(n_cores-2)
registerDoParallel(cluster)

brt_run_sims <- function(train_dat, n_iter = 20, mod_type){
  if(mod_type == "base"){
    pred_vars = c(8:10, 15:18)
  }
  if(mod_type == "do"){
    pred_vars = c(8:11, 16:19, 22, 24, 26, 27, 29)
  }
  if(mod_type == "agi"){
    pred_vars = c(8:10, 15:19, 22, 24, 26, 27, 29)
  }
  if(mod_type == "combo"){
    pred_vars = c(8:11, 16:19, 24, 27, 31,33, 35)
  }
  
  foreach(i = 1:n_iter, .packages = c("here", "gbm", "dismo", "tidyverse")) %dopar% {
    
    try(brt_iter <- dismo::gbm.step(
      data = train_dat, 
      gbm.x = pred_vars, 
      gbm.y = 5,
      family = "bernoulli", 
      tree.complexity = 3,
      learning.rate = 0.05, 
      bag.fraction = 0.75, 
      silent = TRUE, 
      plot.main = TRUE
    )
    )
    
  saveRDS(brt_iter, file = here(paste0("data/brt/mod_outputs/perf_metric_iters/", mod_type, "/", mod_type,"_", i, ".rds")))  
  
  #end parallel
  }
  
#end function
}

### run brt iterations ####
#base model
brt_run_sims(train_dat = dat_train_base_d, mod_type = "base")

#do model
brt_run_sims(train_dat = dat_train_do_all, mod_type = "do")

#agi model
brt_run_sims(train_dat = dat_train_agi_all, mod_type = "agi")

#combo model
brt_run_sims(train_dat = dat_train_do_agi_all, mod_type = "combo")

stopCluster(cl = cluster)

### function to pull model perform metrics and store ####
brt_perf_metric <- function(mod_files, mod_type){
  
  for(i in 1:length(mod_files)){
    
    #create df for performance metric values
    if(i == 1){
      perform_df <- data.frame(matrix(ncol = 4, nrow = 20))
      cols_names <- c("iteration", "AUC", "TSS", "dev_exp")
      colnames(perform_df) <- cols_names 
    }
    
    #load model file
    mod_file <- readRDS(mod_files[i])
    
    #load test data file
    if(mod_type == "base"){
      test_file = readRDS(here("data/brt/mod_eval/base_test_daily.rds"))
    }
    if(mod_type == "do"){
      test_file = readRDS(here("data/brt/mod_eval/do_test_daily_seasonal_annual.rds"))
    }
    if(mod_type == "agi"){
      test_file = readRDS(here("data/brt/mod_eval/agi_test_daily_seasonal_annual.rds"))
    }
    if(mod_type == "combo"){
      test_file = readRDS(here("data/brt/mod_eval/agi_do_test_daily_seasonal_annual.rds"))
    }
  
   #calc performance metrics 
    preds <- predict.gbm(mod_file, test_file,
                         n.trees = mod_file$gbm.call$best.trees,
                         type = "response")
    observed <- test_file$PA
    
    ext.residual.deviance <- calc.deviance(obs = test_file$PA, pred=preds, family="bernoulli", calc.mean=TRUE) #get % deviance
    null.dev =  calc.deviance(test_file$PA ,rep(mean(test_file$PA),length(test_file$PA)), family="bernoulli", calc.mean=T)
    dev=(null.dev - ext.residual.deviance)/null.dev 

    dat_pred <- cbind(test_file$PA, preds)
    pres <- dat_pred[dat_pred[,1] == 1, 2]
    abs <- dat_pred[dat_pred[,1] == 0, 2]
    
    #evaluate (AUC, TSS, TPR)
    e = evaluate(p = pres, a = abs)

  #store metrics in performance metric df
  perform_df$iteration[i] <- i
  perform_df$AUC[i] <- e@auc
  perform_df$TSS[i] <- max((e@TPR + e@TNR - 1))
  perform_df$dev_exp[i] <- dev
  
  #end for loop
  }
  
  saveRDS(perform_df, here(paste0("data/brt/mod_outputs/perf_metric_iters/", mod_type, "_metrics.rds")))
  
  return(perform_df)
  
  #end function
}

### run brt iterations ####
#base model
brt_perf_metric(mod_type = "base", mod_files = list.files(here("data/brt/mod_outputs/perf_metric_iters/base"), full.names = TRUE))

#do model
brt_perf_metric(mod_type = "do", mod_files = list.files(here("data/brt/mod_outputs/perf_metric_iters/do"), full.names = TRUE))

#agi model
brt_perf_metric(mod_type = "agi", mod_files = list.files(here("data/brt/mod_outputs/perf_metric_iters/agi"), full.names = TRUE))

#combo model
brt_perf_metric(mod_type = "combo", mod_files = list.files(here("data/brt/mod_outputs/perf_metric_iters/combo"), full.names = TRUE))### run brt iterations ####








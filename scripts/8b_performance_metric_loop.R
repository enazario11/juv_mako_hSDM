### libraries ####
{library(tidyverse)
 library(doParallel)
 library(foreach)
 library(here)
 library(gbm)
 library(dismo)
  set.seed(1004)}

### load data ####
# CRW daily data 
dat_base_d <- readRDS(here("data/locs_brts/crw_pas/dat_base.rds")) %>% mutate(tag = as.factor(tag))
dat_do_d <- readRDS(here("data/locs_brts/crw_pas/dat_do.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_d <- readRDS(here("data/locs_brts/crw_pas/dat_agi.rds")) %>% mutate(tag = as.factor(tag))

#### CRW seasonal data 
dat_base_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_base_seas.rds")) %>% mutate(tag = as.factor(tag))
dat_do_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_do_seas.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_agi_seas.rds")) %>% mutate(tag = as.factor(tag))

#### CRW annual data 
dat_base_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_base_ann.rds")) %>% mutate(tag = as.factor(tag))
dat_do_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_do_ann.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_agi_ann.rds")) %>% mutate(tag = as.factor(tag))

#### Add seasonal and annual data to daily data df for DO and AGI
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

### combined DO and AGI df 
dat_do_agi_all <- cbind(dat_do_all, dat_agi_all[,c(22, 25, 28)])

### function to run for loop across cores to run model simulations ####
n_cores <- detectCores()
cluster <- makeCluster(n_cores-2)
registerDoParallel(cluster)

brt_run_sims <- function(dat_file, n_iter = 20, mod_type, save_folder){
  
  if(mod_type == "base"){
    pred_vars = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd")
  }
  if(mod_type == "do"){
    pred_vars = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m", "o2_mean_250m", "o2_mean_0m_seas", "o2_mean_0m_ann", "o2_mean_250m_seas", "o2_mean_250m_ann")
  }
  if(mod_type == "agi"){
    pred_vars = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "AGI_0m", "AGI_250m", "AGI_0m_seas", "AGI_0m_ann", "AGI_250m_seas", "AGI_250m_ann")
  }
  if(mod_type == "combo"){
    pred_vars = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m", "o2_mean_0m_seas", "o2_mean_0m_ann", "AGI_250m", "AGI_250m_seas", "AGI_250m_ann")
  }
  
  foreach(i = 1:n_iter, .packages = c("here", "gbm", "dismo", "tidyverse")) %dopar% {
    
    #test vs train files
    dat_temp <- dat_file %>% 
      sample_frac(0.25)
    
    dat_test <-  subset(dat_file, row_id %in% dat_temp$row_id)
    #save test file
    saveRDS(dat_test, file = here(paste0(save_folder, mod_type, "/", "test/", mod_type,"_", "test", i, ".rds")))
    
    dat_train <- subset(dat_file, !(row_id %in% dat_temp$row_id))
    
    try(brt_iter <- dismo::gbm.step(
      data = dat_train, 
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
    
  saveRDS(brt_iter, file = here(paste0(save_folder, mod_type, "/", mod_type,"_", i, ".rds")))  
  
  #end parallel
  }
  
#end function
}

### run brt iterations ####
#### entire domain and study period 
# base model
dat_base_d$row_id <- 1:nrow(dat_base_d)
brt_run_sims(dat_file = dat_base_d, mod_type = "base", save_folder = "data/brt/mod_outputs/perf_metric_iters/")

#do model
dat_do_all$row_id <- 1:nrow(dat_do_all)
brt_run_sims(dat_file = dat_do_all, mod_type = "do", save_folder = "data/brt/mod_outputs/perf_metric_iters/")

#agi model
dat_agi_all$row_id <- 1:nrow(dat_agi_all)
brt_run_sims(dat_file = dat_agi_all, mod_type = "agi", save_folder = "data/brt/mod_outputs/perf_metric_iters/")

#combo model
dat_do_agi_all$row_id <- 1:nrow(dat_do_agi_all)
brt_run_sims(dat_file = dat_do_agi_all, mod_type = "combo", save_folder = "data/brt/mod_outputs/perf_metric_iters/")

stopCluster(cl = cluster)

### performance metrics ####
brt_perf_metric <- function(mod_files, test_files, mod_type, domain = "all"){
  
  #read in model and test file locations
  mod_files = list.files(mod_files, full.names = TRUE, pattern = ".rds")
  test_files = list.files(test_files, full.names = TRUE, pattern = ".rds")
 
  for(i in 1:length(mod_files)){
    
    #load model and test file
    mod_file <- readRDS(mod_files[i])
    test_file <- readRDS(test_files[i])
    
    if(i == 1){
      temp_df <- data.frame(matrix(ncol = 4, nrow = 20))
      cols_names <- c("iteration", "AUC", "TSS", "dev_exp")
      colnames(temp_df) <- cols_names  
    }
    
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
    temp_df$iteration[i] <- i
    temp_df$AUC[i] <- e@auc
    temp_df$TSS[i] <- max((e@TPR + e@TNR - 1))
    temp_df$dev_exp[i] <- dev

  } #end loop per brt iteration
  
  saveRDS(temp_df, here(paste0("data/brt/mod_outputs/perf_metric_iters/", mod_type, "_metrics.rds")))
  return(temp_df)
  
} #end function

#performance metrics entire domain and study period
#base model
base_metrics <- brt_perf_metric(mod_type = "base", mod_files = "data/brt/mod_outputs/perf_metric_iters/base", test_files = "data/brt/mod_outputs/perf_metric_iters/base/test")

#do model
do_metrics <- brt_perf_metric(mod_type = "do", mod_files = "data/brt/mod_outputs/perf_metric_iters/do", test_files="data/brt/mod_outputs/perf_metric_iters/do/test")

#agi model
agi_metrics <- brt_perf_metric(mod_type = "agi", mod_files = "data/brt/mod_outputs/perf_metric_iters/agi", test_files = "data/brt/mod_outputs/perf_metric_iters/agi/test")

#combo model
combo_metrics <- brt_perf_metric(mod_type = "combo", mod_files = "data/brt/mod_outputs/perf_metric_iters/combo", test_files = "data/brt/mod_outputs/perf_metric_iters/combo/test")







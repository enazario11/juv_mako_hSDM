### libraries ####
library(tidyverse)
library(here)
library(gbm)
library(doParallel)
source("functions/brt_explore_quarto_function.R")
set.seed(1004)

### load data ####
# CRW daily data 
dat_base_d <- readRDS(here("data/locs_brts/crw_pas_dail/dat_base_daily.rds")) %>% mutate(tag = as.factor(tag))
dat_do_d <- readRDS(here("data/locs_brts/crw_pas_dail/dat_do_daily.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_d <- readRDS(here("data/locs_brts/crw_pas_dail/dat_agi_daily.rds")) %>% mutate(tag = as.factor(tag))

# CRW seasonal data 
dat_do_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_do_seas.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_agi_seas.rds")) %>% mutate(tag = as.factor(tag))

# CRW annual data 
dat_do_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_do_ann.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_agi_ann.rds")) %>% mutate(tag = as.factor(tag))

# Add seasonal and annual data to daily data df for DO and AGI
dat_do_all <- cbind(dat_do_d, dat_do_s$o2_mean_0m, dat_do_s$o2_mean_250m, dat_do_a$o2_mean_0m, dat_do_a$o2_mean_250m)
dat_do_all <- dat_do_all %>%
  dplyr::rename("o2_mean_0m_seas" = "dat_do_s$o2_mean_0m",
         "o2_mean_250m_seas" = "dat_do_s$o2_mean_250m", 
         "o2_mean_0m_ann" = "dat_do_a$o2_mean_0m",  
         "o2_mean_250m_ann" = "dat_do_a$o2_mean_250m")

dat_agi_all <- cbind(dat_agi_d, dat_agi_s$AGI_0m, dat_agi_s$AGI_250m, dat_agi_a$AGI_0m, dat_agi_a$AGI_250m)
dat_agi_all <- dat_agi_all %>%
  dplyr::rename("AGI_0m_seas" = "dat_agi_s$AGI_0m",
         "AGI_250m_seas" = "dat_agi_s$AGI_250m", 
         "AGI_0m_ann" = "dat_agi_a$AGI_0m", 
         "AGI_250m_ann" = "dat_agi_a$AGI_250m")

# run brts
pred_vars_base = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd")
pred_vars_do = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m", "o2_mean_250m", "o2_mean_0m_seas", "o2_mean_0m_ann", "o2_mean_250m_seas", "o2_mean_250m_ann")
pred_vars_agi = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "AGI_0m", "AGI_250m", "AGI_0m_seas", "AGI_0m_ann", "AGI_250m_seas", "AGI_250m_ann")

n_cores <- detectCores()
cluster <- makeCluster(n_cores-2)
registerDoParallel(cluster)

brt_run <- function(dat_file, mod_type, save_folder, pred_vars, n_iter = 20){
   set.seed(1004)   

    #test vs train files
    dat_file$row_id <- 1:nrow(dat_file)
    
    foreach(i = 1:n_iter, .packages = c("here", "gbm", "dismo", "tidyverse")) %dopar% {
      set.seed(1004 + i)

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
    } #end parallel    
    } #end function

# base model
brt_run(dat_file = dat_base_d, mod_type = "base", pred_vars = pred_vars_base, save_folder = "data/brt/mod_outputs/revised/")

#do model
brt_run(dat_file = dat_do_all, mod_type = "do", pred_vars = pred_vars_do, save_folder = "data/brt/mod_outputs/revised/")

#agi model
brt_run(dat_file = dat_agi_all, mod_type = "agi", pred_vars = pred_vars_agi, save_folder = "data/brt/mod_outputs/revised/")

stopCluster(cluster)

#get performance metrics
brt_perf_metric <- function(mod_files, test_files, test_type, mod_type, domain = "all"){
  
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
  
  base::saveRDS(object = temp_df, file = paste0("data/brt/mod_outputs/", test_type, "/", test_type, "_", mod_type, "_metrics.rds"))
  return(temp_df)
  
} #end function

#performance metrics entire domain and study period
#base model
base_metrics <- brt_perf_metric(mod_type = "base", test_type = "revised", mod_files = "data/brt/mod_outputs/revised/base", test_files = "data/brt/mod_outputs/revised/base/test")

#do model
do_metrics <- brt_perf_metric(mod_type = "do", test_type = "revised", mod_files = "data/brt/mod_outputs/revised/do", test_files="data/brt/mod_outputs/revised/do/test")

#agi model
agi_metrics <- brt_perf_metric(mod_type = "agi", test_type = "revised",  mod_files = "data/brt/mod_outputs/revised/agi", test_files = "data/brt/mod_outputs/revised/agi/test")





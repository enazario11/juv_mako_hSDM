### load libraries ####
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

#### Add seasonal and annual data to daily data df 
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

### cols for stratified sampling ####
soi_dat <- read.csv(here("data/enviro/soi/soi.long.csv"))
soi_dat$Date <- as.Date(soi_dat$Date, "%m/%d/%Y")
soi_dat$dt_ym <- format(as.Date(soi_dat$Date), "%Y-%m")

sample_cols <- function(dat_file){
  
  #spatial columns
  dat_file$region <- "NA"
  
  for(i in 1:nrow(dat_file)){
    if(dat_file$lat[i] < 12) {
      dat_file$region[i] <- "nec"
    } else if(dat_file$lat[i] > 30 & dat_file$lat[i] < 48 & dat_file$lon[i] > -134){
      dat_file$region[i] <- "ccs"
    } else {
      dat_file$region[i] <- "nep"
    }
  } #end region loop
  
  #enso columns
  dat_file$soi <- "NA"
  for(i in 1:nrow(dat_file)){
    yr_mo_dat <- format(as.Date(dat_file$dt[i]), "%Y-%m")
    
    soi_temp <- soi_dat %>%
      filter(dt_ym == as.character(yr_mo_dat))
    
    dat_file$soi[i] <- soi_temp$SOI
  }
  
  dat_file$enso <- "NA"
  for(i in 1:nrow(dat_file)){
    if(dat_file$soi[i] > 0.5) {
      dat_file$enso[i] <- "en"
    } else if(dat_file$soi[i] < -0.5){
      dat_file$enso[i] <- "ln"
    } else {
      dat_file$enso[i] <- "neut"
    }
  } #end enso loop
 
  #add unique ID
  dat_file$st_id <- "NA"
  
  for(i in 1:nrow(dat_file)){
  dat_file$st_id[i] <- paste0(dat_file$region[i], "_", dat_file$enso[i])
  }
  
  return(dat_file) 
} #end function

#### run function ####
dat_base_st <- sample_cols(dat_base_d)
dat_do_st <- sample_cols(dat_do_all)
dat_agi_st <- sample_cols(dat_agi_all)
dat_do_agi_st <- sample_cols(dat_do_agi_all)

dat_base_st %>% group_by(st_id) %>% summarise(count = n())

### stratified sampling w/ 20 BRT iterations ####
brt_st <- function(dat_file, n_iter = 20, mod_type, save_folder){
  
  #predictor vars
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
  
  #brts
  foreach(i = 1:n_iter, .packages = c("here", "gbm", "dismo", "tidyverse")) %dopar% {
    
    #test vs train files
    dat_temp <- dat_file %>% 
      group_by(st_id) %>%
      sample_frac(0.20)
    
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
    
    #save model
    saveRDS(brt_iter, file = here(paste0(save_folder, mod_type, "/", mod_type,"_", i, ".rds")))  
    
  } #end parallel
} #end function

#### run brts ####
#base
dat_base_st$row_id <- 1:nrow(dat_base_st)
brt_st(dat_file = dat_base_st, mod_type = "base", save_folder = "data/brt/mod_outputs/perf_metric_iters/brts_st/")

#do
dat_do_st$row_id <- 1:nrow(dat_do_st)
brt_st(dat_file = dat_do_st, mod_type = "do", save_folder = "data/brt/mod_outputs/perf_metric_iters/brts_st/")

#agi
dat_agi_st$row_id <- 1:nrow(dat_agi_st)
brt_st(dat_file = dat_agi_st, mod_type = "agi", save_folder = "data/brt/mod_outputs/perf_metric_iters/brts_st/")

#combo
dat_do_agi_st$row_id <- 1:nrow(dat_do_agi_st)
brt_st(dat_file = dat_do_agi_st, mod_type = "combo", save_folder = "data/brt/mod_outputs/perf_metric_iters/brts_st/")

### performance metrics ####












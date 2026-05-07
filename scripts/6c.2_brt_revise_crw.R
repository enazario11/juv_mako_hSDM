#script to redo exploration brts for scatter plot supplementary table/figure since revising AGI/DO functions
{library(tidyverse) 
library(here)
library(sf)
library(terra)
library(gbm)
source(here::here("functions/oxy_demand_functions_rev.R"))
source(here::here("functions/hsi_rast_functions.R"))
source(here::here("functions/avg_functions.R"))
source(here::here("functions/brt_explore_quarto_functions.R"))}

#### Calculate AGI across three depth layers ####
##### convert DO to atm #####
#daily
    dat0 <- readRDS(here("data/locs_w_covar/psat_spot/cmem_locs_covar_0m.rds")) 
    dat60 <- readRDS(here("data/locs_w_covar/psat_spot_backup/cmem_locs_covar_60m.rds"))
    dat250 <- readRDS(here("data/locs_w_covar/psat_spot/cmem_locs_covar_250m.rds"))

    # 0m
    dat0_DOatm <- dat0 %>%
      mutate(pO2_0 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))
    thresh0 <- do_to_atm(do = 2, t = median(dat0_DOatm$votemper_mean, na.rm = TRUE), s = median(dat0_DOatm$vosaline_mean, na.rm = TRUE), thresh = TRUE) #defualt do value is 2 mL/L from vetter et al., 2008

    # 60m
    dat60_DOatm <- dat60 %>%
      mutate(pO2_60 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))
    thresh60 <- do_to_atm(do = 2, t = median(dat60_DOatm$votemper_mean, na.rm = TRUE), s = median(dat60_DOatm$vosaline_mean, na.rm = TRUE), thresh = TRUE)

    #250m 
    dat250_DOatm <- dat250 %>%
      mutate(pO2_250 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))
    thresh250 <- do_to_atm(do = 2, t = median(dat250_DOatm$votemper_mean, na.rm = TRUE), s = median(dat250_DOatm$vosaline_mean, na.rm = TRUE), thresh = TRUE)

#seasonal
    dat0_seas <- readRDS(here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_0m_seas.rds"))
    dat60_seas <- readRDS(here("data/locs_w_covar/psat_spot_backup/seasonal/cmem_locs_covar_60m_seas.rds"))
    dat250_seas <- readRDS(here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_250m_seas.rds"))

    # 0m
    dat0_DOatm_seas <- dat0_seas %>%
      mutate(pO2_0 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))
    thresh0_seas <- do_to_atm(do = 2, t = median(dat0_DOatm_seas$votemper_mean, na.rm = TRUE), s = median(dat0_DOatm_seas$vosaline_mean, na.rm = TRUE), thresh = TRUE) #defualt do value is 2 mL/L from vetter et al., 2008

    # 60m
    dat60_DOatm_seas <- dat60_seas %>%
      mutate(pO2_60 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))
    thresh60_seas <- do_to_atm(do = 2, t = median(dat60_DOatm_seas$votemper_mean, na.rm = TRUE), s = median(dat60_DOatm_seas$vosaline_mean, na.rm = TRUE), thresh = TRUE)

    #250m 
    dat250_DOatm_seas <- dat250_seas %>%
      mutate(pO2_250 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))
    thresh250_seas <- do_to_atm(do = 2, t = median(dat250_DOatm_seas$votemper_mean, na.rm = TRUE), s = median(dat250_DOatm_seas$vosaline_mean, na.rm = TRUE), thresh = TRUE)

#annual
    dat0_ann <- readRDS(here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_0m_ann.rds"))
    dat60_ann <- readRDS(here("data/locs_w_covar/psat_spot_backup/annual/cmem_locs_covar_60m_ann.rds"))
    dat250_ann <- readRDS(here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_250m_ann.rds"))

    # 0m 
    dat0_DOatm_ann <- dat0_ann %>%
      mutate(pO2_0 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))
    thresh0_ann <- do_to_atm(do = 2, t = median(dat0_DOatm_ann$votemper_mean, na.rm = TRUE), s = median(dat0_DOatm_ann$vosaline_mean, na.rm = TRUE), thresh = TRUE) #defualt do value is 2 mL/L from vetter et al., 2008

    # 60m 
    dat60_DOatm_ann <- dat60_ann %>%
      mutate(pO2_60 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))
    thresh60_ann <- do_to_atm(do = 2, t = median(dat60_DOatm_ann$votemper_mean, na.rm = TRUE), s = median(dat60_DOatm_ann$vosaline_mean, na.rm = TRUE), thresh = TRUE)

    #250m 
    dat250_DOatm_ann <- dat250_ann %>%
      mutate(pO2_250 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))
    thresh250_ann <- do_to_atm(do = 2, t = median(dat250_DOatm_ann$votemper_mean, na.rm = TRUE), s = median(dat250_DOatm_ann$vosaline_mean, na.rm = TRUE), thresh = TRUE)

##### Calculate AGI ####
#calculate temp pref
Tpref50 = 16.45201 #50m tpref is 16.452

#daily 
dat0_DOatm$O2_demand0 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh0, T_C = dat0_DOatm$votemper_mean)
dat60_DOatm$O2_demand60 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh60, T_C = dat60_DOatm$votemper_mean)
dat250_DOatm$O2_demand250 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh250, T_C = dat250_DOatm$votemper_mean)

dat0_DOatm$AGI_0m <- dat0_DOatm$pO2_0/dat0_DOatm$O2_demand0
dat60_DOatm$AGI_60m <- dat60_DOatm$pO2_60/dat60_DOatm$O2_demand60
dat250_DOatm$AGI_250m <- dat250_DOatm$pO2_250/dat250_DOatm$O2_demand250

saveRDS(dat0_DOatm, here("data/locs_w_covar/explore/cmem_locs_covar_AGI_0m_daily.rds"))
saveRDS(dat60_DOatm, here("data/locs_w_covar/explore/cmem_locs_covar_AGI_60m_daily.rds"))
saveRDS(dat250_DOatm, here("data/locs_w_covar/explore/cmem_locs_covar_AGI_250m_daily.rds"))

#seasonal
dat0_DOatm_seas$O2_demand0 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh0_seas, T_C = dat0_DOatm_seas$votemper_mean)
dat60_DOatm_seas$O2_demand60 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh60_seas, T_C = dat60_DOatm_seas$votemper_mean)
dat250_DOatm_seas$O2_demand250 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh250_seas, T_C = dat250_DOatm_seas$votemper_mean)

dat0_DOatm_seas$AGI_0m <- dat0_DOatm_seas$pO2_0/dat0_DOatm_seas$O2_demand0
dat60_DOatm_seas$AGI_60m <- dat60_DOatm_seas$pO2_60/dat60_DOatm_seas$O2_demand60
dat250_DOatm_seas$AGI_250m <- dat250_DOatm_seas$pO2_250/dat250_DOatm_seas$O2_demand250

saveRDS(dat0_DOatm_seas, here("data/locs_w_covar/explore/seasonal/cmem_locs_covar_AGI_0m_seas.rds"))
saveRDS(dat60_DOatm_seas, here("data/locs_w_covar/explore/seasonal/cmem_locs_covar_AGI_60m_seas.rds"))
saveRDS(dat250_DOatm_seas, here("data/locs_w_covar/explore/seasonal/cmem_locs_covar_AGI_250m_seas.rds"))

#annual
dat0_DOatm_ann$O2_demand0 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh0_ann, T_C = dat0_DOatm_ann$votemper_mean)
dat60_DOatm_ann$O2_demand60 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh60_ann, T_C = dat60_DOatm_ann$votemper_mean)
dat250_DOatm_ann$O2_demand250 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh250_ann, T_C = dat250_DOatm_ann$votemper_mean)

dat0_DOatm_ann$AGI_0m <- dat0_DOatm_ann$pO2_0/dat0_DOatm_ann$O2_demand0
dat60_DOatm_ann$AGI_60m <- dat60_DOatm_ann$pO2_60/dat60_DOatm_ann$O2_demand60
dat250_DOatm_ann$AGI_250m <- dat250_DOatm_ann$pO2_250/dat250_DOatm_ann$O2_demand250

saveRDS(dat0_DOatm_ann, here("data/locs_w_covar/explore/annual/cmem_locs_covar_AGI_0m_ann.rds"))
saveRDS(dat60_DOatm_ann, here("data/locs_w_covar/explore/annual/cmem_locs_covar_AGI_60m_ann.rds"))
saveRDS(dat250_DOatm_ann, here("data/locs_w_covar/explore/annual/cmem_locs_covar_AGI_250m_ann.rds"))

#### format data for BRTs ####
format_dat_crw_brts <- function(dat0_path, dat60_path, dat250_path, res = c("ann", "seas", "daily"), out_path){
  
  set.seed(1004)
  
  #combine all before PA selection and fix PA values before fitting BRTs

  if(res == "daily"){
  dat0 <- readRDS(here(dat0_path))
  dat0$bathy <- replace(dat0$bathy, dat0$bathy == "NaN", NA)
  colnames(dat0) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "o2_mean_0m", "chl_mean", "temp_mean", "sal_mean",  "uo_mean", "uostr_mean", "vo_mean", "vostr_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "pO2_atm_0m", "o2_demand_0m", "AGI_0m")
   
  dat60 <- readRDS(here(dat60_path))
  colnames(dat60) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "o2_mean_60m", "temp_mean", "sal_mean", "pO2_atm_60m", "o2_demand_60m", "AGI_60m")
  
  dat250 <- readRDS(here(dat250_path))
  colnames(dat250) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "o2_mean_250m", "temp_mean", "sal_mean", "pO2_atm_250m", "o2_demand_250m", "AGI_250m")
  
  dat_all_temp <- dat0 %>% cbind(dat250$o2_mean_250m, dat250$AGI_250m, dat60$o2_mean_60m, dat60$AGI_60m)
  }

  if(res == "seas"){
  dat0 <- readRDS(here(dat0_path))
  dat0$bathy <- replace(dat0$bathy, dat0$bathy == "NaN", NA)
  colnames(dat0) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_seas", "mld_mean", "sal_mean", "ssh_mean", "temp_mean", "uo_mean", "uostr_mean", "vo_mean", "vostr_mean", "o2_mean_0m", "chl_mean", "bathy_mean", "bathy_sd", "pO2_atm_0m", "o2_demand_0m", "AGI_0m")
  dat0 <- dat0 %>% select(-c("dt_seas")) 
    
  dat60 <- readRDS(here(dat60_path))
  colnames(dat60) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_seas", "sal_mean", "temp_mean", "o2_mean_60m", "pO2_atm_60m", "o2_demand_60m", "AGI_60m")
  dat60 <- dat60 %>% select(-c("dt_seas")) 

  dat250 <- readRDS(here(dat250_path))
  colnames(dat250) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_seas", "sal_mean", "temp_mean", "o2_mean_250m", "pO2_atm_250m", "o2_demand_250m", "AGI_250m")
  dat250 <- dat250 %>% select(-c("dt_seas")) 

    
  dat_all_temp <- dat0 %>% cbind(dat250$o2_mean_250m, dat250$AGI_250m, dat60$o2_mean_60m, dat60$AGI_60m)
  }

  if(res == "ann"){
  dat0 <- readRDS(here(dat0_path))
  dat0$bathy <- replace(dat0$bathy, dat0$bathy == "NaN", NA)
  colnames(dat0) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_ann", "mld_mean", "sal_mean", "ssh_mean", "temp_mean", "uo_mean", "uostr_mean", "vo_mean", "vostr_mean", "o2_mean_0m", "chl_mean", "bathy_mean", "bathy_sd", "pO2_atm_0m", "o2_demand_0m", "AGI_0m")
  dat0 <- dat0 %>% select(-c("dt_ann"))
    
  dat250 <- readRDS(here(dat250_path))
  colnames(dat250) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_ann", "sal_mean", "temp_mean", "o2_mean_250m", "pO2_atm_250m", "o2_demand_250m", "AGI_250m")
  dat250 <- dat250 %>% select(-c("dt_ann"))

    
  dat_all_temp <- dat0 %>% cbind(dat250$o2_mean_250m, dat250$AGI_250m, dat60$o2_mean_60m, dat60$AGI_60m)
  }
  
  #originally, PA = 0 means a true position. Change so PA = 1 means a true position for fitting the BRT
  dat_all_temp$PA <- replace(dat_all_temp$PA, dat_all_temp$PA == 1, 2) #change PAs to temporarily equal 2
  dat_all_temp$PA <- replace(dat_all_temp$PA, dat_all_temp$PA == 0, 1) #change true positions to a 1
  dat_all_temp$PA <- replace(dat_all_temp$PA, dat_all_temp$PA == 2, 0) #change PA positions to a 0
  
  # randomly select one PA for each tag (CRW dataset only)
  #randomly select 1 PA rep for each tag
  dat_pos <- dat_all_temp %>% filter(PA == 1)
  dat_pa <- dat_all_temp %>% filter(PA == 0)

  dat_temp <- data.frame()
  for(i in 1:length(unique(dat_pa$tag))){
    #select current id
    curr_ID <- unique(dat_pa$tag)[i]
    temp_df <- dat_pa[dat_pa$tag %in% curr_ID,]

    #sample id's randomly
    temp_rep_ID <- sample(unique(temp_df$rep), 1, replace = FALSE)

    #narrow your data set
    temp_df2 <- temp_df[temp_df$rep %in% temp_rep_ID, ]

    #combine in a single df
    dat_temp <- rbind(dat_temp, temp_df2)
  }
  
  dat_all_temp2 <- rbind(dat_temp, dat_pos)
  dat_all_temp3 <- dat_all_temp2 %>% filter(tag != 96365)
  dat_all_temp_fix <- dat_all_temp2 %>% 
    filter(tag == 96365) %>%
    group_by(PA) %>%
    distinct(dt, .keep_all = TRUE) %>%
    ungroup()
  
  dat_all <- rbind(dat_all_temp3, dat_all_temp_fix) %>%
    dplyr::rename(o2_mean_250m = "dat250$o2_mean_250m", 
                  AGI_250m = "dat250$AGI_250m", 
                  o2_mean_60m = "dat60$o2_mean_60m", 
                  AGI_60m = "dat60$AGI_60m")
  
  #combine depth specific data at 60m and 250m for DO and AGI. Erase for base model dataset. Here, I also change the PA values.
  #base model dataset
  dat_base <- dat_all %>% 
    subset(select = -c(o2_mean_0m, pO2_atm_0m, o2_demand_0m, AGI_0m, o2_mean_250m, AGI_250m, o2_mean_60m, AGI_60m))
  saveRDS(dat_base, here(paste0(out_path,"/dat_base","_", res,".rds")))
  
  #DO model dataset
  dat_do <- dat_all %>% 
    subset(select = -c(pO2_atm_0m, o2_demand_0m, AGI_0m, AGI_250m, AGI_60m))
  saveRDS(dat_do, here(paste0(out_path,"/dat_do","_", res,".rds")))
  
  #AGI model dataset 
  dat_agi <- dat_all %>% 
    subset(select = -c(o2_mean_0m, pO2_atm_0m, o2_demand_0m, o2_mean_250m, o2_mean_60m)) 
  saveRDS(dat_agi, here(paste0(out_path,"/dat_agi","_", res,".rds")))
  
  }

#Daily
dat0_path_dail <- "data/locs_w_covar/explore/cmem_locs_covar_AGI_0m_daily.rds"
dat60_path_dail <- "data/locs_w_covar/explore/cmem_locs_covar_AGI_60m_daily.rds"
dat250_path_dail <- "data/locs_w_covar/explore/cmem_locs_covar_AGI_250m_daily.rds"

out_path_dail <- "data/locs_brts/explore/crw_pas_dail"

format_dat_crw_brts(dat0_path = dat0_path_dail, dat60_path = dat60_path_dail, dat250_path = dat250_path_dail, res = "daily", out_path = out_path_dail)

#seasonal
dat0_path_seas <- "data/locs_w_covar/explore/seasonal/cmem_locs_covar_AGI_0m_seas.rds"
dat60_path_seas <- "data/locs_w_covar/explore/seasonal/cmem_locs_covar_AGI_60m_seas.rds"
dat250_path_seas <- "data/locs_w_covar/explore/seasonal/cmem_locs_covar_AGI_250m_seas.rds"

out_path_seas <- "data/locs_brts/explore/crw_pas_seas"

format_dat_crw_brts(dat0_path = dat0_path_seas, dat60_path = dat60_path_seas, dat250_path = dat250_path_seas, res = "seas", out_path = out_path_seas)

#annual
dat0_path_ann <- "data/locs_w_covar/explore/annual/cmem_locs_covar_AGI_0m_ann.rds"
dat60_path_ann <- "data/locs_w_covar/explore/annual/cmem_locs_covar_AGI_60m_ann.rds"
dat250_path_ann <- "data/locs_w_covar/explore/annual/cmem_locs_covar_AGI_250m_ann.rds"

out_path_ann <- "data/locs_brts/explore/crw_pas_ann"

format_dat_crw_brts(dat0_path = dat0_path_ann, dat60_path = dat60_path_ann, dat250_path = dat250_path_ann, res = "ann", out_path = out_path_ann)

#### Run BRTs ####
# CRW daily data 
dat_base_d <- readRDS(here("data/locs_brts/explore/crw_pas_dail/dat_base_daily.rds")) %>% mutate(tag = as.factor(tag))
dat_do_d <- readRDS(here("data/locs_brts/explore/crw_pas_dail/dat_do_daily.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_d <- readRDS(here("data/locs_brts/explore/crw_pas_dail/dat_agi_daily.rds")) %>% mutate(tag = as.factor(tag))

# CRW seasonal data 
dat_do_s <- readRDS(here("data/locs_brts/explore/crw_pas_seas/dat_do_seas.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_s <- readRDS(here("data/locs_brts/explore/crw_pas_seas/dat_agi_seas.rds")) %>% mutate(tag = as.factor(tag))

# CRW annual data 
dat_do_a <- readRDS(here("data/locs_brts/explore/crw_pas_ann/dat_do_ann.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_a <- readRDS(here("data/locs_brts/explore/crw_pas_ann/dat_agi_ann.rds")) %>% mutate(tag = as.factor(tag))

# Add seasonal and annual data to daily data df for DO and AGI
dat_do_all <- cbind(dat_do_d, dat_do_s$o2_mean_0m, dat_do_s$o2_mean_60m, dat_do_s$o2_mean_250m, dat_do_a$o2_mean_0m, dat_do_a$o2_mean_60m, dat_do_a$o2_mean_250m)
dat_do_all <- dat_do_all %>%
  dplyr::rename("o2_mean_0m_seas" = "dat_do_s$o2_mean_0m",
          "o2_mean_60m_seas" = "dat_do_s$o2_mean_60m",
         "o2_mean_250m_seas" = "dat_do_s$o2_mean_250m", 
         "o2_mean_0m_ann" = "dat_do_a$o2_mean_0m", 
         "o2_mean_60m_ann" = "dat_do_a$o2_mean_60m", 
         "o2_mean_250m_ann" = "dat_do_a$o2_mean_250m")

dat_agi_all <- cbind(dat_agi_d, dat_agi_s$AGI_0m, dat_agi_s$AGI_60m, dat_agi_s$AGI_250m, dat_agi_a$AGI_0m,dat_agi_a$AGI_60m, dat_agi_a$AGI_250m)
dat_agi_all <- dat_agi_all %>%
  dplyr::rename("AGI_0m_seas" = "dat_agi_s$AGI_0m",
          "AGI_60m_seas" = "dat_agi_s$AGI_60m",
         "AGI_250m_seas" = "dat_agi_s$AGI_250m", 
         "AGI_0m_ann" = "dat_agi_a$AGI_0m",
         "AGI_60m_ann" = "dat_agi_a$AGI_60m", 
         "AGI_250m_ann" = "dat_agi_a$AGI_250m")

# run brts
brt_run <- function(dat_file, save_file, pred_vars){
   set.seed(1004)   

    #test vs train files
    dat_file$row_id <- 1:nrow(dat_file)
    
    dat_temp <- dat_file %>% 
    sample_frac(0.25)
    
    dat_test <-  subset(dat_file, row_id %in% dat_temp$row_id)
  
    #save test file
    saveRDS(dat_test, file = here(paste0(save_file, "_test", ".rds")))
    
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
    
  saveRDS(brt_iter, file = here(paste0(save_file, ".rds"))) 
    } #end function


# base crw
pred_vars_base1 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd")
brt_run(dat_file = dat_base_d, pred_vars = pred_vars_base1, save_file = "data/brt/mod_outputs/explore/models/base_crw")

test_data1 <- readRDS(here("data/brt/mod_outputs/explore/models/base_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/base_crw.rds", 
            test_data = test_data1)

# agi_0m_daily_crw
pred_vars_agi1 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "AGI_0m")
brt_run(dat_file = dat_agi_all, pred_vars = pred_vars_agi1, save_file = "data/brt/mod_outputs/explore/models/agi_0m_daily_crw")

test_data2 <- readRDS(here("data/brt/mod_outputs/explore/models/agi_0m_daily_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/agi_0m_daily_crw.rds", 
            test_data = test_data2)

# agi_0m_60m_daily_crw
pred_vars_agi2 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "AGI_0m", "AGI_60m")
brt_run(dat_file = dat_agi_all, pred_vars = pred_vars_agi2, save_file = "data/brt/mod_outputs/explore/models/agi_0m_60m_daily_crw")

test_data3 <- readRDS(here("data/brt/mod_outputs/explore/models/agi_0m_60m_daily_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/agi_0m_60m_daily_crw.rds", 
            test_data = test_data3)

# agi_0m_250m_daily_crw
pred_vars_agi3 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "AGI_0m", "AGI_250m")
brt_run(dat_file = dat_agi_all, pred_vars = pred_vars_agi3, save_file = "data/brt/mod_outputs/explore/models/agi_0m_250m_daily_crw")

test_data4 <- readRDS(here("data/brt/mod_outputs/explore/models/agi_0m_250m_daily_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/agi_0m_250m_daily_crw.rds", 
            test_data = test_data4)

# do_0m_daily_crw
pred_vars_do1 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m")
brt_run(dat_file = dat_do_all, pred_vars = pred_vars_do1, save_file = "data/brt/mod_outputs/explore/models/do_0m_daily_crw")

test_data5 <- readRDS(here("data/brt/mod_outputs/explore/models/do_0m_daily_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/do_0m_daily_crw.rds", 
            test_data = test_data5)

# do_0m_60m_daily_crw
pred_vars_do2 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m", "o2_mean_60m")
brt_run(dat_file = dat_do_all, pred_vars = pred_vars_do2, save_file = "data/brt/mod_outputs/explore/models/do_0m_60m_daily_crw")

test_data6 <- readRDS(here("data/brt/mod_outputs/explore/models/do_0m_60m_daily_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/do_0m_60m_daily_crw.rds", 
            test_data = test_data6)

# do_0m_250m_daily_crw
pred_vars_do3 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m", "o2_mean_250m")
brt_run(dat_file = dat_do_all, pred_vars = pred_vars_do3, save_file = "data/brt/mod_outputs/explore/models/do_0m_250m_daily_crw")

test_data7 <- readRDS(here("data/brt/mod_outputs/explore/models/do_0m_250m_daily_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/do_0m_250m_daily_crw.rds", 
            test_data = test_data7)

# agi_all_depths_seasonal_crw
pred_vars_agi4 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "AGI_0m_seas", "AGI_60m_seas", "AGI_250m_seas")
brt_run(dat_file = dat_agi_all, pred_vars = pred_vars_agi4, save_file = "data/brt/mod_outputs/explore/models/agi_all_depths_seasonal_crw")

test_data8 <- readRDS(here("data/brt/mod_outputs/explore/models/agi_all_depths_seasonal_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/agi_all_depths_seasonal_crw.rds", 
            test_data = test_data8)

# agi_all_depths_annual_crw
pred_vars_agi5 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "AGI_0m_ann", "AGI_60m_ann", "AGI_250m_ann")
brt_run(dat_file = dat_agi_all, pred_vars = pred_vars_agi5, save_file = "data/brt/mod_outputs/explore/models/agi_all_depths_annual_crw")

test_data9 <- readRDS(here("data/brt/mod_outputs/explore/models/agi_all_depths_annual_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/agi_all_depths_annual_crw.rds", 
            test_data = test_data9)

# do_all_depths_seasonal_crw
pred_vars_do4 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m_seas", "o2_mean_60m_seas", "o2_mean_250m_seas")
brt_run(dat_file = dat_do_all, pred_vars = pred_vars_do4, save_file = "data/brt/mod_outputs/explore/models/do_all_depths_seasonal_crw")

test_data10 <- readRDS(here("data/brt/mod_outputs/explore/models/do_all_depths_seasonal_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/do_all_depths_seasonal_crw.rds", 
            test_data = test_data10)

# do_all_depths_annual_crw
pred_vars_do5 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m_ann", "o2_mean_60m_ann", "o2_mean_250m_ann")
brt_run(dat_file = dat_do_all, pred_vars = pred_vars_do5, save_file = "data/brt/mod_outputs/explore/models/do_all_depths_annual_crw")

test_data11 <- readRDS(here("data/brt/mod_outputs/explore/models/do_all_depths_annual_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/do_all_depths_annual_crw.rds", 
            test_data = test_data11)

# agi_all_depths_all_resolutions_crw
pred_vars_agi6 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "AGI_0m","AGI_60m", "AGI_250m", "AGI_0m_seas", "AGI_60m_seas", "AGI_0m_ann", "AGI_250m_seas", "AGI_250m_ann", "AGI_60m_ann")
brt_run(dat_file = dat_agi_all, pred_vars = pred_vars_agi6, save_file = "data/brt/mod_outputs/explore/models/agi_all_depths_all_resolutions_crw")

test_data12 <- readRDS(here("data/brt/mod_outputs/explore/models/agi_all_depths_all_resolutions_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/agi_all_depths_all_resolutions_crw.rds", 
            test_data = test_data12)

# do_all_depths_all_resolutions_crw
pred_vars_do6 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m", "o2_mean_60m", "o2_mean_250m", "o2_mean_0m_seas",  "o2_mean_60m_seas", "o2_mean_250m_seas", "o2_mean_0m_ann", "o2_mean_60m_ann", "o2_mean_250m_ann")
brt_run(dat_file = dat_do_all, pred_vars = pred_vars_do6, save_file = "data/brt/mod_outputs/explore/models/do_all_depths_all_resolutions_crw")

test_data13 <- readRDS(here("data/brt/mod_outputs/explore/models/do_all_depths_all_resolutions_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/do_all_depths_all_resolutions_crw.rds", 
            test_data = test_data13)

# agi_0m_250m_daily_seasonal_annual_crw
pred_vars_agi7 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "AGI_0m", "AGI_250m", "AGI_0m_seas", "AGI_0m_ann", "AGI_250m_seas", "AGI_250m_ann")
brt_run(dat_file = dat_all_agi, pred_vars = pred_vars_do7, save_file = "data/brt/mod_outputs/explore/models/agi_0m_250m_daily_seasonal_annual_crw")

test_data14 <- readRDS(here("data/brt/mod_outputs/explore/models/agi_0m_250m_daily_seasonal_annual_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/agi_0m_250m_daily_seasonal_annual_crw.rds", 
            test_data = test_data14)

# do_0m_250m_daily_seasonal_annual_crw
pred_vars_do7 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m", "o2_mean_250m", "o2_mean_0m_seas", "o2_mean_0m_ann", "o2_mean_250m_seas", "o2_mean_250m_ann")
brt_run(dat_file = dat_all_do, pred_vars = pred_vars_do7, save_file = "data/brt/mod_outputs/explore/models/do_0m_250m_daily_seasonal_annual_crw")

test_data15 <- readRDS(here("data/brt/mod_outputs/explore/models/do_0m_250m_daily_seasonal_annual_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/do_0m_250m_daily_seasonal_annual_crw.rds", 
            test_data = test_data15)

# do_agi_combo_crw
dat_do_agi_all <- cbind(dat_do_all, dat_agi_all[,c(19, 22, 24, 26, 27, 29)])
pred_vars_do_agi1 = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m", "o2_mean_0m_seas", "o2_mean_0m_ann", "AGI_250m", "AGI_250m_seas", "AGI_250m_ann")
brt_run(dat_file = dat_do_agi_all, pred_vars = pred_vars_do_agi1, save_file = "data/brt/mod_outputs/explore/models/do_agi_combo_crw")

test_data16 <- readRDS(here("data/brt/mod_outputs/explore/models/do_agi_combo_crw_test.rds"))
explore_brt(mod_file_path = "data/brt/mod_outputs/explore/models/do_agi_combo_crw.rds", 
            test_data = test_data16)

# do_agi_ensemble
    #run agi only model
pred_vars_agi8 = c("AGI_0m", "AGI_250m", "AGI_0m_seas", "AGI_0m_ann", "AGI_250m_seas", "AGI_250m_ann")
brt_run(dat_file = dat_agi_all, pred_vars = pred_vars_agi8, save_file = "data/brt/mod_outputs/explore/models/brt_agi_only")

    #load do final model 
    agi_mod <- readRDS(here("data/brt/mod_outputs/explore/models/brt_agi_only.rds"))
    do_mod <- readRDS(here("data/brt/mod_outputs/explore/models/do_0m_250m_daily_seasonal_annual_crw.rds"))

    # make df of predictions from each model
    test_df_do <- readRDS(here("data/brt/mod_outputs/explore/models/do_0m_250m_daily_seasonal_annual_crw_test.rds"))
    test_df_agi <- readRDS(here("data/brt/mod_outputs/explore/models/brt_agi_only_test.rds"))

    pred_testdata <- data.frame(
      do = predict.gbm(do_mod, test_df_do,
                      n.trees = do_mod$gbm.call$best.trees,
                      type = "response"),
      agi = predict.gbm(agi_mod, test_df_agi,
                        n.trees = agi_mod$gbm.call$best.trees,
                        type = "response")
    )

    summary(pred_testdata)

    # Mean of probabilities
    mean_prob <- rowMeans(pred_testdata)

    # performance measures for "mean of probabilities"
    (perf_mean_prob <- mecofun::evalSDM(test_df$PA, mean_prob))





# libraries ####
library(tidyverse)
library(here)

set.seed(1004)

#format data for brts#####
format_dat_crw_brts <- function(dat0_path, dat250_path, res = c("ann", "seas", "daily"), out_path){
  
  set.seed(1004)
  
  #combine all before PA selection and fix PA values before fitting BRTs

  if(res == "daily"){
  dat0 <- readRDS(here(dat0_path))
  dat0$bathy <- replace(dat0$bathy, dat0$bathy == "NaN", NA)
  colnames(dat0) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "o2_mean_0m", "chl_mean", "temp_mean", "sal_mean",  "uo_mean", "uostr_mean", "vo_mean", "vostr_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "pO2_atm_0m", "o2_demand_0m", "AGI_0m")
                  
  dat250 <- readRDS(here(dat250_path))
  colnames(dat250) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "o2_mean_250m", "temp_mean", "sal_mean", "pO2_atm_250m", "o2_demand_250m", "AGI_250m")
  
  dat_all_temp <- dat0 %>% cbind(dat250$o2_mean_250m, dat250$AGI_250m)
  }

  if(res == "seas"){
  dat0 <- readRDS(here(dat0_path))
  dat0$bathy <- replace(dat0$bathy, dat0$bathy == "NaN", NA)
  colnames(dat0) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_seas", "mld_mean", "sal_mean", "ssh_mean", "temp_mean", "uo_mean", "uostr_mean", "vo_mean", "vostr_mean", "o2_mean_0m", "chl_mean", "bathy_mean", "bathy_sd", "pO2_atm_0m", "o2_demand_0m", "AGI_0m")
  dat0 <- dat0 %>% select(-c("dt_seas")) 
    
  dat250 <- readRDS(here(dat250_path))
  colnames(dat250) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_seas", "sal_mean", "temp_mean", "o2_mean_250m", "pO2_atm_250m", "o2_demand_250m", "AGI_250m")
  dat250 <- dat250 %>% select(-c("dt_seas")) 

    
  dat_all_temp <- dat0 %>% cbind(dat250$o2_mean_250m, dat250$AGI_250m)
  }

  if(res == "ann"){
  dat0 <- readRDS(here(dat0_path))
  dat0$bathy <- replace(dat0$bathy, dat0$bathy == "NaN", NA)
  colnames(dat0) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_ann", "mld_mean", "sal_mean", "ssh_mean", "temp_mean", "uo_mean", "uostr_mean", "vo_mean", "vostr_mean", "o2_mean_0m", "chl_mean", "bathy_mean", "bathy_sd", "pO2_atm_0m", "o2_demand_0m", "AGI_0m")
  dat0 <- dat0 %>% select(-c("dt_ann"))
    
  dat250 <- readRDS(here(dat250_path))
  colnames(dat250) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_ann", "sal_mean", "temp_mean", "o2_mean_250m", "pO2_atm_250m", "o2_demand_250m", "AGI_250m")
  dat250 <- dat250 %>% select(-c("dt_ann"))

    
  dat_all_temp <- dat0 %>% cbind(dat250$o2_mean_250m, dat250$AGI_250m)
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
           AGI_250m = "dat250$AGI_250m")
  
  #combine depth specific data at 60m and 250m for DO and AGI. Erase for base model dataset. Here, I also change the PA values.
  #base model dataset
  dat_base <- dat_all %>% 
    subset(select = -c(o2_mean_0m, pO2_atm_0m, o2_demand_0m, AGI_0m, o2_mean_250m, AGI_250m))
  saveRDS(dat_base, here(paste0(out_path,"/dat_base","_", res,".rds")))
  
  #DO model dataset
  dat_do <- dat_all %>% 
    subset(select = -c(pO2_atm_0m, o2_demand_0m, AGI_0m, AGI_250m))
  saveRDS(dat_do, here(paste0(out_path,"/dat_do","_", res,".rds")))
  
  #AGI model dataset 
  dat_agi <- dat_all %>% 
    subset(select = -c(o2_mean_0m, pO2_atm_0m, o2_demand_0m, o2_mean_250m)) 
  saveRDS(dat_agi, here(paste0(out_path,"/dat_agi","_", res,".rds")))
  
  }

#Daily
dat0_path_dail <- "data/locs_w_covar/psat_spot/cmem_locs_covar_AGI_0m_daily.rds"
dat250_path_dail <- "data/locs_w_covar/psat_spot/cmem_locs_covar_AGI_250m_daily.rds"

out_path_dail <- "data/locs_brts/crw_pas_dail"

format_dat_crw_brts(dat0_path = dat0_path_dail, dat250_path = dat250_path_dail, res = "daily", out_path = out_path_dail)

#seasonal
dat0_path_seas <- "data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_AGI_0m_seas.rds"
dat250_path_seas <- "data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_AGI_250m_seas.rds"

out_path_seas <- "data/locs_brts/crw_pas_seas"

format_dat_crw_brts(dat0_path = dat0_path_seas, dat250_path = dat250_path_seas, res = "seas", out_path = out_path_seas)

#annual
dat0_path_ann <- "data/locs_w_covar/psat_spot/annual/cmem_locs_covar_AGI_0m_ann.rds"
dat250_path_ann <- "data/locs_w_covar/psat_spot/annual/cmem_locs_covar_AGI_250m_ann.rds"

out_path_ann <- "data/locs_brts/crw_pas_ann"

format_dat_crw_brts(dat0_path = dat0_path_ann, dat250_path = dat250_path_ann, res = "ann", out_path = out_path_ann)


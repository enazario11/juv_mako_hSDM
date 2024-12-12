### libraries ####
{library(tidyverse)
library(terra)
library(sf)
library(patchwork)
library(tidyterra)
library(gbm)
library(dismo)
library(here);here <- here::here #plyr's here function masks here::here
library(tidyquant)
library(lessR)
library(rstatix)
library(ggpubr)

set.seed(1004)}

### raster generation function ####
hsi_rast_gen <- function(date_start = c("2003-01-01"), date_end = c("2015-12-31"), season = "WSpSuF", output_name){
  
  #load rast files -------------------------------------------------------------------------------------
  rast_file_daily_0m <- rast(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_CHL_Temp_SO_UO_UOSTR_VO_VOSTR_SSH_MLD_0m_Jan2003_Dec2015_0.25_D.nc"))
  rast_file_daily_250m <- rast(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_Temp_SO_250m_Jan2003_Dec2015_0.25_D.nc"))
  daily_rast_varnames_0m <- c(replicate(4748, "o2"), replicate(4748, "chl"),replicate(4748, "votemper"),replicate(4748, "vosaline"),replicate(4748, "vozocrtx"),replicate(4748, "sozotaux"),replicate(4748, "vomecrty"),replicate(4748, "sometauy"),replicate(4748, "sossheig"),replicate(4748, "somxlavt"))
  daily_rast_varnames_250m <- c(replicate(4748, "o2"), replicate(4748, "votemper"), replicate(4748, "vosaline"))
  names(rast_file_daily_0m) <- daily_rast_varnames_0m
  names(rast_file_daily_250m) <- daily_rast_varnames_250m
  
  rast_file_seas_0m <- rast(here("data/enviro/psat_spot_all/all_processed/season_res/dat_0m_season.nc"))
  rast_file_seas_250m <- rast(here("data/enviro/psat_spot_all/all_processed/season_res/dat_250m_season.nc"))
  
  rast_file_ann_0m <- rast(here("data/enviro/psat_spot_all/all_processed/annual_res/dat_0m_annual.nc"))
  rast_file_ann_250m <- rast(here("data/enviro/psat_spot_all/all_processed/annual_res/dat_250m_annual.nc"))
  
  #get start and end dates into date object --------------------------------------------------------------
  date_start <- as.Date(date_start)
  date_end <- as.Date(date_end)
  
  #subset daily raster files ------------------------------------------------------------------------------
  rast_daily_0m_sub <- subset(rast_file_daily_0m, time(rast_file_daily_0m) >= date_start & time(rast_file_daily_0m) <= date_end)
  rast_daily_250m_sub <- subset(rast_file_daily_250m, time(rast_file_daily_250m) >= date_start & time(rast_file_daily_250m) <= date_end)
  
  if(length(unique(time(rast_daily_0m_sub))) > 1){
    rast_daily_0m_sub <- tapp(rast_daily_0m_sub, names(rast_daily_0m_sub), mean)
    rast_daily_250m_sub <- tapp(rast_daily_250m_sub, names(rast_daily_250m_sub), mean)
  }
 
  #subset seasonal raster files ----------------------------------------------------------------------------
  seas_0m_sub_rasts <- list()
  seas_250m_sub_rasts <- list()
  
    #subset rasters for seasons that are present
  if(str_detect(season, "W")){
    seas_0m_sub_rasts$rast_seas_0m_subW <- subset(rast_file_seas_0m, month(time(rast_file_seas_0m)) == 12)
    seas_250m_sub_rasts$rast_seas_250m_subW <- subset(rast_file_seas_250m, month(time(rast_file_seas_250m)) == 12)
  } 
  if(str_detect(season, "Sp")) {
    seas_0m_sub_rasts$rast_seas_0m_subSp <- subset(rast_file_seas_0m, month(time(rast_file_seas_0m)) == 3)
    seas_250m_sub_rasts$rast_seas_250m_subSp <- subset(rast_file_seas_250m, month(time(rast_file_seas_250m)) == 3)
  } 
  if(str_detect(season, "Su")) {
    seas_0m_sub_rasts$rast_seas_0m_subSu <- subset(rast_file_seas_0m, month(time(rast_file_seas_0m)) == 6)
    seas_250m_sub_rasts$rast_seas_250m_subSu <- subset(rast_file_seas_250m, month(time(rast_file_seas_250m)) == 6)
  } 
  if(str_detect(season, "F")){
    seas_0m_sub_rasts$rast_seas_0m_subF <- subset(rast_file_seas_0m, month(time(rast_file_seas_0m)) == 9)
    seas_250m_sub_rasts$rast_seas_250m_subF <- subset(rast_file_seas_250m, month(time(rast_file_seas_250m)) == 9)
  }
  
    #generate mean season raster if date range includes multiple seasons
  if(length(seas_0m_sub_rasts) == 1){
    rast_seas_0m_sub <- rast(seas_0m_sub_rasts[1])
    rast_seas_250m_sub <- rast(seas_250m_sub_rasts[1])
  }
  if(length(seas_0m_sub_rasts) == 2){
    rast_seas_0m_sub <- mean(rast(seas_0m_sub_rasts[1]), rast(seas_0m_sub_rasts[2]))
    rast_seas_250m_sub <- mean(rast(seas_250m_sub_rasts[1]), rast(seas_250m_sub_rasts[2]))
  }
  if(length(seas_0m_sub_rasts) == 3){
    rast_seas_0m_sub <- mean(rast(seas_0m_sub_rasts[1]), rast(seas_0m_sub_rasts[2]), rast(seas_0m_sub_rasts[3]))
    rast_seas_250m_sub <- mean(rast(seas_250m_sub_rasts[1]), rast(seas_250m_sub_rasts[2]), rast(seas_250m_sub_rasts[3]))
  }
  if(length(seas_0m_sub_rasts) == 4){
    rast_seas_0m_sub <- mean(rast(seas_0m_sub_rasts[1]), rast(seas_0m_sub_rasts[2]), rast(seas_0m_sub_rasts[3]), rast(seas_0m_sub_rasts[4]))
    rast_seas_250m_sub <- mean(rast(seas_250m_sub_rasts[1]), rast(seas_250m_sub_rasts[2]), rast(seas_250m_sub_rasts[3]), rast(seas_250m_sub_rasts[4]))
  }
  
  terra::time(rast_seas_0m_sub) <- NULL
  terra::time(rast_seas_250m_sub) <- NULL
  
  names(rast_seas_0m_sub) <- c("somxlavt_1", "vosaline_1", "sossheig_1", "votemper_1", "vozocrtx_1", "sozotaux_1", "vomecrty_1", "sometauy_1", "chl_1", "o2_1")
  names(rast_seas_250m_sub) <- c("vosaline_1", "votemper_1", "o2_1")
  
  
  #subset annual raster files ---------------------------------------------------------------------------------
  rast_ann_0m_sub <- NULL
  rast_ann_250m_sub <- NULL
  
  for(i in 1:length(substr(seq(date_start, date_end, "years"), 1, 4))){
    curr_year = substr(seq(date_start, date_end, "years"), 1, 4)[i]
    
    ann_0m_sub_rast_temp <- subset(rast_file_ann_0m, year(time(rast_file_ann_0m)) == as.numeric(curr_year))
    ann_250m_sub_rast_temp <- subset(rast_file_ann_250m, year(time(rast_file_ann_250m)) == as.numeric(curr_year))
    
    rast_ann_0m_sub <- c(rast_ann_0m_sub, ann_0m_sub_rast_temp)
    rast_ann_250m_sub <- c(rast_ann_250m_sub, ann_250m_sub_rast_temp)
  }
  
    #generate mean annual raster if date range includes multiple years
  rast_ann_0m_sub <- rast(rast_ann_0m_sub)
  rast_ann_250m_sub <- rast(rast_ann_250m_sub)
  
  if(length(unique(year(time(rast_ann_0m_sub)))) > 1){
    
  rast_ann_0m_sub <- tapp(rast_ann_0m_sub, unique(varnames(rast_ann_0m_sub)), mean)
  rast_ann_250m_sub <- tapp(rast_ann_250m_sub, unique(varnames(rast_ann_250m_sub)), mean)
  
  }
  
  names(rast_ann_0m_sub) <- c("somxlavt", "vosaline", "sossheig", "votemper", "vozocrtx", "sozotaux", "vomecrty", "sometauy", "chl", "o2")
  names(rast_ann_250m_sub) <- c("vosaline", "votemper", "o2")
  
  terra::time(rast_ann_0m_sub) <- NULL
  terra::time(rast_ann_250m_sub) <- NULL
  
  #get bathy rasters ------------------------------------------------------------------------------------------
  bathy <- rast(here("data/enviro/psat_spot_all/bathy_gebco/processed/gebco_bathy_0.25deg2.nc"))
  
    #generate rugosity raster
  #rug <- rast(here("data/enviro/psat_spot_all/bathy_gebco/gebco_2023_n49.0_s1.0_w-153.0_e-103.0.nc"))
  #bathy_sd <- focal(rug, w = 59, fun = "sd", na.rm = TRUE)
  #bathy_sd1 <- aggregate(bathy_sd, fact = 60)
  #bathy_sd2 <- writeCDF(bathy_sd1, filename = "data/enviro/psat_spot_all/bathy_gebco/processed/bathy_sd_0.25.nc")
  
  bathy_sd <- rast(here("data/enviro/psat_spot_all/bathy_gebco/processed/bathy_sd_0.25.nc"))
  
  #generate AGI rasters ---------------------------------------------------------------------------------------
  source(here("functions/oxy_demand_functions.R"))
  OxyThresh_0m = 0.04928389 #value converted from Vetter concentration data to atm according to salinity, temp, and pressure at 0m
  OxyThresh_250m = 0.03816138 #value converted from Vetter concentration data to atm according to salinity, temp, and pressure at 250m
  Tpref = 16.45201 #mean temp experienced by sharks at mean dive depth (50m)
  
  dir.create(here(paste0("data/enviro/psat_spot_all/hsi_rasts/agi_rasts/", output_name)), showWarnings = FALSE) 
  
    #daily rast AGI
  demand_daily_0m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_0m, T_C = rast_daily_0m_sub$votemper)
  atm_daily_0m <- rast_to_atm(do = rast_daily_0m_sub$o2, so = rast_daily_0m_sub$vosaline, temp = rast_daily_0m_sub$votemper, depth = 0)
  AGI_daily_0m <- atm_daily_0m/demand_daily_0m
  writeCDF(AGI_daily_0m, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts/agi_rasts/", output_name, "/", output_name, "_daily_agi_0m.nc")))
  
  demand_daily_250m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_250m, T_C = rast_daily_250m_sub$votemper)
  atm_daily_250m <- rast_to_atm(do = rast_daily_250m_sub$o2, so = rast_daily_250m_sub$vosaline, temp = rast_daily_250m_sub$votemper, depth = 250)
  AGI_daily_250m <- atm_daily_250m/demand_daily_250m
  writeCDF(AGI_daily_250m, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts/agi_rasts/", output_name,"/", output_name, "_daily_agi_250m.nc")))
  
    #seas rast AGI
  demand_seas_0m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_0m, T_C = rast_seas_0m_sub$votemper_1)
  atm_seas_0m <- rast_to_atm(do = rast_seas_0m_sub$o2_1, so = rast_seas_0m_sub$vosaline_1, temp = rast_seas_0m_sub$votemper_1, depth = 0)
  AGI_seas_0m <- atm_seas_0m/demand_seas_0m
  
  demand_seas_250m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_250m, T_C = rast_seas_250m_sub$votemper_1)
  atm_seas_250m <- rast_to_atm(do = rast_seas_250m_sub$o2_1, so = rast_seas_250m_sub$vosaline_1, temp = rast_seas_250m_sub$votemper_1, depth = 250)
  AGI_seas_250m <- atm_seas_250m/demand_seas_250m
  
   #ann rast AGI
  demand_ann_0m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_0m, T_C = rast_ann_0m_sub$votemper)
  atm_ann_0m <- rast_to_atm(do = rast_ann_0m_sub$o2, so = rast_ann_0m_sub$vosaline, temp = rast_ann_0m_sub$votemper, depth = 0)
  AGI_ann_0m <- atm_ann_0m/demand_ann_0m
  
  demand_ann_250m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_250m, T_C = rast_ann_250m_sub$votemper)
  atm_ann_250m <- rast_to_atm(do = rast_ann_250m_sub$o2, so = rast_ann_250m_sub$vosaline, temp = rast_ann_250m_sub$votemper, depth = 250)
  AGI_ann_250m <- atm_ann_250m/demand_ann_250m
  
  #combine rasters --------------------------------------------------------------------------------------------
    #base model
  base_rast <- c(bathy, rast_daily_0m_sub$votemper, rast_daily_0m_sub$vosaline,  rast_daily_0m_sub$chl, rast_daily_0m_sub$sossheig, bathy_sd, rast_daily_0m_sub$somxlavt)
  names(base_rast) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
    
    #DO model
  do_rast <- c(rast_daily_0m_sub$o2, rast_ann_250m_sub$o2, rast_seas_0m_sub$o2_1, rast_daily_0m_sub$votemper, rast_seas_250m_sub$o2_1, bathy, rast_daily_0m_sub$vosaline, rast_daily_0m_sub$chl, rast_ann_0m_sub$o2, rast_daily_250m_sub$o2, rast_daily_0m_sub$sossheig, rast_daily_0m_sub$somxlavt, bathy_sd)
  names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
    
    #AGI model 
  agi_rast <- c(rast_daily_0m_sub$votemper, AGI_ann_250m, AGI_daily_0m, bathy, AGI_seas_0m, rast_daily_0m_sub$vosaline, AGI_seas_250m, AGI_ann_0m, rast_daily_0m_sub$chl, AGI_daily_250m, bathy_sd, rast_daily_0m_sub$somxlavt, rast_daily_0m_sub$sossheig)
  names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  
  #comb model
  comb_rast <- c(rast_daily_0m_sub$votemper, AGI_ann_250m, bathy, rast_daily_0m_sub$vosaline, AGI_seas_250m, rast_daily_0m_sub$chl, AGI_daily_250m, bathy_sd, rast_daily_0m_sub$somxlavt, rast_daily_0m_sub$sossheig, rast_daily_0m_sub$o2, rast_ann_0m_sub$o2, rast_seas_0m_sub$o2_1)
  names(comb_rast) <- c("temp_mean", "AGI_250m_ann", "bathy_mean", "sal_mean", "AGI_250m_seas", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean", "o2_mean_0m", "o2_mean_0m_ann", "o2_mean_0m_seas")
  
  #save HSI raster input file ---------------------------------------------------------------------------------
  dir.create(here(paste0("data/enviro/psat_spot_all/hsi_rasts/", output_name)), showWarnings = FALSE) 
  
    #base model
  writeCDF(base_rast, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts/", output_name,"/", output_name,"_base_rast.nc")))
  
    #DO model
  writeCDF(do_rast, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts/", output_name,"/", output_name,"_do_rast.nc")))
  
    #AGI model
  writeCDF(agi_rast, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts/", output_name,"/", output_name,"_agi_rast.nc")))
  
  #comb model
  writeCDF(comb_rast, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts/", output_name,"/", output_name,"_combo_rast.nc")))

  
# end function  
}

### raster generation function ####
hsi_rast_gen_sd <- function(date_start = c("2003-01-01"), date_end = c("2015-12-31"), season = "WSpSuF", output_name){
  
  #load rast files -------------------------------------------------------------------------------------
  rast_file_daily_0m <- rast(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_CHL_Temp_SO_UO_UOSTR_VO_VOSTR_SSH_MLD_0m_Jan2003_Dec2015_0.25_D.nc"))
  rast_file_daily_250m <- rast(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_Temp_SO_250m_Jan2003_Dec2015_0.25_D.nc"))
  daily_rast_varnames_0m <- c(replicate(4748, "o2"), replicate(4748, "chl"),replicate(4748, "votemper"),replicate(4748, "vosaline"),replicate(4748, "vozocrtx"),replicate(4748, "sozotaux"),replicate(4748, "vomecrty"),replicate(4748, "sometauy"),replicate(4748, "sossheig"),replicate(4748, "somxlavt"))
  daily_rast_varnames_250m <- c(replicate(4748, "o2"), replicate(4748, "votemper"), replicate(4748, "vosaline"))
  names(rast_file_daily_0m) <- daily_rast_varnames_0m
  names(rast_file_daily_250m) <- daily_rast_varnames_250m
  
  rast_file_seas_0m <- rast(here("data/enviro/psat_spot_all/all_processed/season_res/dat_0m_season.nc"))
  rast_file_seas_250m <- rast(here("data/enviro/psat_spot_all/all_processed/season_res/dat_250m_season.nc"))
  
  rast_file_ann_0m <- rast(here("data/enviro/psat_spot_all/all_processed/annual_res/dat_0m_annual.nc"))
  rast_file_ann_250m <- rast(here("data/enviro/psat_spot_all/all_processed/annual_res/dat_250m_annual.nc"))
  
  #get start and end dates into date object --------------------------------------------------------------
  date_start <- as.Date(date_start)
  date_end <- as.Date(date_end)
  
  #subset daily raster files ------------------------------------------------------------------------------
  rast_daily_0m_sub <- subset(rast_file_daily_0m, time(rast_file_daily_0m) >= date_start & time(rast_file_daily_0m) <= date_end)
  rast_daily_250m_sub <- subset(rast_file_daily_250m, time(rast_file_daily_250m) >= date_start & time(rast_file_daily_250m) <= date_end)
  
  if(length(unique(time(rast_daily_0m_sub))) > 1){
    rast_daily_0m_sub <- tapp(rast_daily_0m_sub, names(rast_daily_0m_sub), sd)
    rast_daily_250m_sub <- tapp(rast_daily_250m_sub, names(rast_daily_250m_sub), sd)
  }
  
  #subset seasonal raster files ----------------------------------------------------------------------------
  seas_0m_sub_rasts <- list()
  seas_250m_sub_rasts <- list()
  
  #subset rasters for seasons that are present
  if(str_detect(season, "W")){
    seas_0m_sub_rasts$rast_seas_0m_subW <- subset(rast_file_seas_0m, month(time(rast_file_seas_0m)) == 12)
    seas_250m_sub_rasts$rast_seas_250m_subW <- subset(rast_file_seas_250m, month(time(rast_file_seas_250m)) == 12)
  } 
  if(str_detect(season, "Sp")) {
    seas_0m_sub_rasts$rast_seas_0m_subSp <- subset(rast_file_seas_0m, month(time(rast_file_seas_0m)) == 3)
    seas_250m_sub_rasts$rast_seas_250m_subSp <- subset(rast_file_seas_250m, month(time(rast_file_seas_250m)) == 3)
  } 
  if(str_detect(season, "Su")) {
    seas_0m_sub_rasts$rast_seas_0m_subSu <- subset(rast_file_seas_0m, month(time(rast_file_seas_0m)) == 6)
    seas_250m_sub_rasts$rast_seas_250m_subSu <- subset(rast_file_seas_250m, month(time(rast_file_seas_250m)) == 6)
  } 
  if(str_detect(season, "F")){
    seas_0m_sub_rasts$rast_seas_0m_subF <- subset(rast_file_seas_0m, month(time(rast_file_seas_0m)) == 9)
    seas_250m_sub_rasts$rast_seas_250m_subF <- subset(rast_file_seas_250m, month(time(rast_file_seas_250m)) == 9)
  }
  
  #generate mean season raster if date range includes multiple seasons
  if(length(seas_0m_sub_rasts) == 1){
    rast_seas_0m_sub <- rast(seas_0m_sub_rasts[1])
    rast_seas_250m_sub <- rast(seas_250m_sub_rasts[1])
  }
  if(length(seas_0m_sub_rasts) == 2){
    rast_seas_0m_sub <- stdev(rast(seas_0m_sub_rasts[1]), rast(seas_0m_sub_rasts[2]))
    rast_seas_250m_sub <- stdev(rast(seas_250m_sub_rasts[1]), rast(seas_250m_sub_rasts[2]))
  }
  if(length(seas_0m_sub_rasts) == 3){
    rast_seas_0m_sub <- stdev(rast(seas_0m_sub_rasts[1]), rast(seas_0m_sub_rasts[2]), rast(seas_0m_sub_rasts[3]))
    rast_seas_250m_sub <- stdev(rast(seas_250m_sub_rasts[1]), rast(seas_250m_sub_rasts[2]), rast(seas_250m_sub_rasts[3]))
  }
  if(length(seas_0m_sub_rasts) == 4){
    rast_seas_0m_sub <- stdev(rast(seas_0m_sub_rasts[1]), rast(seas_0m_sub_rasts[2]), rast(seas_0m_sub_rasts[3]), rast(seas_0m_sub_rasts[4]))
    rast_seas_250m_sub <- stdev(rast(seas_250m_sub_rasts[1]), rast(seas_250m_sub_rasts[2]), rast(seas_250m_sub_rasts[3]), rast(seas_250m_sub_rasts[4]))
  }
  
  terra::time(rast_seas_0m_sub) <- NULL
  terra::time(rast_seas_250m_sub) <- NULL
  
  names(rast_seas_0m_sub) <- c("somxlavt_1", "vosaline_1", "sossheig_1", "votemper_1", "vozocrtx_1", "sozotaux_1", "vomecrty_1", "sometauy_1", "chl_1", "o2_1")
  names(rast_seas_250m_sub) <- c("vosaline_1", "votemper_1", "o2_1")
  
  
  #subset annual raster files ---------------------------------------------------------------------------------
  rast_ann_0m_sub <- NULL
  rast_ann_250m_sub <- NULL
  
  for(i in 1:length(substr(seq(date_start, date_end, "years"), 1, 4))){
    curr_year = substr(seq(date_start, date_end, "years"), 1, 4)[i]
    
    ann_0m_sub_rast_temp <- subset(rast_file_ann_0m, year(time(rast_file_ann_0m)) == as.numeric(curr_year))
    ann_250m_sub_rast_temp <- subset(rast_file_ann_250m, year(time(rast_file_ann_250m)) == as.numeric(curr_year))
    
    rast_ann_0m_sub <- c(rast_ann_0m_sub, ann_0m_sub_rast_temp)
    rast_ann_250m_sub <- c(rast_ann_250m_sub, ann_250m_sub_rast_temp)
  }
  
  #generate mean annual raster if date range includes multiple years
  rast_ann_0m_sub <- rast(rast_ann_0m_sub)
  rast_ann_250m_sub <- rast(rast_ann_250m_sub)
  
  if(length(unique(year(time(rast_ann_0m_sub)))) > 1){
    
    rast_ann_0m_sub <- tapp(rast_ann_0m_sub, unique(varnames(rast_ann_0m_sub)), sd)
    rast_ann_250m_sub <- tapp(rast_ann_250m_sub, unique(varnames(rast_ann_250m_sub)), sd)
    
  }
  
  names(rast_ann_0m_sub) <- c("somxlavt", "vosaline", "sossheig", "votemper", "vozocrtx", "sozotaux", "vomecrty", "sometauy", "chl", "o2")
  names(rast_ann_250m_sub) <- c("vosaline", "votemper", "o2")
  
  terra::time(rast_ann_0m_sub) <- NULL
  terra::time(rast_ann_250m_sub) <- NULL
  
  #get bathy rasters ------------------------------------------------------------------------------------------
  bathy <- rast(here("data/enviro/psat_spot_all/bathy_gebco/processed/gebco_bathy_0.25deg2.nc"))
  
  #generate rugosity raster
  #rug <- rast(here("data/enviro/psat_spot_all/bathy_gebco/gebco_2023_n49.0_s1.0_w-153.0_e-103.0.nc"))
  #bathy_sd <- focal(rug, w = 59, fun = "sd", na.rm = TRUE)
  #bathy_sd1 <- aggregate(bathy_sd, fact = 60)
  #bathy_sd2 <- writeCDF(bathy_sd1, filename = "data/enviro/psat_spot_all/bathy_gebco/processed/bathy_sd_0.25.nc")
  
  bathy_sd <- rast(here("data/enviro/psat_spot_all/bathy_gebco/processed/bathy_sd_0.25.nc"))
  
  #generate AGI rasters ---------------------------------------------------------------------------------------
  source(here("functions/oxy_demand_functions.R"))
  OxyThresh_0m = 0.04928389 #value converted from Vetter concentration data to atm according to salinity, temp, and pressure at 0m
  OxyThresh_250m = 0.03816138 #value converted from Vetter concentration data to atm according to salinity, temp, and pressure at 250m
  Tpref = 16.45201 #mean temp experienced by sharks at mean dive depth (50m)
  
  dir.create(here(paste0("data/enviro/psat_spot_all/hsi_rasts_sd/", output_name)), showWarnings = FALSE) 
  dir.create(here(paste0("data/enviro/psat_spot_all/hsi_rasts_sd/agi_rasts/", output_name)), showWarnings = FALSE) 
  
  #daily rast AGI
  demand_daily_0m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_0m, T_C = rast_daily_0m_sub$votemper)
  atm_daily_0m <- rast_to_atm(do = rast_daily_0m_sub$o2, so = rast_daily_0m_sub$vosaline, temp = rast_daily_0m_sub$votemper, depth = 0)
  AGI_daily_0m <- atm_daily_0m/demand_daily_0m
  writeCDF(AGI_daily_0m, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts_sd/agi_rasts/", output_name, "/", output_name, "_daily_agi_0m.nc")))
  
  demand_daily_250m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_250m, T_C = rast_daily_250m_sub$votemper)
  atm_daily_250m <- rast_to_atm(do = rast_daily_250m_sub$o2, so = rast_daily_250m_sub$vosaline, temp = rast_daily_250m_sub$votemper, depth = 250)
  AGI_daily_250m <- atm_daily_250m/demand_daily_250m
  writeCDF(AGI_daily_250m, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts_sd/agi_rasts/", output_name,"/", output_name, "_daily_agi_250m.nc")))
  
  #seas rast AGI
  demand_seas_0m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_0m, T_C = rast_seas_0m_sub$votemper_1)
  atm_seas_0m <- rast_to_atm(do = rast_seas_0m_sub$o2_1, so = rast_seas_0m_sub$vosaline_1, temp = rast_seas_0m_sub$votemper_1, depth = 0)
  AGI_seas_0m <- atm_seas_0m/demand_seas_0m
  
  demand_seas_250m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_250m, T_C = rast_seas_250m_sub$votemper_1)
  atm_seas_250m <- rast_to_atm(do = rast_seas_250m_sub$o2_1, so = rast_seas_250m_sub$vosaline_1, temp = rast_seas_250m_sub$votemper_1, depth = 250)
  AGI_seas_250m <- atm_seas_250m/demand_seas_250m
  
  #ann rast AGI
  demand_ann_0m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_0m, T_C = rast_ann_0m_sub$votemper)
  atm_ann_0m <- rast_to_atm(do = rast_ann_0m_sub$o2, so = rast_ann_0m_sub$vosaline, temp = rast_ann_0m_sub$votemper, depth = 0)
  AGI_ann_0m <- atm_ann_0m/demand_ann_0m
  
  demand_ann_250m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_250m, T_C = rast_ann_250m_sub$votemper)
  atm_ann_250m <- rast_to_atm(do = rast_ann_250m_sub$o2, so = rast_ann_250m_sub$vosaline, temp = rast_ann_250m_sub$votemper, depth = 250)
  AGI_ann_250m <- atm_ann_250m/demand_ann_250m
  
  #combine rasters --------------------------------------------------------------------------------------------
  #base model
  base_rast <- c(bathy, rast_daily_0m_sub$votemper, rast_daily_0m_sub$vosaline,  rast_daily_0m_sub$chl, rast_daily_0m_sub$sossheig, bathy_sd, rast_daily_0m_sub$somxlavt)
  names(base_rast) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
  
  #DO model
  do_rast <- c(rast_daily_0m_sub$o2, rast_ann_250m_sub$o2, rast_seas_0m_sub$o2_1, rast_daily_0m_sub$votemper, rast_seas_250m_sub$o2_1, bathy, rast_daily_0m_sub$vosaline, rast_daily_0m_sub$chl, rast_ann_0m_sub$o2, rast_daily_250m_sub$o2, rast_daily_0m_sub$sossheig, rast_daily_0m_sub$somxlavt, bathy_sd)
  names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
  
  #AGI model 
  agi_rast <- c(rast_daily_0m_sub$votemper, AGI_ann_250m, AGI_daily_0m, bathy, AGI_seas_0m, rast_daily_0m_sub$vosaline, AGI_seas_250m, AGI_ann_0m, rast_daily_0m_sub$chl, AGI_daily_250m, bathy_sd, rast_daily_0m_sub$somxlavt, rast_daily_0m_sub$sossheig)
  names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  
  #comb model
  comb_rast <- c(rast_daily_0m_sub$votemper, AGI_ann_250m, bathy, rast_daily_0m_sub$vosaline, AGI_seas_250m, rast_daily_0m_sub$chl, AGI_daily_250m, bathy_sd, rast_daily_0m_sub$somxlavt, rast_daily_0m_sub$sossheig, rast_daily_0m_sub$o2, rast_ann_0m_sub$o2, rast_seas_0m_sub$o2_1)
  names(comb_rast) <- c("temp_mean", "AGI_250m_ann", "bathy_mean", "sal_mean", "AGI_250m_seas", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean", "o2_mean_0m", "o2_mean_0m_ann", "o2_mean_0m_seas")
  
  #save HSI raster input file ---------------------------------------------------------------------------------
  
  #base model
  writeCDF(base_rast, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts_sd/", output_name,"/", output_name,"_base_rast.nc")))
  
  #DO model
  writeCDF(do_rast, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts_sd/", output_name,"/", output_name,"_do_rast.nc")))
  
  #AGI model
  writeCDF(agi_rast, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts_sd/", output_name,"/", output_name,"_agi_rast.nc")))
  
  #comb model
  writeCDF(comb_rast, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts_sd/", output_name,"/", output_name,"_combo_rast.nc")))
  
  # end function  
}

### hsi map theme  function ####
theme_map <- function(){ 
  font <- "Arial"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      plot.title = element_text(             #axis titles
        family = font,            #font family
        color = "black",
        size = 16), 
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        color = "black",
        size = 16),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        color = "black",
        size = 14),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
    )
  
#end function
}


### hsi map function ####
hsi_maps <- function(rast_folder, ms = c("Y", "N")){
  #only makes maps from base model and the final DO and AGI models. Does not include maps from overfit DO and AGI models. 
  
  #load raster files -----------------------------------------------------------------------------------------------
  base_rast_file <- list.files(here(rast_folder), pattern = "base", full.names = TRUE)
  do_rast_file <- list.files(here(rast_folder), pattern = "do", full.names = TRUE)
  agi_rast_file <- list.files(here(rast_folder), pattern = "agi", full.names = TRUE)
  do_agi_file <- list.files(here(rast_folder), pattern = "comb", full.names = TRUE)
  
  base_rast <- rast(base_rast_file)
  names(base_rast) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
  
  do_rast <- rast(do_rast_file)
  names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
  
  agi_rast <- rast(agi_rast_file)
  names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  
  do_agi_rast <- rast(do_agi_file)
  names(do_agi_rast) <- c("temp_mean", "AGI_250m_ann", "bathy_mean", "sal_mean", "AGI_250m_seas", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean", "o2_mean_0m", "o2_mean_0m_ann", "o2_mean_0m_seas")
  
  extent <- c(-153, -103, 1 , 49)
  
  #creating map dfs -------------------------------------------------------------------------------------------------
    #predict
  base_pred <- predict(base_rast, base_mod, type = "response", n.tress = base_mod$gbm.call$best.trees, na.rm = FALSE)
  base_pred <- crop(base_pred, extent)
  
  do_pred <- predict(do_rast, do_mod_fin, type = "response", n.tress = do_mod_fin$gbm.call$best.trees, na.rm = FALSE)
  do_pred <- crop(do_pred, extent)
  
  agi_pred <- predict(agi_rast, agi_mod_fin, type = "response", n.tress = agi_mod_fin$gbm.call$best.trees, na.rm = FALSE)
  agi_pred <- crop(agi_pred, extent)
  
  #agi_back <- predict(agi_rast, agi_mod_back, type = "response", n.tress = agi_mod_back$gbm.call$best.trees, na.rm = FALSE)
  #agi_back <- crop(agi_back, extent)
  
  do_agi_combo <- predict(do_agi_rast, do_agi_comb, type = "response", n.trees = do_agi_comb$gbm.call$best.trees, na.rm = FALSE)
  do_agi_combo <- crop(do_agi_combo, extent)
  
    #predict to df
  # base_df <- as.points(base_pred) %>% st_as_sf() %>% as.data.frame()
  # colnames(base_df) = c("value", "geometry")
  # 
  # do_df <- as.points(do_pred) %>% st_as_sf() %>% as.data.frame()
  # colnames(do_df) = c("value", "geometry")
  # 
  # agi_df <- as.points(agi_pred) %>% st_as_sf() %>% as.data.frame()
  # colnames(agi_df) = c("value", "geometry")
  
    #predictions for ensemble model
  # agi_mod_e <- readRDS(here("data/brt/mod_outputs/crw/ensemble/brt_agi_only.rds"))
  # agi_pred_e <- predict(agi_rast, agi_mod_e, type = "response", n.tress = agi_mod_e$gbm.call$best.trees, na.rm = FALSE)
  # agi_pred_e <- crop(agi_pred_e, extent)
  # 
  # do_mod_e <- readRDS(here("data/brt/mod_outputs/crw/ensemble/brt_do_final.rds"))
  # do_pred_e <- predict(do_rast, do_mod_e, type = "response", n.tress = do_mod_e$gbm.call$best.trees, na.rm = FALSE)
  # do_pred_e <- crop(do_pred_e, extent)
  # 
  # ensemb_pred <- mean(agi_pred_e, do_pred_e)
  
  #plot maps --------------------------------------------------------------------------------------------------------
    #land files
  map.world = map_data(map="world")
  testt = map.world %>% filter(long <= 180)
  
    #base map
  #calculate percent area polygon takes up of raster 
  base_hsi <- raster::clamp(base_pred, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_base <- expanse(base_hsi)
  rast_area_base <- expanse(base_pred)
  perc_area_base <- (hsi_area_base/rast_area_base$area[1])*100
  #print(paste0("Base hsi > 0.50:", " ", round(perc_area_base$area[1], 2), "%"))
  
  base_map <- ggplot() +
    geom_spatraster(data = base_pred) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "deep", direction = -1) +
    ggtitle("Base model") +
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0(round(perc_area_base$area[1], 2), "%")), 
              hjust = 1.1, vjust = 2, size = 6, color = "black")+
    theme_map() +
    theme(axis.text.x = element_blank(), legend.position = "none", axis.title.x = element_blank())
  
  if(ms == "Y"){ggsave(here("figs/ms/fig6_hsi_all/indiv_panels/base_pred_all.png"), base_map, height = 5, width = 5)}
  
    #do map
  #calculate percent area polygon takes up of raster 
  do_hsi <- raster::clamp(do_pred, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_do <- expanse(do_hsi)
  rast_area_do <- expanse(do_pred)
  perc_area_do <- (hsi_area_do/rast_area_do$area[1])*100
  #print(paste0("DO hsi > 0.50:", " ", round(perc_area_do$area[1], 2), "%"))
  
  do_map <- ggplot() +
    geom_spatraster(data = do_pred) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "deep", direction = -1) +
    ggtitle("DO model") + 
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0(round(perc_area_do$area[1], 2), "%")), 
              hjust = 1.1, vjust = 2, size = 6, color = "black")+
    theme_map() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(), 
          axis.text.x = element_blank(),
          legend.position = "none")
  
  if(ms == "Y"){ggsave(here("figs/ms/fig6_hsi_all/indiv_panels/do_pred_all.png"), do_map, height = 5, width = 5)}
  
    #agi map
  #calculate percent area polygon takes up of raster 
  agi_hsi <- raster::clamp(agi_pred, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_agi <- expanse(agi_hsi)
  rast_area_agi <- expanse(agi_pred)
  perc_area_agi <- (hsi_area_agi/rast_area_agi$area[1])*100
  #print(paste0("agi hsi > 0.50:", " ", round(perc_area_agi$area[1], 2), "%"))
  
  agi_map <- ggplot() +
    geom_spatraster(data = agi_pred) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "deep", direction = -1)  +
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0(round(perc_area_agi$area[1], 2), "%")), 
              hjust = 1.1, vjust = 2, size = 6, color = "black")+
    ggtitle("AGI model") +
    theme_map() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  if(ms == "Y"){ggsave(here("figs/ms/fig6_hsi_all/indiv_panels/agi_pred_all.png"), agi_map, height = 5, width = 5)}
  
  #agi map background PA
  # agi_map_back <- ggplot() +
  #   geom_spatraster(data = agi_back) +
  #   geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  #   scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  #   scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  #   scale_fill_whitebox_c(palette = "muted", direction = -1) +
  #   ggtitle("AGI model (background PA)") +
  #   theme_map() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #do agi combo map
  #calculate percent area polygon takes up of raster 
  combo_hsi <- raster::clamp(do_agi_combo, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_combo <- expanse(combo_hsi)
  rast_area_combo <- expanse(do_agi_combo)
  perc_area_combo <- (hsi_area_combo/rast_area_combo$area[1])*100
  #print(paste0("combo hsi > 0.50:", " ", round(perc_area_combo$area[1], 2), "%"))
  
  combo_map <- ggplot() +
    geom_spatraster(data = do_agi_combo) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "deep", direction = -1) +
    ggtitle("DO + AGI combo model") +
    theme_map() +
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0(round(perc_area_combo$area[1], 2), "%")), 
              hjust = 1.1, vjust = 2, size = 6, color = "black")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.text.y = element_blank(), 
          axis.title.y = element_blank(), 
          legend.position = "none")
  
  if(ms == "Y"){ggsave(here("figs/ms/fig6_hsi_all/indiv_panels/combo_pred_all.png"), combo_map, height = 5, width = 5)}
  
  #ensemble map
  # ensemb_map <- ggplot() +
  #   geom_spatraster(data = ensemb_pred) +
  #   geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  #   scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  #   scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  #   scale_fill_whitebox_c(palette = "muted", direction = -1) +
  #   ggtitle("DO, AGI ensemble model") +
  #   theme_map() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  #combine and return maps ------------------------------------------------------------------------------------------------------
  all_maps <- (base_map|do_map)/(agi_map|combo_map)+
    plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "HSI")
  
  return(all_maps)
  
#end function  
}

# hsi map function ENSO 
hsi_maps_enso <- function(rast_folder, enso, main_text = TRUE){
  #only makes maps from base model and the final DO and AGI models. Does not include maps from overfit DO and AGI models. 
  
  #load raster files -----------------------------------------------------------------------------------------------
  base_rast_file <- list.files(here(rast_folder), pattern = "base", full.names = TRUE)
  do_rast_file <- list.files(here(rast_folder), pattern = "do", full.names = TRUE)
  agi_rast_file <- list.files(here(rast_folder), pattern = "agi", full.names = TRUE)
  do_agi_file <- list.files(here(rast_folder), pattern = "comb", full.names = TRUE)
  
  base_rast <- rast(base_rast_file)
  names(base_rast) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
  
  do_rast <- rast(do_rast_file)
  names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
  
  agi_rast <- rast(agi_rast_file)
  names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  
  do_agi_rast <- rast(do_agi_file)
  names(do_agi_rast) <- c("temp_mean", "AGI_250m_ann", "bathy_mean", "sal_mean", "AGI_250m_seas", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean", "o2_mean_0m", "o2_mean_0m_ann", "o2_mean_0m_seas")
  
  extent <- c(-153, -103, 1 , 49)
  
  #creating map dfs -------------------------------------------------------------------------------------------------
  #predict
  base_pred <- predict(base_rast, base_mod, type = "response", n.tress = base_mod$gbm.call$best.trees, na.rm = FALSE)
  base_pred <- crop(base_pred, extent)
  
  do_pred <- predict(do_rast, do_mod_fin, type = "response", n.tress = do_mod_fin$gbm.call$best.trees, na.rm = FALSE)
  do_pred <- crop(do_pred, extent)
  
  agi_pred <- predict(agi_rast, agi_mod_fin, type = "response", n.tress = agi_mod_fin$gbm.call$best.trees, na.rm = FALSE)
  agi_pred <- crop(agi_pred, extent)
  
  #agi_back <- predict(agi_rast, agi_mod_back, type = "response", n.tress = agi_mod_back$gbm.call$best.trees, na.rm = FALSE)
  #agi_back <- crop(agi_back, extent)
  
  do_agi_combo <- predict(do_agi_rast, do_agi_comb, type = "response", n.trees = do_agi_comb$gbm.call$best.trees, na.rm = FALSE)
  do_agi_combo <- crop(do_agi_combo, extent)
  
  #predict to df
  # base_df <- as.points(base_pred) %>% st_as_sf() %>% as.data.frame()
  # colnames(base_df) = c("value", "geometry")
  # 
  # do_df <- as.points(do_pred) %>% st_as_sf() %>% as.data.frame()
  # colnames(do_df) = c("value", "geometry")
  # 
  # agi_df <- as.points(agi_pred) %>% st_as_sf() %>% as.data.frame()
  # colnames(agi_df) = c("value", "geometry")
  
  #predictions for ensemble model
  # agi_mod_e <- readRDS(here("data/brt/mod_outputs/crw/ensemble/brt_agi_only.rds"))
  # agi_pred_e <- predict(agi_rast, agi_mod_e, type = "response", n.tress = agi_mod_e$gbm.call$best.trees, na.rm = FALSE)
  # agi_pred_e <- crop(agi_pred_e, extent)
  # 
  # do_mod_e <- readRDS(here("data/brt/mod_outputs/crw/ensemble/brt_do_final.rds"))
  # do_pred_e <- predict(do_rast, do_mod_e, type = "response", n.tress = do_mod_e$gbm.call$best.trees, na.rm = FALSE)
  # do_pred_e <- crop(do_pred_e, extent)
  # 
  # ensemb_pred <- mean(agi_pred_e, do_pred_e)
  
  #plot maps --------------------------------------------------------------------------------------------------------
  #land files
  map.world = map_data(map="world")
  testt = map.world %>% filter(long <= 180)
  
  #base map
  #calculate percent area polygon takes up of raster 
  base_hsi <- raster::clamp(base_pred, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  hsi_area_base <- expanse(base_hsi)
  rast_area_base <- expanse(base_pred)
  perc_area_base <- (hsi_area_base/rast_area_base$area[1])*100
  print(paste0("Base hsi > 0.50:", " ", round(perc_area_base$area[1], 2), "%"))
  
  base_map <- ggplot() +
    geom_spatraster(data = base_pred) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "deep", direction = -1) +
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0(round(perc_area_base$area[1], 2), "%")), 
              hjust = 1.1, vjust = 2, size = 3.4, color = "black")+
    theme_map() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none", 
          axis.title.x = element_blank())
  

  #do map
  #calculate percent area polygon takes up of raster 
  do_hsi <- raster::clamp(do_pred, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_do <- expanse(do_hsi)
  rast_area_do <- expanse(do_pred)
  perc_area_do <- (hsi_area_do/rast_area_do$area[1])*100
  print(paste0("DO hsi > 0.50:", " ", round(perc_area_do$area[1], 2), "%"))
  
  if(main_text == FALSE){
  do_map <- ggplot() +
    geom_spatraster(data = do_pred) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "deep", direction = -1)+
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0(round(perc_area_do$area[1], 2), "%")), 
              hjust = 1.1, vjust = 2, size = 3.4, color = "black")+
    theme_map() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")
  }
  
  if(main_text == TRUE){
    do_map <- ggplot() +
      geom_spatraster(data = do_pred) +
      geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
      scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
      scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
      scale_fill_whitebox_c(palette = "deep", direction = -1)+
      geom_text(aes(x = Inf, y = Inf, 
                    label = paste0(round(perc_area_do$area[1], 2), "%")), 
                hjust = 1.1, vjust = 2, size = 8, color = "black")+
      theme_map() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none")
  }

  #agi map
  #calculate percent area polygon takes up of raster 
  agi_hsi <- raster::clamp(agi_pred, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_agi <- expanse(agi_hsi)
  rast_area_agi <- expanse(agi_pred)
  perc_area_agi <- (hsi_area_agi/rast_area_agi$area[1])*100
  print(paste0("agi hsi > 0.50:", " ", round(perc_area_agi$area[1], 2), "%"))
  
  if(main_text == FALSE){
  agi_map <- ggplot() +
    geom_spatraster(data = agi_pred) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "deep", direction = -1) +
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0(round(perc_area_agi$area[1], 2), "%")), 
              hjust = 1.1, vjust = 2, size = 3.4, color = "black")+
    theme_map() +
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
      legend.position = "none")
  }
  
  if(main_text == TRUE){
    agi_map <- ggplot() +
      geom_spatraster(data = agi_pred) +
      geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
      scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
      scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
      scale_fill_whitebox_c(palette = "deep", direction = -1) +
      geom_text(aes(x = Inf, y = Inf, 
                    label = paste0(round(perc_area_agi$area[1], 2), "%")), 
                hjust = 1.1, vjust = 2, size = 8, color = "black")+
      theme_map() +
      theme(axis.text.x = element_blank(), 
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none")   
    
    
    
  }

  #agi map background PA
  # agi_map_back <- ggplot() +
  #   geom_spatraster(data = agi_back) +
  #   geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  #   scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  #   scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  #   scale_fill_whitebox_c(palette = "muted", direction = -1) +
  #   ggtitle("AGI model (background PA)") +
  #   theme_map() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #do agi combo map
  #calculate percent area polygon takes up of raster 
  combo_hsi <- raster::clamp(do_agi_combo, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_combo <- expanse(combo_hsi)
  rast_area_combo <- expanse(do_agi_combo)
  perc_area_combo <- (hsi_area_combo/rast_area_combo$area[1])*100
  print(paste0("combo hsi > 0.50:", " ", round(perc_area_combo$area[1], 2), "%"))
  
  combo_map <- ggplot() +
    geom_spatraster(data = do_agi_combo) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "deep", direction = -1) +
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0(round(perc_area_combo$area[1], 2), "%")), 
              hjust = 1.1, vjust = 2, size = 3.4, color = "black")+
    theme_map() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5), 
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")
  

  #ensemble map
  # ensemb_map <- ggplot() +
  #   geom_spatraster(data = ensemb_pred) +
  #   geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  #   scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  #   scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  #   scale_fill_whitebox_c(palette = "muted", direction = -1) +
  #   ggtitle("DO, AGI ensemble model") +
  #   theme_map() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  #combine and return maps ------------------------------------------------------------------------------------------------------
  if(main_text == FALSE){
        if(enso == "EN"){
        all_maps <- (base_map)/(do_map)/(agi_map)/(combo_map)+
          plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "HSI")
        } 
        if(enso == "base"){
          all_maps <- (base_map+
                         theme(axis.text.y = element_text(size = 14, color = "black"), 
                               axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3)))/(do_map+
                                                                                                        theme(axis.text.y = element_text(size = 14, color = "black"), 
                                                                                                              axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3)))/(agi_map+
                                                                                                                                                                                       theme(axis.text.y = element_text(size = 14, color = "black"), 
                                                                                                                                                                                             axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3)))/(combo_map+
                                                                                                                                                                                                                                                                      theme(axis.text.y = element_text(size = 14, color = "black"), 
                                                                                                                                                                                                                                                                            axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3)))
        } 
        if(enso == "LN"){
          
          all_maps <- (base_map)/(do_map)/(agi_map)/(combo_map)
        }
        if(enso == "diff"){
          all_maps <- (base_map+
                         theme(axis.text.y = element_text(size = 14, color = "black"), 
                               axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3)))/(do_map+
                                                                                                                     theme(axis.text.y = element_text(size = 14, color = "black"), 
                                                                                                                           axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3)))/(agi_map+
                                                                                                                                                                                                                 theme(axis.text.y = element_text(size = 14, color = "black"), 
                                                                                                                                                                                                                       axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3)))/(combo_map+
                                                                                                                                                                                                                                                                                                             theme(axis.text.y = element_text(size = 14, color = "black"), 
                                                                                                                                                                                                                                                                                                                   axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3)))
          
          all_maps <- all_maps+
            plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "HSI")
        }
        }
  
  if(main_text == TRUE){
    
          if(enso == "EN"){
            all_maps <- (do_map)/(agi_map)+
              plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "HSI")
          } 
          if(enso == "base"){
            all_maps <- (do_map+theme(axis.text.y = element_text(size = 14, color = "black"), axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3)))/(agi_map + axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3))
          } 
          if(enso == "LN"){
            
            all_maps <- (do_map)/(agi_map)
          }
          if(enso == "diff"){
            all_maps <- (do_map+theme(axis.text.y = element_text(size = 14, color = "black"), axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 1)))/(agi_map+theme(axis.text.y = element_text(size = 14, color = "black"), 
                                                                                                                                                                                                                         axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 1), 
                                                                                                                                                                                              axis.text.x = element_text(color = "black", angle = 45, hjust = 0.5, size = 14), 
                                                                                                                                                                                              axis.title.x = element_text(size = 16, color = "black", vjust = -1)))
            all_maps <- all_maps+
              plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "HSI")
          }
    
  }
  
  return(all_maps)
  
  #end function  
}

# hsi map function ENSO 
hsi_maps_difference_enso <- function(neut_rast_folder, enso_rast_folder, enso, main_text = TRUE){

  ### NEUTRAL
  #load raster files -----------------------------------------------------------------------------------------------
  base_rast_file <- list.files(here(neut_rast_folder), pattern = "base", full.names = TRUE)
  do_rast_file <- list.files(here(neut_rast_folder), pattern = "do", full.names = TRUE)
  agi_rast_file <- list.files(here(neut_rast_folder), pattern = "agi", full.names = TRUE)
  do_agi_file <- list.files(here(neut_rast_folder), pattern = "comb", full.names = TRUE)
  
  base_rast <- rast(base_rast_file)
  names(base_rast) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
  
  do_rast <- rast(do_rast_file)
  names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
  
  agi_rast <- rast(agi_rast_file)
  names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  
  do_agi_rast <- rast(do_agi_file)
  names(do_agi_rast) <- c("temp_mean", "AGI_250m_ann", "bathy_mean", "sal_mean", "AGI_250m_seas", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean", "o2_mean_0m", "o2_mean_0m_ann", "o2_mean_0m_seas")
  
  extent <- c(-153, -103, 1 , 49)
  
  #creating map dfs -------------------------------------------------------------------------------------------------
  #predict
  base_pred <- predict(base_rast, base_mod, type = "response", n.tress = base_mod$gbm.call$best.trees, na.rm = FALSE)
  base_pred <- crop(base_pred, extent)
  
  do_pred <- predict(do_rast, do_mod_fin, type = "response", n.tress = do_mod_fin$gbm.call$best.trees, na.rm = FALSE)
  do_pred <- crop(do_pred, extent)
  
  agi_pred <- predict(agi_rast, agi_mod_fin, type = "response", n.tress = agi_mod_fin$gbm.call$best.trees, na.rm = FALSE)
  agi_pred <- crop(agi_pred, extent)
  
  #agi_back <- predict(agi_rast, agi_mod_back, type = "response", n.tress = agi_mod_back$gbm.call$best.trees, na.rm = FALSE)
  #agi_back <- crop(agi_back, extent)
  
  do_agi_pred <- predict(do_agi_rast, do_agi_comb, type = "response", n.trees = do_agi_comb$gbm.call$best.trees, na.rm = FALSE)
  do_agi_pred <- crop(do_agi_pred, extent)
  
  ### ENSO
  #load raster files -----------------------------------------------------------------------------------------------
  base_rast_file_enso <- list.files(here(enso_rast_folder), pattern = "base", full.names = TRUE)
  do_rast_file_enso  <- list.files(here(enso_rast_folder), pattern = "do", full.names = TRUE)
  agi_rast_file_enso  <- list.files(here(enso_rast_folder), pattern = "agi", full.names = TRUE)
  do_agi_file_enso  <- list.files(here(enso_rast_folder), pattern = "comb", full.names = TRUE)
  
  base_rast_enso  <- rast(base_rast_file_enso )
  names(base_rast_enso ) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
  
  do_rast_enso  <- rast(do_rast_file_enso )
  names(do_rast_enso ) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
  
  agi_rast_enso  <- rast(agi_rast_file_enso )
  names(agi_rast_enso ) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  
  do_agi_rast_enso  <- rast(do_agi_file_enso )
  names(do_agi_rast_enso ) <- c("temp_mean", "AGI_250m_ann", "bathy_mean", "sal_mean", "AGI_250m_seas", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean", "o2_mean_0m", "o2_mean_0m_ann", "o2_mean_0m_seas")
  
  #creating map dfs -------------------------------------------------------------------------------------------------
  #predict
  base_pred_enso  <- predict(base_rast_enso , base_mod, type = "response", n.tress = base_mod$gbm.call$best.trees, na.rm = FALSE)
  base_pred_enso  <- crop(base_pred_enso , extent)
  
  do_pred_enso  <- predict(do_rast_enso , do_mod_fin, type = "response", n.tress = do_mod_fin$gbm.call$best.trees, na.rm = FALSE)
  do_pred_enso  <- crop(do_pred_enso , extent)
  
  agi_pred_enso  <- predict(agi_rast_enso , agi_mod_fin, type = "response", n.tress = agi_mod_fin$gbm.call$best.trees, na.rm = FALSE)
  agi_pred_enso  <- crop(agi_pred_enso , extent)
  
  do_agi_pred_enso  <- predict(do_agi_rast_enso , do_agi_comb, type = "response", n.trees = do_agi_comb$gbm.call$best.trees, na.rm = FALSE)
  do_agi_pred_enso  <- crop(do_agi_pred_enso , extent)
  
 # create difference rasters ------------------------------------------------------------------------------------------
  #make difference maps
  diff_base <- diff(c(base_pred, base_pred_enso))*100
  diff_agi <- diff(c(agi_pred, agi_pred_enso))*100
  diff_do <- diff(c(do_pred, do_pred_enso))*100
  diff_do_agi_combo <- diff(c(do_agi_pred, do_agi_pred_enso))*100
  
  #plot maps --------------------------------------------------------------------------------------------------------
  #land files
  map.world = map_data(map="world")
  testt = map.world %>% filter(long <= 180)
  
  #base map
  #calculate percent area polygon takes up of raster 
  base_hsi <- raster::clamp(base_pred, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  hsi_area_base <- expanse(base_hsi)
  rast_area_base <- expanse(base_pred)
  perc_area_base <- (hsi_area_base/rast_area_base$area[1])*100
  
  #enso area
  base_hsi_enso <- raster::clamp(base_pred_enso, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  hsi_area_base_enso <- expanse(base_hsi_enso)
  rast_area_base_enso <- expanse(base_pred_enso)
  perc_area_base_enso <- (hsi_area_base_enso/rast_area_base_enso$area[1])*100
  
  #calculate difference
  perc_base <- paste0(round(perc_area_base_enso$area[1] - perc_area_base$area[1], 2), "%")
  
  #plot
  base_map <- ggplot() +
    geom_spatraster(data = diff_base) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "muted", limits = c(-100, 100), direction = -1) +
    geom_text(aes(x = Inf, y = Inf, 
                  label = perc_base), 
              hjust = 1.1, vjust = 2, size = 3.4, color = "black")+
    theme_map() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none", 
          axis.title.x = element_blank())
  
  
  #do map
  #calculate percent area polygon takes up of raster 
  do_hsi <- raster::clamp(do_pred, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_do <- expanse(do_hsi)
  rast_area_do <- expanse(do_pred)
  perc_area_do <- (hsi_area_do/rast_area_do$area[1])*100

  #enso area
  do_hsi_enso <- raster::clamp(do_pred_enso, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  hsi_area_do_enso <- expanse(do_hsi_enso)
  rast_area_do_enso <- expanse(do_pred_enso)
  perc_area_do_enso <- (hsi_area_do_enso/rast_area_do_enso$area[1])*100
  
  #calculate difference
  perc_do <- paste0(round(perc_area_do_enso$area[1] - perc_area_do$area[1], 2), "%")
  
  if(main_text == FALSE){
  do_map <- ggplot() +
    geom_spatraster(data = diff_do) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "muted", limits = c(-100, 100), direction = -1)+
    geom_text(aes(x = Inf, y = Inf, 
                  label = perc_do), 
              hjust = 1.1, vjust = 2, size = 3.4, color = "black")+
    theme_map() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")
  }
  
  if(main_text == TRUE){
    do_map <- ggplot() +
      geom_spatraster(data = diff_do) +
      geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
      scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
      scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
      scale_fill_whitebox_c(palette = "muted", limits = c(-100, 100), direction = -1)+
      geom_text(aes(x = Inf, y = Inf, 
                    label = perc_do), 
                hjust = 1.1, vjust = 2, size = 8, color = "black")+
      theme_map() +
      theme(axis.title.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            legend.position = "none")

  }
  
  #agi map
  #calculate percent area polygon takes up of raster 
  agi_hsi <- raster::clamp(agi_pred, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_agi <- expanse(agi_hsi)
  rast_area_agi <- expanse(agi_pred)
  perc_area_agi <- (hsi_area_agi/rast_area_agi$area[1])*100
  
  #enso area
  agi_hsi_enso <- raster::clamp(agi_pred_enso, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  hsi_area_agi_enso <- expanse(agi_hsi_enso)
  rast_area_agi_enso <- expanse(agi_pred_enso)
  perc_area_agi_enso <- (hsi_area_agi_enso/rast_area_agi_enso$area[1])*100
  
  #calculate difference
  perc_agi <- paste0(round(perc_area_agi_enso$area[1] - perc_area_agi$area[1], 2), "%")
  
  if(main_text == FALSE){
  agi_map <- ggplot() +
    geom_spatraster(data = diff_agi) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "muted", limits = c(-100, 100), direction = -1) +
    geom_text(aes(x = Inf, y = Inf, 
                  label = perc_agi), 
              hjust = 1.1, vjust = 2, size = 3.4, color = "black")+
    theme_map() +
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")
  }
  
  if(main_text == TRUE){
    agi_map <- ggplot() +
      geom_spatraster(data = diff_agi) +
      geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
      scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
      scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
      scale_fill_whitebox_c(palette = "muted", limits = c(-100, 100), direction = -1) +
      geom_text(aes(x = Inf, y = Inf, 
                    label = perc_agi), 
                hjust = 1.1, vjust = 2, size = 8, color = "black")+
      theme_map() +
      theme(legend.position = "none", 
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_text(color = "black", angle = 45, hjust = 0.5, size = 14), 
            axis.title.x = element_text(size = 16, color = "black", vjust = 1))

  }
  
  #do agi combo map
  #calculate percent area polygon takes up of raster 
  combo_hsi <- raster::clamp(do_agi_pred, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_combo <- expanse(combo_hsi)
  rast_area_combo <- expanse(do_agi_pred)
  perc_area_combo <- (hsi_area_combo/rast_area_combo$area[1])*100

  #enso area
  combo_hsi_enso <- raster::clamp(do_agi_pred_enso, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  hsi_area_combo_enso <- expanse(combo_hsi_enso)
  rast_area_combo_enso <- expanse(do_agi_pred_enso)
  perc_area_combo_enso <- (hsi_area_combo_enso/rast_area_combo_enso$area[1])*100
  
  #calculate difference
  perc_combo <- paste0(round(perc_area_combo_enso$area[1] - perc_area_combo$area[1], 2), "%")
  
  combo_map <- ggplot() +
    geom_spatraster(data = diff_do_agi_combo) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "muted", limits = c(-100, 100), direction = -1) +
    geom_text(aes(x = Inf, y = Inf, 
                  label = perc_combo), 
              hjust = 1.1, vjust = 2, size = 3.4, color = "black")+
    theme_map() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5), 
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")
  
  #combine and return maps ------------------------------------------------------------------------------------------------------
  
  if(main_text == FALSE){
  if(enso == "EN"){
    all_maps <- (base_map)/(do_map)/(agi_map)/(combo_map)+
      plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "% change")
  } 
  if(enso == "LN"){
    all_maps <- (base_map)/(do_map)/(agi_map)/(combo_map)
  }
  }
  
  if(main_text == TRUE){
    if(enso == "EN"){
      all_maps <- (do_map)/(agi_map)+
        plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "% change")
    } 
    if(enso == "LN"){
      all_maps <- (do_map)/(agi_map)
    } 
    
  }
    
    
  return(all_maps)
  
  #end function  
}


### hsi difference map function ####
hsi_diff_maps <- function(rast_folder){
  #load raster files -----------------------------------------------------------------------------------------------
  base_rast_file <- list.files(here(rast_folder), pattern = "base", full.names = TRUE)
  do_rast_file <- list.files(here(rast_folder), pattern = "do", full.names = TRUE)
  agi_rast_file <- list.files(here(rast_folder), pattern = "agi", full.names = TRUE)
  
  base_rast <- rast(base_rast_file)
  names(base_rast) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
  
  do_rast <- rast(do_rast_file)
  names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
  
  agi_rast <- rast(agi_rast_file)
  names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  
  extent <- c(-153, -103, 1 , 49)
  
  #creating map dfs -------------------------------------------------------------------------------------------------
  #predict
  base_pred <- predict(base_rast, base_mod, type = "response", n.tress = base_mod$gbm.call$best.trees, na.rm = FALSE)
  base_pred <- crop(base_pred, extent)
  
  do_pred <- predict(do_rast, do_mod_fin, type = "response", n.tress = do_mod_fin$gbm.call$best.trees, na.rm = FALSE)
  do_pred <- crop(do_pred, extent)
  
  agi_pred <- predict(agi_rast, agi_mod_fin, type = "response", n.tress = agi_mod_fin$gbm.call$best.trees, na.rm = FALSE)
  agi_pred <- crop(agi_pred, extent)
  
  #plot maps ----------------------------------------------------------------------------------------------------------
    #land files
  map.world = map_data(map="world")
  testt = map.world %>% filter(long <= 180)
  
    #make difference maps
  diff_base_do <- diff(c(base_pred, do_pred))*100
  diff_base_agi <- diff(c(base_pred, agi_pred))*100
  diff_do_agi <- diff(c(do_pred, agi_pred))*100
  
  base_do_map <- ggplot() + 
    geom_spatraster(data = diff_base_do) + 
    geom_map(data = testt, map = testt, aes(map_id = region, x = long, y = lat),fill = "darkgrey", color = "black") +
    scale_x_continuous(expand = c(0,0), limits = c(-153,-103)) +
    scale_y_continuous(expand = c(0,0),limits = c(1,49))+
    scale_fill_whitebox_c(palette = "muted", direction = -1) +
    ggtitle("Comparing HSI: Base vs. DO") +
    labs(fill = "% change") +
    theme_map() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  base_agi_map <- ggplot() + 
    geom_spatraster(data = diff_base_agi) + 
    geom_map(data = testt, map = testt, aes(map_id = region, x = long, y = lat),fill = "darkgrey", color = "black") +
    scale_x_continuous(expand = c(0,0), limits = c(-153,-103)) +
    scale_y_continuous(expand = c(0,0),limits = c(1,49))+
    scale_fill_whitebox_c(palette = "muted", direction = -1) +
    ggtitle("Comparing HSI: Base vs. AGI") +
    labs(fill = "% change") +
    theme_map() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  do_agi_map <- ggplot() + 
    geom_spatraster(data = diff_do_agi) + 
    geom_map(data = testt, map = testt, aes(map_id = region, x = long, y = lat),fill = "darkgrey", color = "black") +
    scale_x_continuous(expand = c(0,0), limits = c(-153,-103)) +
    scale_y_continuous(expand = c(0,0),limits = c(1,49))+
    scale_fill_whitebox_c(palette = "muted", direction = -1) +
    ggtitle("Comparing HSI: DO vs. AGI") +
    labs(fill = "% change") +
    theme_map() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #combine and return maps --------------------------------------------------------------------------------------------
  all_maps <- base_do_map | base_agi_map | do_agi_map
  
  return(all_maps)

#end function
}

### AGI map function ####
agi_maps <- function(rast_folder = NULL, get_rast = c("Y", "N"), agi_0m_rast = NULL, agi_250m_rast = NULL){
    #load agi raster data
  if(get_rast == "Y"){
  agi_0m <- rast(list.files(here(rast_folder), full.names = TRUE, pattern = "0m"))
  agi_0m <- agi_0m[[1]]
  agi_250m <- rast(list.files(here(rast_folder), full.names = TRUE, pattern = "250m"))
  } 
  if(get_rast == "N"){
    agi_0m = agi_0m_rast
    agi_250m = agi_250m_rast
  }
  
    #calculate AGIcrit value (10th percentile)
  cmem_dat0_file <- readRDS(here("data/locs_w_covar/psat_spot/cmem_locs_covar_AGI_0m.rds"))
  cmem_dat0 <- cmem_dat0_file %>% filter(PA == 0)
  AGIcrit_0 <- quantile(cmem_dat0$AGI_0m, 0.10, na.rm = TRUE)
  
  cmem_dat250_file <- readRDS(here("data/locs_w_covar/psat_spot/cmem_locs_covar_AGI_250m.rds"))
  cmem_dat250 <- cmem_dat250_file %>% filter(PA == 0)
  AGIcrit_250 <- quantile(cmem_dat250$AGI_250m, 0.10, na.rm = TRUE)
  
    #create shape file of areas surrounding AGIcrit
  crit_0m_map <- raster::clamp(agi_0m, upper = AGIcrit_0, values = FALSE) #create raster of values below AGIcrit
  crit_poly_0m <- as.polygons(ext(crit_0m_map))
  crit_poly_0m <- as.polygons(crit_0m_map > -Inf)
  
  crit_250m_map <- raster::clamp(agi_250m, upper = AGIcrit_250, values = FALSE) #create raster of values below AGIcrit
  crit_poly_250m <- as.polygons(ext(crit_250m_map))
  crit_poly_250m <- as.polygons(crit_250m_map > -Inf)
  
    #calculate percent area of AGI crit polygon
  poly_area_250m_crit <- expanse(crit_poly_250m)
  rast_area_crit <- expanse(agi_250m)
  perc_area_250m_crit <- (poly_area_250m_crit/rast_area_crit$area[1])*100
  
    #agi crit map 
  map.world = map_data(map="world")
  testt=map.world %>% filter(long<=180)
  
  agi_crit_0m <- ggplot() + 
    geom_spatraster(data = agi_0m) + 
    geom_spatvector(data = crit_poly_0m, color = "black", fill = NA, linewidth = 0.8) +
    geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
    scale_x_continuous(expand=c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49))+
    scale_fill_whitebox_c(palette = "muted", direction = -1)+
    ggtitle("AGIcrit 0m") +
    labs(fill = "AGI")+
    theme_map()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none")
  
  agi_crit_250m <- ggplot() + 
    geom_spatraster(data = agi_250m) + 
    geom_spatvector(data = crit_poly_250m, color = "black", fill = NA, linewidth = 0.8) +
    geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
    scale_x_continuous(expand=c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49))+
    scale_fill_whitebox_c(palette = "muted", direction = -1)+
    ggtitle("AGIcrit 250m") +
    labs(fill = "AGI")+
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0("Area: ", round(perc_area_250m_crit, 2), "%")), 
              hjust = 1.1, vjust = 2, size = 5, color = "black")+
    theme_map()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none")
  
    #create shape file of areas surrounding AGI < 1
  one_0m_map <- raster::clamp(agi_0m, upper = 1, values = FALSE) #create raster of values below 1
  one_poly_0m <- as.polygons(ext(one_0m_map))
  one_poly_0m <- as.polygons(one_0m_map > -Inf)
  
  one_250m_map <- raster::clamp(agi_250m, upper = 1, values = FALSE) #create raster of values below 1
  one_poly_250m <- as.polygons(ext(one_250m_map))
  one_poly_250m <- as.polygons(one_250m_map > -Inf)  
  
    #calculate percent area polygon takes up of raster 
  poly_area_250m <- expanse(one_poly_250m)
  rast_area <- expanse(agi_250m)
  perc_area_250m <- (poly_area_250m/rast_area$area[1])*100
  
    #agi <1 map
  agi_one_0m <- ggplot() + 
    geom_spatraster(data = agi_0m) + 
    geom_spatvector(data = one_poly_0m, color = "black", fill = NA, linewidth = 0.8) +
    geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
    scale_x_continuous(expand=c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49))+
    scale_fill_whitebox_c(palette = "muted", direction = -1)+
    ggtitle("AGI<1 0m") +
    labs(fill = "AGI")+
    theme_map()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none")
  
  agi_one_250m <- ggplot() + 
    geom_spatraster(data = agi_250m) + 
    geom_spatvector(data = one_poly_250m, color = "black", fill = NA, linewidth = 0.8) +
    geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
    scale_x_continuous(expand=c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49))+
    scale_fill_whitebox_c(palette = "muted", direction = -1)+
    ggtitle("AGI<1 250m") +
    labs(fill = "AGI")+
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0("Area: ", round(perc_area_250m, 2), "%")), 
              hjust = 1.1, vjust = 2, size = 5, color = "black")+
    theme_map()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none")
  
  return((agi_crit_0m | agi_crit_250m)/(agi_one_0m | agi_one_250m))
  
#end function 
}

agi_maps_ms <- function(rast_folder = NULL, get_rast = c("Y", "N"), agi_0m_rast = NULL, agi_250m_rast = NULL, fig_pos){
  #load agi raster data
  if(get_rast == "Y"){
    agi_0m <- rast(list.files(here(rast_folder), full.names = TRUE, pattern = "0m"))
    agi_0m <- agi_0m[[1]]
    agi_250m <- rast(list.files(here(rast_folder), full.names = TRUE, pattern = "250m"))
  } 
  if(get_rast == "N"){
    agi_0m = agi_0m_rast
    agi_250m = agi_250m_rast
  }
  
  #create shape file of areas surrounding AGI < 1
  one_250m_map <- raster::clamp(agi_250m, upper = 1, values = FALSE) #create raster of values below 1
  one_poly_250m <- as.polygons(ext(one_250m_map))
  one_poly_250m <- as.polygons(one_250m_map > -Inf)  
  
  #calculate percent area polygon takes up of raster 
  poly_area_250m <- expanse(one_poly_250m)
  rast_area <- expanse(agi_250m)
  perc_area_250m <- (poly_area_250m/rast_area$area[1])*100
  
  #agi <1 map
  map.world = map_data(map="world")
  testt=map.world %>% filter(long<=180)
  
  if(fig_pos == 1){
  
  agi_one_250m <- ggplot() + 
    geom_spatraster(data = agi_250m) + 
    geom_spatvector(data = one_poly_250m, color = "black", fill = NA, linewidth = 0.8) +
    geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
    scale_x_continuous(expand=c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49))+
    scale_fill_whitebox_c(palette = "muted", direction = -1)+
    labs(fill = "AGI")+
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0("Area: ", round(perc_area_250m, 2), "%")), 
              hjust = 1.1, vjust = 2, size = 8, color = "black")+
    theme_map()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18, color = "black"), 
          axis.text.y = element_text(size = 18, color = "black"), 
          axis.title = element_text(size = 20, color = "black"),
          legend.position = "none") 
  
  }
  if(fig_pos == 2){
    
    agi_one_250m <- ggplot() + 
      geom_spatraster(data = agi_250m) + 
      geom_spatvector(data = one_poly_250m, color = "black", fill = NA, linewidth = 0.8) +
      geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
      scale_x_continuous(expand=c(0,0),limits = c(-153,-103)) +
      scale_y_continuous(expand=c(0,0),limits = c(1,49))+
      scale_fill_whitebox_c(palette = "muted", direction = -1)+
      labs(fill = "AGI")+
      geom_text(aes(x = Inf, y = Inf, 
                    label = paste0("Area: ", round(perc_area_250m, 2), "%")), 
                hjust = 1.1, vjust = 2, size = 8, color = "black")+
      theme_map()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18, color = "black"), 
            axis.title.x = element_text(size = 20, color = "black"),
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(),
    legend.position = "none") 
    
  }
  if(fig_pos == 3){
    
    agi_one_250m <- ggplot() + 
      geom_spatraster(data = agi_250m) + 
      geom_spatvector(data = one_poly_250m, color = "black", fill = NA, linewidth = 0.8) +
      geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
      scale_x_continuous(expand=c(0,0),limits = c(-153,-103)) +
      scale_y_continuous(expand=c(0,0),limits = c(1,49))+
      scale_fill_whitebox_c(palette = "muted", direction = -1)+
      labs(fill = "AGI")+
      geom_text(aes(x = Inf, y = Inf, 
                    label = paste0("Area: ", round(perc_area_250m, 2), "%")), 
                hjust = 1.1, vjust = 2, size = 8, color = "black")+
      theme_map()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18, color = "black"), 
            axis.text.y = element_blank(), 
            axis.title.x = element_text(size = 20, color = "black"),
            axis.title.y = element_blank(), 
            legend.text = element_text(size = 18), 
            legend.title = element_text(size = 20), 
            legend.position = "right", 
            legend.justification = "center",
            legend.background = element_blank(), 
            legend.box.background = element_blank())
    
  }
  
  
  return(agi_one_250m)
  
  #end function 
}

agi_maps_layerd <- function(rast_folder_base = NULL, rast_folder_LN = NULL, rast_folder_EN = NULL){
#load rasters
  #base agi
    agi_250m_base <- rast(list.files(here(rast_folder_base), full.names = TRUE, pattern = "250m"))
    
  #LN agi
    agi_250m_LN <- rast(list.files(here(rast_folder_LN), full.names = TRUE, pattern = "250m"))
    
  #EN agi
    agi_250m_EN <- rast(list.files(here(rast_folder_EN), full.names = TRUE, pattern = "250m"))
  
  #create shape file of areas surrounding AGI < 1
    #base
  one_250m_base <- raster::clamp(agi_250m_base, upper = 1, values = FALSE) #create raster of values below 1
  one_poly_250m_base <- as.polygons(ext(one_250m_base))
  one_poly_250m_base <- as.polygons(one_250m_base > -Inf) 
  
    #LN
  one_250m_LN <- raster::clamp(agi_250m_LN, upper = 1, values = FALSE) #create raster of values below 1
  one_poly_250m_LN <- as.polygons(ext(one_250m_LN))
  one_poly_250m_LN <- as.polygons(one_250m_LN > -Inf)
  
    #EN
  one_250m_EN <- raster::clamp(agi_250m_EN, upper = 1, values = FALSE) #create raster of values below 1
  one_poly_250m_EN <- as.polygons(ext(one_250m_EN))
  one_poly_250m_EN <- as.polygons(one_250m_EN > -Inf)
  
  #calculate percent area polygon takes up of raster 
    #base
  poly_area_250m_base <- expanse(one_poly_250m_base)
  rast_area_base <- expanse(agi_250m_base)
  perc_area_250m_base <- (poly_area_250m_base/rast_area_base$area[1])*100
  
    #LN
  poly_area_250m_LN <- expanse(one_poly_250m_LN)
  rast_area_LN <- expanse(agi_250m_LN)
  perc_area_250m_LN <- (poly_area_250m_LN/rast_area_LN$area[1])*100
  
    #EN
  poly_area_250m_EN <- expanse(one_poly_250m_EN)
  rast_area_EN <- expanse(agi_250m_EN)
  perc_area_250m_EN <- (poly_area_250m_EN/rast_area_EN$area[1])*100
  
  #land shapes
  map.world = map_data(map="world")
  testt=map.world %>% filter(long<=180)
  
  #map
    agi_one_250m <- ggplot() + 
      geom_spatraster(data = agi_250m_base) + 
      geom_spatvector(data = one_poly_250m_base, color = "black", fill = NA, linewidth = 0.8) +
      geom_spatvector(data = one_poly_250m_LN, color = "lightskyblue1", fill = NA, linewidth = 0.8) +
      geom_spatvector(data = one_poly_250m_EN, color = "white", fill = NA, linewidth = 0.8) +
      geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
      scale_x_continuous(expand=c(0,0),limits = c(-153,-103)) +
      scale_y_continuous(expand=c(0,0),limits = c(1,49))+
      scale_fill_whitebox_c(palette = "muted", direction = -1)+
      labs(fill = "AGI")+
      geom_text(aes(x = Inf, y = Inf, 
                    label = paste0("Area: ", round(perc_area_250m_base, 2), "%")), 
                hjust = 1.1, vjust = 2, size = 8, color = "black")+
      geom_text(aes(x = Inf, y = Inf, 
                    label = paste0("La Niña: +", round(perc_area_250m_LN - perc_area_250m_base, 2), "%")), 
                hjust = 1.05, vjust = 4, size = 8, color = "black")+
      geom_text(aes(x = Inf, y = Inf, 
                    label = paste0("El Niño: -", round(perc_area_250m_base - perc_area_250m_EN, 2), "%")), 
                hjust = 1.05, vjust = 6, size = 8, color = "black")+
      theme_map()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18, color = "black"),
            axis.text.y = element_text(color = "black", size = 18),
            axis.title =element_text(size = 20, color = "black"), 
            legend.text = element_text(size = 18), 
            legend.title = element_text(size = 20), 
            legend.position = "right", 
            legend.justification = "center",
            legend.background = element_blank(), 
            legend.box.background = element_blank())
    
  return(agi_one_250m)
  
  #end function 
}


### libraries ####
library(tidyverse)
library(terra)
library(sf)
library(here)


hsi_rast_gen <- function(date_start = c("2003-01-01"), date_end = c("2015-12-31"), season = "WSpSuF"){
  
  #load rast files -------------------------------------------------------------------------------------
  rast_file_daily_0m <- rast(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_CHL_Temp_SO_UO_UOSTR_VO_VOSTR_SSH_MLD_0m_Jan2003_Dec2015_0.25_D.nc"))
  rast_file_daily_250m <- rast(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_Temp_SO_250m_Jan2003_Dec2015_0.25_D.nc"))
  
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
  
  if(length(time(rast_daily_0m_sub)) > 1){
    rast_daily_0m_sub <- tapp(rast_daily_0m_sub, varnames(rast_daily_0m_sub), mean)
    rast_daily_250m_sub <- tapp(rast_daily_250m_sub, varnames(rast_daily_250m_sub), mean)
  }
 
  #subset seasonal raster files ----------------------------------------------------------------------------
  seas_0m_sub_rasts <- list()
  seas_250m_sub_rasts <- list()
  
    #subset rasters for seasons that are present
  if(str_detect(season, "W")){
    seas_0m_sub_rasts$rast_seas_0m_subW <- subset(rast_file_seas_0m, month(time(rast_file_seas_0m)) == 12)
    seas_250m_sub_rasts$rast_seas_250m_subW <- subset(rast_file_seas_250m, month(time(rast_file_seas_250m)) == 12)
  } else if(str_detect(season, "Sp")) {
    seas_0m_sub_rasts$rast_seas_0m_subSp <- subset(rast_file_seas_0m, month(time(rast_file_seas_0m)) == 3)
    seas_250m_sub_rasts$rast_seas_250m_subSp <- subset(rast_file_seas_250m, month(time(rast_file_seas_250m)) == 3)
  } else if(str_detect(season, "Su")) {
    seas_0m_sub_rasts$rast_seas_0m_subSu <- subset(rast_file_seas_0m, month(time(rast_file_seas_0m)) == 6)
    seas_250m_sub_rasts$rast_seas_250m_subSu <- subset(rast_file_seas_250m, month(time(rast_file_seas_250m)) == 6)
  } else if(str_detect(season, "F")){
    seas_0m_sub_rasts$rast_seas_0m_subF <- subset(rast_file_seas_0m, month(time(rast_file_seas_0m)) == 9)
    seas_250m_sub_rasts$rast_seas_250m_subF <- subset(rast_file_seas_250m, month(time(rast_file_seas_250m)) == 9)
  }
  
    #generate mean season raster
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
  
    #generate mean annual raster
  rast_ann_0m_sub <- rast(rast_ann_0m_sub)
  rast_ann_250m_sub <- rast(rast_ann_250m_sub)
  
  if(length(year(time(rast_ann_0m_sub))) > 1){
    
  rast_ann_0m_sub <- tapp(rast_ann_0m_sub, unique(varnames(rast_ann_0m_sub)), mean)
  rast_ann_250m_sub <- tapp(rast_ann_250m_sub, unique(varnames(rast_ann_250m_sub)), mean)
  
  }
  
  #get bathy rasters ------------------------------------------------------------------------------------------
  bathy <- rast(here("data/enviro/psat_spot_all/bathy_gebco/processed/gebco_bathy_0.25deg2.nc"))
  
  #generate AGI rasters ---------------------------------------------------------------------------------------
  source(here("functions/oxy_demand_functions.R"))
  OxyThresh_0m = 0.04928389
  OxyThresh_250m = 0.03816138
  Tpref = 16.45201
  
    #daily rast AGI
  demand_daily_0m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_0m, T_C = rast_daily_0m_sub$votemper)
  atm_daily_0m <- rast_to_atm(do = rast_daily_0m_sub$o2, so = rast_daily_0m_sub$vosaline, temp = rast_daily_0m_sub$votemper, depth = 0)
  AGI_daily_0m <- atm_daily_0m/demand_daily_0m
  
  demand_daily_250m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh_250m, T_C = rast_daily_250m_sub$votemper)
  atm_daily_250m <- rast_to_atm(do = rast_daily_250m_sub$o2, so = rast_daily_250m_sub$vosaline, temp = rast_daily_250m_sub$votemper, depth = 250)
  AGI_daily_250m <- atm_daily_250m/demand_daily_250m
  
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
  
  #save HSI raster input file ---------------------------------------------------------------------------------
  
# end function  
}

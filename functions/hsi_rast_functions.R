### libraries ####
library(tidyverse)
library(terra)
library(sf)
library(here)
library(patchwork)

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
    #base model
  base_rast <- c(bathy, rast_daily_0m_sub$votemper, rast_daily_0m_sub$vosaline,  rast_daily_0m_sub$chl, rast_daily_0m_sub$sossheig, bathy_sd, rast_daily_0m_sub$somxlavt)
  names(base_rast) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
    
    #DO model
  do_rast <- c(rast_daily_0m_sub$o2, rast_ann_250m_sub$o2, rast_seas_0m_sub$o2_1, rast_daily_0m_sub$votemper, rast_seas_250m_sub$o2_1, bathy, rast_daily_0m_sub$vosaline, rast_daily_0m_sub$chl, rast_ann_0m_sub$o2, rast_daily_250m_sub$o2, rast_daily_0m_sub$sossheig, rast_daily_0m_sub$somxlavt, bathy_sd)
  names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
    
    #AGI model 
  agi_rast <- c(rast_daily_0m_sub$votemper, AGI_ann_250m, AGI_daily_0m, bathy, AGI_seas_0m, rast_daily_0m_sub$vosaline, AGI_seas_250m, AGI_ann_0m, rast_daily_0m_sub$chl, AGI_daily_250m, bathy_sd, rast_daily_0m_sub$somxlavt, rast_daily_0m_sub$sossheig)
  names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  
  #save HSI raster input file ---------------------------------------------------------------------------------
  dir.create(here(paste0("data/enviro/psat_spot_all/hsi_rasts/", output_name)), showWarnings = FALSE) 
  
    #base model
  writeCDF(base_rast, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts/", output_name,"/", output_name,"_base_rast.nc")))
  
    #DO model
  writeCDF(do_rast, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts/", output_name,"/", output_name,"_do_rast.nc")))
  
    #AGI model
  writeCDF(agi_rast, filename = here(paste0("data/enviro/psat_spot_all/hsi_rasts/", output_name,"/", output_name,"_agi_rast.nc")))
  
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
        size = 14),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        color = "black",
        size = 12),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
    )
  
#end function
}


### hsi map function ####
hsi_maps <- function(rast_folder){
  #only makes maps from base model and the final DO and AGI models. Does not include maps from overfit DO and AGI models. 
  
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
  
    #predict to df
  base_df <- as.points(base_pred) %>% st_as_sf() %>% as.data.frame()
  colnames(base_df) = c("value", "geometry")
  
  do_df <- as.points(do_pred) %>% st_as_sf() %>% as.data.frame()
  colnames(do_df) = c("value", "geometry")
  
  agi_df <- as.points(agi_pred) %>% st_as_sf() %>% as.data.frame()
  colnames(agi_df) = c("value", "geometry")
  
  #plot maps --------------------------------------------------------------------------------------------------------
    #land files
  map.world = map_data(map="world")
  testt = map.world %>% filter(long <= 180)
  
    #base map
  base_map <- ggplot() +
    geom_spatraster(data = base_pred) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "muted", direction = -1) +
    ggtitle("Base model") +
    theme_map() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
    #do map
  do_map <- ggplot() +
    geom_spatraster(data = do_pred) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "muted", direction = -1) +
    ggtitle("DO model") +
    theme_map() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
    #agi map
  agi_map <- ggplot() +
    geom_spatraster(data = agi_pred) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "muted", direction = -1) +
    ggtitle("AGI model") +
    theme_map() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #combine and return maps ------------------------------------------------------------------------------------------------------
  all_maps <- base_map|do_map|agi_map
  
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
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.text.y = element_blank(), 
          axis.title.y = element_blank())
  
  base_agi_map <- ggplot() + 
    geom_spatraster(data = diff_base_agi) + 
    geom_map(data = testt, map = testt, aes(map_id = region, x = long, y = lat),fill = "darkgrey", color = "black") +
    scale_x_continuous(expand = c(0,0), limits = c(-153,-103)) +
    scale_y_continuous(expand = c(0,0),limits = c(1,49))+
    scale_fill_whitebox_c(palette = "muted", direction = -1) +
    ggtitle("Comparing HSI: Base vs. AGI") +
    labs(fill = "% change") +
    theme_map() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.text.y = element_blank(), 
          axis.title.y = element_blank())
  
  do_agi_map <- ggplot() + 
    geom_spatraster(data = diff_do_agi) + 
    geom_map(data = testt, map = testt, aes(map_id = region, x = long, y = lat),fill = "darkgrey", color = "black") +
    scale_x_continuous(expand = c(0,0), limits = c(-153,-103)) +
    scale_y_continuous(expand = c(0,0),limits = c(1,49))+
    scale_fill_whitebox_c(palette = "muted", direction = -1) +
    ggtitle("Comparing HSI: DO vs. AGI") +
    labs(fill = "% change") +
    theme_map() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.text.y = element_blank(), 
          axis.title.y = element_blank())
  
  #combine and return maps --------------------------------------------------------------------------------------------
  all_maps <- base_do_map | base_agi_map | do_agi_map
  
  return(all_maps)

#end function
}

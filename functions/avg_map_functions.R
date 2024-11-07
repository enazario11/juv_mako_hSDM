#libraries
library(tidyverse)
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

set.seed(1004)

### average over study period ####
hsi_maps_avg <- function(rast_folder, ms = c("Y", "N"), iter = 20){
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
  
  # load model locations ---------------------------------------------------------------------------------------------
  base_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/base"), full.names = TRUE)
  do_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/do"), full.names = TRUE)
  agi_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/agi"), full.names = TRUE)
  combo_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/combo"), full.names = TRUE)
  
  base_list <- rast()
  do_list <- rast()
  agi_list <- rast()
  combo_list <- rast()
  
  #for loop to create raster for each model iteration
  for(i in 1:iter){
    #creating map dfs -------------------------------------------------------------------------------------------------
    print(i)
    
    #predict
    base_mod <- readRDS(base_mod_files[i])
    base_pred <- predict(base_rast, base_mod, type = "response", n.tress = base_mod$gbm.call$best.trees, na.rm = FALSE)
    base_pred <- crop(base_pred, extent)
    base_list <- c(base_list, base_pred)
    base_avg <- mean(base_list)
    
    do_mod <- readRDS(do_mod_files[i])
    do_pred <- predict(do_rast, do_mod, type = "response", n.tress = do_mod$gbm.call$best.trees, na.rm = FALSE)
    do_pred <- crop(do_pred, extent)
    do_list <- c(do_list, do_pred)
    do_avg <- mean(do_list)
    
    agi_mod <- readRDS(agi_mod_files[i])
    agi_pred <- predict(agi_rast, agi_mod, type = "response", n.tress = agi_mod$gbm.call$best.trees, na.rm = FALSE)
    agi_pred <- crop(agi_pred, extent)
    agi_list <- c(agi_list, agi_pred)
    agi_avg <- mean(agi_list)
    
    do_agi_comb <- readRDS(combo_mod_files[i])
    do_agi_combo <- predict(do_agi_rast, do_agi_comb, type = "response", n.trees = do_agi_comb$gbm.call$best.trees, na.rm = FALSE)
    do_agi_combo <- crop(do_agi_combo, extent)
    combo_list <- c(combo_list, do_agi_combo)
    combo_avg <- mean(combo_list)
    
  }
  
  #plot maps --------------------------------------------------------------------------------------------------------
  #land files
  map.world = map_data(map="world")
  testt = map.world %>% filter(long <= 180)
  
  #base map
  #calculate percent area polygon takes up of raster 
  base_hsi <- raster::clamp(base_avg, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_base <- expanse(base_hsi)
  rast_area_base <- expanse(base_avg)
  perc_area_base <- (hsi_area_base/rast_area_base$area[1])*100
  #print(paste0("Base hsi > 0.50:", " ", round(perc_area_base$area[1], 2), "%"))
  
  base_map <- ggplot() +
    geom_spatraster(data = base_avg) +
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
  
  if(ms == "Y"){ggsave(here("figs/ms/fig6_hsi_all/indiv_panels/base_avg_all.png"), base_map, height = 5, width = 5)}
  
  #do map
  #calculate percent area polygon takes up of raster 
  do_hsi <- raster::clamp(do_avg, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_do <- expanse(do_hsi)
  rast_area_do <- expanse(do_avg)
  perc_area_do <- (hsi_area_do/rast_area_do$area[1])*100
  #print(paste0("DO hsi > 0.50:", " ", round(perc_area_do$area[1], 2), "%"))
  
  do_map <- ggplot() +
    geom_spatraster(data = do_avg) +
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
  
  if(ms == "Y"){ggsave(here("figs/ms/fig6_hsi_all/indiv_panels/do_avg_all.png"), do_map, height = 5, width = 5)}
  
  #agi map
  #calculate percent area polygon takes up of raster 
  agi_hsi <- raster::clamp(agi_avg, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_agi <- expanse(agi_hsi)
  rast_area_agi <- expanse(agi_avg)
  perc_area_agi <- (hsi_area_agi/rast_area_agi$area[1])*100
  #print(paste0("agi hsi > 0.50:", " ", round(perc_area_agi$area[1], 2), "%"))
  
  agi_map <- ggplot() +
    geom_spatraster(data = agi_avg) +
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
  
  if(ms == "Y"){ggsave(here("figs/ms/fig6_hsi_all/indiv_panels/agi_avg_all.png"), agi_map, height = 5, width = 5)}
  
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
  rast_area_combo <- expanse(combo_avg)
  perc_area_combo <- (hsi_area_combo/rast_area_combo$area[1])*100
  #print(paste0("combo hsi > 0.50:", " ", round(perc_area_combo$area[1], 2), "%"))
  
  combo_map <- ggplot() +
    geom_spatraster(data = combo_avg) +
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
  
  if(ms == "Y"){ggsave(here("figs/ms/fig6_hsi_all/indiv_panels/combo_avg.png"), combo_map, height = 5, width = 5)}
  
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


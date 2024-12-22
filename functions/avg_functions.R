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
  #do_agi_file <- list.files(here(rast_folder), pattern = "comb", full.names = TRUE)
  
  base_rast <- rast(base_rast_file)
  names(base_rast) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
  
  do_rast <- rast(do_rast_file)
  names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
  
  agi_rast <- rast(agi_rast_file)
  names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  
  # do_agi_rast <- rast(do_agi_file)
  # names(do_agi_rast) <- c("temp_mean", "AGI_250m_ann", "bathy_mean", "sal_mean", "AGI_250m_seas", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean", "o2_mean_0m", "o2_mean_0m_ann", "o2_mean_0m_seas")
  # 
  extent <- c(-153, -103, 1 , 49)
  
  # load model locations ---------------------------------------------------------------------------------------------
  base_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/base"), full.names = TRUE)
  do_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/do"), full.names = TRUE)
  agi_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/agi"), full.names = TRUE)
  #combo_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/combo"), full.names = TRUE)
  
  base_list <- rast()
  do_list <- rast()
  agi_list <- rast()
  #combo_list <- rast()
  
  #for loop to create raster for each model iteration
  for(i in 1:iter){
    #creating map dfs -------------------------------------------------------------------------------------------------
    print(i)
    
    #predict
    base_mod <- readRDS(base_mod_files[i])
    base_pred <- predict(base_rast, base_mod, type = "response", n.tress = base_mod$gbm.call$best.trees, na.rm = FALSE)
    base_pred <- crop(base_pred, extent)
    base_list <- c(base_list, base_pred)
    
    do_mod <- readRDS(do_mod_files[i])
    do_pred <- predict(do_rast, do_mod, type = "response", n.tress = do_mod$gbm.call$best.trees, na.rm = FALSE)
    do_pred <- crop(do_pred, extent)
    do_list <- c(do_list, do_pred)
    
    agi_mod <- readRDS(agi_mod_files[i])
    agi_pred <- predict(agi_rast, agi_mod, type = "response", n.tress = agi_mod$gbm.call$best.trees, na.rm = FALSE)
    agi_pred <- crop(agi_pred, extent)
    agi_list <- c(agi_list, agi_pred)
    
    # do_agi_comb <- readRDS(combo_mod_files[i])
    # do_agi_combo <- predict(do_agi_rast, do_agi_comb, type = "response", n.trees = do_agi_comb$gbm.call$best.trees, na.rm = FALSE)
    # do_agi_combo <- crop(do_agi_combo, extent)
    # combo_list <- c(combo_list, do_agi_combo)
    
  }
  
  base_avg <- mean(base_list)
  do_avg <- mean(do_list)
  agi_avg <- mean(agi_list)
  #combo_avg <- mean(combo_list)
  
  
  #plot maps--------------------------------------------------------------------------------------------------------
  #land files
  map.world = map_data(map="world")
  testt = map.world %>% filter(long <= 180)
  
  #base map
  #calculate percent area polygon takes up of raster 
  base_hsi <- raster::clamp(base_avg, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_base <- expanse(base_hsi)
  rast_area_base <- expanse(base_avg)
  perc_area_base <- (hsi_area_base/rast_area_base$area[1])*100

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
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  if(ms == "Y"){ggsave(here("figs/ms/fig7_hsi_all/indiv_panels/base_avg_all.png"), base_map, height = 5, width = 5)}
  
  #do map
  #calculate percent area polygon takes up of raster 
  do_hsi <- raster::clamp(do_avg, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_do <- expanse(do_hsi)
  rast_area_do <- expanse(do_avg)
  perc_area_do <- (hsi_area_do/rast_area_do$area[1])*100

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
    theme(axis.text.y = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  if(ms == "Y"){ggsave(here("figs/ms/fig7_hsi_all/indiv_panels/do_avg_all.png"), do_map, height = 5, width = 5)}
  
  #agi map
  #calculate percent area polygon takes up of raster 
  agi_hsi <- raster::clamp(agi_avg, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_agi <- expanse(agi_hsi)
  rast_area_agi <- expanse(agi_avg)
  perc_area_agi <- (hsi_area_agi/rast_area_agi$area[1])*100

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
    theme(axis.text.x = element_text(angle = 45, hjust = 0.3, color = "black"), 
          legend.position = "none", 
          axis.text.y = element_blank(), 
          axis.title.y = element_blank())
  
  if(ms == "Y"){ggsave(here("figs/ms/fig7_hsi_all/indiv_panels/agi_avg_all.png"), agi_map, height = 5, width = 5)}
  
  #do agi combo map
  #calculate percent area polygon takes up of raster 
  # combo_hsi <- raster::clamp(do_agi_combo, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  # 
  # hsi_area_combo <- expanse(combo_hsi)
  # rast_area_combo <- expanse(combo_avg)
  # perc_area_combo <- (hsi_area_combo/rast_area_combo$area[1])*100
  # 
  # combo_map <- ggplot() +
  #   geom_spatraster(data = combo_avg) +
  #   geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  #   scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  #   scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  #   scale_fill_whitebox_c(palette = "deep", direction = -1) +
  #   ggtitle("DO + AGI combo model") +
  #   theme_map() +
  #   geom_text(aes(x = Inf, y = Inf, 
  #                 label = paste0(round(perc_area_combo$area[1], 2), "%")), 
  #             hjust = 1.1, vjust = 2, size = 6, color = "black")+
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #         axis.text.y = element_blank(), 
  #         axis.title.y = element_blank(), 
  #         legend.position = "none")
  
  #if(ms == "Y"){ggsave(here("figs/ms/fig7_hsi_all/indiv_panels/combo_avg.png"), combo_map, height = 5, width = 5)}
  
  
  #combine and return maps ------------------------------------------------------------------------------------------------------
  all_maps <- (base_map|do_map|agi_map)+
    plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "HSI")
  
  return(all_maps)
  
  #end function  
}

# enso diet maps ####
hsi_maps_enso_avg <- function(rast_folder, enso, iter = 20){
  #only makes maps from base model and the final DO and AGI models. Does not include maps from overfit DO and AGI models. 
  
  #load raster files -----------------------------------------------------------------------------------------------
  base_rast_file <- list.files(here(rast_folder), pattern = "base", full.names = TRUE)
  do_rast_file <- list.files(here(rast_folder), pattern = "do", full.names = TRUE)
  agi_rast_file <- list.files(here(rast_folder), pattern = "agi", full.names = TRUE)
  #do_agi_file <- list.files(here(rast_folder), pattern = "comb", full.names = TRUE)
  
  base_rast <- rast(base_rast_file)
  names(base_rast) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
  
  do_rast <- rast(do_rast_file)
  names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
  
  agi_rast <- rast(agi_rast_file)
  names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  
  # do_agi_rast <- rast(do_agi_file)
  # names(do_agi_rast) <- c("temp_mean", "AGI_250m_ann", "bathy_mean", "sal_mean", "AGI_250m_seas", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean", "o2_mean_0m", "o2_mean_0m_ann", "o2_mean_0m_seas")
  # 
  extent <- c(-153, -103, 1 , 49)
  
  #creating map dfs -------------------------------------------------------------------------------------------------
  base_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/base"), full.names = TRUE)
  do_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/do"), full.names = TRUE)
  agi_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/agi"), full.names = TRUE)
  #combo_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/combo"), full.names = TRUE)
  
  base_list <- rast()
  do_list <- rast()
  agi_list <- rast()
  #combo_list <- rast()
  
  #for loop to create raster for each model iteration
  for(i in 1:iter){
    #creating map dfs -------------------------------------------------------------------------------------------------
    print(i)
    
    #predict
    base_mod <- readRDS(base_mod_files[i])
    base_pred <- predict(base_rast, base_mod, type = "response", n.tress = base_mod$gbm.call$best.trees, na.rm = FALSE)
    base_pred <- crop(base_pred, extent)
    base_list <- c(base_list, base_pred)
    
    do_mod <- readRDS(do_mod_files[i])
    do_pred <- predict(do_rast, do_mod, type = "response", n.tress = do_mod$gbm.call$best.trees, na.rm = FALSE)
    do_pred <- crop(do_pred, extent)
    do_list <- c(do_list, do_pred)
    
    agi_mod <- readRDS(agi_mod_files[i])
    agi_pred <- predict(agi_rast, agi_mod, type = "response", n.tress = agi_mod$gbm.call$best.trees, na.rm = FALSE)
    agi_pred <- crop(agi_pred, extent)
    agi_list <- c(agi_list, agi_pred)
    
    # do_agi_comb <- readRDS(combo_mod_files[i])
    # do_agi_combo <- predict(do_agi_rast, do_agi_comb, type = "response", n.trees = do_agi_comb$gbm.call$best.trees, na.rm = FALSE)
    # do_agi_combo <- crop(do_agi_combo, extent)
    # combo_list <- c(combo_list, do_agi_combo)
    
  }
  
  #take average of rasters produced from each model
  base_avg <- mean(base_list)
  do_avg <- mean(do_list)
  agi_avg <- mean(agi_list)
  #combo_avg <- mean(combo_list)
  
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
  print(paste0("Base hsi > 0.75:", " ", round(perc_area_base$area[1], 2), "%"))
  
  base_map <- ggplot() +
    geom_spatraster(data = base_avg) +
    geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
    scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
    scale_fill_whitebox_c(palette = "deep", direction = -1) +
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0(round(perc_area_base$area[1], 2), "%")), 
              hjust = 1.1, vjust = 2, size = 6, color = "black")+
    theme_map() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none", 
          axis.title.x = element_blank())
  
  
  #do map
  #calculate percent area polygon takes up of raster 
  do_hsi <- raster::clamp(do_avg, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_do <- expanse(do_hsi)
  rast_area_do <- expanse(do_avg)
  perc_area_do <- (hsi_area_do/rast_area_do$area[1])*100
  print(paste0("DO hsi > 0.75:", " ", round(perc_area_do$area[1], 2), "%"))

    do_map <- ggplot() +
      geom_spatraster(data = do_avg) +
      geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
      scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
      scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
      scale_fill_whitebox_c(palette = "deep", direction = -1)+
      geom_text(aes(x = Inf, y = Inf, 
                    label = paste0(round(perc_area_do$area[1], 2), "%")), 
                hjust = 1.1, vjust = 2, size = 6, color = "black")+
      theme_map() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none")
  
  #agi map
  #calculate percent area polygon takes up of raster 
  agi_hsi <- raster::clamp(agi_avg, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_agi <- expanse(agi_hsi)
  rast_area_agi <- expanse(agi_avg)
  perc_area_agi <- (hsi_area_agi/rast_area_agi$area[1])*100
  print(paste0("agi hsi > 0.75:", " ", round(perc_area_agi$area[1], 2), "%"))

    agi_map <- ggplot() +
      geom_spatraster(data = agi_avg) +
      geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
      scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
      scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
      scale_fill_whitebox_c(palette = "deep", direction = -1) +
      geom_text(aes(x = Inf, y = Inf, 
                    label = paste0(round(perc_area_agi$area[1], 2), "%")), 
                hjust = 1.1, vjust = 2, size = 6, color = "black")+
      theme_map() +
      theme(axis.text.x = element_blank(), 
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none")   
  
  #do agi combo map
  #calculate percent area polygon takes up of raster 
  # combo_hsi <- raster::clamp(combo_avg, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  # 
  # hsi_area_combo <- expanse(combo_hsi)
  # rast_area_combo <- expanse(combo_avg)
  # perc_area_combo <- (hsi_area_combo/rast_area_combo$area[1])*100
  # print(paste0("combo hsi > 0.75:", " ", round(perc_area_combo$area[1], 2), "%"))
  # 
  # combo_map <- ggplot() +
  #   geom_spatraster(data = combo_avg) +
  #   geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  #   scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  #   scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  #   scale_fill_whitebox_c(palette = "deep", direction = -1) +
  #   geom_text(aes(x = Inf, y = Inf, 
  #                 label = paste0(round(perc_area_combo$area[1], 2), "%")), 
  #             hjust = 1.1, vjust = 2, size = 3.4, color = "black")+
  #   theme_map() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 0.5), 
  #         axis.text.y = element_blank(),
  #         axis.title.y = element_blank(),
  #         legend.position = "none")
  
  #combine and return maps ------------------------------------------------------------------------------------------------------
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
                                                                                                                                                                                                                   axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3)))
    } 
    if(enso == "LN"){
      
      all_maps <- (base_map)/(do_map)/(agi_map)
    }
    if(enso == "diff"){
      all_maps <- (base_map+
                     theme(axis.text.y = element_text(size = 14, color = "black"), 
                           axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3)))/(do_map+
                                                                                                                 theme(axis.text.y = element_text(size = 14, color = "black"), 
                                                                                                                       axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3)))/(agi_map+
                                                                                                                                                                                                             theme(axis.text.x = element_text(size = 14, color = "black", angle = 45, vjust = 0.3), 
                                                                                                                                                                                                                   axis.text.y = element_text(size = 14, color = "black"), 
                                                                                                                                                                                                                   axis.title.y = element_text(size = 16, color = "black", angle = 90, vjust = 0.3), 
                                                                                                                                                                                                                   axis.title.x = element_text(size = 16, color = "black", vjust = 0.7)))
      
      all_maps <- all_maps+
        plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "HSI")
    }
  
  return(all_maps)
  
  #end function  
}


hsi_maps_difference_enso_avg <- function(neut_rast_folder, enso_rast_folder, enso, iter = 20){
  
  ### NEUTRAL
  #load raster files -----------------------------------------------------------------------------------------------
  base_rast_file <- list.files(here(neut_rast_folder), pattern = "base", full.names = TRUE)
  do_rast_file <- list.files(here(neut_rast_folder), pattern = "do", full.names = TRUE)
  agi_rast_file <- list.files(here(neut_rast_folder), pattern = "agi", full.names = TRUE)
  #do_agi_file <- list.files(here(neut_rast_folder), pattern = "comb", full.names = TRUE)
  
  base_rast <- rast(base_rast_file)
  names(base_rast) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
  
  do_rast <- rast(do_rast_file)
  names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
  
  agi_rast <- rast(agi_rast_file)
  names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  
  # do_agi_rast <- rast(do_agi_file)
  # names(do_agi_rast) <- c("temp_mean", "AGI_250m_ann", "bathy_mean", "sal_mean", "AGI_250m_seas", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean", "o2_mean_0m", "o2_mean_0m_ann", "o2_mean_0m_seas")
  # 
  extent <- c(-153, -103, 1 , 49)
  
  #creating map dfs -------------------------------------------------------------------------------------------------
  #predict
  base_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/base"), full.names = TRUE)
  do_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/do"), full.names = TRUE)
  agi_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/agi"), full.names = TRUE)
  #combo_mod_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters/combo"), full.names = TRUE)
  
  base_list <- rast()
  do_list <- rast()
  agi_list <- rast()
  #combo_list <- rast()
  
  #for loop to create raster for each model iteration
  for(i in 1:iter){
    #creating map dfs -------------------------------------------------------------------------------------------------
    print(i)
    
    #predict
    base_mod <- readRDS(base_mod_files[i])
    base_pred <- predict(base_rast, base_mod, type = "response", n.tress = base_mod$gbm.call$best.trees, na.rm = FALSE)
    base_pred <- crop(base_pred, extent)
    base_list <- c(base_list, base_pred)
    
    do_mod <- readRDS(do_mod_files[i])
    do_pred <- predict(do_rast, do_mod, type = "response", n.tress = do_mod$gbm.call$best.trees, na.rm = FALSE)
    do_pred <- crop(do_pred, extent)
    do_list <- c(do_list, do_pred)
    
    agi_mod <- readRDS(agi_mod_files[i])
    agi_pred <- predict(agi_rast, agi_mod, type = "response", n.tress = agi_mod$gbm.call$best.trees, na.rm = FALSE)
    agi_pred <- crop(agi_pred, extent)
    agi_list <- c(agi_list, agi_pred)
    
    # do_agi_comb <- readRDS(combo_mod_files[i])
    # do_agi_combo <- predict(do_agi_rast, do_agi_comb, type = "response", n.trees = do_agi_comb$gbm.call$best.trees, na.rm = FALSE)
    # do_agi_combo <- crop(do_agi_combo, extent)
    # combo_list <- c(combo_list, do_agi_combo)
    
  }
  
  base_avg_neut <- mean(base_list)
  do_avg_neut <- mean(do_list)
  agi_avg_neut <- mean(agi_list)
  #combo_avg_neut <- mean(combo_list)
  
  ### ENSO
  #load raster files -----------------------------------------------------------------------------------------------
  base_rast_file_enso <- list.files(here(enso_rast_folder), pattern = "base", full.names = TRUE)
  do_rast_file_enso  <- list.files(here(enso_rast_folder), pattern = "do", full.names = TRUE)
  agi_rast_file_enso  <- list.files(here(enso_rast_folder), pattern = "agi", full.names = TRUE)
  #do_agi_file_enso  <- list.files(here(enso_rast_folder), pattern = "comb", full.names = TRUE)
  
  base_rast_enso  <- rast(base_rast_file_enso )
  names(base_rast_enso ) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
  
  do_rast_enso  <- rast(do_rast_file_enso )
  names(do_rast_enso ) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
  
  agi_rast_enso  <- rast(agi_rast_file_enso )
  names(agi_rast_enso ) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  # 
  # do_agi_rast_enso  <- rast(do_agi_file_enso )
  # names(do_agi_rast_enso ) <- c("temp_mean", "AGI_250m_ann", "bathy_mean", "sal_mean", "AGI_250m_seas", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean", "o2_mean_0m", "o2_mean_0m_ann", "o2_mean_0m_seas")
  # 
  #creating map dfs -------------------------------------------------------------------------------------------------
  #predict
  base_list <- rast()
  do_list <- rast()
  agi_list <- rast()
  #combo_list <- rast()
  
  #for loop to create raster for each model iteration
  for(i in 1:iter){
    #creating map dfs -------------------------------------------------------------------------------------------------
    print(i)
    
    #predict
    base_mod <- readRDS(base_mod_files[i])
    base_pred <- predict(base_rast_enso, base_mod, type = "response", n.tress = base_mod$gbm.call$best.trees, na.rm = FALSE)
    base_pred <- crop(base_pred, extent)
    base_list <- c(base_list, base_pred)
    
    do_mod <- readRDS(do_mod_files[i])
    do_pred <- predict(do_rast_enso, do_mod, type = "response", n.tress = do_mod$gbm.call$best.trees, na.rm = FALSE)
    do_pred <- crop(do_pred, extent)
    do_list <- c(do_list, do_pred)
    
    agi_mod <- readRDS(agi_mod_files[i])
    agi_pred <- predict(agi_rast_enso, agi_mod, type = "response", n.tress = agi_mod$gbm.call$best.trees, na.rm = FALSE)
    agi_pred <- crop(agi_pred, extent)
    agi_list <- c(agi_list, agi_pred)
    
    # do_agi_comb <- readRDS(combo_mod_files[i])
    # do_agi_combo <- predict(do_agi_rast_enso, do_agi_comb, type = "response", n.trees = do_agi_comb$gbm.call$best.trees, na.rm = FALSE)
    # do_agi_combo <- crop(do_agi_combo, extent)
    # combo_list <- c(combo_list, do_agi_combo)
    
  }
  
  base_avg_enso <- mean(base_list)
  do_avg_enso <- mean(do_list)
  agi_avg_enso <- mean(agi_list)
  #combo_avg_enso <- mean(combo_list)
  
  # create difference rasters ------------------------------------------------------------------------------------------
  #make difference maps
  diff_base <- diff(c(base_avg_neut, base_avg_enso))*100
  diff_agi <- diff(c(agi_avg_neut, agi_avg_enso))*100
  diff_do <- diff(c(do_avg_neut, do_avg_enso))*100
  #diff_do_agi_combo <- diff(c(combo_avg_neut, combo_avg_enso))*100
  
  #plot maps --------------------------------------------------------------------------------------------------------
  #land files
  map.world = map_data(map="world")
  testt = map.world %>% filter(long <= 180)
  
  #base map
  #calculate percent area polygon takes up of raster 
  base_hsi <- raster::clamp(base_avg_neut, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  hsi_area_base <- expanse(base_hsi)
  rast_area_base <- expanse(base_avg_neut)
  perc_area_base <- (hsi_area_base/rast_area_base$area[1])*100
  
  #enso area
  base_hsi_enso <- raster::clamp(base_avg_enso, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  hsi_area_base_enso <- expanse(base_hsi_enso)
  rast_area_base_enso <- expanse(base_avg_enso)
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
              hjust = 1.1, vjust = 2, size = 6, color = "black")+
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
  rast_area_do <- expanse(do_avg_neut)
  perc_area_do <- (hsi_area_do/rast_area_do$area[1])*100
  
  #enso area
  do_hsi_enso <- raster::clamp(do_avg_enso, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  hsi_area_do_enso <- expanse(do_hsi_enso)
  rast_area_do_enso <- expanse(do_avg_enso)
  perc_area_do_enso <- (hsi_area_do_enso/rast_area_do_enso$area[1])*100
  
  #calculate difference
  perc_do <- paste0(round(perc_area_do_enso$area[1] - perc_area_do$area[1], 2), "%")
  
    do_map <- ggplot() +
      geom_spatraster(data = diff_do) +
      geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
      scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
      scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
      scale_fill_whitebox_c(palette = "muted", limits = c(-100, 100), direction = -1)+
      geom_text(aes(x = Inf, y = Inf, 
                    label = perc_do), 
                hjust = 1.1, vjust = 2, size = 6, color = "black")+
      theme_map() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none")
  
  #agi map
  #calculate percent area polygon takes up of raster 
  agi_hsi <- raster::clamp(agi_avg_neut, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  
  hsi_area_agi <- expanse(agi_hsi)
  rast_area_agi <- expanse(agi_avg_neut)
  perc_area_agi <- (hsi_area_agi/rast_area_agi$area[1])*100
  
  #enso area
  agi_hsi_enso <- raster::clamp(agi_avg_enso, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  hsi_area_agi_enso <- expanse(agi_hsi_enso)
  rast_area_agi_enso <- expanse(agi_avg_enso)
  perc_area_agi_enso <- (hsi_area_agi_enso/rast_area_agi_enso$area[1])*100
  
  #calculate difference
  perc_agi <- paste0(round(perc_area_agi_enso$area[1] - perc_area_agi$area[1], 2), "%")
  
    agi_map <- ggplot() +
      geom_spatraster(data = diff_agi) +
      geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
      scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
      scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
      scale_fill_whitebox_c(palette = "muted", limits = c(-100, 100), direction = -1) +
      geom_text(aes(x = Inf, y = Inf, 
                    label = perc_agi), 
                hjust = 1.1, vjust = 2, size = 6, color = "black")+
      theme_map() +
      theme(legend.position = "none", 
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank())
    
  
  #do agi combo map
  #calculate percent area polygon takes up of raster 
  # combo_hsi <- raster::clamp(combo_avg_neut, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  # 
  # hsi_area_combo <- expanse(combo_hsi)
  # rast_area_combo <- expanse(combo_avg_neut)
  # perc_area_combo <- (hsi_area_combo/rast_area_combo$area[1])*100
  # 
  # #enso area
  # combo_hsi_enso <- raster::clamp(combo_avg_enso, lower = 0.75, values = FALSE) #create raster of values with HSI > 0.75
  # hsi_area_combo_enso <- expanse(combo_hsi_enso)
  # rast_area_combo_enso <- expanse(combo_avg_enso)
  # perc_area_combo_enso <- (hsi_area_combo_enso/rast_area_combo_enso$area[1])*100
  # 
  # #calculate difference
  # perc_combo <- paste0(round(perc_area_combo_enso$area[1] - perc_area_combo$area[1], 2), "%")
  # 
  # combo_map <- ggplot() +
  #   geom_spatraster(data = diff_do_agi_combo) +
  #   geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  #   scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  #   scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  #   scale_fill_whitebox_c(palette = "muted", limits = c(-100, 100), direction = -1) +
  #   geom_text(aes(x = Inf, y = Inf, 
  #                 label = perc_combo), 
  #             hjust = 1.1, vjust = 2, size = 3.4, color = "black")+
  #   theme_map() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 0.5), 
  #         axis.text.y = element_blank(),
  #         axis.title.y = element_blank(),
  #         legend.position = "none")
  
  #combine and return maps ------------------------------------------------------------------------------------------------------
  
    if(enso == "EN"){
      all_maps <- (base_map)/(do_map)/(agi_map+theme(
        axis.text.x = element_text(color = "black", angle = 45, hjust = 0.3, size = 14), 
        axis.title.x = element_text(size = 16, color = "black", vjust = 0.3)))+
        plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "% change")
    } 
    if(enso == "LN"){
      all_maps <- (base_map)/(do_map)/(agi_map+theme(
        axis.text.x = element_text(color = "black", angle = 45, hjust = 0.3, size = 14), 
        axis.title.x = element_text(size = 16, color = "black", vjust = 0.3)))
    }
  
  return(all_maps)
  
  #end function  
}


# average predictor relative importance ####
avg_pred <- function(base_mods, do_mods, agi_mods, iter = 20){
  
  #list models
  base_mod_files <- base_mods
  do_mod_files <- do_mods
  agi_mod_files <- agi_mods
  #combo_mod_files <- combo_mods
  
  inf_df <- NULL
  for(i in 1:iter){
    
  #base model 
  base_inf <- as.data.frame(ggBRT::ggInfluence(readRDS(base_mod_files[i]), plot = FALSE)) %>% rownames_to_column()
  colnames(base_inf) <- c("Predictor_variable", "relative_influence")
  base_inf$Predictor_variable <- gsub("bathy_mean", "z", base_inf$Predictor_variable)
  base_inf$Predictor_variable <- gsub("temp_mean", "temp", base_inf$Predictor_variable)
  base_inf$Predictor_variable <- gsub("sal_mean", "sal", base_inf$Predictor_variable)
  base_inf$Predictor_variable <- gsub("chl_mean", "chl-a", base_inf$Predictor_variable)
  base_inf$Predictor_variable <- gsub("bathy_sd", "z_sd", base_inf$Predictor_variable)
  base_inf$Predictor_variable <- gsub("ssh_mean", "SSH", base_inf$Predictor_variable)
  base_inf$Predictor_variable <- gsub("mld_mean", "MLD", base_inf$Predictor_variable)
  
  base_df <- data.frame(model = c("Base", "Base", "Base", "Base", "Base", "Base", "Base"), 
                       var = base_inf$Predictor_variable, 
                       inf_val = base_inf$relative_influence)
  
  base_df <- base_df %>% mutate(iter = i)
  
  inf_df <- rbind(inf_df, base_df)
  
  }
  
  #do model
  for(i in 1:iter){
  
  do_inf <- as.data.frame(ggBRT::ggInfluence(readRDS(do_mod_files[i]), plot = FALSE)) %>% rownames_to_column()
  colnames(do_inf) <- c("var", "inf_val")
  do_inf$var <- gsub("\\<o2_mean_0m\\>", "DO, daily, 0m", do_inf$var)
  do_inf$var <- gsub("o2_mean_250m_ann", "DO, annual, 250m", do_inf$var)
  do_inf$var <- gsub("o2_mean_0m_seas", "DO, seasonal, 0m", do_inf$var)
  do_inf$var <- gsub("o2_mean_250m_seas", "DO, seasonal, 250m", do_inf$var)
  do_inf$var <- gsub("temp_mean", "temp", do_inf$var)
  do_inf$var <- gsub("sal_mean", "sal", do_inf$var)
  do_inf$var <- gsub("bathy_mean", "z", do_inf$var)
  do_inf$var <- gsub("chl_mean", "chl-a", do_inf$var)
  do_inf$var <- gsub("o2_mean_0m_ann", "DO, annual, 0m", do_inf$var)
  do_inf$var <- gsub("o2_mean_250m", "DO, daily, 250m", do_inf$var)
  do_inf$var <- gsub("ssh_mean", "SSH", do_inf$var)
  do_inf$var <- gsub("mld_mean", "MLD", do_inf$var)
  do_inf$var <- gsub("bathy_sd", "z_sd", do_inf$var)
  
  do_inf <- do_inf %>% mutate(model = "DO", iter = i) 
  colnames(do_inf) <- c("var", "inf_val", "model", "iter")
  inf_df <- rbind(inf_df, do_inf)
  
  }
  
  #agi model
  for(i in 1:iter){
  
  agi_inf <- as.data.frame(ggBRT::ggInfluence(readRDS(agi_mod_files[i]), plot = FALSE)) %>% rownames_to_column()
  colnames(agi_inf) <- c("var", "inf_val")
  agi_inf$var <- gsub("\\<AGI_0m\\>", "AGI, daily, 0m", agi_inf$var)
  agi_inf$var <- gsub("AGI_250m_ann", "AGI, annual, 250m", agi_inf$var)
  agi_inf$var <- gsub("AGI_0m_seas", "AGI, seasonal, 0m", agi_inf$var)
  agi_inf$var <- gsub("AGI_250m_seas", "AGI, seasonal, 250m", agi_inf$var)
  agi_inf$var <- gsub("temp_mean", "temp", agi_inf$var)
  agi_inf$var <- gsub("sal_mean", "sal", agi_inf$var)
  agi_inf$var <- gsub("bathy_mean", "z", agi_inf$var)
  agi_inf$var <- gsub("chl_mean", "chl-a", agi_inf$var)
  agi_inf$var <- gsub("AGI_0m_ann", "AGI, annual, 0m", agi_inf$var)
  agi_inf$var <- gsub("AGI_250m", "AGI, daily, 250m", agi_inf$var)
  agi_inf$var <- gsub("ssh_mean", "SSH", agi_inf$var)
  agi_inf$var <- gsub("mld_mean", "MLD", agi_inf$var)
  agi_inf$var <- gsub("bathy_sd", "z_sd", agi_inf$var)
  
  agi_inf <- agi_inf %>% mutate(model = "AGI", iter = i) 
  colnames(agi_inf) <- c("var", "inf_val", "model", "iter")
  inf_df <- rbind(inf_df, agi_inf)
  
  }
  
  #combo model
  # for(i in 1:iter){
  # 
  # do_agi_inf <- as.data.frame(ggBRT::ggInfluence(readRDS(combo_mod_files[i]), plot = FALSE)) %>% rownames_to_column()
  # colnames(do_agi_inf) <- c("var", "inf_val")
  # do_agi_inf$var <- gsub("\\<o2_mean_0m\\>", "DO, daily, 0m", do_agi_inf$var)
  # do_agi_inf$var <- gsub("AGI_250m_ann", "AGI, annual, 250m", do_agi_inf$var)
  # do_agi_inf$var <- gsub("o2_mean_0m_seas", "DO, seasonal, 0m", do_agi_inf$var)
  # do_agi_inf$var <- gsub("AGI_250m_seas", "AGI, seasonal, 250m", do_agi_inf$var)
  # do_agi_inf$var <- gsub("temp_mean", "temp", do_agi_inf$var)
  # do_agi_inf$var <- gsub("sal_mean", "sal", do_agi_inf$var)
  # do_agi_inf$var <- gsub("bathy_mean", "z", do_agi_inf$var)
  # do_agi_inf$var <- gsub("chl_mean", "chl-a", do_agi_inf$var)
  # do_agi_inf$var <- gsub("o2_mean_0m_ann", "DO, annual, 0m", do_agi_inf$var)
  # do_agi_inf$var <- gsub("AGI_250m", "AGI, daily, 250m", do_agi_inf$var)
  # do_agi_inf$var <- gsub("ssh_mean", "SSH", do_agi_inf$var)
  # do_agi_inf$var <- gsub("mld_mean", "MLD", do_agi_inf$var)
  # do_agi_inf$var <- gsub("bathy_sd", "z_sd", do_agi_inf$var)
  # 
  # do_agi_inf <- do_agi_inf %>% mutate(model = "DO+AGI combo", iter = i) 
  # colnames(do_agi_inf) <- c("var", "inf_val", "model", "iter")
  # inf_df <- rbind(inf_df, do_agi_inf)
  # }
  
  return(inf_df)
  
} #end function




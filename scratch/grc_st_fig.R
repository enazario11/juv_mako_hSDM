{library(tidyverse)
  library(doParallel)
  library(foreach)
  library(here)
  library(gbm)
  library(dismo)
  library(doParallel)
  library(foreach)
  library(tidyquant)
  library(ggpubr)
  set.seed(1004)}


#get enso mod output data
base_dat_e <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/enso_test/base_metrics.rds")) %>% drop_na() %>% mutate(mod_type = "Base model")
do_dat_e <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/enso_test/do_metrics.rds")) %>% drop_na() %>% mutate(mod_type = "DO model")
agi_dat_e <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/enso_test/agi_metrics.rds")) %>% drop_na() %>% mutate(mod_type = "AGI model")


all_enso <- rbind(base_dat_e, do_dat_e, agi_dat_e) %>%
  mutate(ENSO = NA, 
         dev_exp = dev_exp*100)

for(i in 1:nrow(all_enso)){
  if(grepl('en',all_enso$st_id[i])){
    all_enso$ENSO[i] = "El Niño"
  } 
  if(grepl('ln',all_enso$st_id[i])){
    all_enso$ENSO[i] = "La Niña"
  } 
  if(grepl('neut', all_enso$st_id[i])){
    all_enso$ENSO[i] = "Neutral"
  }
  if(grepl('Overall',all_enso$st_id[i])){
    all_enso$ENSO[i] = "Overall"}
}

#get region mod output data
base_dat_r <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/reg_test/base_metrics.rds")) %>% drop_na() %>% mutate(mod_type = "Base model")
do_dat_r <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/reg_test/do_metrics.rds")) %>% drop_na() %>% mutate(mod_type = "DO model")
agi_dat_r <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/reg_test/agi_metrics.rds")) %>% drop_na() %>% mutate(mod_type = "AGI model")

all_reg <- rbind(base_dat_r, do_dat_r, agi_dat_r) %>%
  mutate(region = NA, 
         dev_exp = dev_exp*100)

for(i in 1:nrow(all_reg)){
  if(grepl('ccs',all_reg$st_id[i])){
    all_reg$region[i] = "CCS"
  } 
  if(grepl('nec',all_reg$st_id[i])){
    all_reg$region[i] = "NEC"
  } 
  if(grepl('nep', all_reg$st_id[i])){
    all_reg$region[i] = "NEP"
  }
  if(grepl('Overall',all_reg$st_id[i])){
    all_reg$region[i] = "Overall"
  }  
}

#enso boxplots
TSS_neut_e <- all_enso %>% mutate(mod_type = as.factor(mod_type), 
                              mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                              ENSO = as.factor(ENSO), 
                              ENSO = fct_relevel(ENSO, c("Neutral", "El Niño", "La Niña"))) %>%
  filter(ENSO == "Neutral") %>%
  ggplot(aes(x = ENSO, y=TSS)) +
  geom_boxplot(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), lwd = 1, alpha = 0.55)+
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("TSS") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14), 
        rect = element_rect(fill = "transparent") ) 


TSS_stress_e <- all_enso %>% mutate(mod_type = as.factor(mod_type), 
                                  mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                                  ENSO = as.factor(ENSO), 
                                  ENSO = fct_relevel(ENSO, c("Neutral", "El Niño", "La Niña"))) %>%
  filter(ENSO != "Neutral") %>%
  ggplot(aes(x = ENSO, y=TSS)) +
  geom_boxplot(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), lwd = 1, alpha = 0.55)+
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("TSS") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14), 
        rect = element_rect(fill = "transparent") ) 

dev_neut_e <- all_enso %>% mutate(mod_type = as.factor(mod_type), 
                                mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                                ENSO = as.factor(ENSO), 
                                ENSO = fct_relevel(ENSO, c("Neutral", "El Niño", "La Niña"))) %>%
  filter(ENSO == "Neutral") %>%
  ggplot(aes(x = ENSO, y=dev_exp)) +
  geom_boxplot(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), lwd = 1, alpha = 0.55)+
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Deviance explained (%)") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14)) 


#region boxplots
TSS_neut_r <- all_reg %>% mutate(mod_type = as.factor(mod_type), 
                              mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                              st_id = as.factor(st_id), 
                              region = as.factor(region), 
                              region = fct_relevel(region, c("NEP", "CCS", "NEC"))) %>%
  filter(region != "NEC") %>%
  ggplot(aes(x = region, y=TSS)) +
  geom_boxplot(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), lwd = 1, alpha = 0.55)+
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("TSS") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14),
        rect = element_rect(fill = "transparent"))

TSS_stress_r <- all_reg %>% mutate(mod_type = as.factor(mod_type), 
                                 mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                                 st_id = as.factor(st_id), 
                                 region = as.factor(region), 
                                 region = fct_relevel(region, c("NEP", "CCS", "NEC"))) %>%
  filter(region == "NEC") %>%
  ggplot(aes(x = region, y=TSS)) +
  geom_boxplot(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), lwd = 1, alpha = 0.55)+
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("TSS") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14),
        rect = element_rect(fill = "transparent") ) 


dev_neut_r <- all_reg %>% mutate(mod_type = as.factor(mod_type), 
                               mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                               st_id = as.factor(st_id), 
                               region = as.factor(region), 
                               region = fct_relevel(region, c("NEP", "CCS", "NEC"))) %>%
  filter(region != "NEC") %>%
  ggplot(aes(x = region, y=dev_exp)) +
  geom_boxplot(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), lwd = 1, alpha = 0.55)+
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Deviance explained (%)") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14)) 


all_neut <- ggarrange(TSS_neut_r, TSS_neut_e, common.legend = TRUE)
ggsave(here("figs/grc/tss_neut.svg"), all_neut, width = 10, height = 5, units = c("in"))

all_stress <- ggarrange(TSS_stress_r, TSS_stress_e, common.legend = TRUE)
ggsave(here("figs/grc/tss_stress.svg"), all_stress, width = 10, height = 5, units = c("in"))

### agi map ####
agi_maps_layerd_grc <- function(rast_folder_base = NULL){
  
  #load rasters
  #base agi
  agi_250m_base <- rast(list.files(here(rast_folder_base), full.names = TRUE, pattern = "250m"))
  
  #create shape file of areas surrounding AGI < 1
  #base
  one_250m_base <- raster::clamp(agi_250m_base, upper = 1, values = FALSE) #create raster of values below 1
  one_poly_250m_base <- as.polygons(ext(one_250m_base))
  one_poly_250m_base <- as.polygons(one_250m_base > -Inf) 
  
  #calculate percent area polygon takes up of raster 
  #base
  poly_area_250m_base <- expanse(one_poly_250m_base)
  rast_area_base <- expanse(agi_250m_base)
  perc_area_250m_base <- (poly_area_250m_base/rast_area_base$area[1])*100
  
  #land shapes
  map.world = map_data(map="world")
  testt=map.world %>% filter(long<=180)
  
  #map
  agi_one_250m <- ggplot() + 
    geom_spatraster(data = agi_250m_base) + 
    geom_spatvector(data = one_poly_250m_base, color = "black", fill = NA, linewidth = 0.8) +
    geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
    scale_x_continuous(expand=c(0,0),limits = c(-153,-103)) +
    scale_y_continuous(expand=c(0,0),limits = c(1,49))+
    scale_fill_whitebox_c(palette = "muted", direction = -1)+
    labs(fill = "AGI")+
    geom_text(aes(x = Inf, y = Inf, 
                  label = paste0("Area: ", round(perc_area_250m_base, 2), "%")), 
              hjust = 1.1, vjust = 2, size = 8, color = "black")+
    theme_map()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18, color = "white"),
          axis.text.y = element_text(color = "white", size = 18),
          axis.title =element_text(size = 20, color = "white"), 
          legend.text = element_text(size = 18, color = "white"), 
          legend.title = element_text(size = 20, color = "white"), 
          legend.position = "right", 
          legend.justification = "center",
          legend.background = element_blank(), 
          legend.box.background = element_blank())
  
  return(agi_one_250m)
  
  #end function 
}

agi_250m_layered_grc <- agi_maps_layerd_grc(rast_folder_base = here("data/enviro/psat_spot_all/hsi_rasts/agi_rasts/Jan13_Dec13"))

ggsave(here("figs/grc/agi.svg"), height = 7, width = 8, units = c("in"))

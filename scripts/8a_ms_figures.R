### libraries ####
{library(here);here <- here::here #plyr's here function masks here::here
  library(MetBrewer)
  library(terra)
  library(ggBRT)
  library(patchwork)
  library(ggrepel)
  library(tidyverse)
  library(tidyr)
  set.seed(1004)
  source(here("functions/hsi_rast_functions.R"))
  source(here("functions/BRT_evaluation_functions.R"))
  source(here("functions/oxy_demand_functions.R"))
  source(here("functions/avg_functions.R"))
  source(here("functions/partial_plot.R"))
  source(here("scripts/7a_diet_data.R"))
}

### saved custom themes ####
theme_ms_map <- function(){ 
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
        size = 16),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      legend.position = "none"
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

### Figure 1: Conceptual fig of hypotheses ###
#coastline data
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="USA"
                      | north_map$region=="Mexico",]

ggplot(shore, aes(long, lat)) +
  #plot coastline
  coord_map("mercator", xlim = c(-153, -103), ylim = c(1, 49))+
  geom_polygon(aes(group=group), fill="grey75",lwd=1)+
  theme_ms_map()

ggsave(here("figs/ms/fig1_concept_hyp/coast.png"), height = 7 , width = 5, units = c("in"))

### Figure 2: locs over study area bathymetry ####
#### presence location data ####
#shark data
ani_locs <- readRDS(here("data/presence_locs/psat_spot_domain/processed/psat_spot_animotum.RDS")) %>% 
  mutate(PA = 0, rep = NA) %>% 
  subset(select = -c(geometry))

names(ani_locs) <- c("tag", "date", "lon", "lat", "PA", "rep")

#bathy data
r = rast(here("data/enviro/psat_spot_all/bathy_gebco/processed/gebco_bathy_0.25deg2.nc"))
bathy_df <- as.data.frame(r,xy = TRUE)
bathy_df <- bathy_df %>%
  filter(gebco_bathy_0.25deg2 <= 0)

mycolors <- c("#08306B","#023858", "#034B76", "#0B559F", "#045D92","#0469A6","#1379B4","#2F8BBD","#6BAED6", "#509AC6","#74A9CF","#88BEDC", "#90B4D5","#ACBFDC","#C4CBE2" )

ggplot(shore, aes(long, lat)) +
  #plot coastline
  coord_map("mercator", xlim = c(-153, -103), ylim = c(1, 49))+
  geom_polygon(aes(group=group), fill="grey75",lwd=1) +
  #plot bathymetry map
  geom_contour_filled(data=bathy_df, 
                      aes(x,y,z=gebco_bathy_0.25deg2)) +#breaks=seq(0,1,by=0.2)
  scale_fill_manual(values = mycolors)+
  #plot shark locations
  geom_path(data=test, aes(lon, lat, colour=as.factor(tag)), size = 0.5) +
  scale_color_manual(values = met.brewer("OKeeffe2", 73)) +
theme_ms_map() +
  xlim(-153, -103)+
  ylim(1, 49) +
  theme(legend.position = "none", 
        axis.title = element_blank()) + 
  guides(fill = guide_legend(title = "Bathymetry (m)", reverse = TRUE), 
         color = FALSE)

ggsave(here("figs/ms/fig2_tracks_bathy/tracks_bathy.png"), height = 7, width = 5, units = c("in"))

### Figure 4: AGI maps ####
#neutral year
#hsi_rast_gen(date_start = c("2013-09-01"), date_end = c("2014-01-31"), season = "FW", output_name = "neut_FW_Sept2013_Jan2014")

#La Niña
#hsi_rast_gen(date_start = c("2010-09-01"), date_end = c("2010-11-30"), season = "F", output_name = "LN_F_2010")


#EL Niño
#2014 
#hsi_rast_gen(date_start = c("2014-11-01"), date_end = c("2015-01-31"), season = "FW", output_name = "EN_FW_Nov2014_Jan2015")


agi_250m_layered <- agi_maps_layerd(rast_folder_base = here("data/enviro/psat_spot_all/hsi_rasts/agi_rasts/Jan13_Dec13"), 
                                    rast_folder_LN = here("data/enviro/psat_spot_all/hsi_rasts/agi_rasts/LN_F_2010"), 
                                    rast_folder_EN = here("data/enviro/psat_spot_all/hsi_rasts/agi_rasts/EN_FW_Nov2014_Jan2015"))

ggsave(here("figs/ms/fig2_agi/agi_250m_layered.png"), agi_250m_layered, height = 7, width = 8, units = c("in"))


### Figure 5: performance metrics overall ####
#entire domain and study period
mod_metric_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters"), pattern = ".rds", full.names = TRUE)

base_file <- readRDS(mod_metric_files[2])
do_file <- readRDS(mod_metric_files[4])
agi_file <- readRDS(mod_metric_files[1])
combo_file <- readRDS(mod_metric_files[3])

base_file$mod_type <- "Base model"
agi_file$mod_type <- "AGI model"
do_file$mod_type <- "DO model"
#combo_file$mod_type <- "DO+AGI combo model"

mod_metrics <- rbind(base_file, agi_file, do_file)
mod_metrics <- mod_metrics %>% mutate(st_id = "Overall")

#combine datasets
mod_metrics <- mod_metrics %>% mutate(dev_exp = dev_exp*100)
all_sum <- mod_metrics %>%
  group_by(mod_type) %>%
  summarise(mean_auc = mean(AUC), 
            sd_auc = sd(AUC), 
            mean_tss = mean(TSS), 
            sd_tss = sd(TSS), 
            mean_dev = mean(dev_exp),
            sd_dev = sd(dev_exp)) %>%
  ungroup() 
  
# analysis of variance
#anova <- aov(TSS ~ mod_type, data = mod_metrics)

# Tukey's test
#tukey <- TukeyHSD(anova)

# compact letter display
#cld <- multcompView::multcompLetters4(anova, tukey, reversed = TRUE)

# dt_tss <- mod_metrics %>%
#   group_by(mod_type) %>%
#   summarise(mean_tss=mean(TSS), sd = sd(TSS)) %>%
#   arrange(desc(mean_tss))

# extracting the compact letter display and adding to the Tk table
#cld <- as.data.frame.list(cld$mod_type)
#dt_tss$cld <- cld$Letters

#overall plots
TSS_overall <- all_sum %>% mutate(mod_type = as.factor(mod_type), 
                                  mod_type = fct_relevel(mod_type, c("Base model", "AGI model", "DO model"))) %>%
  arrange(desc(mean_tss)) %>%
  ggplot(aes(x = mod_type, y=mean_tss)) +
  geom_errorbar(aes(ymin = mean_tss - 2*sd_tss, ymax = mean_tss + 2*sd_tss), color = "black", size =  1, width = 0, linewidth = 1)+
  geom_point(color = "black", size = 4)+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("TSS") + 
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16)) 

AUC_overall <- all_sum %>% mutate(mod_type = as.factor(mod_type), 
                                  mod_type = fct_relevel(mod_type, c("Base model", "AGI model", "DO model"))) %>%
  arrange(desc(mean_auc)) %>%
  ggplot(aes(x = mod_type, y=mean_auc)) +
  geom_errorbar(aes(ymin = mean_auc - 2*sd_auc, ymax = mean_auc + 2*sd_auc), color = "black", size =  1, width = 0, linewidth = 1)+
  geom_point(color = "black", size = 4)+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("AUC") + 
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16)) 

dev_overall <- all_sum %>% mutate(mod_type = as.factor(mod_type), 
                                                 mod_type = fct_relevel(mod_type, c("Base model", "AGI model", "DO model"))) %>%
  arrange(desc(mean_dev)) %>%
  ggplot(aes(x = mod_type, y=mean_dev)) +
  geom_errorbar(aes(ymin = mean_dev - 2*sd_dev, ymax = mean_dev + 2*sd_dev), color = "black", size =  1, width = 0, linewidth = 1)+
  geom_point(color = "black", size = 4)+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("% Deviance explained") + 
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16)) 

overall_metric_plots <- TSS_overall/AUC_overall/dev_overall

ggsave(here("figs/ms/fig5_metrics_all/overall_metrics.png"), overall_metric_plots, height = 13, width = 5, units = c("in"))

#Figure 6: performance metrics st ####
#spatiotemporal analysis
base_st <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/base_metrics.rds")) %>% mutate(mod_type = "Base model")
do_st <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/do_metrics.rds")) %>% mutate(mod_type = "DO model")
agi_st <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/agi_metrics.rds")) %>% mutate(mod_type = "AGI model")
#combo_st <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/combo_metrics.rds")) %>% mutate    (mod_type = "DO+AGI model")

all_st <- rbind(base_st, do_st, agi_st) %>%
  mutate(region = NA, 
         ENSO = NA, 
         dev_exp = dev_exp*100)

for(i in 1:nrow(all_st)){
  if(grepl('ccs',all_st$st_id[i])){
    all_st$region[i] = "CCS"
  } 
  if(grepl('nec',all_st$st_id[i])){
    all_st$region[i] = "NEC"
  } 
  if(grepl('nep', all_st$st_id[i])){
    all_st$region[i] = "NEP"
  }
  if(grepl('Overall',all_st$st_id[i])){
    all_st$region[i] = "Overall"
  }  
  if(grepl('en',all_st$st_id[i])){
    all_st$ENSO[i] = "El Niño"
  } 
  if(grepl('ln',all_st$st_id[i])){
    all_st$ENSO[i] = "La Niña"
  } 
  if(grepl('neut', all_st$st_id[i])){
    all_st$ENSO[i] = "Neutral"
  }
  if(grepl('Overall',all_st$st_id[i])){
    all_st$ENSO[i] = "Overall"}
}

#spatiotemporal plots
TSS_plot <- all_st %>% mutate(mod_type = as.factor(mod_type), 
                               mod_type = fct_relevel(mod_type, c("Base model", "AGI model", "DO model")), 
                               st_id = as.factor(st_id), 
                               region = as.factor(region), 
                               region = fct_relevel(region, c("NEP", "CCS", "NEC")), 
                               ENSO = as.factor(ENSO), 
                               ENSO = fct_relevel(ENSO, c("Neutral", "El Niño", "La Niña"))) %>%
  ggplot(aes(x = mod_type, y=TSS)) +
  geom_boxplot(aes(fill = region, color = region), position = position_dodge(width = 1), lwd = 1, alpha = 0.55)+
  #geom_errorbar(aes(ymin = mean_tss - 2*sd_tss, ymax = mean_tss + 2*sd_tss, color = region), size =  1, width = 0, linewidth = 1, position=position_dodge(width=0.5))+
  #geom_point(aes(color = region), size = 4, position=position_dodge(width=0.5))+
  #geom_segment(aes(x=mod_type, xend=mod_type, y=0.4, yend=mean_tss), color="#92351e", linewidth = 1.5) +
  #geom_point(color="orange", size=6) +
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_wrap(~ENSO, scales = "free_y")+
  xlab("") +
  ylab("TSS") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "Region:")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14)) 
#geom_text(aes(label = cld, y = mean_tss + 0.03), vjust = -0.5, size = 5)


# analysis of variance
#anova <- aov(AUC ~ mod_type, data = mod_metrics)

# Tukey's test
#tukey <- TukeyHSD(anova)

# compact letter display
#cld <- multcompView::multcompLetters4(anova, tukey, reversed = TRUE)

# dt_auc <- mod_metrics %>%
#   group_by(mod_type) %>%
#   summarise(mean_auc=mean(AUC), sd = sd(AUC)) %>%
#   arrange(desc(mean_auc))

# extracting the compact letter display and adding to the Tk table
#cld <- as.data.frame.list(cld$mod_type)
#dt_auc$cld <- cld$Letters

AUC_plot <- all_st %>% mutate(mod_type = as.factor(mod_type), 
                               mod_type = fct_relevel(mod_type, c("Base model", "AGI model", "DO model")), 
                               st_id = as.factor(st_id), 
                               region = as.factor(region), 
                               region = fct_relevel(region, c("NEP", "CCS", "NEC")), 
                               ENSO = as.factor(ENSO), 
                               ENSO = fct_relevel(ENSO, c("Neutral", "El Niño", "La Niña"))) %>%
  ggplot(aes(x = mod_type, y=AUC)) +
  geom_boxplot(aes(fill = region, color = region), position = position_dodge(width = 1), lwd = 1, alpha = 0.55)+
  #geom_errorbar(aes(ymin = mean_auc - 2*sd_auc, ymax = mean_auc + 2*sd_auc, color = region), size =  1, width = 0, linewidth = 1, position=position_dodge(width=0.5))+
  #geom_point(aes(color = region), size = 4, position=position_dodge(width=0.5))+
  #geom_segment(aes(x=mod_type, xend=mod_type, y=0.4, yend=mean_auc), color="#92351e", linewidth = 1.5) +
  #geom_point(color="orange", size=6) +
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_wrap(~ENSO, scales = "free_y")+
  xlab("") +
  ylab("AUC") + 
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "none", 
        strip.text.x = element_text(size = 14)) 
#geom_text(aes(label = cld, y = mean_auc + 0.03), vjust = -0.5, size = 5)


# analysis of variance
#anova <- aov(dev_exp ~ mod_type, data = mod_metrics)

# Tukey's test
#tukey <- TukeyHSD(anova)

# compact letter display
#cld <- multcompView::multcompLetters4(anova, tukey, reversed = TRUE)

# dt_dev <- mod_metrics %>%
#   group_by(mod_type) %>%
#   summarise(mean_dev=mean(dev_exp), sd = sd(dev_exp)) %>%
#   arrange(desc(mean_dev))

# extracting the compact letter display and adding to the Tk table
#cld <- as.data.frame.list(cld$mod_type)
#dt_dev$cld <- cld$Letters

perc_exp_plot <- all_st %>% mutate(mod_type = as.factor(mod_type), 
                                    mod_type = fct_relevel(mod_type, c("Base model", "AGI model", "DO model")), 
                                    st_id = as.factor(st_id), 
                                    region = as.factor(region), 
                                    region = fct_relevel(region, c("NEP", "CCS", "NEC")), 
                                    ENSO = as.factor(ENSO), 
                                    ENSO = fct_relevel(ENSO, c("Neutral", "El Niño", "La Niña"))) %>%
  ggplot(aes(x = mod_type, y=dev_exp)) +
  geom_boxplot(aes(fill = region, color = region), position = position_dodge(width = 1), lwd = 1, alpha = 0.55)+
  #geom_errorbar(aes(ymin = mean_dev - 2*sd_dev, ymax = mean_dev + 2*sd_dev, color = region), size =  1, width = 0, linewidth = 1, position=position_dodge(width=0.5))+
  #geom_point(aes(color = region), size = 4, position=position_dodge(width=0.5))+
  #geom_segment(aes(x=mod_type, xend=mod_type, y=0.4, yend=mean_dev), color="#92351e", linewidth = 1.5) +
  #geom_point(color="orange", size=6) +
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_wrap(~ENSO, scales = "free_y")+
  xlab("") +
  ylab("% Deviance explained") + 
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "none", 
        strip.text.x = element_text(size = 14)) 
#geom_text(aes(label = cld, y = mean_dev + 0.03), vjust = -0.5, size = 5)

all_metric_plots <- TSS_plot/AUC_plot/perc_exp_plot

ggsave(here("figs/ms/fig6_metrics_st/Figure_6_Metrics_st.png"), all_metric_plots, height = 10, width = 13, units = c("in"))

# Figure 7: predictor relative importance ####
#list models
base_mod <- readRDS(here("data/brt/mod_outputs/final_mods/brt_base_0m_dail_no_wind.rds"))
do_mod_fin <- readRDS(here("data/brt/mod_outputs/final_mods/brt_do_0m_250m_dail_seas_ann.rds"))
agi_mod_fin <- readRDS(here("data/brt/mod_outputs/final_mods/brt_agi_0m_250m_dail_seas_ann.rds"))
do_agi_comb <- readRDS(here("data/brt/mod_outputs/final_mods/brt_agi_250_DO_0_dail_seas_ann.rds"))

#base model 
base_inf <- as.data.frame(ggBRT::ggInfluence(base_mod, plot = FALSE)) %>% rownames_to_column()
colnames(base_inf) <- c("Predictor_variable", "relative_influence")
base_inf$Predictor_variable <- gsub("bathy_mean", "z", base_inf$Predictor_variable)
base_inf$Predictor_variable <- gsub("temp_mean", "temp", base_inf$Predictor_variable)
base_inf$Predictor_variable <- gsub("sal_mean", "sal", base_inf$Predictor_variable)
base_inf$Predictor_variable <- gsub("chl_mean", "chl-a", base_inf$Predictor_variable)
base_inf$Predictor_variable <- gsub("bathy_sd", "z_sd", base_inf$Predictor_variable)
base_inf$Predictor_variable <- gsub("ssh_mean", "SSH", base_inf$Predictor_variable)
base_inf$Predictor_variable <- gsub("mld_mean", "MLD", base_inf$Predictor_variable)

inf_df <- data.frame(model = c("Base", "Base", "Base", "Base", "Base", "Base", "Base"), 
                     var = base_inf$Predictor_variable, 
                     inf_val = base_inf$relative_influence)

#base_cols <- MetBrewer::met.brewer("OKeeffe2", n = length(unique(base_inf$Predictor_variable))+6, direction = -1)

# base_inf2 <- base_inf %>% mutate(fraction = relative_influence / sum(relative_influence),
#                                     ymax = cumsum(fraction),
#                                     ymin = c(0, head(ymax, n=-1)),
#                                     labelPosition = (ymax + ymin)/2)

# base_pred <- ggplot(base_inf2, aes(fill = reorder(Predictor_variable, -relative_influence))) +
#   geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
#   geom_text(aes(x = 3.5, y = labelPosition, label = paste0(round(relative_influence, digits = 0), "%")), color = "white", size = 4.5)+
#   geom_label_repel(aes(x = 4, y = labelPosition, label = Predictor_variable),
#                    fill = alpha(c("white"),0),
#                    label.size = NA,
#                    size = 4.5, hjust = .5,
#                    nudge_x = 0.3, direction = "x",
#                    segment.color = "transparent"
#                    #segment.curvature = -0.1,
#                    #segment.ncp = 3,
#                    #segment.angle = 20, seed = 123
#   ) +
#   coord_polar(theta = "y", clip = "off") +
#   xlim(c(2, 5)) +
#   scale_fill_manual(values = base_cols) +
#   theme(legend.position = "none", 
#         plot.margin =  margin(-1,-1,-1,-1)) +
#   theme_void() +
#   guides(
#     fill = "none"
#   )

#do model
do_inf <- as.data.frame(ggBRT::ggInfluence(do_mod_fin, plot = FALSE)) %>% rownames_to_column()
colnames(do_inf) <- c("Predictor_variable", "relative_influence")
do_inf$Predictor_variable <- gsub("\\<o2_mean_0m\\>", "DO, daily, 0m", do_inf$Predictor_variable)
do_inf$Predictor_variable <- gsub("o2_mean_250m_ann", "DO, annual, 250m", do_inf$Predictor_variable)
do_inf$Predictor_variable <- gsub("o2_mean_0m_seas", "DO, seasonal, 0m", do_inf$Predictor_variable)
do_inf$Predictor_variable <- gsub("o2_mean_250m_seas", "DO, seasonal, 250m", do_inf$Predictor_variable)
do_inf$Predictor_variable <- gsub("temp_mean", "temp", do_inf$Predictor_variable)
do_inf$Predictor_variable <- gsub("sal_mean", "sal", do_inf$Predictor_variable)
do_inf$Predictor_variable <- gsub("bathy_mean", "z", do_inf$Predictor_variable)
do_inf$Predictor_variable <- gsub("chl_mean", "chl-a", do_inf$Predictor_variable)
do_inf$Predictor_variable <- gsub("o2_mean_0m_ann", "DO, annual, 0m", do_inf$Predictor_variable)
do_inf$Predictor_variable <- gsub("o2_mean_250m", "DO, daily, 250m", do_inf$Predictor_variable)
do_inf$Predictor_variable <- gsub("ssh_mean", "SSH", do_inf$Predictor_variable)
do_inf$Predictor_variable <- gsub("mld_mean", "MLD", do_inf$Predictor_variable)
do_inf$Predictor_variable <- gsub("bathy_sd", "z_sd", do_inf$Predictor_variable)

do_inf <- do_inf %>% mutate(model = "DO") 
colnames(do_inf) <- c("var", "inf_val", "model")
inf_df <- rbind(inf_df, do_inf)

#do_cols <- NatParksPalettes::natparks.pals("Acadia", n = length(unique(do_inf$Predictor_variable))+20, direction = -1)

# do_inf2 <- do_inf %>% mutate(fraction = relative_influence / sum(relative_influence),
#                                  ymax = cumsum(fraction),
#                                  ymin = c(0, head(ymax, n=-1)),
#                                  labelPosition = (ymax + ymin)/2)
# 
# do_pred <- ggplot(do_inf2, aes(fill = reorder(Predictor_variable, -relative_influence))) +
#   geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
#   geom_text(aes(x = 3.5, y = labelPosition, label = paste0(round(relative_influence, digits = 0), "%")), color = "white", size = 4.5)+
#   geom_label_repel(aes(x = 4, y = labelPosition, label = Predictor_variable),
#                    fill = alpha(c("white"),0),
#                    label.size = NA,
#                    size = 4.5, hjust = .5,
#                    nudge_x = 0.63, direction = "x",
#                    segment.color = "transparent"
#                    #segment.curvature = -0.1,
#                    #segment.ncp = 3,
#                    #segment.angle = 20, seed = 123
#   ) +
#   coord_polar(theta = "y", clip = "off") +
#   xlim(c(2, 5)) +
#   scale_fill_manual(values = do_cols) +
#   theme(legend.position = "none", 
#         plot.margin =  margin(-1,-1,-1,-1)) +
#   theme_void() +
#   guides(
#     fill = "none"
#   )


#agi model
agi_inf <- as.data.frame(ggBRT::ggInfluence(agi_mod_fin, plot = FALSE)) %>% rownames_to_column()
colnames(agi_inf) <- c("Predictor_variable", "relative_influence")
agi_inf$Predictor_variable <- gsub("\\<AGI_0m\\>", "AGI, daily, 0m", agi_inf$Predictor_variable)
agi_inf$Predictor_variable <- gsub("AGI_250m_ann", "AGI, annual, 250m", agi_inf$Predictor_variable)
agi_inf$Predictor_variable <- gsub("AGI_0m_seas", "AGI, seasonal, 0m", agi_inf$Predictor_variable)
agi_inf$Predictor_variable <- gsub("AGI_250m_seas", "AGI, seasonal, 250m", agi_inf$Predictor_variable)
agi_inf$Predictor_variable <- gsub("temp_mean", "temp", agi_inf$Predictor_variable)
agi_inf$Predictor_variable <- gsub("sal_mean", "sal", agi_inf$Predictor_variable)
agi_inf$Predictor_variable <- gsub("bathy_mean", "z", agi_inf$Predictor_variable)
agi_inf$Predictor_variable <- gsub("chl_mean", "chl-a", agi_inf$Predictor_variable)
agi_inf$Predictor_variable <- gsub("AGI_0m_ann", "AGI, annual, 0m", agi_inf$Predictor_variable)
agi_inf$Predictor_variable <- gsub("AGI_250m", "AGI, daily, 250m", agi_inf$Predictor_variable)
agi_inf$Predictor_variable <- gsub("ssh_mean", "SSH", agi_inf$Predictor_variable)
agi_inf$Predictor_variable <- gsub("mld_mean", "MLD", agi_inf$Predictor_variable)
agi_inf$Predictor_variable <- gsub("bathy_sd", "z_sd", agi_inf$Predictor_variable)

agi_inf <- agi_inf %>% mutate(model = "AGI") 
colnames(agi_inf) <- c("var", "inf_val", "model")
inf_df <- rbind(inf_df, agi_inf)

# agi_inf2 <- agi_inf %>% mutate(fraction = relative_influence / sum(relative_influence),
#                                  ymax = cumsum(fraction),
#                                  ymin = c(0, head(ymax, n=-1)),
#                                  labelPosition = (ymax + ymin)/2)
# agi_cols <- MetBrewer::met.brewer("Greek", n = length(unique(agi_inf$Predictor_variable))+5)
# 
# agi_pred <- ggplot(agi_inf2, aes(fill = reorder(Predictor_variable, -relative_influence))) +
#   geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
#   geom_text(aes(x = 3.5, y = labelPosition, label = paste0(round(relative_influence, digits = 0), "%")), color = "white", size = 4.5)+
#   geom_label_repel(aes(x = 4, y = labelPosition, label = Predictor_variable),
#                    fill = alpha(c("white"),0),
#                    label.size = NA,
#                    size = 4.5, hjust = .5,
#                    nudge_x = 0.7, direction = "x",
#                    segment.color = "transparent"
#                    #segment.curvature = -0.1,
#                    #segment.ncp = 3,
#                    #segment.angle = 20, seed = 123
#   ) +
#   coord_polar(theta = "y", clip = "off") +
#   xlim(c(2, 5)) +
#   scale_fill_manual(values = agi_cols) +
#   theme(legend.position = "none", 
#         plot.margin =  margin(-1,-1,-1,-1)) +
#   theme_void() +
#   guides(
#     fill = "none"
#   )

#combo model
do_agi_inf <- as.data.frame(ggBRT::ggInfluence(do_agi_comb, plot = FALSE)) %>% rownames_to_column()
colnames(do_agi_inf) <- c("Predictor_variable", "relative_influence")
do_agi_inf$Predictor_variable <- gsub("\\<o2_mean_0m\\>", "DO, daily, 0m", do_agi_inf$Predictor_variable)
do_agi_inf$Predictor_variable <- gsub("AGI_250m_ann", "AGI, annual, 250m", do_agi_inf$Predictor_variable)
do_agi_inf$Predictor_variable <- gsub("o2_mean_0m_seas", "DO, seasonal, 0m", do_agi_inf$Predictor_variable)
do_agi_inf$Predictor_variable <- gsub("AGI_250m_seas", "AGI, seasonal, 250m", do_agi_inf$Predictor_variable)
do_agi_inf$Predictor_variable <- gsub("temp_mean", "temp", do_agi_inf$Predictor_variable)
do_agi_inf$Predictor_variable <- gsub("sal_mean", "sal", do_agi_inf$Predictor_variable)
do_agi_inf$Predictor_variable <- gsub("bathy_mean", "z", do_agi_inf$Predictor_variable)
do_agi_inf$Predictor_variable <- gsub("chl_mean", "chl-a", do_agi_inf$Predictor_variable)
do_agi_inf$Predictor_variable <- gsub("o2_mean_0m_ann", "DO, annual, 0m", do_agi_inf$Predictor_variable)
do_agi_inf$Predictor_variable <- gsub("AGI_250m", "AGI, daily, 250m", do_agi_inf$Predictor_variable)
do_agi_inf$Predictor_variable <- gsub("ssh_mean", "SSH", do_agi_inf$Predictor_variable)
do_agi_inf$Predictor_variable <- gsub("mld_mean", "MLD", do_agi_inf$Predictor_variable)
do_agi_inf$Predictor_variable <- gsub("bathy_sd", "z_sd", do_agi_inf$Predictor_variable)

do_agi_inf <- do_agi_inf %>% mutate(model = "DO+AGI combo") 
colnames(do_agi_inf) <- c("var", "inf_val", "model")
inf_df <- rbind(inf_df, do_agi_inf)

# comb_cols <- NatParksPalettes::natparks.pals("BryceCanyon", n = length(unique(do_agi_inf$Predictor_variable))+30)
# 
# do_agi_inf2 <- do_agi_inf %>% mutate(fraction = relative_influence / sum(relative_influence),
#                                ymax = cumsum(fraction),
#                                ymin = c(0, head(ymax, n=-1)),
#                                labelPosition = (ymax + ymin)/2)
# 
# do_agi_pred <- ggplot(do_agi_inf2, aes(fill = reorder(Predictor_variable, -relative_influence))) +
#   geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
#   geom_text(aes(x = 3.5, y = labelPosition, label = paste0(round(relative_influence, digits = 0), "%")), color = "white", size = 4.5)+
#   geom_label_repel(aes(x = 4, y = labelPosition, label = Predictor_variable),
#                    fill = alpha(c("white"),0),
#                    label.size = NA,
#                    size = 4.5, hjust = .5,
#                    nudge_x = 0.7, direction = "x",
#                    segment.color = "transparent"
#                    #segment.curvature = -0.1,
#                    #segment.ncp = 3,
#                    #segment.angle = 20, seed = 123
#   ) +
#   coord_polar(theta = "y", clip = "off") +
#   xlim(c(2, 5)) +
#   scale_fill_manual(values = comb_cols) +
#   theme(legend.position = "none", 
#         plot.margin = margin(-1,-1,-1,-1)) +
#   theme_void() +
#   guides(
#     fill = "none"
#   )

avg_inf_df <- avg_pred(base_mods = list.files(here("data/brt/mod_outputs/perf_metric_iters/base"), full.names = TRUE),
                       do_mods = list.files(here("data/brt/mod_outputs/perf_metric_iters/do"), full.names = TRUE), 
                       agi_mods = list.files(here("data/brt/mod_outputs/perf_metric_iters/agi"), full.names = TRUE)) 
                       #combo_mods = list.files(here("data/brt/mod_outputs/perf_metric_iters/combo"), full.names = TRUE))

##### revised plot  #####
avg_inf_sum <- avg_inf_df %>% 
  group_by(model, var) %>%
  summarise(inf_val = mean(inf_val))

for(i in 1:nrow(avg_inf_sum)){
  if(str_detect(avg_inf_sum$var[i], "AGI")){
    avg_inf_sum$var_type[i] = "AGI predictor"
  }
  
  if(str_detect(avg_inf_sum$var[i], "DO")){
    avg_inf_sum$var_type[i] = "DO predictor"
  }
  
  if(!str_detect(avg_inf_sum$var[i], "DO")&!str_detect(avg_inf_sum$var[i], "AGI")){
    avg_inf_sum$var_type[i] = "Environmental predictor"
  }
}

avg_inf_sum <- avg_inf_sum %>% 
  mutate(model = as.factor(model), 
         model= fct_relevel(model, c("Base", "DO", "AGI")), 
         var = as.factor(var), 
         var = fct_relevel(var, c("AGI, annual, 250m", "AGI, daily, 0m", "AGI, seasonal, 0m", "AGI, seasonal, 250m","AGI, annual, 0m", "AGI, daily, 250m")))

do_agi <- avg_inf_sum %>% 
  filter(var_type != "Environmental predictor") %>% 
  mutate(var = as.factor(var)) %>%
  mutate(var = as.factor(var), var = fct_reorder(var, -inf_val)) %>%
  ggplot(aes(x = var, y = inf_val))+
  geom_bar(aes(fill = model), stat = "identity", position = position_dodge2(width = 0.8, preserve = "single"), color = "white")+
  scale_fill_manual(values = met.brewer("Hokusai1", direction = -1, n = 15)[2:4])+
  xlab("")+
  ylab("Relative importance (%)")+
  labs(fill = "Model")+
  facet_wrap(~var_type, scales = "free_x")+
 tidyquant::theme_tq()+
  theme(axis.text.y = element_text(size = 14, color = "black"), 
        axis.text.x = element_text(size = 14, color = "black", angle = 45, hjust = 1),
        axis.title = element_text(size = 16, color = "black"),
        strip.text = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14),
        legend.position = "none")

enviro <- avg_inf_sum %>%
  filter(var_type == "Environmental predictor") %>% 
  mutate(var = as.factor(var), var = fct_reorder(var, -inf_val)) %>%
  ggplot(aes(x = var, y = inf_val))+
  geom_bar(aes(fill = model), stat = "identity", position = "dodge", color = "white", width = 0.8)+
  scale_fill_manual(values = met.brewer("Hokusai1", direction = -1, n = 15))+
  xlab("")+
  ylab("Relative importance (%)")+
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 30, by = 10)) +
  labs(fill = "Model")+
  annotate(geom = "rect", fill = "#2c3e50", color = "#2c3e50", xmin = -Inf, xmax = +Inf, ymin = 34, ymax = +Inf)+
  annotate(label = "Environmental predictor", geom = "text", color = "white", x = 4, y = 38, size = 6)+
  tidyquant::theme_tq()+
  theme(axis.text = element_text(size = 14, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        strip.text = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        legend.position = "top", 
        legend.justification = "left")

pred_fig <- enviro/do_agi
pred_fig

ggsave(here("figs/ms/fig6_pred/bar_pred.png"), pred_fig, width = 11, height = 8, units = c("in"))
  
#ggsave(here("figs/ms/rel_inf_pred.png"), all_pred,  height = 15, width = 15, units = c("in"))
ggsave(here("figs/ms/fig3_pred/base_pred.png"), base_pred, height = 7, width = 7, units = c("in"))
ggsave(here("figs/ms/fig3_pred/do_pred.png"), do_pred, height = 7, width = 7, units = c("in"))
ggsave(here("figs/ms/fig3_pred/agi_pred.png"), agi_pred, height = 7, width = 7, units = c("in"))
ggsave(here("figs/ms/fig3_pred/do_agi_pred.png"), do_agi_pred, height = 7, width = 7, units = c("in"))

### Figure 8: HSI maps study period ####
#all_maps <- hsi_maps(rast_folder = "data/enviro/psat_spot_all/hsi_rasts/Jan03_Dec15", ms = "Y")
#ggsave(here("figs/ms/fig6_hsi_all/all_maps.png"), all_maps, height = 7, width = 7, units = c("in"))

all_maps_avg <- hsi_maps_avg(rast_folder = "data/enviro/psat_spot_all/hsi_rasts/Jan03_Dec15", ms = "Y")
ggsave(here("figs/ms/fig7_hsi_all/all_maps_avg_20.png"), all_maps_avg, height = 5, width = 8, units = c("in"))


### Figure 9: ENSO HSI maps ####
#have to save using export button otherwise adds border, using height of 750 and width 500 (LN width 300)

#base year
enso_base <- hsi_maps_enso_avg(rast_folder = "data/enviro/psat_spot_all/hsi_rasts/Jan13_Dec13", enso = "diff")

#LN year 
enso_LN <- hsi_maps_difference_enso_avg(enso_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/LN_F_2010", neut_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/Jan13_Dec13", enso = "LN")

#EN year
enso_EN <- hsi_maps_difference_enso_avg(enso_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/EN_FW_Nov2014_Jan2015", neut_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/Jan13_Dec13", enso = "EN")

#diet data 
#neutral year 
diet_neutral <- rmpq_prey_year2 %>% ungroup() %>%
  filter(perc_GII >= 1 & Year == 2013) %>% 
  top_n(3) %>%
  ggplot(aes(x = reorder(Common_Name, -perc_GII), y = perc_GII)) +
  geom_bar(stat = "identity", fill = "#224B5E", alpha = 0.85) +
  theme_minimal() + 
  theme(axis.text.y=element_text(size=20, color = "black"), 
        axis.title=element_text(size=22, color = "black"), 
        axis.text.x = element_blank(),
        panel.grid = element_blank()) +
  xlab('')+
  ylab('% GII')+
  scale_x_discrete(position = "top") 
ggsave(here("figs/ms/fig7_enso_diet/diet_neutral.png"), diet_neutral, height = 4, width = 6, units = c("in"))


#LN year 
diet_LN <- rmpq_prey_year2 %>% ungroup() %>%
  filter(perc_GII >= 1 & Year == 2010) %>% 
  top_n(3) %>%
  ggplot(aes(x = reorder(Common_Name, -perc_GII), y = perc_GII)) +
  geom_bar(stat = "identity", fill = "#224B5E", alpha = 0.85) +
  theme_minimal() + 
  theme(axis.text=element_blank(), 
        axis.title=element_blank(), 
        #axis.text.y = element_blank(), 
        #axis.text.x = element_blank(),
        panel.grid = element_blank()) +
  xlab('')+
  ylab('% GII')+
  scale_x_discrete(position = "top") 
ggsave(here("figs/ms/fig7_enso_diet/diet_LN.png"), diet_LN, height = 4, width = 6, units = c("in"))


#EN year 
diet_EN <- rmpq_prey_year2 %>%
  filter(perc_GII >= 1 & Year == 2014) %>% 
  top_n(3) %>%
  ggplot(aes(x = reorder(Common_Name, -perc_GII), y = perc_GII)) +
  geom_bar(stat = "identity", fill = "#224B5E", alpha = 0.85) +
  theme_minimal() + 
  theme(axis.text=element_blank(), 
        axis.title=element_blank(), 
        #axis.text.y = element_blank(), 
        #axis.text.x = element_text(),
        panel.grid = element_blank()) +
  xlab('')+
  ylab('% GII')+
  scale_x_discrete(position = "top") 

ggsave(here("figs/ms/fig7_enso_diet/diet_EN.png"), diet_EN, height = 4, width = 6, units = c("in"))

### Supplementary files ####
#### ST1: Shark metadata ####
md_df <- readRDS(here("data/presence_locs/psat_spot_domain/psat_spot_data.rds"))
spot <- read.csv(here("data/presence_locs/mako_spot_filtered_1_step_per_day.csv"))
spot_ptts <- unique(spot$PTT)

md_df2 <- md_df %>% 
  group_by(ptt) %>%
  arrange(date) %>%
  mutate(deploy_dur = difftime(tail(date, n = 1), head(date, n = 1), units = "days"), 
         tag_type = ifelse(ptt %in% spot_ptts, "SPOT", "SPOT + PSAT")) %>%
  distinct(ptt, .keep_all = TRUE) %>%
  subset(select = c(ptt, tag_type, FL, sex, deploy_dur, date, lat, lon))

#write.csv(md_df2, here("data/presence_locs/metadata_sum.csv"))

#### SF 3: Model exploration performance metrics ####
output_sum <- read.csv(here::here("data/brt/mod_outputs/brt_supp_table.csv"))
output_sum$deviance_exp <- output_sum$deviance_exp*100

mod_metrics <- ggplot(output_sum, aes(AUC, TSS, color = deviance_exp, label = model)) +
  geom_point(size = 5) +
  xlab('AUC') +
  ylab('TSS') +
  labs(color = "Deviance explained (%)")+
  scale_color_gradientn(colors = MetBrewer::met.brewer("Greek")) +
  ggrepel::geom_label_repel(aes(label = model),
                            box.padding   = 0.35,
                            point.padding = 1,
                            segment.color = 'grey50',
                            max.overlaps = 40,
                            label.size = 0.5)+
  theme_minimal()+
  theme(legend.position = "right", 
        axis.title = element_text(color = "black"), 
        axis.text = element_text(color = "black"))
ggsave(here::here("figs/ms/supp_figs/explore_mod_metrics.png"), mod_metrics, width = 10, height = 8, units = c("in"))

#### SF 4: Final model's partial plots w/ CIs ####
ggPD_boot <- function (gbm.object, predictor = NULL, n.plots = length(pred.names), 
                       list.4.preds = NULL, booted.preds = NULL, nrow = NULL, ncol = NULL, 
                       col.line = "darkorange", cex.line = 0.5, type.ci = "lines", 
                       col.ci = "grey80", cex.ci = 0.3, lty.ci = 2, alpha.ci = 0.5, 
                       smooth = FALSE, col.smooth = "blue", cex.smooth = 0.3, span = 0.3, 
                       rug = FALSE, rug.pos = "t", common.scale = TRUE, type = NULL, cis = c(0.025, 
                                                                                0.975), y.label = "Fitted function", x.label = paste(var.name, "  (", 
                                                                                                                                     round(gbm.object$contributions[predictor, 2], 
                                                                                                                                           1), "%)", sep = ""), 
                       ...) 
{
  gbm.call <- gbm.object$gbm.call
  pred.names <- gbm.call$predictor.names
  ggPD_boot.plots <- function(gbm.object) {
    if (!requireNamespace("gbm")) {
      stop("you need to install the gbm package to run this function")
    }
    if (is.null(booted.preds)) {
      stop("you need to set booted.preds as the array from the bootstrap run\n           (eg testboot$function.preds using testboot<-gbm.bootstrap.functions())")
    }
    if (is.null(list.4.preds)) {
      stop("you need to set list.4.preds as the result of plot.gbm.4list()")
    }
    requireNamespace("splines")
    gbm.x <- gbm.call$gbm.x
    response.name <- gbm.call$response.name
    nt <- gbm.object$n.trees
    data <- gbm.call$dataframe
    max.vars <- length(gbm.object$contributions$var)
    if (n.plots > max.vars) {
      n.plots <- max.vars
      warning("reducing no of plotted predictors to maximum available (", 
              max.vars, ")")
    }
    predictors <- list(rep(NA, n.plots))
    responses <- list(rep(NA, n.plots))
    responses.lower <- list(rep(NA, n.plots))
    responses.upper <- list(rep(NA, n.plots))
    for (j in c(1:max.vars)) {
      k <- match(gbm.object$contributions$var[j], pred.names)
      if (is.null(x.label)) {
        var.name <- gbm.call$predictor.names[k]
      }
      else {
        var.name <- x.label
      }
      pred.data <- data[, gbm.call$gbm.x[k]]
      response.matrix <- gbm::plot.gbm(gbm.object, i.var = k, 
                                       n.trees = nt, return.grid = TRUE, ...)
      predictors[[j]] <- response.matrix[, 1]
      if (is.factor(data[, gbm.call$gbm.x[k]])) {
        predictors[[j]] <- factor(predictors[[j]], levels = levels(data[, 
                                                                        gbm.call$gbm.x[k]]))
      }
      responses[[j]] <- response.matrix[, 2] - mean(response.matrix[, 
                                                                    2])
      num.values <- nrow(response.matrix)
      temp <- apply(booted.preds[, k, ] - mean(booted.preds[, 
                                                            k, ]), 1, function(x) {
                                                              quantile(x, cis[1], na.rm = T)
                                                            })
      responses.lower[[j]] <- temp[1:num.values]
      temp <- apply(booted.preds[, k, ] - mean(booted.preds[, 
                                                            k, ]), 1, function(x) {
                                                              quantile(x, cis[2], na.rm = T)
                                                            })
      responses.upper[[j]] <- temp[1:num.values]
      if (j == 1) {
        ymin = min(responses.lower[[j]])
        ymax = max(responses.upper[[j]])
        dat <- data.frame(pred.data)
      }
      else {
        ymin = min(ymin, min(responses.lower[[j]]))
        ymax = max(ymax, max(responses.upper[[j]]))
        dat <- data.frame(dat, pred.data)
      }
    }
    if (is.null(predictor)) {
      fittedFunc <- list()
      fittedFunc.lower <- list()
      fittedFunc.upper <- list()
      fittedVal <- list()
      ribbon <- list()
      ggPD <- list()
      for (i in 1:n.plots) {
        k <- match(gbm.object$contributions$var[i], pred.names)
        var.name <- gbm.call$predictor.names[k]
        fittedFunc[[i]] <- data.frame(predictors[i], 
                                      responses[i])
        colnames(fittedFunc[[i]]) <- c("x", "y")
        fittedFunc.lower[[i]] <- data.frame(predictors[i], 
                                            responses.lower[i])
        colnames(fittedFunc.lower[[i]]) <- c("x", "y")
        fittedFunc.upper[[i]] <- data.frame(predictors[i], 
                                            responses.upper[i])
        colnames(fittedFunc.upper[[i]]) <- c("x", "y")
        fittedVal[[i]] <- data.frame(gbm.object$fitted, 
                                     dat[i])
        colnames(fittedVal[[i]]) <- c("y", "x")
        ribbon[[i]] <- data.frame(x = fittedFunc.lower[[i]]$x, 
                                  ylow = fittedFunc.lower[[i]]$y, yup = fittedFunc.upper[[i]]$y)
        if (is.factor(fittedFunc[[i]]$x)) {
          ggPD[[i]] <- ggplot(fittedFunc[[i]], aes(x = x, 
                                                   y = y)) + geom_boxplot(color = col.line, 
                                                                          size = cex.line) + geom_boxplot(data = fittedFunc.lower[[i]], 
                                                                                                          aes(x = x, y = y), color = col.ci) + geom_boxplot(data = fittedFunc.upper[[i]], 
                                                                                                                                                            aes(x = x, y = y), color = col.ci) + ylab(y.label) + 
            xlab(paste(var.name, "  (", round(gbm.object$contributions[i, 
                                                                       2], 1), "%)", sep = "")) + theme_bw() + 
            theme(panel.grid.minor = element_line(linetype = "blank"), 
                  panel.grid.major = element_line(linetype = "blank"), 
                  axis.text.x = element_text(size = 6), axis.title.x = element_text(size = 10), 
                  axis.line.y = element_line(size = 0.1), 
                  axis.line.x = element_line(size = 0.1))
          if (common.scale == T) {
            ggPD[[i]] <- ggPD[[i]] + ylim(c(ymin, ymax))
          }
        }
        if (type.ci == "lines") {
          ggPD[[i]] <- ggplot(fittedFunc[[i]], aes(x = x, 
                                                   y = y)) + geom_line(color = col.line, size = cex.line) + 
            geom_line(data = fittedFunc.lower[[i]], aes(x = x, 
                                                        y = y), size = cex.ci, color = col.ci, 
                      linetype = lty.ci) + geom_line(data = fittedFunc.upper[[i]], 
                                                     aes(x = x, y = y), size = cex.ci, color = col.ci, 
                                                     linetype = lty.ci) + ylab(y.label) + xlab(paste(var.name, 
                                                                                                     "  (", round(gbm.object$contributions[i, 
                                                                                                                                           2], 1), "%)", sep = "")) + theme_bw() + 
            theme(panel.grid.minor = element_line(linetype = "blank"), 
                  panel.grid.major = element_line(linetype = "blank"), 
                  axis.title.x = element_text(size = 10), 
                  axis.line.y = element_line(size = 0.1), 
                  axis.line.x = element_line(size = 0.1))
          if (smooth == T) {
            ggPD[[i]] <- ggPD[[i]] + geom_smooth(span = span, 
                                                 size = 0.3, color = col.smooth, se = F, 
                                                 linetype = 2)
          }
          if (rug == T) {
            ggPD[[i]] <- ggPD[[i]] + geom_rug(data = fittedVal[[i]], 
                                              aes(x = x, y = y), sides = rug.pos, position = "jitter", 
                                              color = "#EBEBEB")
          }
          if (common.scale == T) {
            ggPD[[i]] <- ggPD[[i]] + ylim(c(ymin, ymax))
          }
        }
        if (type.ci == "ribbon") {
          ggPD[[i]] <- ggplot() + geom_ribbon(data = ribbon[[i]], 
                                              aes(x = x, ymin = ylow, ymax = yup), fill = col.ci, 
                                              alpha = alpha.ci) + geom_line(data = fittedFunc[[i]], 
                                                                            aes(x = x, y = y), color = col.line, size = cex.line) + 
            ylab(y.label) + xlab(paste(var.name, "  (", 
                                       round(gbm.object$contributions[i, 2], 1), 
                                       "%)", sep = "")) + theme_bw() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                                             panel.grid.major = element_line(linetype = "blank"), 
                                                                             axis.title.x = element_text(size = 10), axis.line.y = element_line(size = 0.1), 
                                                                             axis.line.x = element_line(size = 0.1))
          if (smooth == T) {
            ggPD[[i]] <- ggPD[[i]] + geom_smooth(data = fittedFunc[[i]], 
                                                 aes(x = x, y = y), span = span, size = 0.3, 
                                                 color = col.smooth, se = F, linetype = 2)
          }
          if (rug == T) {
            ggPD[[i]] <- ggPD[[i]] + geom_rug(data = fittedVal[[i]], 
                                              aes(x = x, y = y), sides = rug.pos, position = "jitter", 
                                              color = "#EBEBEB")
          }
          if (common.scale == T) {
            ggPD[[i]] <- ggPD[[i]] + ylim(c(ymin, ymax))
          }
        }
      }
      list(ggPD = ggPD)
    }
    else {
      if (is.character(predictor)) {
        predictor <- match(predictor, gbm.object$contributions$var)
      }
      k <- match(gbm.object$contributions$var[predictor], 
                 pred.names)
      var.name <- gbm.call$predictor.names[k]
      fittedFunc <- data.frame(predictors[predictor], responses[predictor])
      colnames(fittedFunc) <- c("x", "y")
      fittedFunc.lower <- data.frame(predictors[predictor], 
                                     responses.lower[predictor])
      colnames(fittedFunc.lower) <- c("x", "y")
      fittedFunc.upper <- data.frame(predictors[predictor], 
                                     responses.upper[predictor])
      colnames(fittedFunc.upper) <- c("x", "y")
      ribbon <- data.frame(x = fittedFunc.lower$x, ylow = fittedFunc.lower$y, 
                           yup = fittedFunc.upper$y)
      fittedVal <- data.frame(gbm.object$fitted, dat[predictor])
      colnames(fittedVal) <- c("y", "x")
      if (is.factor(fittedFunc$x)) {
        ggPD <- ggplot(fittedFunc, aes(x = x, y = y)) + 
          geom_boxplot(color = col.line, size = cex.line) + 
          geom_boxplot(data = fittedFunc.lower, aes(x = x, 
                                                    y = y), color = col.ci) + geom_boxplot(data = fittedFunc.upper, 
                                                                                           aes(x = x, y = y), color = col.ci) + ylab(y.label) + 
          xlab(paste(var.name, "  (", round(gbm.object$contributions[predictor, 
                                                                     2], 1), "%)", sep = "")) + theme_bw() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                                                                                   panel.grid.major = element_line(linetype = "blank"), 
                                                                                                                   axis.text.x = element_text(size = 6), axis.title.x = element_text(size = 10), 
                                                                                                                   axis.line.y = element_line(size = 0.1), axis.line.x = element_line(size = 0.1))
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      if (type.ci == "lines") {
        ggPD <- ggplot(fittedFunc, aes(x = x, y = y)) + 
          geom_line(color = col.line, size = cex.line) + 
          geom_line(data = fittedFunc.lower, aes(x = x, 
                                                 y = y), size = cex.ci, color = col.ci, linetype = lty.ci) + 
          geom_line(data = fittedFunc.upper, aes(x = x, 
                                                 y = y), size = cex.ci, color = col.ci, linetype = lty.ci) + 
          ylab(y.label) + xlab(paste(var.name, "  (", 
                                     round(gbm.object$contributions[predictor, 2], 
                                           1), "%)", sep = "")) + theme_bw() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                                                     panel.grid.major = element_line(linetype = "blank"), 
                                                                                     axis.title.x = element_text(size = 10), axis.line.y = element_line(size = 0.1), 
                                                                                     axis.line.x = element_line(size = 0.1))
        if (smooth == T) {
          ggPD <- ggPD + geom_smooth(span = span, size = 0.3, 
                                     color = col.smooth, se = F, linetype = 2)
        }
        if (rug == T) {
          ggPD <- ggPD + geom_rug(data = fittedVal, aes(x = x, 
                                                        y = y), sides = rug.pos, position = "jitter", 
                                  color = "#EBEBEB")
        }
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      if (type.ci == "ribbon") {
        ggPD <- ggplot() + geom_ribbon(data = ribbon, 
                                       aes(x = x, ymin = ylow, ymax = yup), fill = col.ci, 
                                       alpha = alpha.ci) + geom_line(data = fittedFunc, 
                                                                     aes(x = x, y = y), color = col.line, size = cex.line) + 
          ylab(y.label) + xlab(x.label) + theme_minimal() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                                                     panel.grid.major = element_line(linetype = "blank"), 
                                                                                     axis.title.x = element_text(size = 14), axis.line.y = element_line(size = 0.1), 
                                                                                     axis.line.x = element_line(size = 0.1), 
                                                                  axis.text = element_text(size = 14, color = "black"))
        if (smooth == T) {
          ggPD <- ggPD + geom_smooth(data = fittedFunc, 
                                     aes(x = x, y = y), span = span, size = 0.3, 
                                     color = col.smooth, se = F, linetype = 2)
        }
        if (rug == T) {
          ggPD <- ggPD + geom_rug(data = fittedVal, aes(x = x, 
                                                        y = y), sides = rug.pos, position = "jitter", 
                                  color = "#EBEBEB")
        }
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      
      if (type.ci == "ribbon" & type == "do" & i != 1 & i != 10) {
        ggPD <- ggplot() + geom_ribbon(data = ribbon, 
                                       aes(x = x, ymin = ylow, ymax = yup), fill = col.ci, 
                                       alpha = alpha.ci) + geom_line(data = fittedFunc, 
                                                                     aes(x = x, y = y), color = col.line, size = cex.line) + 
          ylab(y.label) + xlab(x.label) + theme_minimal() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                             panel.grid.major = element_line(linetype = "blank"), 
                                                             axis.title.x = element_text(size = 14), axis.line.y = element_blank(), 
                                                             axis.line.x = element_line(size = 0.1), 
                                                             axis.text.x = element_text(size = 14, color = "black"),
                                                             axis.text.y = element_blank(), 
                                                             axis.title.y = element_blank())
        if (smooth == T) {
          ggPD <- ggPD + geom_smooth(data = fittedFunc, 
                                     aes(x = x, y = y), span = span, size = 0.3, 
                                     color = col.smooth, se = F, linetype = 2)
        }
        if (rug == T) {
          ggPD <- ggPD + geom_rug(data = fittedVal, aes(x = x, 
                                                        y = y), sides = rug.pos, position = "jitter", 
                                  color = "#EBEBEB")
        }
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      
      if (type.ci == "ribbon" & type == "agi" & i != 3 & i != 10) {
        ggPD <- ggplot() + geom_ribbon(data = ribbon, 
                                       aes(x = x, ymin = ylow, ymax = yup), fill = col.ci, 
                                       alpha = alpha.ci) + geom_line(data = fittedFunc, 
                                                                     aes(x = x, y = y), color = col.line, size = cex.line) + 
          ylab(y.label) + xlab(x.label) + theme_minimal() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                                  panel.grid.major = element_line(linetype = "blank"), 
                                                                  axis.title.x = element_text(size = 14), axis.line.y = element_blank(), 
                                                                  axis.line.x = element_line(size = 0.1), 
                                                                  axis.text.x = element_text(size = 14, color = "black"),
                                                                  axis.text.y = element_blank(), 
                                                                  axis.title.y = element_blank())
        if (smooth == T) {
          ggPD <- ggPD + geom_smooth(data = fittedFunc, 
                                     aes(x = x, y = y), span = span, size = 0.3, 
                                     color = col.smooth, se = F, linetype = 2)
        }
        if (rug == T) {
          ggPD <- ggPD + geom_rug(data = fittedVal, aes(x = x, 
                                                        y = y), sides = rug.pos, position = "jitter", 
                                  color = "#EBEBEB")
        }
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      
      list(ggPD = ggPD)
    }
  }
  plot <- ggPD_boot.plots(gbm.object)
  if (is.null(predictor)) {
    do.call(grid.arrange, c(plot$ggPD, list(nrow = nrow, 
                                            ncol = ncol)))
  }
  else grid.draw(plot$ggPD)
}

# Boostrap the BRT 1000 times to build confidence intervals
brt1.prerun_base<- plot.gbm.4list(base_mod)
base_boot <- gbm.bootstrap.functions(base_mod, list.predictors=brt1.prerun, n.reps=20)

# Draw partial dependency plots a given predictor (i.e., "Complexity")
#base model
plot_list <- list()
base_names <- c("z", "temp", "sal", "chl-a", "z_sd", "SSH", "MLD")
for(i in 1:nrow(base_mod$contributions)){
  plot_temp <- ggPD_boot(base_mod, 
                         predictor = base_mod$contributions[i, 1], 
                         list.4.preds = brt1.prerun_base, 
                         booted.preds = base_boot$function.preds, 
                         type.ci = "ribbon",
                         rug = T, 
                         alpha.ci = 0.75, 
                         y.label = "Probability of presence", 
                         x.label = paste(base_names[i], "  (", 
                                         round(base_mod$contributions[i, 2], 
                                               1), "%)", sep = ""))
  
  plot_list[[i]] <- plot_temp
}

base_plots <- do.call(grid.arrange, c(plot_list, ncol = 5))
ggsave(here("figs/ms/supp_figs/par_plot_base.png"), base_plots, height = 4, width = 8.5, units = c("in"))

#do model
brt1.prerun_do<- plot.gbm.4list(do_mod_fin)
do_boot <- gbm.bootstrap.functions(do_mod_fin, list.predictors=brt1.prerun_do, n.reps=20)

plot_list <- list()
do_names <- c("DO, daily, 0m", "DO, annual, 250m", "DO, seasonal, 0m", "DO, seasonal, 250m", "temp", "sal", "z", "chl-a", "DO, annual, 0m", "DO, daily, 250m", "SSH", "MLD", "z_sd")
for(i in 1:nrow(do_mod_fin$contributions)){
  plot_temp <- ggPD_boot(do_mod_fin, 
                         predictor = do_mod_fin$contributions[i, 1], 
                         list.4.preds = brt1.prerun_do, 
                         booted.preds = do_boot$function.preds, 
                         type.ci = "ribbon",
                         col.line = "#92351e", 
                         cex.line = 1.5,
                         type = "do",
                         rug = T, 
                         alpha.ci = 0.75, 
                         y.label = "",
                         x.label = "")
  
  plot_list[[i]] <- plot_temp
}
#saveRDS(do_boot, file = here("figs/ms/fig4_par/do_ribbons.rds"))
do_plots_0m <- grid.arrange(grobs = list(plot_list[[1]], plot_list[[3]], plot_list[[9]]), ncol = 3)
ggsave(here("figs/ms/fig4_par/do_0m.png"), do_plots_0m, height = 5, width = 9, units = c("in"))

do_plots_250m <- grid.arrange(grobs = list(plot_list[[10]], plot_list[[4]], plot_list[[2]]), ncol = 3)
ggsave(here("figs/ms/fig4_par/do_250m.png"), do_plots_250m, height = 5, width = 9, units = c("in"))

#agi model
brt1.prerun_agi<- plot.gbm.4list(agi_mod_fin)
agi_boot <- gbm.bootstrap.functions(agi_mod_fin, list.predictors=brt1.prerun_agi, n.reps=20)

plot_list <- list()
agi_names <- c("AGI, annual, 250m", "temp", "AGI, daily, 0m", "z", "AGI, seasonal, 0m", "sal", "AGI, seasonal, 250m", "SSH", "AGI, annual, 0m", "AGI, daily, 250m", "chl-a", "z_sd", "MLD")
for(i in 1:nrow(agi_mod_fin$contributions)){
  plot_temp <- ggPD_boot(agi_mod_fin, 
                         predictor = agi_mod_fin$contributions[i, 1], 
                         list.4.preds = brt1.prerun_agi, 
                         booted.preds = agi_boot$function.preds, 
                         type.ci = "ribbon",
                         rug = T, 
                         type = "agi",
                         col.line = "#92351e", 
                         cex.line = 1.5,
                         alpha.ci = 0.75, 
                         y.label = "",
                         x.label = "")
  
  plot_list[[i]] <- plot_temp
}

#saveRDS(agi_boot, file = here("figs/ms/fig4_par/agi_ribbons.rds"))

agi_plots_0m <- grid.arrange(grobs = list(plot_list[[3]], plot_list[[5]], plot_list[[9]]), ncol = 3)
ggsave(here("figs/ms/fig4_par/agi_0m.png"), agi_plots_0m, height = 5, width = 9, units = c("in"))

agi_plots_250m <- grid.arrange(grobs = list(plot_list[[10]], plot_list[[7]], plot_list[[1]]), ncol = 3)
ggsave(here("figs/ms/fig4_par/agi_250m.png"), agi_plots_250m, height = 5, width = 9, units = c("in"))

#DO, AGI combo model
brt1.prerun_do_agi<- plot.gbm.4list(do_agi_comb)
do_agi_boot <- gbm.bootstrap.functions(do_agi_comb, list.predictors=brt1.prerun_do_agi, n.reps=20)

plot_list <- list()
do_agi_names <- c("DO, daily, 0m", "AGI, annual, 250m", "DO, seasonal, 0m", "AGI, seasonal, 250m", "temp", "z", "sal", "AGI, daily, 250m", "chl-a", "DO, annual, 0m", "SSH", "MLD", "z_sd")
for(i in 1:nrow(do_agi_comb$contributions)){
  plot_temp <- ggPD_boot(do_agi_comb, 
                         predictor = do_agi_comb$contributions[i, 1], 
                         list.4.preds = brt1.prerun_do_agi, 
                         booted.preds = do_agi_boot$function.preds, 
                         type.ci = "ribbon",
                         rug = T, 
                         alpha.ci = 0.75, 
                         y.label = "Probability of presence",
                         x.label = paste(do_agi_names[i], "  (", 
                                         round(agi_mod_fin$contributions[i, 2], 
                                               1), "%)", sep = ""))
  
  plot_list[[i]] <- plot_temp
}

do_agi_plots <- do.call(grid.arrange, c(plot_list, ncol = 5))
ggsave(here("figs/ms/supp_figs/par_plot_do_agi.png"), do_agi_plots, height = 7, width = 11, units = c("in"))

#### SF 5: LOO CV ####
year_base <- readRDS(here("data/brt/mod_outputs/crw/evaluation/soo_year_base.rds")) %>% mutate(model = "Base")
year_do <- readRDS(here("data/brt/mod_outputs/crw/evaluation/soo_year_do.rds")) %>% mutate(model = "DO")
year_agi <- readRDS(here("data/brt/mod_outputs/crw/evaluation/soo_year_agi.rds")) %>% mutate(model = "AGI")
year_do_agi <- readRDS(here("data/brt/mod_outputs/crw/evaluation/soo_year_combo.rds")) %>% mutate(model = 'DO+AGI combo')

year_all <- rbind(year_base, year_do, year_agi, year_do_agi) %>% mutate(Year = as.factor(Year), model = fct_relevel(model,  c("Base", "AGI", "DO", "DO-AGI combo")))
dodger = position_dodge(width = 0.9)

  TSS <- ggplot(year_all, aes(x = model, y = TSS, group = Year)) +
    geom_bar(stat = "identity", aes(fill = Year), position = "dodge", color = "black") +
    scale_fill_manual(values = met.brewer("Greek", n = 20, direction = 1)) + 
    theme_ms_map()+
    xlab("")+
    theme(legend.justification = "center", 
          legend.title = element_text(size = 20), 
          legend.text = element_text(size = 16))+
    guides(fill = guide_legend(nrow = 1, ncol = 15))
  
  AUC <- ggplot(year_all, aes(x = model, y = AUC, group = Year)) +
    geom_bar(stat = "identity", aes(fill = Year), position = "dodge", color = "black") +
    scale_fill_manual(values = met.brewer("Greek", n = 20, direction = 1)) + 
    theme_ms_map()+
    xlab("")+
    theme(legend.justification = "center", 
          legend.title = element_text(size = 20), 
          legend.text = element_text(size = 16))+
    guides(fill = guide_legend(nrow = 1, ncol = 15))
  
  dev <- ggplot(year_all, aes(x = model, y = Deviance, group = Year)) +
    geom_bar(stat = "identity", aes(fill = Year), position = "dodge", color = "black") +
    scale_fill_manual(values = met.brewer("Greek", n = 20, direction = 1)) + 
    ylab("Deviance explained (%)")+
    theme_ms_map()+
    xlab("")+
    theme(legend.justification = "center", 
          legend.title = element_text(size = 20), 
          legend.text = element_text(size = 16))+
    guides(fill = guide_legend(nrow = 1, ncol = 15))

all <- ggpubr::ggarrange(TSS, AUC, dev, common.legend = TRUE, nrow = 3, ncol = 1, legend = "bottom"); all
ggsave(here("figs/ms/supp_figs/loo_metrics_year.png"), all, height = 9, width = 13, units = c("in"))

year_df <- year_all %>%
  subset(select = c("Year", "N. test points")) %>%
  rename("Number of test points" = "N. test points") %>%
  dplyr::distinct() %>%
  pivot_wider(names_from = Year, values_from = "Number of test points") %>%
  as.data.frame()
rownames(year_df) <- "Number of test points"

year_df %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = "LOO Test Year") 

#### SF: spatial figs (DO, temp, lat/lon)
##### Number of points by season ####
dat_base_d <- readRDS(here("data/locs_brts/crw_pas/dat_base.rds")) %>% mutate(tag = as.factor(tag), date = as.Date(date))
dat_locs <- dat_base_d %>% filter(PA == 1)
dat_locs$month <- month(dat_locs$date)

locs_mo <- ggplot(dat_locs, aes(x = as.factor(month))) + 
  geom_bar(fill = "#92351e")+
  xlab("Month")+
  ylab("Number of locations")+
  tidyquant::theme_tq()+
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title= element_text(size = 20, color = "black"))

ggsave(here("figs/ms/supp_figs/locs_by_mo.png"), height = 5, width = 7, units = c("in"))

##### Map of DO at surface and depth across study domain averaged across study period ####
#create averaged rasters
rast_0m <- rast(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_CHL_Temp_SO_UO_UOSTR_VO_VOSTR_SSH_MLD_0m_Jan2003_Dec2015_0.25_D.nc"))
rast_do_0m <- rast_0m["o2"]
rast_do_0m_mean <- mean(rast_do_0m)

rast_250m <- rast(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_Temp_SO_250m_Jan2003_Dec2015_0.25_D.nc"))
rast_do_250m <- rast_250m["o2"]
rast_do_250m_mean <- mean(rast_do_250m)

#plot
#land files
map.world = map_data(map="world")
testt = map.world %>% filter(long <= 180)

do_0m_map <- ggplot() +
  geom_spatraster(data = rast_do_0m_mean) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = -1) +
  labs(fill = "Dissolved oxygen (mmol/m^3)")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))
  
ggsave(here("figs/ms/supp_figs/do_2003_2015_0m.png"), do_0m_map, height = 3, width = 6, units = c("in"))

do_250m_map <- ggplot() +
  geom_spatraster(data = rast_do_250m_mean) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = -1) +
  labs(fill = "Dissolved oxygen (mmol/m^3)")+
theme_map() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))

ggsave(here("figs/ms/supp_figs/do_2003_2015_250m.png"), do_250m_map, height = 3, width = 6, units = c("in"))

# temp 0m 
rast_temp_0m <- rast_0m["votemper"]
rast_temp_0m_mean <- mean(rast_temp_0m)

#plot
temp_0m_map <- ggplot() +
  geom_spatraster(data = rast_temp_0m_mean) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = -1) +
  labs(fill = "Temperature (C)")+
  theme_ms_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5), 
        legend.position = "right", 
        legend.title =element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"))

ggsave(here("figs/ms/supp_figs/temp_2003_2015_0m.png"), temp_0m_map, height = 3, width = 6, units = c("in"))


##### Temp vs. DO by season and spatially #####
#get observed location + covariate data
dat_do_d <- readRDS(here("data/locs_brts/crw_pas/dat_do.rds")) %>% mutate(tag = as.factor(tag), date = as.Date(date))
dat_agi_d <- readRDS(here("data/locs_brts/crw_pas/dat_agi.rds")) %>% mutate(tag = as.factor(tag), date = as.Date(date))

dat_locs_do <- dat_do_d %>% filter(PA == 1)
dat_locs_do$month <- month(dat_locs_do$date)

dat_locs_agi <- dat_agi_d %>% filter(PA == 1)
dat_locs_agi$month <- month(dat_locs_agi$date)

#plot DO vs. temp by month
dat_do_sum <- dat_locs_do %>%
  group_by(as.factor(month)) %>%
  summarise(mean_do_0m = mean(o2_mean_0m, na.rm = TRUE), 
            sd_do_0m = sd(o2_mean_0m, na.rm = TRUE), 
            mean_do_250m = mean(o2_mean_250m, na.rm = TRUE),
            sd_do_250m = sd(o2_mean_250m, na.rm = TRUE),
            mean_temp = mean(temp_mean, na.rm = TRUE), 
            sd_temp = sd(temp_mean, na.rm = TRUE))
colnames(dat_do_sum) <- c("month", "mean_do_0m", "sd_do_0m", "mean_do_250m", "sd_do_250m", "mean_temp", "sd_temp")

do_0m_month <- ggplot(dat_do_sum, aes(x = month, y = mean_do_0m))+
  geom_bar(fill = "#92351e", stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = mean_do_0m - sd_do_0m, ymax = mean_do_0m + sd_do_0m), size =  1, color = "black", width = 0.4)+
  xlab("Month")+
  ylab("Dissolved oxygen (mmol/m^3)")+
  tidyquant::theme_tq()+
  theme(axis.title = element_text(size = 20, color = "black"), 
        axis.text = element_text(size = 16, color = "black"))

do_250m_month <- ggplot(dat_do_sum, aes(x = month, y = mean_do_250m))+
  geom_bar(fill = "#92351e", stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = mean_do_250m - sd_do_250m, ymax = mean_do_250m + sd_do_0m), size =  1, color = "black", width = 0.4)+
  xlab("Month")+
  ylab("Dissolved oxygen (mmol/m^3)")+
  tidyquant::theme_tq()+
  theme(axis.title = element_text(size = 20, color = "black"), 
        axis.text = element_text(size = 16, color = "black"))

temp_0m_month <- ggplot(dat_do_sum, aes(x = month, y = mean_temp))+
  geom_bar(fill = "#92351e", stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), size =  1, color = "black", width = 0.4)+
  xlab("Month")+
  ylab("Temperature (C)")+
  tidyquant::theme_tq()+
  theme(axis.title = element_text(size = 20, color = "black"), 
        axis.text = element_text(size = 16, color = "black"))

ggsave(here("figs/ms/supp_figs/do_0m_month.png"),  do_0m_month, height = 5, width = 5, units = c("in"))
ggsave(here("figs/ms/supp_figs/do_250m_month.png"),  do_250m_month, height = 5, width = 5, units = c("in"))
ggsave(here("figs/ms/supp_figs/temp_0m_month.png"), temp_0m_month,  height = 5, width = 5, units = c("in"))


#seasonal maps of DO (0m and 250m) and temp
rast_0m_seas <- rast(here("data/enviro/psat_spot_all/all_processed/season_res/dat_0m_season.nc"))

  #winter Do 0m
rast_seas_0m_subW <- subset(rast_0m_seas, month(time(rast_0m_seas)) == 12)
rast_seas_0m_do_W <- rast_seas_0m_subW["o2_1"]

do_0m_W <- ggplot() +
  geom_spatraster(data = rast_seas_0m_do_W) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = -1, limits = c(150, 350)) +
  labs(fill = "Dissolved oxygen (mmol/m^3)")+
  ggtitle("Winter, 0m")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))

  #spring DO 0m 
rast_seas_0m_subSp <- subset(rast_0m_seas, month(time(rast_0m_seas)) == 3)
rast_seas_0m_do_Sp <- rast_seas_0m_subSp["o2_2"]

do_0m_Sp <- ggplot() +
  geom_spatraster(data = rast_seas_0m_do_Sp) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = -1, limits = c(150, 350)) +
  labs(fill = "Dissolved oxygen (mmol/m^3)")+
  ggtitle("Spring, 0m")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))

  #summer DO 0m
rast_seas_0m_subSu <- subset(rast_0m_seas, month(time(rast_0m_seas)) == 6)
rast_seas_0m_do_Su <- rast_seas_0m_subSu["o2_3"]

do_0m_Su <- ggplot() +
  geom_spatraster(data = rast_seas_0m_do_Su) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = -1, limits = c(150, 350)) +
  labs(fill = "Dissolved oxygen (mmol/m^3)")+
  ggtitle("Summer, 0m")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))

  #fall DO 0m 
rast_seas_0m_subF <- subset(rast_0m_seas, month(time(rast_0m_seas)) == 9)
rast_seas_0m_do_F <- rast_seas_0m_subF["o2_4"]

do_0m_F <- ggplot() +
  geom_spatraster(data = rast_seas_0m_do_F) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = -1, limits = c(150, 350)) +
  labs(fill = "Dissolved oxygen (mmol/m^3)")+
  ggtitle("Fall, 0m")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))


all_do_0m <- (do_0m_W|do_0m_Sp)/(do_0m_Su|do_0m_F)+
  plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "Dissolved oxygen (mmol/m^3)")

ggsave(here("figs/ms/supp_figs/do_0m_seasonal.png"), all_do_0m, height = 8, width = 10, units = c("in"))

#DO 250m
rast_250m_seas <- rast(here("data/enviro/psat_spot_all/all_processed/season_res/dat_250m_season.nc"))

#winter Do 250m
rast_seas_250m_subW <- subset(rast_250m_seas, month(time(rast_250m_seas)) == 12)
rast_seas_250m_do_W <- rast_seas_250m_subW["o2_1"]

do_250m_W <- ggplot() +
  geom_spatraster(data = rast_seas_250m_do_W) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = -1, limits = c(0, 250)) +
  labs(fill = "Dissolved oxygen (mmol/m^3)")+
  ggtitle("Winter, 250m")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))

#spring DO 250m 
rast_seas_250m_subSp <- subset(rast_250m_seas, month(time(rast_250m_seas)) == 3)
rast_seas_250m_do_Sp <- rast_seas_250m_subSp["o2_2"]

do_250m_Sp <- ggplot() +
  geom_spatraster(data = rast_seas_250m_do_Sp) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = -1, limits = c(0, 250)) +
  labs(fill = "Dissolved oxygen (mmol/m^3)")+
  ggtitle("Spring, 250m")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))

#summer DO 250m
rast_seas_250m_subSu <- subset(rast_250m_seas, month(time(rast_250m_seas)) == 6)
rast_seas_250m_do_Su <- rast_seas_250m_subSu["o2_3"]

do_250m_Su <- ggplot() +
  geom_spatraster(data = rast_seas_250m_do_Su) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = -1, limits = c(0, 250)) +
  labs(fill = "Dissolved oxygen (mmol/m^3)")+
  ggtitle("Summer, 250m")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))

#fall DO 250m 
rast_seas_250m_subF <- subset(rast_250m_seas, month(time(rast_250m_seas)) == 9)
rast_seas_250m_do_F <- rast_seas_250m_subF["o2_4"]

do_250m_F <- ggplot() +
  geom_spatraster(data = rast_seas_250m_do_F) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = -1, limits = c(0, 250)) +
  labs(fill = "Dissolved oxygen (mmol/m^3)")+
  ggtitle("Fall, 250m")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))


all_do_250m <- (do_250m_W|do_250m_Sp)/(do_250m_Su|do_250m_F)+
  plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "Dissolved oxygen (mmol/m^3)")

ggsave(here("figs/ms/supp_figs/do_250m_seasonal.png"), all_do_250m, height = 8, width = 10, units = c("in"))

#temperature, 0m, seasonal
rast_0m_seas <- rast(here("data/enviro/psat_spot_all/all_processed/season_res/dat_0m_season.nc"))

#winter temp 0m
rast_seas_0m_subW <- subset(rast_0m_seas, month(time(rast_0m_seas)) == 12)
rast_seas_0m_temp_W <- rast_seas_0m_subW["votemper_1"]

temp_0m_W <- ggplot() +
  geom_spatraster(data = rast_seas_0m_temp_W) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = 1, limits = c(0, 35)) +
  labs(fill = "Temperature (C)")+
  ggtitle("Winter, 0m")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))

#spring temp 0m 
rast_seas_0m_subSp <- subset(rast_0m_seas, month(time(rast_0m_seas)) == 3)
rast_seas_0m_temp_Sp <- rast_seas_0m_subSp["votemper_2"]

temp_0m_Sp <- ggplot() +
  geom_spatraster(data = rast_seas_0m_temp_Sp) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = 1, limits = c(0, 35)) +
  labs(fill = "Temperature (C)")+
  ggtitle("Spring, 0m")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))

#summer temp 0m
rast_seas_0m_subSu <- subset(rast_0m_seas, month(time(rast_0m_seas)) == 6)
rast_seas_0m_temp_Su <- rast_seas_0m_subSu["votemper_3"]

temp_0m_Su <- ggplot() +
  geom_spatraster(data = rast_seas_0m_temp_Su) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = 1, limits = c(0, 35)) +
  labs(fill = "Temperature")+
  ggtitle("Summer, 0m")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))

#fall temp 0m 
rast_seas_0m_subF <- subset(rast_0m_seas, month(time(rast_0m_seas)) == 9)
rast_seas_0m_temp_F <- rast_seas_0m_subF["votemper_4"]

temp_0m_F <- ggplot() +
  geom_spatraster(data = rast_seas_0m_temp_F) +
  geom_map(data = testt,map = testt,aes(map_id = region, x = long, y = lat), fill = "grey75", color = "black") +
  scale_x_continuous(expand =c(0,0),limits = c(-153,-103)) +
  scale_y_continuous(expand=c(0,0),limits = c(1,49)) +
  scale_fill_whitebox_c(palette = "muted", direction = 1, limits = c(0, 35)) +
  labs(fill = "Temperature (C)")+
  ggtitle("Fall, 0m")+
  theme_map()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))


all_temp_0m <- (temp_0m_W|temp_0m_Sp)/(temp_0m_Su|temp_0m_F)+
  plot_layout(guides = "collect") & theme(legend.position = 'right', legend.title = element_text(size = 16), legend.text = element_text(size = 14)) & labs(fill = "Temperature (C)")

ggsave(here("figs/ms/supp_figs/temp_0m_seasonal.png"), all_temp_0m, height = 8, width = 10, units = c("in"))

#### SF 6: Histos of FL by sex ####
fl_dat <- read.csv(here("data/presence_locs/metadata_csv.csv"))

fl_histo <- ggplot(fl_dat, aes(Fork.length..cm.))+
  geom_histogram(bins = 10, color = "black", fill = "#92351e", alpha = 0.9)+
  facet_wrap(~Sex)+
  ylab("Count")+
  xlab("Fork length (cm)")+
  tidyquant::theme_tq()

ggsave(here("figs/ms/supp_figs/fl_histo.png"), fl_histo, width = 5, height = 4, units = c("in"))


#### SF 15: ENSO HSI maps ####
#have to save using export button otherwise adds border, using height of 750 and width 350 (200 for LN panel)

#base year
enso_base <- hsi_maps_enso_avg(rast_folder = "data/enviro/psat_spot_all/hsi_rasts/Jan13_Dec13", enso = "diff", main_text = FALSE)

#LN year 
enso_LN <- hsi_maps_difference_enso_avg(enso_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/LN_F_2010", neut_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/Jan13_Dec13", enso = "LN", main_text = FALSE)

#EN year
enso_EN <- hsi_maps_difference_enso_avg(enso_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/EN_FW_Nov2014_Jan2015", neut_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/Jan13_Dec13", enso = "EN", main_text = FALSE)

#### SF 16: diet data time series ####
#use functions to reorder the set of bars in each facet (from GitHub: dgrtwo/drlib)
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

by_year <- rmpq_prey_year2 %>%
  filter(GII > 4.06 & Year <= 2014 & Year >= 2003) %>% #25% quantile value
  ggplot(aes(x = reorder_within(Common_Name, -GII, Year), y = GII)) +
  geom_bar(stat = "identity", fill = "#92351e") +
  scale_x_reordered()+
  facet_wrap(~Year, scales = "free_x")+
  theme_tq() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 0.95)) +
  xlab('') 

ggsave(here("figs/ms/supp_figs/diet_data_by_year.png"), width = 8, height = 9, units = c("in"))

### libraries ####
{library(here)
  library(MetBrewer)
  library(terra)
  library(ggBRT)
  library(patchwork)
  library(ggrepel)
  library(tidyverse)
  set.seed(1004)}

### saved custom themes ####
theme_bls_map <- function(){ 
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

### Figure 1: locs over study area bathymetry ####
#### presence location data ####
#shark data
ani_locs <- readRDS(here("data/presence_locs/psat_spot_domain/processed/psat_spot_animotum.RDS")) %>% 
  mutate(PA = 0, rep = NA) %>% 
  subset(select = -c(geometry))

names(ani_locs) <- c("tag", "date", "lon", "lat", "PA", "rep")

#coastline data
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="USA"
                      | north_map$region=="Mexico",]

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
  geom_path(data=ani_locs, aes(lon, lat, colour=as.factor(tag)), size = 0.5) +
  scale_color_manual(values = met.brewer("OKeeffe2", 73)) +
theme_bls_map() +
  xlim(-153, -103)+
  ylim(1, 49) +
  theme(legend.position = "right") + 
  guides(fill = guide_legend(title = "Bathymetry (m)", reverse = TRUE), 
         color = FALSE)

ggsave(here("figs/ms/tracks_bathy.png"), height = 7, width = 5, units = c("in"))

### Figure 2: AGI maps ####
#neutral year
#hsi_rast_gen(date_start = c("2013-09-01"), date_end = c("2014-01-31"), season = "FW", output_name = "neut_FW_Sept2013_Jan2014")
agi_250m_2013 <- agi_maps_ms(get_rast = "Y", rast_folder = "data/enviro/psat_spot_all/hsi_rasts/agi_rasts/Jan13_Dec13", fig_pos = 1)
ggsave(here("figs/ms/fig2_agi/250_2013.png"), agi_250m_2013, height = 7, width = 7, units = c("in"))

#La Niña
#hsi_rast_gen(date_start = c("2010-09-01"), date_end = c("2010-11-30"), season = "F", output_name = "LN_F_2010")
agi_250m_2010 <- agi_maps_ms(get_rast = "Y", rast_folder = "data/enviro/psat_spot_all/hsi_rasts/agi_rasts/LN_F_2010", fig_pos = 2)
ggsave(here("figs/ms/fig2_agi/250_2010.png"), agi_250m_2010, height = 7, width = 7, units = c("in"))

#EL Niño
#2014 
#hsi_rast_gen(date_start = c("2014-11-01"), date_end = c("2015-01-31"), season = "FW", output_name = "EN_FW_Nov2014_Jan2015")
agi_250m_2014 <- agi_maps_ms(get_rast = "Y", rast_folder = "data/enviro/psat_spot_all/hsi_rasts/agi_rasts/EN_FW_Nov2014_Jan2015", fig_pos = 3)
ggsave(here("figs/ms/fig2_agi/250_2014.png"), agi_250m_2014, height = 7, width = 7, units = c("in"))


# Figure 3: predictor relative importance ####
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

base_cols <- MetBrewer::met.brewer("OKeeffe2", n = length(unique(base_inf$Predictor_variable))+6, direction = -1)

base_inf2 <- base_inf %>% mutate(fraction = relative_influence / sum(relative_influence),
                                    ymax = cumsum(fraction),
                                    ymin = c(0, head(ymax, n=-1)),
                                    labelPosition = (ymax + ymin)/2)

base_pred <- ggplot(base_inf2, aes(fill = reorder(Predictor_variable, -relative_influence))) +
  geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_text(aes(x = 3.5, y = labelPosition, label = paste0(round(relative_influence, digits = 0), "%")), color = "white", size = 4.5)+
  geom_label_repel(aes(x = 4, y = labelPosition, label = Predictor_variable),
                   fill = alpha(c("white"),0),
                   label.size = NA,
                   size = 4.5, hjust = .5,
                   nudge_x = 0.3, direction = "x",
                   segment.color = "transparent"
                   #segment.curvature = -0.1,
                   #segment.ncp = 3,
                   #segment.angle = 20, seed = 123
  ) +
  coord_polar(theta = "y", clip = "off") +
  xlim(c(2, 5)) +
  scale_fill_manual(values = base_cols) +
  theme(legend.position = "none", 
        plot.margin =  margin(-1,-1,-1,-1)) +
  theme_void() +
  guides(
    fill = "none"
  )

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

do_cols <- NatParksPalettes::natparks.pals("Acadia", n = length(unique(do_inf$Predictor_variable))+20, direction = -1)

do_inf2 <- do_inf %>% mutate(fraction = relative_influence / sum(relative_influence),
                                 ymax = cumsum(fraction),
                                 ymin = c(0, head(ymax, n=-1)),
                                 labelPosition = (ymax + ymin)/2)

do_pred <- ggplot(do_inf2, aes(fill = reorder(Predictor_variable, -relative_influence))) +
  geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_text(aes(x = 3.5, y = labelPosition, label = paste0(round(relative_influence, digits = 0), "%")), color = "white", size = 4.5)+
  geom_label_repel(aes(x = 4, y = labelPosition, label = Predictor_variable),
                   fill = alpha(c("white"),0),
                   label.size = NA,
                   size = 4.5, hjust = .5,
                   nudge_x = 0.63, direction = "x",
                   segment.color = "transparent"
                   #segment.curvature = -0.1,
                   #segment.ncp = 3,
                   #segment.angle = 20, seed = 123
  ) +
  coord_polar(theta = "y", clip = "off") +
  xlim(c(2, 5)) +
  scale_fill_manual(values = do_cols) +
  theme(legend.position = "none", 
        plot.margin =  margin(-1,-1,-1,-1)) +
  theme_void() +
  guides(
    fill = "none"
  )


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

agi_inf2 <- agi_inf %>% mutate(fraction = relative_influence / sum(relative_influence),
                                 ymax = cumsum(fraction),
                                 ymin = c(0, head(ymax, n=-1)),
                                 labelPosition = (ymax + ymin)/2)
agi_cols <- MetBrewer::met.brewer("Greek", n = length(unique(agi_inf$Predictor_variable))+5)

agi_pred <- ggplot(agi_inf2, aes(fill = reorder(Predictor_variable, -relative_influence))) +
  geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_text(aes(x = 3.5, y = labelPosition, label = paste0(round(relative_influence, digits = 0), "%")), color = "white", size = 4.5)+
  geom_label_repel(aes(x = 4, y = labelPosition, label = Predictor_variable),
                   fill = alpha(c("white"),0),
                   label.size = NA,
                   size = 4.5, hjust = .5,
                   nudge_x = 0.7, direction = "x",
                   segment.color = "transparent"
                   #segment.curvature = -0.1,
                   #segment.ncp = 3,
                   #segment.angle = 20, seed = 123
  ) +
  coord_polar(theta = "y", clip = "off") +
  xlim(c(2, 5)) +
  scale_fill_manual(values = agi_cols) +
  theme(legend.position = "none", 
        plot.margin =  margin(-1,-1,-1,-1)) +
  theme_void() +
  guides(
    fill = "none"
  )

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

comb_cols <- NatParksPalettes::natparks.pals("BryceCanyon", n = length(unique(do_agi_inf$Predictor_variable))+30)

do_agi_inf2 <- do_agi_inf %>% mutate(fraction = relative_influence / sum(relative_influence),
                               ymax = cumsum(fraction),
                               ymin = c(0, head(ymax, n=-1)),
                               labelPosition = (ymax + ymin)/2)

do_agi_pred <- ggplot(do_agi_inf2, aes(fill = reorder(Predictor_variable, -relative_influence))) +
  geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_text(aes(x = 3.5, y = labelPosition, label = paste0(round(relative_influence, digits = 0), "%")), color = "white", size = 4.5)+
  geom_label_repel(aes(x = 4, y = labelPosition, label = Predictor_variable),
                   fill = alpha(c("white"),0),
                   label.size = NA,
                   size = 4.5, hjust = .5,
                   nudge_x = 0.7, direction = "x",
                   segment.color = "transparent"
                   #segment.curvature = -0.1,
                   #segment.ncp = 3,
                   #segment.angle = 20, seed = 123
  ) +
  coord_polar(theta = "y", clip = "off") +
  xlim(c(2, 5)) +
  scale_fill_manual(values = comb_cols) +
  theme(legend.position = "none", 
        plot.margin = margin(-1,-1,-1,-1)) +
  theme_void() +
  guides(
    fill = "none"
  )

#ggsave(here("figs/ms/rel_inf_pred.png"), all_pred,  height = 15, width = 15, units = c("in"))
ggsave(here("figs/ms/fig3_pred/base_pred.png"), base_pred, height = 7, width = 7, units = c("in"))
ggsave(here("figs/ms/fig3_pred/do_pred.png"), do_pred, height = 7, width = 7, units = c("in"))
ggsave(here("figs/ms/fig3_pred/agi_pred.png"), agi_pred, height = 7, width = 7, units = c("in"))
ggsave(here("figs/ms/fig3_pred/do_agi_pred.png"), do_agi_pred, height = 7, width = 7, units = c("in"))

### Figure 4: Partial plots ####
source(here("functions/partial_plot.R"))

#DO, 0m
do_0m <- par_plot_func(do_mod_fin, vars = c("o2_mean_0m", "o2_mean_0m_seas", "o2_mean_0m_ann"), vars_type = "o2_0m")
ggsave(here("figs/ms/fig3/do_0m.png"), do_0m, height = 5, width = 7, units = c("in"))

#DO, 250m 
do_250m <- par_plot_func(do_mod_fin, vars = c("o2_mean_250m", "o2_mean_250m_seas", "o2_mean_250m_ann"), vars_type = "o2_250m")
ggsave(here("figs/ms/fig3/do_250m.png"), do_250m, height = 5, width = 7, units = c("in"))

#AGI, 0m
agi_0m <- par_plot_func(agi_mod_fin, vars = c("AGI_0m", "AGI_0m_seas", "AGI_0m_ann"), vars_type = "AGI_0m")
ggsave(here("figs/ms/fig3/agi_0m.png"), agi_0m, height = 5, width = 9, units = c("in"))

#AGI, 250m
agi_250m <- par_plot_func(agi_mod_fin, vars = c("AGI_250m", "AGI_250m_seas", "AGI_250m_ann"), vars_type = "AGI_250m")
ggsave(here("figs/ms/fig3/agi_250m.png"), agi_250m, height = 5, width = 7, units = c("in"))

all_do <- ggarrange(do_0m, do_250m, nrow = 2, ncol = 1, common.legend = TRUE)
ggsave(here("figs/ms/fig3/all_do.png"), all_do, height = 8, width = 7, units = c("in"))

all_agi <- ggarrange(agi_0m, agi_250m, nrow = 2, ncol = 1, common.legend = TRUE)
ggsave(here("figs/ms/fig3/all_agi.png"), all_agi, height = 8, width = 7, units = c("in"))

### Figure 5: model performance ####
mod_metric_files <- list.files(here("data/brt/mod_outputs/perf_metric_iters"), pattern = ".rds", full.names = TRUE)

base_file <- readRDS(mod_metric_files[2])
do_file <- readRDS(mod_metric_files[4])
agi_file <- readRDS(mod_metric_files[1])
combo_file <- readRDS(mod_metric_files[3])

base_file$mod_type <- "Base model"
agi_file$mod_type <- "AGI model"
do_file$mod_type <- "DO model"
combo_file$mod_type <- "DO, AGI combo model"

mod_metrics <- rbind(base_file, agi_file, do_file, combo_file)
mod_metrics <- mod_metrics %>% mutate(dev_exp = dev_exp*100)

# analysis of variance
anova <- aov(TSS ~ mod_type, data = mod_metrics)

# Tukey's test
tukey <- TukeyHSD(anova)

# compact letter display
cld <- multcompView::multcompLetters4(anova, tukey, reversed = TRUE)

dt_tss <- mod_metrics %>%
  group_by(mod_type) %>%
  summarise(mean_tss=mean(TSS), sd = sd(TSS)) %>%
  arrange(desc(mean_tss))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$mod_type)
dt_tss$cld <- cld$Letters

TSS_plot <- dt_tss %>% mutate(mod_type = as.factor(mod_type), 
                              mod_type = fct_relevel(mod_type, c("Base model", "AGI model", "DO model", "DO, AGI combo model"))) %>%
  ggplot(aes(x = mod_type, y=mean_tss)) +
  geom_bar(stat = "identity", fill = "#92351e", width = 0.7)+
  geom_errorbar(aes(ymin = mean_tss - sd, ymax = mean_tss + sd), size =  1, color = "black", width = 0.4)+
  #geom_segment(aes(x=mod_type, xend=mod_type, y=0.4, yend=mean_tss), color="#92351e", linewidth = 1.5) +
  #geom_point(color="orange", size=6) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("TSS") + 
  ylim(0, 0.75)+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  geom_text(aes(label = cld, y = mean_tss + 0.03), vjust = -0.5, size = 5) 


# analysis of variance
anova <- aov(AUC ~ mod_type, data = mod_metrics)

# Tukey's test
tukey <- TukeyHSD(anova)

# compact letter display
cld <- multcompView::multcompLetters4(anova, tukey, reversed = TRUE)

dt_auc <- mod_metrics %>%
  group_by(mod_type) %>%
  summarise(mean_auc=mean(AUC), sd = sd(AUC)) %>%
  arrange(desc(mean_auc))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$mod_type)
dt_auc$cld <- cld$Letters

AUC_plot <- dt_auc %>% mutate(mod_type = as.factor(mod_type), 
                              mod_type = fct_relevel(mod_type, c("Base model", "AGI model", "DO model", "DO, AGI combo model"))) %>%
  ggplot(aes(x = mod_type, y=mean_auc)) +
  geom_bar(stat = "identity", fill = "#92351e", width = 0.7)+
  geom_errorbar(aes(ymin = mean_auc - sd, ymax = mean_auc + sd), size =  1, color = "black", width = 0.4)+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("AUC") + 
  ylim(0, 1) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  geom_text(aes(label = cld, y = mean_auc + 0.015), vjust = -0.5, size = 5) 


# analysis of variance
anova <- aov(dev_exp ~ mod_type, data = mod_metrics)

# Tukey's test
tukey <- TukeyHSD(anova)

# compact letter display
cld <- multcompView::multcompLetters4(anova, tukey, reversed = TRUE)

dt_dev <- mod_metrics %>%
  group_by(mod_type) %>%
  summarise(mean_dev=mean(dev_exp), sd = sd(dev_exp)) %>%
  arrange(desc(mean_dev))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$mod_type)
dt_dev$cld <- cld$Letters

perc_exp_plot <- dt_dev %>% mutate(mod_type = as.factor(mod_type), 
                                   mod_type = fct_relevel(mod_type, c("Base model", "AGI model", "DO model", "DO, AGI combo model"))) %>%
  ggplot(aes(x = mod_type, y=mean_dev)) +
  geom_bar(stat = "identity", fill = "#92351e", width = 0.7)+
  geom_errorbar(aes(ymin = mean_dev - sd, ymax = mean_dev + sd), size =  1, color = "black", width = 0.4)+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Deviance explained (%)") + 
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14))+
  ylim(0, 70)+
  geom_text(aes(label = cld, y = mean_dev + 3), vjust = -0.5, size = 5) 

all_metrics <- TSS_plot|AUC_plot|perc_exp_plot

ggsave(here("figs/ms/fig5_metrics/perform_metrics.png"), all_metrics, height = 6, width = 12, units = c("in"))

### Figure 6: HSI maps study period ####
all_maps <- hsi_maps(rast_folder = "data/enviro/psat_spot_all/hsi_rasts/Jan03_Dec15", ms = "Y")
ggsave(here("figs/ms/fig6_hsi_all/all_maps.png"), all_maps, height = 7, width = 7, units = c("in"))

### Figure 7: ENSO HSI maps ####
#have to save using export button otherwise adds border, using width of 750 and height 350 (200 for LN panel)

#base year
enso_base <- hsi_maps_enso(rast_folder = "data/enviro/psat_spot_all/hsi_rasts/Jan13_Dec13", enso = "base")
#ggsave(here("figs/ms/fig7_enso_diet/base_panel.png"), enso_base, width = 3, height = 8, units = c("in"))

#LN year 
enso_LN <- hsi_maps_enso(rast_folder = "data/enviro/psat_spot_all/hsi_rasts/LN_F_2010", enso = "LN")
#ggsave(here("figs/ms/fig7_enso_diet/LN_panel.png"), enso_LN, width = 3, height = 8, units = c("in"))

#EN year
enso_EN <- hsi_maps_enso(rast_folder = "data/enviro/psat_spot_all/hsi_rasts/EN_FW_Nov2014_Jan2015", enso = "EN")
#ggsave(here("figs/ms/fig7_enso_diet/EN_panel.png"), enso_EN, width = 3, height = 8, units = c("in"))


#diet data 
source(here("scripts/7a_diet_data.R"))

#neutral year 
diet_neutral <- rmpq_prey_year2 %>% ungroup() %>%
  filter(perc_GII >= 1 & Year == 2013) %>% 
  top_n(3) %>%
  ggplot(aes(x = reorder(Common_Name, -perc_GII), y = perc_GII)) +
  geom_bar(stat = "identity", fill = "#92351e", alpha = 0.85) +
  theme_minimal() + 
  theme(axis.text.y=element_text(size=20, color = "black"), 
        axis.title=element_text(size=22, color = "black"), 
        axis.text.x = element_blank(),
        panel.grid = element_blank()) +
  xlab('')+
  ylab('% GII')+
  scale_x_discrete(position = "top") 
ggsave(here("figs/ms/fig7_enso_diet/diet_neutral.png"), diet_neutral, height = 4, width = 4, units = c("in"))


#LN year 
diet_LN <- rmpq_prey_year2 %>% ungroup() %>%
  filter(perc_GII >= 1 & Year == 2010) %>% 
  top_n(3) %>%
  ggplot(aes(x = reorder(Common_Name, -perc_GII), y = perc_GII)) +
  geom_bar(stat = "identity", fill = "#92351e", alpha = 0.85) +
  theme_minimal() + 
  theme(axis.text=element_text(size=20, color = "black"), 
        axis.title=element_text(size=22, color = "black"), 
        #axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        #axis.text.x = element_blank(),
        panel.grid = element_blank()) +
  xlab('')+
  ylab('% GII')+
  scale_x_discrete(position = "top") 
ggsave(here("figs/ms/fig7_enso_diet/diet_LN.png"), diet_LN, height = 4, width = 4, units = c("in"))


#EN year 
diet_EN <- rmpq_prey_year2 %>%
  filter(perc_GII >= 1 & Year == 2009) %>% 
  top_n(3) %>%
  ggplot(aes(x = reorder(Common_Name, -perc_GII), y = perc_GII)) +
  geom_bar(stat = "identity", fill = "#92351e", alpha = 0.85) +
  theme_minimal() + 
  theme(axis.text=element_text(size=20, color = "black"), 
        axis.title=element_text(size=22, color = "black"), 
        #axis.text.y = element_blank(), 
        #axis.text.x = element_blank(),
        axis.title.y = element_blank(), 
        panel.grid = element_blank()) +
  xlab('')+
  ylab('% GII')+
  scale_x_discrete(position = "top") 

ggsave(here("figs/ms/fig7_enso_diet/diet_EN.png"), diet_EN, height = 4, width = 4, units = c("in"))



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

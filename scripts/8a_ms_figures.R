### libraries ####
{library(tidyverse)
  library(here)
  library(MetBrewer)
  library(terra)
  library(cowplot)}

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

# Figure 2: predictor relative importance ####
#list models
base_mod <- readRDS(here("data/brt/mod_outputs/final_mods/brt_base_0m_dail_no_wind.rds"))
do_mod_of <- readRDS(here("data/brt/mod_outputs/final_mods/brt_do_0m_60m_250m_dail_seas_ann_no_wind.rds"))
do_mod_fin <- readRDS(here("data/brt/mod_outputs/final_mods/brt_do_0m_250m_dail_seas_ann.rds"))
agi_mod_of <- readRDS(here("data/brt/mod_outputs/final_mods/brt_agi_0m_60m_250m_dail_seas_ann_no_wind.rds"))
agi_mod_fin <- readRDS(here("data/brt/mod_outputs/final_mods/brt_agi_0m_250m_dail_seas_ann.rds"))
agi_mod_back <- readRDS(here("data/brt/mod_outputs/final_mods/brt_agi_0m_250m_dail_seas_ann_back.rds"))
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
  geom_text(aes(x = 3.5, y = labelPosition, label = paste0(round(relative_influence, digits = 0), "%")), color = "white", size = 5)+
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

do_cols <- MetBrewer::met.brewer("OKeeffe2", n = length(unique(do_inf$Predictor_variable))+6, direction = -1)

do_inf2 <- do_inf %>% mutate(fraction = relative_influence / sum(relative_influence),
                                 ymax = cumsum(fraction),
                                 ymin = c(0, head(ymax, n=-1)),
                                 labelPosition = (ymax + ymin)/2)

do_pred <- ggplot(do_inf2, aes(fill = reorder(Predictor_variable, -relative_influence))) +
  geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_text(aes(x = 3.5, y = labelPosition, label = paste0(round(relative_influence, digits = 0), "%")), color = "white", size = 5)+
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
agi_cols <- MetBrewer::met.brewer("OKeeffe2", n = length(unique(agi_inf$Predictor_variable))+8, direction = -1)

agi_pred <- ggplot(agi_inf2, aes(fill = reorder(Predictor_variable, -relative_influence))) +
  geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_text(aes(x = 3.5, y = labelPosition, label = paste0(round(relative_influence, digits = 0), "%")), color = "white", size = 5)+
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

comb_cols <- MetBrewer::met.brewer("OKeeffe2", n = length(unique(do_agi_inf$Predictor_variable))+8, direction = -1)

do_agi_inf2 <- do_agi_inf %>% mutate(fraction = relative_influence / sum(relative_influence),
                               ymax = cumsum(fraction),
                               ymin = c(0, head(ymax, n=-1)),
                               labelPosition = (ymax + ymin)/2)

do_agi_pred <- ggplot(do_agi_inf2, aes(fill = reorder(Predictor_variable, -relative_influence))) +
  geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_text(aes(x = 3.5, y = labelPosition, label = paste0(round(relative_influence, digits = 0), "%")), color = "white", size = 5)+
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

#combine plots and save
all_pred <- (base_pred|do_pred)/(agi_pred|do_agi_pred) + 
  plot_annotation(tag_levels = "A", theme = theme(plot.margin = unit(c(0,0,0,0), "in"))) & 
  theme(plot.tag = element_text(size = 20))

ggsave(here("figs/ms/rel_inf_pred.png"), all_pred,  height = 15, width = 15, units = c("in"))

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










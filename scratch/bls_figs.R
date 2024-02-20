### libraries ####
library(tidyverse)
library(terra)
library(sf)
library(tidyquant)
library(here)
library(MetBrewer)
library(gbm)
library(tidyterra)
library(extrafont)

theme_bls_map <- function(){ 
  font <- "Arial"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      plot.title = element_text(             #axis titles
        family = font,            #font family
        color = "white",
        size = 40), 
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
       color = "white",
         size = 40),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        color = "white",
        size = 30),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      legend.position = "none"
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

theme_bls_inf <- function(){ 
  font <- "Arial"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      plot.title = element_text(             #axis titles
        family = font,            #font family
        color = "white",
        size = 20), 
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        color = "white",
        size = 20),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        color = "white",
        size = 15),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      legend.position = "none"
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

theme_bls_agi <- function(){ 
  font <- "Arial"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      plot.title = element_text(             #axis titles
        family = font,            #font family
        color = "white",
        size = 20), 
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        color = "white",
        size = 20),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        color = "white",
        size = 15),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)), 
      
      legend.text = element_text(family = font,            #font family
                                 color = "white",
                                 size = 15), 
      
      legend.title = element_text(family = font,            #font family
                                  color = "white",
                                  size = 20)

            #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}
### data ####
#loc & covar data
cmem_dat <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_AGIwdemand_250m.rds"))

#model data
agi_tc5_lr01 <- readRDS(here("data/brt/mod_bls/agi_tc5_lr01.rds"))
do_tc5_lr01 <- readRDS(here("data/brt/mod_bls/do_temp_tc5_lr01.rds"))
agi_temp_tc5_lr01 <- readRDS(here("data/brt/mod_bls/agi_temp_tc5_lr01.rds"))
agi_all_tc5_lr01 <- readRDS(here("data/brt/mod_bls/agi_all_tc5_lr01.rds"))

### track maps ####
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="USA"
                      | north_map$region=="Mexico",]

cmem_dat2 <- cmem_dat %>% filter(PA == 0)

ggplot(shore, aes(long, lat)) +
  coord_map("mercator", xlim = c(-140, -110), ylim = c(10, 50))+
  geom_point(data=cmem_dat2, aes(lon, lat, colour=as.factor(tag)), size = 4.5) +
  geom_polygon(aes(group=group), fill="grey75",lwd=1) +
  scale_color_manual(values = met.brewer("OKeeffe2", 23))+
  theme_bls_map()
  

ggsave(here("figs/bls/track_map.png"), height = 20, width = 15, units = c("in"))


### mod results ####
##### agi ####
agi_inf <- agi_tc5_lr01$contributions %>% as.data.frame()
agi_inf$var <- replace(agi_inf$var, agi_inf$var == "dist_coast", "distance to coast")
agi_inf$var <- replace(agi_inf$var, agi_inf$var == "AGI250", "AGI 250m")
agi_inf$var <- replace(agi_inf$var, agi_inf$var == "j_day", "julian day")
agi_inf$var <- replace(agi_inf$var, agi_inf$var == "chl250", "chl 250m")
agi_inf$var <- replace(agi_inf$var, agi_inf$var == "so0", "salinity 0m")
agi_inf$var <- replace(agi_inf$var, agi_inf$var == "chl0", "chl 0m")
agi_inf$var <- replace(agi_inf$var, agi_inf$var == "AGI0", "AGI 0m")

ggplot(agi_inf, aes(rel.inf, reorder(var, rel.inf))) + 
  geom_col(fill = "#ADB9CA") +
  xlab("Relative influence (%)")+
  ylab("")+
  ggtitle("AGI")+
  theme_bls_inf()  

ggsave(here("figs/bls/agi_inf.png"), height = 6, width = 5, units = c("in"))

#### do, temp ####
do_inf <- do_tc5_lr01$contributions %>% as.data.frame()

do_inf$var <- replace(do_inf$var, do_inf$var == "dist_coast", "distance to coast")
do_inf$var <- replace(do_inf$var, do_inf$var == "o2250", "DO 250m")
do_inf$var <- replace(do_inf$var, do_inf$var == "j_day", "julian day")
do_inf$var <- replace(do_inf$var, do_inf$var == "chl250", "chl 250m")
do_inf$var <- replace(do_inf$var, do_inf$var == "so0", "salinity 0m")
do_inf$var <- replace(do_inf$var, do_inf$var == "chl0", "chl 0m")
do_inf$var <- replace(do_inf$var, do_inf$var == "o20", "DO 0m")
do_inf$var <- replace(do_inf$var, do_inf$var == "thetao0", "temp. 0m")
do_inf$var <- replace(do_inf$var, do_inf$var == "thetao250", "temp. 250m")

ggplot(do_inf, aes(rel.inf, reorder(var, rel.inf))) + 
  geom_col(fill = "#ADB9CA") +
  xlab("Relative influence (%)")+
  ylab("")+
  ggtitle("DO & Temperature")+
  theme_bls_inf()  

ggsave(here("figs/bls/do_inf.png"), height = 6, width = 5, units = c("in"))


#### agi, temp ####
agi_temp_inf <- agi_temp_tc5_lr01$contributions %>% as.data.frame()

agi_temp_inf$var <- replace(agi_temp_inf$var, agi_temp_inf$var == "dist_coast", "distance to coast")
agi_temp_inf$var <- replace(agi_temp_inf$var, agi_temp_inf$var == "AGI250", "AGI 250m")
agi_temp_inf$var <- replace(agi_temp_inf$var, agi_temp_inf$var == "j_day", "julian day")
agi_temp_inf$var <- replace(agi_temp_inf$var, agi_temp_inf$var == "chl250", "chl 250m")
agi_temp_inf$var <- replace(agi_temp_inf$var, agi_temp_inf$var == "so0", "salinity 0m")
agi_temp_inf$var <- replace(agi_temp_inf$var, agi_temp_inf$var == "chl0", "chl 0m")
agi_temp_inf$var <- replace(agi_temp_inf$var, agi_temp_inf$var == "AGI0", "AGI 0m")
agi_temp_inf$var <- replace(agi_temp_inf$var, agi_temp_inf$var == "thetao0", "temp. 0m")
agi_temp_inf$var <- replace(agi_temp_inf$var, agi_temp_inf$var == "thetao250", "temp. 250m")

ggplot(agi_temp_inf, aes(rel.inf, reorder(var, rel.inf))) + 
  geom_col(fill = "#ADB9CA") +
  xlab("Relative influence (%)")+
  ylab("")+
  ggtitle("AGI & Temperature")+
  theme_bls_inf()  

ggsave(here("figs/bls/AGI_temp_inf.png"), height = 6, width = 5, units = c("in"))

#### agi 250m and 50m ####
agi_all_inf <- agi_all_tc5_lr01$contributions %>% as.data.frame()

agi_all_inf$var <- replace(agi_all_inf$var, agi_all_inf$var == "dist_coast", "distance to coast")
agi_all_inf$var <- replace(agi_all_inf$var, agi_all_inf$var == "AGI250", "AGI 250m")
agi_all_inf$var <- replace(agi_all_inf$var, agi_all_inf$var == "j_day", "julian day")
agi_all_inf$var <- replace(agi_all_inf$var, agi_all_inf$var == "chl250", "chl 250m")
agi_all_inf$var <- replace(agi_all_inf$var, agi_all_inf$var == "so0", "salinity 0m")
agi_all_inf$var <- replace(agi_all_inf$var, agi_all_inf$var == "chl0", "chl 0m")
agi_all_inf$var <- replace(agi_all_inf$var, agi_all_inf$var == "AGI0", "AGI 0m")
agi_all_inf$var <- replace(agi_all_inf$var, agi_all_inf$var == "thetao0", "temp. 0m")
agi_all_inf$var <- replace(agi_all_inf$var, agi_all_inf$var == "thetao250", "temp. 250m")

ggplot(agi_all_inf, aes(rel.inf, reorder(var, rel.inf))) + 
  geom_col(fill = "#ADB9CA") +
  xlab("Relative influence (%)")+
  ylab("")+
  ggtitle("AGI 0m & 250m")+
  theme_bls_inf()  

ggsave(here("figs/bls/AGI_all_inf.png"), height = 6, width = 5, units = c("in"))

### surface plots from training dataset ####
source(here("functions/oxy_demand_functions.R"))

o2_range0 <- seq(200.5485, 294.1288, length.out = 100) #mmol/m3 at the surface
temp_range0 <- seq(9.022785, 29.35271, length.out = 100)

AGI_surf_p0 <- expand_grid(o2 = o2_range0, temp = temp_range0) %>% 
  mutate(po2 = rast_to_atm(do = o2, so = 33.46948, temp = temp, depth = 0), #used mean sal at 0m
         met_dem = OxyDemand(Tpref = 16.452, PO2_thresh = 0.0843513526, T_C = temp), 
         AGI = po2/met_dem)

ggplot(AGI_surf_p0, aes(temp, o2)) +
  geom_raster(aes(fill = AGI)) +
  stat_contour(aes(z = AGI), color = "black") +
  scale_fill_gradient() + 
  xlab("Temperature (C)") + 
  ylab(bquote('Dissolved oxygen' ('mmol/m'^'3'))) +
  labs(fill = "AGI") +
  scale_fill_whitebox_c(palette = "deep", direction = -1)+
  ggtitle("AGI at 0m") +
  theme_bls_agi()

ggsave(here("figs/bls/surf_plot0.png"), height = 7, width = 6, units = c("in"))

o2_range250 <- seq(1.000503, 220.2202, length.out = 100)
temp_range250 <- seq(4.493278, 12.88808, length.out = 100)

AGI_surf_p250 <- expand_grid(o2 = o2_range250, temp = temp_range250) %>% 
  mutate(po2 = rast_to_atm(do = o2, so = 34.221, temp = temp, depth = 250), #used mean sal at 0m
         met_dem = OxyDemand(Tpref = 16.452, PO2_thresh = 0.0843513526, T_C = temp), 
         AGI = po2/met_dem)

ggplot(AGI_surf_p250, aes(temp, o2)) +
  geom_raster(aes(fill = AGI)) +
  stat_contour(aes(z = AGI), color = "black") +
  scale_fill_gradient() + 
  xlab("Temperature (C)") + 
  ylab(bquote('Dissolved oxygen' ('mmol/m'^'3'))) + 
  labs(fill = "AGI") + 
  scale_fill_whitebox_c(palette = "deep", direction = -1)+
  ggtitle("AGI at 250m") +
  theme_bls_agi()

ggsave(here("figs/bls/surf_plot250.png"), height = 7, width = 6, units = c("in"))

### lat, dist coast, agi contour ####
contour <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_AGIwdemand_250m.rds"))
temp <- readRDS(here("data/locs_w_covar/cmems/cmems_locs_covar_0m_AGI_dist.rds"))

contour$dist_coast <- temp$dist_coast
contour <- contour %>% 
  filter(PA == 0) %>% 
  filter(AGI_250m != "NA") %>% 
  subset(select = c(dist_coast, lat, AGI_250m))

interp_cont <- with(contour, interp(x = dist_coast, y = lat, z = AGI_250m, 
                                    xo = seq(min(dist_coast), max(dist_coast), length = 100),
                                    yo = seq(min(lat), max(lat), length = 100), 
                                    duplicate = "mean"))

grid_cont <- expand.grid(dist_coast = interp_cont$x, lat = interp_cont$y)
grid_cont$agi <- as.vector(interp_cont$z)

ggplot(grid_cont, aes(x = dist_coast, y = lat, z = agi)) + 
  geom_contour_filled(bins = 5) +
  scale_x_reverse() +
  labs(fill = "AGI at 250m")+
  scale_fill_whitebox_d(palette = "deep", direction = -1)+
  xlab("distance to coast (km)") + 
  ylab("lat")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme_bls_agi()

ggsave(here("figs/bls/agi_lat_dist.png"), height = 7, width = 6, units = c("in"))

### HSI maps ####
extent1<-c(-140,-110,10,50)

#### agi ####
agi_rast_2007 <- rast(here("data/enviro/CMEMS/hsi_map/agi_covar2007_spatrast.nc"))
names(agi_rast_2007) <- c("so0", "chl0","so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")

map_pred = predict(agi_rast_2007, agi_tc5_lr01, type = "response", n.trees = agi_tc5_lr01$gbm.call$best.trees, na.rm = FALSE) 
map_pred = crop(map_pred,extent1)

df_map = as.points(map_pred) %>% st_as_sf() %>% as.data.frame()
colnames(df_map) = c("value", "geometry") 

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = map_pred) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="grey75",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1)+
  ggtitle("AGI") +
  theme_bls_() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("figs/bls/AGI_hsi07.png"), height = 6, width = 5, units = c("in"))

agi_rast_2009 <- rast(here("data/enviro/CMEMS/hsi_map/agi_covar2009_spatrast.nc"))
names(agi_rast_2009) <- c("so0", "chl0","so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")

map_pred = predict(agi_rast_2009, agi_tc5_lr01, type = "response", n.trees = agi_tc5_lr01$gbm.call$best.trees, na.rm = FALSE) 
map_pred = crop(map_pred,extent1)

df_map = as.points(map_pred) %>% st_as_sf() %>% as.data.frame()
colnames(df_map) = c("value", "geometry") 

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = map_pred) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="grey75",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1)+
  ggtitle("AGI") +
  theme_bls_inf() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("figs/bls/AGI_hsi09.png"), height = 6, width = 5, units = c("in"))

#### do, temp ####
do_rast_2005 <- rast(here("data/enviro/CMEMS/hsi_map/do_covar2005_spatrast.nc"))
names(do_rast_2005) <- c("thetao0", "so0", "chl0", "o20", "thetao250", "so250", "chl250", "o2250", "bathy", "dist_coast", "lat", "j_day")

map_pred_do5 = predict(do_rast_2005, do_tc5_lr01, type = "response", n.trees = do_tc5_lr01$gbm.call$best.trees, na.rm = FALSE) 
map_pred_do5 = crop(map_pred_do5,extent1)

df_map = as.points(map_pred_do5) %>% st_as_sf() %>% as.data.frame()

colnames(df_map) = c("value", "geometry") 

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = map_pred_do5) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1)+
  ggtitle("BRT DO & Temperature") +
  theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

ggsave(here("figs/bls/do_temp_hsi05.png"), height = 6, width = 5, units = c("in"))

do_rast_2007 <- rast(here("data/enviro/CMEMS/hsi_map/do_covar2007_spatrast.nc"))
names(do_rast_2007) <- c("thetao0", "so0", "chl0", "o20", "thetao250", "so250", "chl250", "o2250", "bathy", "dist_coast", "lat", "j_day")

map_pred_do7 = predict(do_rast_2007, do_tc5_lr01, type = "response", n.trees = do_tc5_lr01$gbm.call$best.trees, na.rm = FALSE) 
map_pred_do7 = crop(map_pred_do7,extent1)

df_map = as.points(map_pred_do7) %>% st_as_sf() %>% as.data.frame()

colnames(df_map) = c("value", "geometry") 

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = map_pred_do7) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1)+
  ggtitle("BRT DO & Temperature") +
  theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

ggsave(here("figs/bls/do_temp_hsi07.png"), height = 6, width = 5, units = c("in"))

do_rast_2009 <- rast(here("data/enviro/CMEMS/hsi_map/do_covar2009_spatrast.nc"))
names(do_rast_2009) <- c("thetao0", "so0", "chl0", "o20", "thetao250", "so250", "chl250", "o2250", "bathy", "dist_coast", "lat", "j_day")

map_pred_do = predict(do_rast_2009, do_tc5_lr01, type = "response", n.trees = do_tc5_lr01$gbm.call$best.trees, na.rm = FALSE) 
map_pred_do = crop(map_pred_do,extent1)

df_map = as.points(map_pred) %>% st_as_sf() %>% as.data.frame()
colnames(df_map) = c("value", "geometry") 

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = map_pred) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1)+
  ggtitle("DO & Temperature") +
  theme_bls_inf() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("figs/bls/do_temp_hsi09.png"), height = 6, width = 5, units = c("in"))

#### agi, temp ####
agi_temp_rast_2005 <- rast(here("data/enviro/CMEMS/hsi_map/agi_all_covar2005_spatrast.nc"))
names(agi_temp_rast_2005) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")

map_pred_agi_temp5 = predict(agi_temp_rast_2005, agi_all_tc5_lr01, type = "response", n.trees = agi_all_tc5_lr01$gbm.call$best.trees, na.rm = FALSE) 
map_pred_agi_temp5 = crop(map_pred_agi_temp5,extent1)

df_map = as.points(map_pred_agi_temp5) %>% st_as_sf() %>% as.data.frame()
colnames(df_map) = c("value", "geometry") 

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = map_pred_agi_temp5) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1)+
  ggtitle("BRT AGI & Temperature") +
  labs(fill = "HSI")+
  theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())

ggsave(here("figs/bls/agi_temp_hsi05.png"), height = 6, width = 5, units = c("in"))


agi_temp_rast_2007 <- rast(here("data/enviro/CMEMS/hsi_map/agi_all_covar2007_spatrast.nc"))
names(agi_temp_rast_2007) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")

map_pred_agi_temp7 = predict(agi_temp_rast_2007, agi_all_tc5_lr01, type = "response", n.trees = agi_all_tc5_lr01$gbm.call$best.trees, na.rm = FALSE) 
map_pred_agi_temp7 = crop(map_pred_agi_temp7,extent1)

df_map = as.points(map_pred_agi_temp7) %>% st_as_sf() %>% as.data.frame()
colnames(df_map) = c("value", "geometry") 

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = map_pred_agi_temp7) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1)+
  ggtitle("BRT AGI & Temperature") +
  labs(fill = "HSI")+
  theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())

ggsave(here("figs/bls/agi_temp_hsi07.png"), height = 6, width = 5, units = c("in"))

agi_temp_rast_2009 <- rast(here("data/enviro/CMEMS/hsi_map/agi_temp_covar2009_spatrast.nc"))
names(agi_temp_rast_2009) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")

map_pred_agi_temp = predict(agi_temp_rast_2009, agi_temp_tc5_lr01, type = "response", n.trees = agi_temp_tc5_lr01$gbm.call$best.trees, na.rm = FALSE) 
map_pred_agi_temp = crop(map_pred_agi_temp,extent1)

df_map = as.points(map_pred) %>% st_as_sf() %>% as.data.frame()
colnames(df_map) = c("value", "geometry") 

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = map_pred) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1)+
  ggtitle("AGI & Temperature") +
  labs(fill = "HSI")+
  theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())

ggsave(here("figs/bls/agi_temp_hsi09.png"), height = 6, width = 5, units = c("in"))

#### agi 0m 2005 ####
agi_0m_rast_2005 <- rast(here("data/enviro/CMEMS/hsi_map/agi_0m_covar2005_spatrast.nc"))
names(agi_0m_rast_2005) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI0", "lat", "j_day")

map_pred_0_2005 = predict(agi_0m_rast_2005, agi_0_tc5_lr01, type = "response", n.trees = agi_0_tc5_lr01$gbm.call$best.trees, na.rm = FALSE) 
map_pred_0_2005 = crop(map_pred_0_2005,extent1)

df_map = as.points(map_pred_0_2005) %>% st_as_sf() %>% as.data.frame()
colnames(df_map) = c("value", "geometry") 

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = map_pred_0_2005) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1) +
  ggtitle("BRT w/ AGI at 0m") +
  labs(fill = "HSI")+
  theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

ggsave(here("figs/bls/agi_0m_hsi05.png"), height = 6, width = 5, units = c("in"))

#### agi 0m 2007####
agi_0m_rast_2007 <- rast(here("data/enviro/CMEMS/hsi_map/agi_0m_covar2007_spatrast.nc"))
names(agi_0m_rast_2007) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI0", "lat", "j_day")

map_pred_0_2007 = predict(agi_0m_rast_2007, agi_0_tc5_lr01, type = "response", n.trees = agi_0_tc5_lr01$gbm.call$best.trees, na.rm = FALSE) 
map_pred_0_2007 = crop(map_pred_0_2007,extent1)

df_map = as.points(map_pred_0_2007) %>% st_as_sf() %>% as.data.frame()
colnames(df_map) = c("value", "geometry") 

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = map_pred) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1) +
  ggtitle("BRT w/ AGI at 0m") +
  labs(fill = "HSI")+
  theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

ggsave(here("figs/bls/agi_0m_hsi07.png"), height = 6, width = 5, units = c("in"))

#### agi 250m 2005 ####
agi_250m_rast_2005 <- rast(here("data/enviro/CMEMS/hsi_map/agi_250m_covar2005_spatrast.nc"))
names(agi_250m_rast_2005) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI250", "lat", "j_day")

map_pred_250_2005 = predict(agi_250m_rast_2005, agi_250_tc5_lr01, type = "response", n.trees = agi_250_tc5_lr01$gbm.call$best.trees, na.rm = FALSE) 
map_pred_250_2005 = crop(map_pred_250_2005,extent1)

df_map = as.points(map_pred_250_2005) %>% st_as_sf() %>% as.data.frame()
colnames(df_map) = c("value", "geometry") 

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = map_pred_250_2005) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1) +
  ggtitle("BRT w/ AGI at 250m") +
  labs(fill = "HSI")+
  theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())

ggsave(here("figs/bls/agi_250m_hsi05.png"), height = 6, width = 5, units = c("in"))

#### agi 250m 2007####
agi_250m_rast_2007 <- rast(here("data/enviro/CMEMS/hsi_map/agi_250m_covar2007_spatrast.nc"))
names(agi_250m_rast_2007) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI250", "lat", "j_day")

map_pred_250_2007 = predict(agi_250m_rast_2007, agi_250_tc5_lr01, type = "response", n.trees = agi_250_tc5_lr01$gbm.call$best.trees, na.rm = FALSE) 
map_pred_250_2007 = crop(map_pred_250_2007,extent1)

df_map = as.points(map_pred) %>% st_as_sf() %>% as.data.frame()
colnames(df_map) = c("value", "geometry") 

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = map_pred) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1) +
  ggtitle("BRT w/ AGI at 250m") +
  labs(fill = "HSI")+
  theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())

ggsave(here("figs/bls/agi_250m_hsi07.png"), height = 6, width = 5, units = c("in"))

#### compare 0 and 250 2005 ####
diff_2005 <- diff(c(map_pred_0_2005, map_pred_250_2005))*100

ggplot() + 
  geom_spatraster(data = diff_2005) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1) +
  ggtitle("Comparing HSI") +
  labs(fill = "% change") +
  theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())

ggsave(here("figs/bls/diff_hsi05.png"), height = 6, width = 5.3, units = c("in"))

diff_2007 <- diff(c(map_pred_0_2007, map_pred_250_2007))*100

ggplot() + 
  geom_spatraster(data = diff_2007) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1) +
  ggtitle("Comparing HSI") +
  labs(fill = "% change") +
  theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())

ggsave(here("figs/bls/diff_hsi07.png"), height = 6, width = 5.3, units = c("in"))

### agi maps ####
AGI2005_250m <- rast(here("data/enviro/CMEMS/hsi_map/agi/AGI2005_250m.nc"))

ggplot() + 
  geom_spatraster(data = AGI2005_250m) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "muted", direction = -1)+
  ggtitle("AGI 2005 250m") +
  labs(fill = "AGI")+
  theme_bls_agi()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

ggsave(here("figs/bls/agi2005.png"), height = 6, width = 5, units = c("in"))

AGI2007_250m <- rast(here("data/enviro/CMEMS/hsi_map/agi/AGI2007_250m.nc"))

ggplot() + 
  geom_spatraster(data = AGI2009_0m) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "muted", direction = -1)+
  ggtitle("AGI 2009 0m") +
  labs(fill = "AGI")+
  theme_bls_agi()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())

ggsave(here("figs/bls/agi2009.png"), height = 6, width = 5, units = c("in"))

### agi crit maps ####
cmem_dat250 <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_AGIwdemand_250m.rds"))

AGIcrit_250 <- quantile(cmem_dat250$AGI_250m, 0.10, na.rm = TRUE)

crit_250map <- clamp(AGI2005_250m, upper=AGIcrit_250, values = FALSE) #create raster of values below AGIcrit
crit_poly <- as.polygons(ext(crit_250map))
crit_poly <- as.polygons(crit_250map > -Inf)

map.world = map_data(map="world")
testt=map.world %>% filter(long<=180)

ggplot() + 
  geom_spatraster(data = AGI2005_250m) + 
  geom_spatvector(data = crit_poly, color = "black", fill = NA, linewidth = 0.8) +
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "muted", direction = -1)+
  ggtitle("AGI 2005 250m") +
  labs(fill = "AGI")+
  theme_bls_agi()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

ggsave(here("figs/bls/agi_crit2005.png"), height = 6, width = 5, units = c("in"))

crit_250map <- clamp(AGI2007_250m, upper=AGIcrit_250, values = FALSE) #create raster of values below AGIcrit
crit_poly <- as.polygons(ext(crit_250map))
crit_poly <- as.polygons(crit_250map > -Inf)

ggplot() + 
  geom_spatraster(data = AGI2007_250m) + 
  geom_spatvector(data = crit_poly, color = "black", fill = NA, linewidth = 0.8) +
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "muted", direction = -1)+
  ggtitle("AGI 2007 250m") +
  labs(fill = "AGI")+
  theme_bls_agi()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())

ggsave(here("figs/bls/agi_crit2007.png"), height = 6, width = 5, units = c("in"))

### sanity AF check ####
AGI2007_250m <- rast(here("data/enviro/CMEMS/hsi_map/agi/AGI2007_250m.nc"))

ggplot() + 
  geom_spatraster(data = AGI2007_250m) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "muted", direction = -1)+
  ggtitle("AGI 2007 250m") +
  labs(fill = "AGI")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("figs/bls/agi250.png"), height = 6, width = 5, units = c("in"))

ggplot() + 
  geom_spatraster(data = AGI2007_0m) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "muted", direction = -1)+
  ggtitle("AGI 2007 0m") +
  labs(fill = "AGI")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("figs/bls/agi0.png"), height = 6, width = 5, units = c("in"))

### raster difference between DO/temp model and AGI/temp model ####
diff_2005 <- diff(c(map_pred_do5, map_pred_agi_temp5))*100

ggplot() + 
  geom_spatraster(data = diff_2005) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1) +
  ggtitle("Comparing BRT HSIs") +
  labs(fill = "% change") +
theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())
ggsave(here("figs/bls/diff_hsi05.png"), height = 6, width = 5.3, units = c("in"))

diff_2007 <- diff(c(map_pred_do7, map_pred_agi_temp7))*100

ggplot() + 
  geom_spatraster(data = diff_2007) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1) +
  ggtitle("Comparing BRT HSIs") +
  labs(fill = "% change") +
  theme_bls_agi() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())

ggsave(here("figs/bls/diff_hsi07.png"), height = 6, width = 5.3, units = c("in"))


### example covar maps ####

ggplot() + 
  geom_spatraster(data = sst0_2004) + 
  geom_map(data=testt,map=testt,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  scale_x_continuous(expand=c(0,0),limits = c(-140,-110)) +
  scale_y_continuous(expand=c(0,0),limits = c(10,50))+
  scale_fill_whitebox_c(palette = "deep", direction = -1)+
  theme_bls_map() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.text.x = element_blank())

ggsave(here("figs/bls/covar1.png"), height = 4, width = 3, units = c("in"))

#libraries
{library(tidyverse)
library(terra)
library(sf)
library(here)
library(distancetocoast)}

### Daily data ####
#load covar data
cmems_covar0 <- readRDS(here("data/locs_w_covar/psat_spot/cmem_locs_covar_AGI_0m.rds"))

cmems_locs <- cmems_covar0 %>%
  subset(select = c(tag, lat, lon))

    #turn df locs into geom points 
cmems_locs <- cmems_locs %>%
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326)


#create West Coast N.Am polygon
# create spatVect for CMEMS domain used to generate gradient
#get the bounding box of the two x & y coordintates, make sfc
ylims <- c(1, 49)
xlims <- c(-153, -103)
box_coords <- tibble(x = xlims, y = ylims) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(4326))

bounding_box <- st_bbox(box_coords) %>% st_as_sfc()

#load distance to coast DF
dist_coast_rast <- rast(here("data/enviro/continents_shp/dist_coast.nc")) #distance in meters
dist_coast <- terra::crop(dist_coast_rast, bounding_box)

cmems_locs$dist_coast <- extract(dist_coast, cmems_locs)

#add as covar column
cmems_covar0$dist_coast <- cmems_locs$dist_coast$layer
cmems_covar0$dist_coast <- as.numeric(cmems_covar0$dist_coast)

ggplot(cmems_covar0, aes(x = dist_coast, fill = as.factor(PA))) + 
  geom_density(alpha = 0.5) + 
  theme_bw()+
  scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))

#saveRDS(cmems_covar0, here("data/locs_w_covar/psat_spot/cmem_locs_covar_AGI_distcoast_0m.rds"))

### seasonal/annual data ####
ann_dat <- readRDS(here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_AGI_0m_ann.rds"))
seas_dat <- readRDS(here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_AGI_0m_seas.rds"))

ann_dat_back <- readRDS(here("data/locs_w_covar/psat_spot/annual/back_locs_covar_AGI_0m_ann.rds"))
seas_dat_back <- readRDS(here("data/locs_w_covar/psat_spot/seasonal/back_locs_covar_AGI_0m_seas.rds"))

dist_shor_func <- function(input_file, covar_path, save_path){
  #format input file 
  input_locs <- input_file %>%
    subset(select = c(tag, lat, lon))
  
  #turn df locs into geom points 
  input_locs_s <- input_locs %>%
    st_as_sf(coords = c("lon", "lat"), 
             crs = 4326)
  
  #create West Coast N.Am polygon
  # create spatVect for CMEMS domain used to generate gradient
  #get the bounding box of the two x & y coordintates, make sfc
  ylims <- c(1, 49)
  xlims <- c(-153, -103)
  box_coords <- tibble(x = xlims, y = ylims) %>% 
    st_as_sf(coords = c("x", "y")) %>% 
    st_set_crs(st_crs(4326))
  
  bounding_box <- st_bbox(box_coords) %>% st_as_sfc()
  
  #load distance to coast DF
  dist_coast_rast <- rast(here("data/enviro/continents_shp/dist_coast.nc")) #distance in meters
  dist_coast <- terra::crop(dist_coast_rast, bounding_box)
  
  input_locs_s$dist_coast <- extract(dist_coast, input_locs_s)
  
  covar_file <- readRDS(here(covar_path))
    
  #add as covar column
  covar_file$dist_coast <- input_locs_s$dist_coast$layer
  covar_file$dist_coast <- as.numeric(covar_file$dist_coast)
  
  ggplot(covar_file, aes(x = dist_coast, fill = as.factor(PA))) + 
    geom_density(alpha = 0.5) + 
    theme_bw()+
    scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))
  
  saveRDS(covar_file, here(save_path))
  
  return(covar_file)
}

### CRW ###
#### Annual ####
covar_path = "data/locs_w_covar/psat_spot/annual/cmem_locs_covar_AGI_0m_ann.rds"
save_path = "data/locs_w_covar/psat_spot/annual/cmem_locs_covar_AGI_distcoast_0m_ann.rds"

crw_ann_distcoast <- dist_shor_func(ann_dat, covar_path = covar_path, save_path = save_path)

#### Seasonal ####
covar_path = "data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_AGI_0m_seas.rds"
save_path = "data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_AGI_distcoast_0m_seas.rds"

crw_seas_distcoast <- dist_shor_func(seas_dat, covar_path = covar_path, save_path = save_path)

### Background ###
#### Annual ####
covar_path = "data/locs_w_covar/psat_spot/annual/back_locs_covar_AGI_0m_ann.rds"
save_path = "data/locs_w_covar/psat_spot/annual/back_locs_covar_AGI_distcoast_0m_ann.rds"

back_ann_distcoast <- dist_shor_func(ann_dat_back, covar_path = covar_path, save_path = save_path)

#### Seasonal ####
covar_path = "data/locs_w_covar/psat_spot/seasonal/back_locs_covar_AGI_0m_seas.rds"
save_path = "data/locs_w_covar/psat_spot/seasonal/back_locs_covar_AGI_distcoast_0m_seas.rds"

back_seas_distcoast <- dist_shor_func(seas_dat_back, covar_path = covar_path, save_path = save_path)







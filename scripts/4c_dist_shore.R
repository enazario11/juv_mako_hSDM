#libraries
library(tidyverse)
library(terra)
library(sf)
library(here)

#load covar data
cmems_covar <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_AGIwdemand_0m.rds"))
cmems_locs <- cmems_covar %>%
  subset(select = c(tag, lat, lon))

    #turn df locs into geom points 
cmems_locs <- cmems_locs %>%
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) 

cmems_locs <- cmems_locs %>%
  st_transform(geometry, crs = st_crs(land_subset))

#create West Coast N.Am polygon
# create spatVect for CMEMS domain used to generate gradient
#get the bounding box of the two x & y coordintates, make sfc
ylims <- c(10, 50)
xlims <- c(-140, -110)
box_coords <- tibble(x = xlims, y = ylims) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(4326))

bounding_box <- st_bbox(box_coords) %>% st_as_sfc()

#reproject to CRS that aligns with SSM output
bb_merc <- st_transform(bounding_box, crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs")

#read in continents polygon
land_vect <- read_sf(here("data/enviro/continents_shp/World_Continents.shp"))

land_merc = land_vect %>%
  st_as_sfc() %>%
  st_transform(crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs")

land_subset <- st_intersection(land_merc, bb_merc)
plot(land_subset)

coast_dist <- st_distance(cmems_locs$geometry, land_subset) #distance in km

#add as covar column
cmems_covar$dist_coast <- coast_dist[,1]
cmems_covar$dist_coast <- as.numeric(cmems_covar$dist_coast)

saveRDS(cmems_covar, here("data/locs_w_covar/cmems/cmems_locs_covar_0m_AGI_dist.rds"))

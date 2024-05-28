#libraries
library(tidyverse)
library(terra)
library(sf)
library(here)
library(distancetocoast)

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

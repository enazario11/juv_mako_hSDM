### libraries ####
library(tidyverse)
library(terra)
library(sf)
library(here)

### data ####
cmem_0m <- rast(here("data/enviro/CMEMS/processed/CMEM_SST_SAL_MLD_SSH_UO_VO_CHL_NPP_DO_0m_Jan2004_Dec2009_0.25_D.nc"))
cmem_250m <- rast(here("data/enviro/CMEMS/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_250m_Jan2004_Dec2009_0.25_D.nc"))
bathy <- rast(here("data/enviro/bathy/processed/gebco_bathy_cmems_domain.nc"))

### randomly select 3 dates to focus on #### 
set.seed(1004)
sample(1:2192, 3) #randomly select 3 dates: 2007-11-19 (day 1419), 2009-02-20 (day 1878), and 2004-02-21 (day 52)

#remove MLD, SSH, UO, VO, NPP and crop to same extent as bathy
surf_temp0 <- str_split(names(cmem_0m), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")
surf_temp250 <- str_split(names(cmem_250m), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")
extent <- ext(-140, -110, 10, 50)

### cmem vars #### nov 11 2005
#separate by all enviro vars at the surface (incldues all depth layers for each variable (1:10))
sst_pos0 <- which(surf_temp0 == "thetao")
sst_only0 <- cmem_0m %>% subset(sst_pos0) 
sst_crop0 <- crop(sst_only0, extent)
sst0_2004 <- subset(sst_crop0, time(sst_crop0) == as.Date("2004-02-21"))
sst0_2007 <- subset(sst_crop0, time(sst_crop0) == as.Date("2007-11-19"))
sst0_2009 <- subset(sst_crop0, time(sst_crop0) == as.Date("2009-02-20"))
sst0_2005 <- subset(sst_crop0, time(sst_crop0) == as.Date("2005-11-19"))

sal_pos0 <- which(surf_temp0 == "so")
sal_only0 <- cmem_0m %>% subset(sal_pos0) 
sal_crop0 <- crop(sal_only0, extent)
sal0_2004 <- subset(sal_crop0, time(sal_crop0) == as.Date("2004-02-21"))
sal0_2007 <- subset(sal_crop0, time(sal_crop0) == as.Date("2007-11-19"))
sal0_2009 <- subset(sal_crop0, time(sal_crop0) == as.Date("2009-02-20"))
sal0_2005 <- subset(sal_crop0, time(sal_crop0) == as.Date("2005-11-19"))

chl_pos0 <- which(surf_temp0 == "chl")
chl_only0 <- cmem_0m %>% subset(chl_pos0) 
chl_crop0 <- crop(chl_only0, extent)
chl0_2004 <- subset(chl_crop0, time(chl_crop0) == as.Date("2004-02-21"))
chl0_2007 <- subset(chl_crop0, time(chl_crop0) == as.Date("2007-11-19"))
chl0_2009 <- subset(chl_crop0, time(chl_crop0) == as.Date("2009-02-20"))
chl0_2005 <- subset(chl_crop0, time(chl_crop0) == as.Date("2005-11-19"))

o2_pos0 <- which(surf_temp0 == "o2")
o2_only0 <- cmem_0m %>% subset(o2_pos0) 
o2_crop0 <- crop(o2_only0, extent)
o20_2004 <- subset(o2_crop0, time(o2_crop0) == as.Date("2004-02-21"))
o20_2007 <- subset(o2_crop0, time(o2_crop0) == as.Date("2007-11-19"))
o20_2009 <- subset(o2_crop0, time(o2_crop0) == as.Date("2009-02-20"))
o20_2005 <- subset(o2_crop0, time(o2_crop0) == as.Date("2005-11-19"))


sst_pos250 <- which(surf_temp250 == "thetao")
sst_only250 <- cmem_250m %>% subset(sst_pos250) 
sst_crop250 <- crop(sst_only250, extent)
sst250_2004 <- subset(sst_crop250, time(sst_crop250) == as.Date("2004-02-21"))
sst250_2007 <- subset(sst_crop250, time(sst_crop250) == as.Date("2007-11-19"))
sst250_2009 <- subset(sst_crop250, time(sst_crop250) == as.Date("2009-02-20"))
sst250_2005 <- subset(sst_crop250, time(sst_crop250) == as.Date("2005-11-19"))

sal_pos250 <- which(surf_temp250 == "so")
sal_only250 <- cmem_250m %>% subset(sal_pos250) 
sal_crop250 <- crop(sal_only250, extent)
sal250_2004 <- subset(sal_crop250, time(sal_crop250) == as.Date("2004-02-21"))
sal250_2007 <- subset(sal_crop250, time(sal_crop250) == as.Date("2007-11-19"))
sal250_2009 <- subset(sal_crop250, time(sal_crop250) == as.Date("2009-02-20"))
sal250_2005 <- subset(sal_crop250, time(sal_crop250) == as.Date("2005-11-19"))

chl_pos250 <- which(surf_temp250 == "chl")
chl_only250 <- cmem_250m %>% subset(chl_pos250) 
chl_crop250 <- crop(chl_only250, extent)
chl250_2004 <- subset(chl_crop250, time(chl_crop250) == as.Date("2004-02-21"))
chl250_2007 <- subset(chl_crop250, time(chl_crop250) == as.Date("2007-11-19"))
chl250_2009 <- subset(chl_crop250, time(chl_crop250) == as.Date("2009-02-20"))
chl250_2005 <- subset(chl_crop250, time(chl_crop250) == as.Date("2005-11-19"))

o2_pos250 <- which(surf_temp250 == "o2")
o2_only250 <- cmem_250m %>% subset(o2_pos250) 
o2_crop250 <- crop(o2_only250, extent)
o2250_2004 <- subset(o2_crop250, time(o2_crop250) == as.Date("2004-02-21"))
o2250_2007 <- subset(o2_crop250, time(o2_crop250) == as.Date("2007-11-19"))
o2250_2009 <- subset(o2_crop250, time(o2_crop250) == as.Date("2009-02-20"))
o2250_2005 <- subset(o2_crop250, time(o2_crop250) == as.Date("2005-11-19"))

### bathy ####
bathy_crop <- crop(bathy, extent)

d2004 <- as.Date("2004-02-21")
bathy2004 <- bathy_crop
time(bathy2004) <- d2004

d2007 <- as.Date("2007-11-19")
bathy2007 <- bathy_crop
time(bathy2007) <- d2007

d2009 <- as.Date("2009-02-20")
bathy2009 <- bathy_crop
time(bathy2009) <- d2009

d2005 <- as.Date("2005-11-19")
bathy2005 <- bathy_crop
time(bathy2005) <- d2005


### distance to coast ####
#create West Coast N.Am polygon
# create spatVect for CMEMS domain used to generate gradient
#get the bounding box of the two x & y coordintates, make sfc
ylims <- c(9, 51)
xlims <- c(-141, -109)
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

land_subset <- st_intersection(land_merc, bb_merc) #sfc
land_vect <- vect(land_subset)

#crop where ROMS domain polygon and continents polygons intersect to get a final polygon of the CMEMS domain
grad_poly <- st_difference(bb_merc, land_subset)

df <- data.frame(id = seq(length(grad_poly)))
df$geometry <- grad_poly
grad_sf <- st_as_sf(df)

grad_spatVect <- vect(grad_poly)

# get raster of study domain
CMEMS_large_rast <- rast(
  crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs",
  extent = ext(-18033.76, -12228.67, -2824.927, 19262.71), 
  resolution =  47.19581
)

x <- rasterize(grad_spatVect, CMEMS_large_rast, fun = "mean") 

dist_rast <- distance(x, land_vect)
dist_rast <- terra::project(dist_rast, "+init=EPSG:4326")
dist_rast <- crop(dist_rast, extent)
ext(dist_rast) <- extent

dist2004 <- dist_rast
time(dist2004) <- d2004

dist2007 <- dist_rast
time(dist2007) <- d2007

dist2009 <- dist_rast
time(dist2009) <- d2009

dist2005 <- dist_rast
time(dist2005) <- d2005

### latitudes per day ####
lat_rast2004 <- init(dist2004, 'y')
time(lat_rast2004) <- d2004

lat_rast2007 <- init(dist2007, 'y')
time(lat_rast2007) <- d2007

lat_rast2009 <- init(dist2009, 'y')
time(lat_rast2009) <- d2009

lat_rast2005 <- init(dist2005, 'y')
time(lat_rast2005) <- d2005

### day of year rast ####
jday_2004 <- rast(
  crs = "EPSG:4326",
  extent = ext(-140, -110, 10, 50), 
  resolution =  0.25
); values(jday_2004) <- 52

jday_2007 <- rast(
  crs = "EPSG:4326",
  extent = ext(-140, -110, 10, 50), 
  resolution =  0.25
); values(jday_2007) <- 315

jday_2009 <- rast(
  crs = "EPSG:4326",
  extent = ext(-140, -110, 10, 50), 
  resolution =  0.25
); values(jday_2009) <- 51

jday_2005 <- rast(
  crs = "EPSG:4326",
  extent = ext(-140, -110, 10, 50), 
  resolution =  0.25
); values(jday_2005) <- 51

### AGI for each day ####
source(here("functions/oxy_demand_functions.R"))
OxyThresh = 0.0843513526
Tpref = 16.4520073

  #0 - 2004
demand2004_0m <- OxyDemand(Tpref = Tpref0, PO2_thresh = OxyThresh0, T_C = sst0_2004)
o2_2004_atm0 <- rast_to_atm(do = o20_2004, so = sal0_2004, temp = sst0_2004, depth = 0)
AGI2004_0m <- o2_2004_atm0/demand2004_0m
#writeCDF(AGI2004_0m, filename = here("data/enviro/CMEMS/hsi_map/agi/AGI2004_0m.nc"))

  #250 - 2004
demand2004_250m <- OxyDemand(Tpref = Tpref250, PO2_thresh = OxyThresh250, T_C = sst250_2004)
o2_2004_atm250 <- rast_to_atm(do = o2250_2004, so = sal250_2004, temp = sst250_2004, depth = 250)
AGI2004_250m <- o2_2004_atm250/demand2004_250m
#writeCDF(AGI2004_250m, filename = here("data/enviro/CMEMS/hsi_map/agi/AGI2004_250m.nc"))

  #0 - 2007
demand2007_0m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh, T_C = sst0_2007)
o2_2007_atm0 <- rast_to_atm(do = o20_2007, so = sal0_2007, temp = sst0_2007, depth = 0)
AGI2007_0m <- o2_2007_atm0/demand2007_0m
#writeCDF(AGI2007_0m, filename = here("data/enviro/CMEMS/hsi_map/agi/AGI2007_0m.nc"))

  #250 - 2007
demand2007_250m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh, T_C = sst250_2007)
o2_2007_atm250 <- rast_to_atm(do = o2250_2007, so = sal250_2007, temp = sst250_2007, depth = 250)
AGI2007_250m <- o2_2007_atm250/demand2007_250m
#writeCDF(AGI2007_250m, filename = here("data/enviro/CMEMS/hsi_map/agi/AGI2007_250m.nc"), overwrite = TRUE)

  #0 - 2009
demand2009_0m <- OxyDemand(Tpref = Tpref0, PO2_thresh = OxyThresh0, T_C = sst0_2009)
o2_2009_atm0 <- rast_to_atm(do = o20_2009, so = sal0_2009, temp = sst0_2009, depth = 0)
AGI2009_0m <- o2_2009_atm0/demand2009_0m
#writeCDF(AGI2009_0m, filename = here("data/enviro/CMEMS/hsi_map/agi/AGI2009_0m.nc"))

  #250 - 2009
demand2009_250m <- OxyDemand(Tpref = Tpref250, PO2_thresh = OxyThresh250, T_C = sst250_2009)
o2_2009_atm250 <- rast_to_atm(do = o2250_2009, so = sal250_2009, temp = sst250_2009, depth = 250)
AGI2009_250m <- o2_2009_atm250/demand2009_250m
#writeCDF(AGI2009_250m, filename = here("data/enviro/CMEMS/hsi_map/agi/AGI2009_250m.nc"))

#0 - 2005
demand2005_0m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh, T_C = sst0_2005)
o2_2005_atm0 <- rast_to_atm(do = o20_2005, so = sal0_2005, temp = sst0_2005, depth = 0)
AGI2005_0m <- o2_2005_atm0/demand2005_0m
#writeCDF(AGI2005_0m, filename = here("data/enviro/CMEMS/hsi_map/agi/AGI2005_0m.nc"))

#250 - 2005
demand2005_250m <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh, T_C = sst250_2005)
o2_2005_atm250 <- rast_to_atm(do = o2250_2005, so = sal250_2005, temp = sst250_2005, depth = 250)
AGI2005_250m <- o2_2005_atm250/demand2005_250m
#writeCDF(AGI2005_250m, filename = here("data/enviro/CMEMS/hsi_map/agi/AGI2005_250m.nc"), overwrite = T)

### spatraster save (easier to use with predict and mapping code) ####

#2004
agi_rast_2004 <- c(sal0_2004, chl0_2004, sal250_2004, chl250_2004, bathy2004, dist2004, AGI2004_0m, AGI2004_250m, lat_rast2004, jday_2004)
names(agi_rast_2004) <- c("so0", "chl0","so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")
writeCDF(agi_rast_2004, filename = here("data/enviro/CMEMS/hsi_map/agi_covar2004_spatrast.nc"))

do_rast_2004 <- c(sst0_2004, sal0_2004, chl0_2004, o20_2004, sst250_2004, sal250_2004, chl250_2004, o2250_2004, bathy2004, dist2004, lat_rast2004, jday_2004)
names(do_rast_2004) <- c("thetao0", "so0", "chl0", "o20", "thetao250", "so250", "chl250", "o2250", "bathy", "dist_coast", "lat", "j_day")
writeCDF(do_rast_2004, filename = here("data/enviro/CMEMS/hsi_map/do_covar2004_spatrast.nc"))

agi_temp_rast_2004 <- c(sst0_2004, sal0_2004, chl0_2004, sst250_2004, sal250_2004, chl250_2004, bathy2004, dist2004, AGI2004_0m, AGI2004_250m, lat_rast2004, jday_2004)
names(agi_temp_rast_2004) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")
writeCDF(agi_temp_rast_2004, filename = here("data/enviro/CMEMS/hsi_map/agi_temp_covar2004_spatrast.nc"))

#2007
agi_rast_2007 <- c(sal0_2007, chl0_2007, sal250_2007, chl250_2007, bathy2007, dist2007, AGI2007_0m, AGI2007_250m, lat_rast2007, jday_2007)
names(agi_rast_2007) <- c("so0", "chl0","so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")
writeCDF(agi_rast_2007, filename = here("data/enviro/CMEMS/hsi_map/agi_covar2007_spatrast.nc"))

do_rast_2007 <- c(sst0_2007, sal0_2007, chl0_2007, o20_2007, sst250_2007, sal250_2007, chl250_2007, o2250_2007, bathy2007, dist2007, lat_rast2007, jday_2007)
names(do_rast_2007) <- c("thetao0", "so0", "chl0", "o20", "thetao250", "so250", "chl250", "o2250", "bathy", "dist_coast", "lat", "j_day")
writeCDF(do_rast_2007, filename = here("data/enviro/CMEMS/hsi_map/do_covar2007_spatrast.nc"))

agi_temp_rast_2007 <- c(sst0_2007, sal0_2007, chl0_2007, sst250_2007, sal250_2007, chl250_2007, bathy2007, dist2007, AGI2007_0m, AGI2007_250m, lat_rast2007, jday_2007)
names(agi_temp_rast_2007) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")
writeCDF(agi_temp_rast_2007, filename = here("data/enviro/CMEMS/hsi_map/agi_temp_covar2007_spatrast.nc"))

agi_all_rast_2007 <- c(sst0_2007, sal0_2007, chl0_2007, sst250_2007, sal250_2007, chl250_2007, bathy2007, dist2007, AGI2007_0m, AGI2007_250m, lat_rast2007, jday_2007)
names(agi_all_rast_2007) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")
writeCDF(agi_all_rast_2007, filename = here("data/enviro/CMEMS/hsi_map/agi_all_covar2007_spatrast.nc"))

agi_0m_rast_2007 <- c(sst0_2007, sal0_2007, chl0_2007, sst250_2007, sal250_2007, chl250_2007, bathy2007, dist2007, AGI2007_0m, lat_rast2007, jday_2007)
names(agi_0m_rast_2007) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI0", "lat", "j_day")
writeCDF(agi_0m_rast_2007, filename = here("data/enviro/CMEMS/hsi_map/agi_0m_covar2007_spatrast.nc"))

agi_250m_rast_2007 <- c(sst0_2007, sal0_2007, chl0_2007, sst250_2007, sal250_2007, chl250_2007, bathy2007, dist2007, AGI2007_250m, lat_rast2007, jday_2007)
names(agi_250m_rast_2007) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI250", "lat", "j_day")
writeCDF(agi_250m_rast_2007, filename = here("data/enviro/CMEMS/hsi_map/agi_250m_covar2007_spatrast.nc"))

#2009
agi_rast_2009 <- c(sal0_2009, chl0_2009, sal250_2009, chl250_2009, bathy2009, dist2009, AGI2009_0m, AGI2009_250m, lat_rast2009, jday_2009)
names(agi_rast_2009) <- c("so0", "chl0","so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")
writeCDF(agi_rast_2009, filename = here("data/enviro/CMEMS/hsi_map/agi_covar2009_spatrast.nc"))

do_rast_2009 <- c(sst0_2009, sal0_2009, chl0_2009, o20_2009, sst250_2009, sal250_2009, chl250_2009, o2250_2009, bathy2009, dist2009, lat_rast2009, jday_2009)
names(do_rast_2009) <- c("thetao0", "so0", "chl0", "o20", "thetao250", "so250", "chl250", "o2250", "bathy", "dist_coast", "lat", "j_day")
writeCDF(do_rast_2009, filename = here("data/enviro/CMEMS/hsi_map/do_covar2009_spatrast.nc"))

agi_temp_rast_2009 <- c(sst0_2009, sal0_2009, chl0_2009, sst250_2009, sal250_2009, chl250_2009, bathy2009, dist2009, AGI2009_0m, AGI2009_250m, lat_rast2009, jday_2009)
names(agi_temp_rast_2009) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")
writeCDF(agi_temp_rast_2009, filename = here("data/enviro/CMEMS/hsi_map/agi_temp_covar2009_spatrast.nc"))

#2005
agi_all_rast_2005 <- c(sst0_2005, sal0_2005, chl0_2005, sst250_2005, sal250_2005, chl250_2005, bathy2005, dist2005, AGI2005_0m, AGI2005_250m, lat_rast2005, jday_2005)
names(agi_all_rast_2005) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI0", "AGI250", "lat", "j_day")
writeCDF(agi_all_rast_2005, filename = here("data/enviro/CMEMS/hsi_map/agi_all_covar2005_spatrast.nc"), overwrite = T)

agi_0m_rast_2005 <- c(sst0_2005, sal0_2005, chl0_2005, sst250_2005, sal250_2005, chl250_2005, bathy2005, dist2005, AGI2005_0m, lat_rast2005, jday_2005)
names(agi_0m_rast_2005) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI0", "lat", "j_day")
writeCDF(agi_0m_rast_2005, filename = here("data/enviro/CMEMS/hsi_map/agi_0m_covar2005_spatrast.nc"), overwrite = T)

agi_250m_rast_2005 <- c(sst0_2005, sal0_2005, chl0_2005, sst250_2005, sal250_2005, chl250_2005, bathy2005, dist2005, AGI2005_250m, lat_rast2005, jday_2005)
names(agi_250m_rast_2005) <- c("thetao0", "so0", "chl0", "thetao250", "so250", "chl250", "bathy", "dist_coast", "AGI250", "lat", "j_day")
writeCDF(agi_250m_rast_2005, filename = here("data/enviro/CMEMS/hsi_map/agi_250m_covar2005_spatrast.nc"), overwrite = T)

do_rast_2005 <- c(sst0_2005, sal0_2005, chl0_2005, o20_2005, sst250_2005, sal250_2005, chl250_2005, o2250_2005, bathy2005, dist2005, lat_rast2005, jday_2005)
names(do_rast_2005) <- c("thetao0", "so0", "chl0", "o20", "thetao250", "so250", "chl250", "o2250", "bathy", "dist_coast", "lat", "j_day")
writeCDF(do_rast_2005, filename = here("data/enviro/CMEMS/hsi_map/do_covar2005_spatrast.nc"))

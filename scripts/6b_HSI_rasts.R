#libraries 
library(tidyverse)
library(terra)
library(sf)
library(here)

#data
cmem_0m <- rast(here("data/enviro/CMEMS/processed/CMEM_SST_SAL_MLD_SSH_UO_VO_CHL_NPP_DO_0m_Jan2004_Dec2009_0.25_D.nc"))
cmem_250m <- rast(here("data/enviro/CMEMS/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_250m_Jan2004_Dec2009_0.25_D.nc"))
bathy <- rast(here("data/enviro/bathy/processed/gebco_bathy_cmems_domain.nc"))

#randomly select 3 dates to focus on 
set.seed(1004)
sample(1:2192, 3) #randomly select 3 dates: 2007-11-19 (day 1419), 2009-02-20 (day 1878), and 2004-02-21 (day 52)

#remove MLD, SSH, UO, VO, NPP and crop to same extent as bathy
surf_temp0 <- str_split(names(cmem_0m), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")
surf_temp250 <- str_split(names(cmem_250m), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")
extent <- ext(-140, -110, 10, 50)

#separate by all enviro vars at the surface (incldues all depth layers for each variable (1:10))
sst_pos0 <- which(surf_temp0 == "thetao")
sst_only0 <- cmem_0m %>% subset(sst_pos0) 
sst_crop0 <- crop(sst_only0, extent)
sst0_2004 <- subset(sst_crop0, time(sst_crop0) == as.Date("2004-02-21"))
sst0_2007 <- subset(sst_crop0, time(sst_crop0) == as.Date("2007-11-19"))
sst0_2009 <- subset(sst_crop0, time(sst_crop0) == as.Date("2009-02-20"))

sal_pos0 <- which(surf_temp0 == "so")
sal_only0 <- cmem_0m %>% subset(sal_pos0) 
sal_crop0 <- crop(sal_only0, extent)
sal0_2004 <- subset(sal_crop0, time(sal_crop0) == as.Date("2004-02-21"))
sal0_2007 <- subset(sal_crop0, time(sal_crop0) == as.Date("2007-11-19"))
sal0_2009 <- subset(sal_crop0, time(sal_crop0) == as.Date("2009-02-20"))

chl_pos0 <- which(surf_temp0 == "chl")
chl_only0 <- cmem_0m %>% subset(chl_pos0) 
chl_crop0 <- crop(chl_only0, extent)
chl0_2004 <- subset(chl_crop0, time(chl_crop0) == as.Date("2004-02-21"))
chl0_2007 <- subset(chl_crop0, time(chl_crop0) == as.Date("2007-11-19"))
chl0_2009 <- subset(chl_crop0, time(chl_crop0) == as.Date("2009-02-20"))

o2_pos0 <- which(surf_temp0 == "o2")
o2_only0 <- cmem_0m %>% subset(o2_pos0) 
o2_crop0 <- crop(o2_only0, extent)
o20_2004 <- subset(o2_crop0, time(o2_crop0) == as.Date("2004-02-21"))
o20_2007 <- subset(o2_crop0, time(o2_crop0) == as.Date("2007-11-19"))
o20_2009 <- subset(o2_crop0, time(o2_crop0) == as.Date("2009-02-20"))


sst_pos250 <- which(surf_temp250 == "thetao")
sst_only250 <- cmem_250m %>% subset(sst_pos250) 
sst_crop250 <- crop(sst_only250, extent)
sst250_2004 <- subset(sst_crop250, time(sst_crop250) == as.Date("2004-02-21"))
sst250_2007 <- subset(sst_crop250, time(sst_crop250) == as.Date("2007-11-19"))
sst250_2009 <- subset(sst_crop250, time(sst_crop250) == as.Date("2009-02-20"))

sal_pos250 <- which(surf_temp250 == "so")
sal_only250 <- cmem_250m %>% subset(sal_pos250) 
sal_crop250 <- crop(sal_only250, extent)
sal250_2004 <- subset(sal_crop250, time(sal_crop250) == as.Date("2004-02-21"))
sal250_2007 <- subset(sal_crop250, time(sal_crop250) == as.Date("2007-11-19"))
sal250_2009 <- subset(sal_crop250, time(sal_crop250) == as.Date("2009-02-20"))

chl_pos250 <- which(surf_temp250 == "chl")
chl_only250 <- cmem_250m %>% subset(chl_pos250) 
chl_crop250 <- crop(chl_only250, extent)
chl250_2004 <- subset(chl_crop250, time(chl_crop250) == as.Date("2004-02-21"))
chl250_2007 <- subset(chl_crop250, time(chl_crop250) == as.Date("2007-11-19"))
chl250_2009 <- subset(chl_crop250, time(chl_crop250) == as.Date("2009-02-20"))

o2_pos250 <- which(surf_temp250 == "o2")
o2_only250 <- cmem_250m %>% subset(o2_pos250) 
o2_crop250 <- crop(o2_only250, extent)
o2250_2004 <- subset(o2_crop250, time(o2_crop250) == as.Date("2004-02-21"))
o2250_2007 <- subset(o2_crop250, time(o2_crop250) == as.Date("2007-11-19"))
o2250_2009 <- subset(o2_crop250, time(o2_crop250) == as.Date("2009-02-20"))

#combine to one raster per date 
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

rast2004 <- sds(sst0_2004, sal0_2004, chl0_2004, o20_2004, sst250_2004, sal250_2004, chl250_2004, o2250_2004, bathy2004)
names(rast2004) <- c("thetao0", "so0", "chl0", "o20", "thetao250", "so250", "chl250", "o2250", "bathy")

rast2007 <- sds(sst0_2007, sal0_2007, chl0_2007, o20_2007, sst250_2007, sal250_2007, chl250_2007, o2250_2007, bathy2007)
names(rast2007) <- c("thetao0", "so0", "chl0", "o20", "thetao250", "so250", "chl250", "o2250", "bathy")

rast2009 <- sds(sst0_2009, sal0_2009, chl0_2009, o20_2009, sst250_2009, sal250_2009, chl250_2009, o2250_2009, bathy2009)
names(rast2009) <- c("thetao0", "so0", "chl0", "o20", "thetao250", "so250", "chl250", "o2250", "bathy")

writeCDF(rast2004, filename = here("data/enviro/CMEMS/hsi_map/all_covar2004.nc"))
writeCDF(rast2007, filename = here("data/enviro/CMEMS/hsi_map/all_covar2007.nc"))
writeCDF(rast2009, filename = here("data/enviro/CMEMS/hsi_map/all_covar2009.nc"))

test <- rast(here("data/enviro/CMEMS/hsi_map/all_covar2004.nc"))



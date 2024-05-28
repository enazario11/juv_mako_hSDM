###load packages####
library(tidyverse)
library(terra)
library(ncdf4)
library(here)
library(stars)
library(stringr)

template_rast <- rast(
  crs = 'EPSG:4326',
  extent = ext(-153, -103, 1, 49), #study domain (+/- 2 deg of min and max lat/lons from observed and PA locs)
  resolution = 0.25 #coarsest spatial resolution
)

###GEBCO Data####
bathy_dat <- rast(here("data/enviro/psat_spot_all/bathy_gebco/gebco_2023_n49.0_s1.0_w-153.0_e-103.0.nc"))

bathy_mod <- resample(bathy_dat, template_rast)
names(bathy_mod) <- c("bathy")
#writeCDF(bathy_mod, here("data/enviro/psat_spot_all/all_processed/gebco_bathy_0.25deg2.nc"))

###CMEM Data####
##### 0m #####
do_0 <- rast(here("data/enviro/psat_spot_all/biogeo_cmem/0m/CMEMS_DO_0m_JAN2003_Dec2015_0.25deg_D.nc"))
do_0h <- do_0 %>% resample(template_rast)

chl_0 <- rast(here("data/enviro/psat_spot_all/biogeo_cmem/0m/CMEMS_CHL_0m_JAN2003_Dec2015_0.25deg_D.nc"))
chl_0h <- chl_0 %>% resample(template_rast)

##### 60m #####
do_60 <- rast(here("data/enviro/psat_spot_all/biogeo_cmem/60m/CMEMS_DO_60m_JAN2003_Dec2015_0.25deg_D.nc"))
do_60h <- do_60 %>% resample(template_rast)

##### 250m #####
do_250 <- rast(here("data/enviro/psat_spot_all/biogeo_cmem/250m/CMEMS_DO_250m_JAN2003_Dec2015_0.25deg_D.nc"))
do_250h <- do_250 %>% resample(template_rast)

###Merc Data####
#### warp from curvilinear grid to regular grid ####
stars_to_terra <- function(nc_files, nc_var_name, depth){
  
  stars_to_terra_all <- NULL
  
  for(i in 1:length(nc_files)){
    # As stars object
    merc_stars <- read_stars(nc_files[i]) %>% 
      st_set_dimensions("time_counter", as.POSIXct(st_get_dimension_values(., "time_counter"))) %>% 
      setNames(nc_var_name)
    
    #template
    dest <- st_bbox(merc_stars) %>% 
      st_as_stars(dx = 0.25, dy = 0.25, crs = "EPSG:4326")
    
    #seq along each time step and warp each curv time layer to reg (aka template aka dest)
    #stupid st_warp doesn't work with time...maybe later check github comment
    merc_list <- lapply(seq_along(st_get_dimension_values(merc_stars, "time_counter")),
                        \(i) st_warp(merc_stars[1, , , 1, i], dest))
    
    merc_reg <- do.call(c, append(merc_list, list(along = "time_counter")))[drop = TRUE]
    
    #save as a .nc file 
    dat_year <- substr(as.character(st_get_dimension_values(merc_reg, which = "time_counter")[1]), 1, 4)
    dat_mo <- substr(as.character(st_get_dimension_values(merc_reg, which = "time_counter")[1]), 6, 7)
    print(paste0(dat_year, "_", dat_mo))
    write_mdim(merc_reg, here(paste0("data/enviro/psat_spot_all/phys_merc/stars_to_terra_temp_files/", nc_var_name, "_", dat_year, "_", dat_mo, "_", depth, "m", ".nc")))
    
    #read with terra 
    temp_terra <- rast(here(paste0("data/enviro/psat_spot_all/phys_merc/stars_to_terra_temp_files/", nc_var_name, "_", dat_year, "_", dat_mo, "_", depth, "m", ".nc")))
    
    #assign time 
    d1a <- seq(as.Date(paste0(dat_year,"-","01-01")), by = "day",length = 59)
    d1b <- seq(as.Date(paste0(dat_year,"-","03-01")), by = "day",length = 122)
    d1 <- c(d1a, d1b)
    d1_L <- seq(as.Date(paste0(dat_year,"-","01-01")), by = "day",length = 182) #leap year number of days
    
    d2 <- seq(as.Date(paste0(dat_year,"-","07-01")), by = "day",length = 184)
    
    #add year-month info to raster files
    if(str_detect(dat_mo, "01") == TRUE & str_detect(dat_year, "2004|2008|2012") == TRUE){
      time(temp_terra) <- d1_L
    } else if(str_detect(dat_mo, "01") == TRUE & str_detect(dat_year, "2004|2008|2012") == FALSE) {
      time(temp_terra) <- d1
    } else if(str_detect(dat_mo, "07") == TRUE) {
      time(temp_terra) <- d2
    }
    
    #append with terra
    stars_to_terra_all <- append(stars_to_terra_all, temp_terra)
  }
  
  return(stars_to_terra_all)
}

stars_to_terra_no_depth <- function(nc_files, nc_var_name, depth){
  
  stars_to_terra_all <- NULL
  
  for(i in 1:length(nc_files)){
    # As stars object
    merc_stars <- read_stars(nc_files[i]) %>% 
      st_set_dimensions("time_counter", as.POSIXct(st_get_dimension_values(., "time_counter"))) %>% 
      setNames(nc_var_name)
    
    #template
    dest <- st_bbox(merc_stars) %>% 
      st_as_stars(dx = 0.25, dy = 0.25, crs = "EPSG:4326")
    
    #seq along each time step and warp each curv time layer to reg (aka template aka dest)
    #stupid st_warp doesn't work with time...maybe later check github comment
    merc_list <- lapply(seq_along(st_get_dimension_values(merc_stars, "time_counter")),
                        \(i) st_warp(merc_stars[1, , , i], dest))
    
    merc_reg <- do.call(c, append(merc_list, list(along = "time_counter")))[drop = TRUE]
    
    #save as a .nc file 
    dat_year <- substr(as.character(st_get_dimension_values(merc_reg, which = "time_counter")[1]), 1, 4)
    dat_mo <- substr(as.character(st_get_dimension_values(merc_reg, which = "time_counter")[1]), 6, 7)
    print(paste0(dat_year, "_", dat_mo))
    write_mdim(merc_reg, here(paste0("data/enviro/psat_spot_all/phys_merc/stars_to_terra_temp_files/", nc_var_name, "_", dat_year, "_", dat_mo, "_", depth, "m", ".nc")))
    
    Sys.sleep(0.1)
    
    #read with terra 
    temp_terra <- rast(here(paste0("data/enviro/psat_spot_all/phys_merc/stars_to_terra_temp_files/", nc_var_name, "_", dat_year, "_", dat_mo, "_", depth, "m", ".nc")))
    
    #assign time 
    d1a <- seq(as.Date(paste0(dat_year,"-","01-01")), by = "day",length = 59)
    d1b <- seq(as.Date(paste0(dat_year,"-","03-01")), by = "day",length = 122)
    d1 <- c(d1a, d1b)
    d1_L <- seq(as.Date(paste0(dat_year,"-","01-01")), by = "day",length = 182) #leap year number of days
    
    d2 <- seq(as.Date(paste0(dat_year,"-","07-01")), by = "day",length = 184)
    
    #add year-month info to raster files
    if(str_detect(dat_mo, "01") == TRUE & str_detect(dat_year, "2004|2008|2012") == TRUE){
      time(temp_terra) <- d1_L
    } else if(str_detect(dat_mo, "01") == TRUE & str_detect(dat_year, "2004|2008|2012") == FALSE) {
      time(temp_terra) <- d1
    } else if(str_detect(dat_mo, "07") == TRUE) {
      time(temp_terra) <- d2
    }
    
    #append with terra
    stars_to_terra_all <- append(stars_to_terra_all, temp_terra)
  }
  
  return(stars_to_terra_all)
}

##### 0m #####
#temperature
temp0_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/0m/temp"), full.names = TRUE)
temp0_reg <- stars_to_terra(temp0_curv, nc_var_name = "votemper", depth = 0)
#saveRDS(temp0_reg, here("data/enviro/psat_spot_all/phys_merc/0m/processed/temp0_terra.rds"))
temp0_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/0m/processed/temp0_terra.rds"))

temp_0h <- temp0_terra %>% resample(template_rast)

#salinity
sal0_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/0m/so"), full.names = TRUE)
sal0_reg <- stars_to_terra(sal0_curv, nc_var_name = "vosaline", depth = 0)
#saveRDS(sal0_reg, here("data/enviro/psat_spot_all/phys_merc/0m/processed/sal0_terra.rds"))
sal0_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/0m/processed/sal0_terra.rds"))

sal_0h <- sal0_terra %>% resample(template_rast)

#UO (downward x sea water velocity(vozocrtx))
uo0_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/0m/uo"), full.names = TRUE)
uo0_reg <- stars_to_terra(uo0_curv, nc_var_name = "vozocrtx", depth = 0)
#saveRDS(uo0_reg, here("data/enviro/psat_spot_all/phys_merc/0m/processed/uo0_terra.rds"))
uo0_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/0m/processed/uo0_terra.rds"))

uo_0h <- uo0_terra %>% resample(template_rast)

#UO stress (downward x sea water stress (sozotaux))
uostr0_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/0m/uo_stress"), full.names = TRUE)
uostr0_reg <- stars_to_terra_no_depth(uostr0_curv, nc_var_name = "sozotaux", depth = 0)
#saveRDS(uostr0_reg, here("data/enviro/psat_spot_all/phys_merc/0m/processed/uostr0_terra.rds"))
uostr0_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/0m/processed/uostr0_terra.rds"))

uostr_0h <- uostr0_terra %>% resample(template_rast)

#VO (downward y sea water velocity (vomecrty))
vo0_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/0m/vo"), full.names = TRUE)
vo0_reg <- stars_to_terra(vo0_curv, nc_var_name = "vomecrty", depth = 0)
#saveRDS(vo0_reg, here("data/enviro/psat_spot_all/phys_merc/0m/processed/vo0_terra.rds"))
vo0_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/0m/processed/vo0_terra.rds"))

vo_0h <- vo0_terra %>% resample(template_rast)

#VO (downward x sea water stress (sometauy))
vostr0_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/0m/vo_stress"), full.names = TRUE)
vostr0_reg <- stars_to_terra_no_depth(vostr0_curv, nc_var_name = "sometauy", depth = 0)
#saveRDS(vostr0_reg, here("data/enviro/psat_spot_all/phys_merc/0m/processed/vostr0_terra.rds"))
vostr0_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/0m/processed/vostr0_terra.rds"))

vostr_0h <- vostr0_terra %>% resample(template_rast)

#SSH (height above geoid)
ssh0_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/0m/ssh"), full.names = TRUE)
ssh0_reg <- stars_to_terra_no_depth(ssh0_curv, nc_var_name = "sossheig", depth = 0)
#saveRDS(ssh0_reg, here("data/enviro/psat_spot_all/phys_merc/0m/processed/ssh0_terra.rds"))
ssh0_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/0m/processed/ssh0_terra.rds"))

ssh_0h <- ssh0_terra %>% resample(template_rast)

#MLD (ocean mixed layer thickness defined by turbocline)
mld0_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/0m/mld"), full.names = TRUE)
mld0_reg <- stars_to_terra_no_depth(mld0_curv, nc_var_name = "somxlavt", depth = 0)
#saveRDS(mld0_reg, here("data/enviro/psat_spot_all/phys_merc/0m/processed/mld0_terra.rds"))
mld0_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/0m/processed/mld0_terra.rds"))

mld_0h <- mld0_terra %>% resample(template_rast)

##### 60m #####
#temperature
temp60_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/60m/temp"), full.names = TRUE)
temp60_reg <- stars_to_terra(temp60_curv, nc_var_name = "votemper", depth = 60)
#saveRDS(temp60_reg, here("data/enviro/psat_spot_all/phys_merc/60m/processed/temp60_terra.rds"))
temp60_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/60m/processed/temp60_terra.rds"))

temp_60h <- temp60_terra %>% resample(template_rast)

#salinity
sal60_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/60m/so"), full.names = TRUE)
sal60_reg <- stars_to_terra(sal60_curv, nc_var_name = "vosaline", depth = 60)
#saveRDS(sal60_reg, here("data/enviro/psat_spot_all/phys_merc/60m/processed/sal60_terra.rds"))
sal60_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/60m/processed/sal60_terra.rds"))

sal_60h <- sal60_terra %>% resample(template_rast)

#UO (downward x sea water stress (sozotaux) and velocity(vozocrtx))
# uo60_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/60m/uo"), full.names = TRUE)
# uo60_reg <- stars_to_terra(uo60_curv, nc_var_name = "vozocrtx", depth = 60)
# #saveRDS(uo60_reg, here("data/enviro/psat_spot_all/phys_merc/60m/processed/uo60_terra.rds"))
# uo60_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/60m/processed/uo60_terra.rds"))
# 
# uo_0h <- uo0_terra %>% resample(template_rast)

##### 250m #####
#temperature
temp250_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/250m/temp"), full.names = TRUE)
temp250_reg <- stars_to_terra(temp250_curv, nc_var_name = "votemper", depth = 250)
#saveRDS(temp250_reg, here("data/enviro/psat_spot_all/phys_merc/250m/processed/temp250_terra.rds"))
temp250_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/250m/processed/temp250_terra.rds"))

temp_250h <- temp250_terra %>% resample(template_rast)

#salinity
sal250_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/250m/so"), full.names = TRUE)
sal250_reg <- stars_to_terra(sal250_curv, nc_var_name = "vosaline", depth = 250)
#saveRDS(sal250_reg, here("data/enviro/psat_spot_all/phys_merc/250m/processed/sal250_terra.rds"))
sal250_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/250m/processed/sal250_terra.rds"))

sal_250h <- sal250_terra %>% resample(template_rast)

#UO (downward x sea water stress (sozotaux) and velocity(vozocrtx))
# uo250_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/250m/uo"), full.names = TRUE)
# uo250_reg <- stars_to_terra(uo250_curv, nc_var_name = "vozocrtx", depth = 250)
# #saveRDS(uo250_reg, here("data/enviro/psat_spot_all/phys_merc/250m/processed/uo250_terra.rds"))
# uo250_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/250m/processed/uo250_terra.rds"))
# 
# uo_250h <- uo250_terra %>% resample(template_rast)

### Merge MERC and CMEM data ####
##### 0m ####
all_0h <- sds(do_0h, chl_0h, temp_0h, sal_0h, uo_0h, uostr_0h, vo_0h, vostr_0h, ssh_0h, mld_0h)
names(all_0h) <- c("o2", "chl", "votemper", "vosaline", "vozocrtx", "sozotaux", "vomecrty", "sometauy", "sossheig", "somxlavt")
longnames(all_0h) <- c("dissolved oxygen", "chlorophyll", "sea water temperature", "salinity", "eastward velocity", "eastward wind stress", "northward velocity", "northward wind stress", "sea surface height", "mixed layer depth")
units(all_0h) <- c("mmol/m^3", "mg/m^3", "C", "PSU", "m/s", "Pa", "m/s", "Pa", "m", "m")
all_0h

#writeCDF(all_0h, filename = here("data/enviro/psat_spot_all/all_processed/CMEM_DO_CHL_Temp_SO_UO_UOSTR_VO_VOSTR_SSH_MLD_0m_Jan2003_Dec2015_0.25_D.nc"))
test <- rast(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_CHL_Temp_SO_UO_UOSTR_VO_VOSTR_SSH_MLD_0m_Jan2003_Dec2015_0.25_D.nc"))

##### 60m ####
all_60h <- sds(do_60h, temp_60h, sal_60h)
names(all_60h) <- c("o2", "votemper", "vosaline")
longnames(all_60h) <- c("dissolved oxygen", "sea water temperature", "salinity")
units(all_60h) <- c("mmol/m^3", "C", "PSU")
all_60h

#writeCDF(all_60h, filename = here("data/enviro/psat_spot_all/all_processed/CMEM_DO_Temp_SO_60m_Jan2003_Dec2015_0.25_D.nc"))
test <- rast(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_Temp_SO_60m_Jan2003_Dec2015_0.25_D.nc"))

##### 250m ####
all_250h <- sds(do_250h, temp_250h, sal_250h)
names(all_250h) <- c("o2", "votemper", "vosaline")
longnames(all_250h) <- c("dissolved oxygen", "sea water temperature", "salinity")
units(all_250h) <- c("mmol/m^3", "C", "PSU")
all_250h

#writeCDF(all_250h, filename = here("data/enviro/psat_Spot_all/all_processed/CMEM_DO_Temp_SO_250m_Jan2003_Dec2015_0.25_D.nc"))
test <- rast(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_Temp_SO_250m_Jan2003_Dec2015_0.25_D.nc"))



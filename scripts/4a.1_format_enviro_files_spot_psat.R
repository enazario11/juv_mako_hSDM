
###load packages####
library(tidyverse)
library(terra)
library(ncdf4)
library(here)
library(stars)
library(stringr)

template_rast <- rast(
  crs = 'EPSG:4326',
  extent = ext(-153, -103, -1, 49), #study domain (+/- 2 deg of min and max lat/lons from observed and PA locs)
  resolution = 0.25 #coarsest spatial resolution
)

###GEBCO Data####
bathy_dat <- rast(here("data/enviro/psat_spot_all/bathy_gebco/gebco_2023_n49.0_s1.0_w-153.0_e-103.0.nc"))

bathy_mod <- resample(bathy_dat, template_rast)
#writeCDF(bathy_mod, here("data/enviro/psat_spot_all/bathy_gebco/processed/gebco_bathy_0.25deg.nc"))

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

##### 0m #####
temp0_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/0m/temp"), full.names = TRUE)
temp0_reg <- stars_to_terra(temp0_curv, nc_var_name = "votemper", depth = 0)
#saveRDS(temp0_reg, here("data/enviro/psat_spot_all/phys_merc/0m/processed/temp0_terra.rds"))
temp0_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/0m/processed/temp0_terra.rds"))

temp_0h <- temp0_terra %>% resample(template_rast)

##### 60m #####
temp60_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/60m/temp"), full.names = TRUE)
temp60_reg <- stars_to_terra(temp60_curv, nc_var_name = "votemper", depth = 60)
#saveRDS(temp60_reg, here("data/enviro/psat_spot_all/phys_merc/60m/processed/temp60_terra.rds"))
temp60_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/60m/processed/temp60_terra.rds"))

temp_60h <- temp60_terra %>% resample(template_rast)

##### 250m #####
temp250_curv <- list.files(here("data/enviro/psat_spot_all/phys_merc/250m/temp"), full.names = TRUE)
temp250_reg <- stars_to_terra(temp250_curv, nc_var_name = "votemper", depth = 250)
#saveRDS(temp250_reg, here("data/enviro/psat_spot_all/phys_merc/250m/processed/temp250_terra.rds"))
temp250_terra <- readRDS(here("data/enviro/psat_spot_all/phys_merc/250m/processed/temp250_terra.rds"))

temp_250h <- temp250_terra %>% resample(template_rast)



### Merge MERC and CMEM data ####
##### 0m ####
all_0h <- sds(do_0h, chl_0h, temp_0h)
names(all_0h) <- c("o2", "chl", "votemper")
longnames(all_0h) <- c("dissolved oxygen", "chlorophyll", "Sea water tempearture")
units(all_0h) <- c("mmol/m^3", "mg/m^3", "C")
all_0h

#writeCDF(all_0h, filename = here("data/enviro/processed/CMEM_DO_Temp_SO_0m_Jan2003_Dec2015_0.25_D.nc"))
test <- rast(here("data/enviro/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_250m_Jan2004_Dec2009_0.25_D.nc"))

##### 60m ####
all_60h <- sds(do_60h, temp_60h, so_60h)
names(all_60h) <- c("o2", "thetao", "so")
longnames(all_60h) <- c("dissolved oxygen", "Sea water tempearture", "salinity")
units(all_60h) <- c("mmol/m^3", "C", "psu")
all_60h

#writeCDF(all_60h, filename = here("data/enviro/processed/CMEM_DO_Temp_SO_60m_Jan2003_Dec2015_0.25_D.nc"))
test <- rast(here("data/enviro/processed/CMEM_DO_Temp_SO_60m_Jan2003_Dec2015_0.25_D.nc"))

##### 250m ####
all_250h <- sds(do_250h, temp_250h, so_250h)
names(all_250h) <- c("o2", "thetao", "so")
longnames(all_250h) <- c("dissolved oxygen", "Sea water tempearture", "salinity")
units(all_250h) <- c("mmol/m^3", "C", "psu")
all_250h

#writeCDF(all_250h, filename = here("data/enviro/processed/CMEM_DO_Temp_SO_250m_Jan2003_Dec2015_0.25_D.nc"))
test <- rast(here("data/enviro/processed/CMEM_DO_Temp_SO_250m_Jan2003_Dec2015_0.25_D.nc"))



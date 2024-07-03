### libraries ####
{ library(tidyverse)
  library(terra)
  library(here)
}

### load raster data ####
dat0_phys <- list.files(here("data/enviro/psat_spot_all/phys_merc/0m/processed"), full.names = TRUE)
dat60_phys <- list.files(here("data/enviro/psat_spot_all/phys_merc/60m/processed"), full.names = TRUE)
dat250_phys <- list.files(here("data/enviro/psat_spot_all/phys_merc/250m/processed"), full.names = TRUE)

test <- rast(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_CHL_Temp_SO_UO_UOSTR_VO_VOSTR_SSH_MLD_0m_Jan2003_Dec2015_0.25_D.nc"))

dat0_bgc <- list.files(here("data/enviro/psat_spot_all/biogeo_cmem/0m"), full.names = TRUE)
dat60_bgc <- list.files(here("data/enviro/psat_spot_all/biogeo_cmem/60m"), full.names = TRUE)
dat250_bgc <- list.files(here("data/enviro/psat_spot_all/biogeo_cmem/250m"), full.names = TRUE)

template_rast <- rast(
  crs = 'EPSG:4326',
  extent = ext(-153, -103, 1, 49), #study domain (+/- 2 deg of min and max lat/lons from observed and PA locs)
  resolution = 0.25 #coarsest spatial resolution
)

### average by year ####
yr_avg_merge <- function(Phys_Input, BGC_Input, template_rast = template_rast, out_rast = NULL, all_names, longnames_input, units_input){
  for(i in 1:length(Phys_Input)){
    phys_rast <- rast(Phys_Input[i])
    phys_name <- names(phys_rast)[1]
    
    phys_rast_h <- resample(phys_rast, template_rast)
    phys_yr <- tapp(phys_rast_h, "years", mean)
    time(phys_yr, tstep = "years") <- 2003 + 0:12
      
    assign(phys_name, phys_yr)
    all_names <- c(all_names, phys_name)
    
  }
  
  for(i in 1:length(BGC_Input)){
    bgc_rast <- rast(BGC_Input[i])
    bgc_name <- names(bgc_rast)[1]
    
    bgc_rast_h <- resample(bgc_rast, template_rast)
    bgc_yr <- tapp(bgc_rast_h, "years", mean)
    time(phys_yr, tstep = "years") <- 2003 + 0:12
    
    assign(bgc_name, bgc_yr)
    all_names <- c(all_names, bgc_name)
  }
  
  out_rast <- sds(mget(all_names))
  names(out_rast) <- all_names
  longnames(out_rast) <- longnames_input
  units(out_rast) <- units_input
  
  return(out_rast)
}

#### 0m ####
all_names = NULL
dat0_yr <- yr_avg_merge(Phys_Input = dat0_phys, BGC_Input = dat0_bgc,
                        template_rast = template_rast,
                        all_names = NULL,
                        longnames_input = c("mixed layer depth", "salinity", "sea surface height", "sea water temperature", "eastward velocity", "eastward wind stress", "northward velocity", "northward wind stress", "dissolved oxygen", "chlorophyll"), 
                        units_input = c("m", "PSU", "m", "C", "m/s", "Pa", "m/s", "Pa", "m", "m", "mmol/m^3", "mg/m^3"))

#writeCDF(dat0_yr, here("data/enviro/psat_spot_all/all_processed/annual_res/dat_0m_annual.nc"))

#### 60m ####
all_names = NULL
dat60_yr <- yr_avg_merge(Phys_Input = dat60_phys, BGC_Input = dat60_bgc[2],
                        template_rast = template_rast,
                        all_names = NULL,
                        longnames_input = c("salinity", "sea water temperature", "dissolved oxygen"), 
                        units_input = c("PSU", "C", "mmol/m^3"))

#writeCDF(dat60_yr, here("data/enviro/psat_spot_all/all_processed/annual_res/dat_60m_annual.nc"))

#### 250m #####
all_names = NULL
dat250_yr <- yr_avg_merge(Phys_Input = dat250_phys, BGC_Input = dat250_bgc[2],
                        template_rast = template_rast,
                        all_names = NULL,
                        longnames_input = c("salinity", "sea water temperature", "dissolved oxygen"), 
                        units_input = c("PSU", "C", "mmol/m^3"))

#writeCDF(dat250_yr, here("data/enviro/psat_spot_all/all_processed/annual_res/dat_250m_annual.nc"))

### average by season (static) ####
seas_avg_merge <- function(Phys_Input, BGC_Input, template_rast = template_rast, out_rast = NULL, all_names, longnames_input, units_input){
  for(i in 1:length(Phys_Input)){
    phys_rast <- rast(Phys_Input[i])
    phys_name <- names(phys_rast)[1]
    
    phys_rast_h <- resample(phys_rast, template_rast)
    phys_mo <- tapp(phys_rast_h, "months", mean)
    
    phys_wn <- mean(phys_mo[[c(12, 1, 2)]], na.rm = TRUE)
    phys_sp <- mean(phys_mo[[c(3, 4, 5)]], na.rm = TRUE)
    phys_su <- mean(phys_mo[[c(6, 7, 8)]], na.rm = TRUE)
    phys_fa <- mean(phys_mo[[c(9, 10, 11)]], na.rm = TRUE)
    
    phys_seas <- c(phys_wn, phys_sp, phys_su, phys_fa)
    
    time(phys_seas, tstep = "months") <- c(12, 3, 6, 9)
    
    assign(phys_name, phys_seas)
    all_names <- c(all_names, phys_name)
    
  }
  
  for(i in 1:length(BGC_Input)){
    bgc_rast <- rast(BGC_Input[i])
    bgc_name <- names(bgc_rast)[1]
    
    bgc_rast_h <- resample(bgc_rast, template_rast)
    bgc_mo <- tapp(bgc_rast_h, "months", mean)
    
    bgc_wn <- mean(bgc_mo[[c(12, 1, 2)]], na.rm = TRUE)
    bgc_sp <- mean(bgc_mo[[c(3, 4, 5)]], na.rm = TRUE)
    bgc_su <- mean(bgc_mo[[c(6, 7, 8)]], na.rm = TRUE)
    bgc_fa <- mean(bgc_mo[[c(9, 10, 11)]], na.rm = TRUE)
    
    bgc_seas <- c(bgc_wn, bgc_sp, bgc_su, bgc_fa)
    
    time(bgc_seas, tstep = "months") <- c(12, 3, 6, 9)
    
    assign(bgc_name, bgc_seas)
    all_names <- c(all_names, bgc_name)
  }
  
  out_rast <- sds(mget(all_names))
  names(out_rast) <- all_names
  longnames(out_rast) <- longnames_input
  units(out_rast) <- units_input
  
  return(out_rast)
}

#### 0m ####
all_names = NULL
dat0_seas <- seas_avg_merge(Phys_Input = dat0_phys, BGC_Input = dat0_bgc,
                        template_rast = template_rast,
                        all_names = NULL,
                        longnames_input = c("mixed layer depth", "salinity", "sea surface height", "sea water temperature", "eastward velocity", "eastward wind stress", "northward velocity", "northward wind stress", "dissolved oxygen", "chlorophyll"), 
                        units_input = c("m", "PSU", "m", "C", "m/s", "Pa", "m/s", "Pa", "m", "m", "mmol/m^3", "mg/m^3"))

#writeCDF(dat0_seas, here("data/enviro/psat_spot_all/all_processed/season_res/dat_0m_season.nc"))

#### 60m ####
all_names = NULL
dat60_seas <- seas_avg_merge(Phys_Input = dat60_phys, BGC_Input = dat60_bgc[2],
                            template_rast = template_rast,
                            all_names = NULL,
                            longnames_input = c("salinity", "sea water temperature", "dissolved oxygen"), 
                            units_input = c("PSU", "C", "mmol/m^3"))

#writeCDF(dat60_seas, here("data/enviro/psat_spot_all/all_processed/season_res/dat_60m_season.nc"))

#### 250m ####
all_names = NULL
dat250_seas <- seas_avg_merge(Phys_Input = dat250_phys, BGC_Input = dat250_bgc[2],
                             template_rast = template_rast,
                             all_names = NULL,
                             longnames_input = c("salinity", "sea water temperature", "dissolved oxygen"), 
                             units_input = c("PSU", "C", "mmol/m^3"))

#writeCDF(dat250_seas, here("data/enviro/psat_spot_all/all_processed/season_res/dat_250m_season.nc"))


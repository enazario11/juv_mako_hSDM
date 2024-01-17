#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####load packages####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(terra)
library(ncdf4)
library(here)

#create template raster to resample to -- all on same grid and resolution
template_rast <- rast(
    crs = 'EPSG:4326',
    extent = ext(-153, -100, -3, 55), #cmems domain
    resolution = 0.25
  )

#### Surface vars ####
# Look at UO and VO info to see how to separate, resample, and save as diff files. Then, use writeCDF to combine (see details at bottom of help page, using sds to combine rast files)
surf_vars <- rast(here("data/enviro/CMEMS/final/phys/CMEMS_SST_SAL_MLD_SSH_UO_VO_0mto10m_Jan2004_Dec2009_0.083_D.nc"))

#surface vars that vary with depth: SST, SO, VO, UO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#x[1] pulls the first (left) item from the separated string produced by str_split(), if want right, change to a 2

surf_dep <-str_split(names(surf_vars), "=") %>% map_chr(\(x) x[2]) %>% str_extract("[^_]+") #splits the depth var and  only keeps the 0.49 depth layer for all variables
surf <- which(surf_dep == "0.49402499")

surf_fin <- surf_vars %>% subset(surf) #this new raster only has the top most depth layer

#filter to separate variables by the "=" and prep to be able to filter by variable in next step
surf_temp <- str_split(names(surf_fin), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")

#separate by all enviro vars at the surface (incldues all depth layers for each variable (1:10))
sst_pos <- which(surf_temp == "thetao")
sal_pos <- which(surf_temp == "so")
uo_pos <- which(surf_temp == "uo")
vo_pos <- which(surf_temp == "vo")

#subset the single depth layer by the specific variable
sst_only <- surf_fin %>% subset(sst_pos) 
sal_only <- surf_fin %>% subset(sal_pos)
uo_only <- surf_fin %>% subset(uo_pos) 
vo_only <- surf_fin %>% subset(vo_pos)

#resample by the template raster
sst_0h <- resample(sst_only, template_rast) 
#writeCDF(sst_0h, here("data/enviro/CMEMS/extract_format/0m/cmem_sst_0m.nc"))
sal_0h <- resample(sal_only, template_rast)
uo_0h <- resample(uo_only, template_rast)
vo_0h <- resample(vo_only, template_rast)

#surface vars that DO NOT vary with depth: MLD, SSH ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
surf_surf <- str_split(names(surf_vars), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")

#separate by the enviro var
mld_pos <- which(surf_surf == "mlotst")
ssh_pos <- which(surf_surf == "zos")

#subset the single depth layer (all surface variables and depths) by the specific variable. We use the full dataset becuase MLD and SSH don't have a depth component
mld_only <- surf_vars %>% subset(mld_pos)   
ssh_only <- surf_vars %>% subset(ssh_pos)

#resample by the template raster 
mld_0h <- resample(mld_only, template_rast) 
ssh_0h <- resample(ssh_only, template_rast)

# Add biological variables (CHL, NPP, and DO)
chl_npp_0 <- rast(here("data/enviro/final/biol/CMEMS_CHL_NPP_0m_Jan2004_Dec2009_0.25_D.nc"))
do_0 <- rast(here("data/enviro/final/biol/CMEMS_DO_0m_Jan2004_Dec2009_0.25_D.nc"))

split_biol <-str_split(names(chl_npp_0), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")

chl_pos <- which(split_biol == "chl")
npp_pos <- which(split_biol == "nppv")

chl_only <- chl_npp_0 %>% subset(chl_pos) 
npp_only <- chl_npp_0 %>% subset(npp_pos)

chl_0h <- resample(chl_only, template_rast) 
npp_0h <- resample(npp_only, template_rast)
do_0h <- resample(do_0, template_rast)

#combine all harmonized surface vars ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_0h <- sds(sst_0h, sal_0h, mld_0h, ssh_0h, uo_0h, vo_0h, chl_0h, npp_0h, do_0h)
names(all_0h) <- c("thetao", "so", "mlotst", "zos", "uo", "vo", "chl", "nppv", "o2")
longnames(all_0h) <- c("sea water potential temperature", "salinity", "ocean mixed layer depth", "sea surface height", "eastward sea water velocity", "northward sea water velocity", "chlorophyll", "net primary productivity", "dissolved oxygen")
units(all_0h) <- c("C", "psu", "m", "m", "m/s", "m/s", "mg/m^3", "mg/m^3", "mmol /m^3")

all_0h
writeCDF(all_0h, filename = here("data/enviro/processed/CMEM_SST_SAL_MLD_SSH_UO_VO_CHL_NPP_DO_0m_Jan2004_Dec2009_0.25_D.nc"))

#test <- rast("data/enviro/processed/CMEM_SST_SAL_MLD_SSH_UO_VO_CHL_NPP_DO_0m_Jan2004_Dec2009_0.25_D.nc")


#### 50 m vars ####
# Salinity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#identify files -- salinity 50m
so_files <- list.files("C:/Users/nazar/OneDrive/Desktop/enviro_files/SAL_temp_data_10to50m",
                    full.names = TRUE) #full.names = T copies entire path 

read_so <- function(so_path){
 rast(so_path) %>%
    #total nc file has 10 depth layers -- last layer is closest to 50 m (47)
    subset(10) %>%
    #used template raster to change resolution (to 0.25)
    resample(template_rast)
}

so_50h <- so_files %>%
  map(read_so) %>% #map is purr version of lapply()
  rast() 

# SST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sst_fifty <- rast("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/enviro/final/phys/CMEMS_SST_50m_Jan2004_Dec2009_0.083_D.nc")

sst_50h <- sst_fifty %>% resample(template_rast)

# UO and VO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vel_fifty <- rast("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/enviro/final/phys/CMEMS_UO_VO_50m_Jan2004_Dec2009_0.083_D.nc") 

#x[1] pulls the first (left) item from the separated string produced by str_split(), if want right, change to a 2
v_temp <- str_split(names(vel_fifty), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")
uo_pos <- which(v_temp == "uo")
vo_pos <- which(v_temp == "vo")

uo_only <- vel_fifty %>% subset(uo_pos)
vo_only <- vel_fifty %>% subset(vo_pos)

uo_50h <- resample(uo_only, template_rast)
vo_50h <- resample(vo_only, template_rast)

## Add biological variables (CHL, NPP, DO)
chl_npp_50 <- rast(here("data/enviro/final/biol/CMEMS_CHL_NPP_50m_Jan2004_Dec2009_0.25_D.nc"))
do_50 <- rast(here("data/enviro/final/biol/CMEMS_DO_50m_Jan2004_Dec2009_0.25_D.nc"))

split_biol <-str_split(names(chl_npp_50), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")

chl50_pos <- which(split_biol == "chl")
npp50_pos <- which(split_biol == "nppv")

chl50_only <- chl_npp_50 %>% subset(chl50_pos) 
npp50_only <- chl_npp_50 %>% subset(npp50_pos)

chl_50h <- resample(chl50_only, template_rast) 
npp_50h <- resample(npp50_only, template_rast)
do_50h <- resample(do_50, template_rast)

#Combine all 50m vars~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#combine all harmonized surface vars ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_50h <- sds(sst_50h, so_50h, uo_50h, vo_50h, chl_50h, npp_50h, do_50h)
names(all_50h) <- c("thetao", "so", "uo", "vo", "chl", "nppv", "o2")
longnames(all_50h) <- c("sea water potential temperature", "salinity", "eastward sea water velocity", "northward sea water velocity", "chlorophyll", "net primary productivity", "dissolved oxygen")
units(all_50h) <- c("C", "psu", "m/s", "m/s", "mg/m^3", "mg/m^3", "mmol/m^3")
all_50h

writeCDF(all_50h, filename = here("data/enviro/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_50m_Jan2004_Dec2009_0.25_D.nc"))
test <- rast(here("data/enviro/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_50m_Jan2004_Dec2009_0.25_D.nc"))

#plot variable for first 16 days 
#terra::plot(all_0h$mlotst)


#### 100m vars ####
# Salinity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sal_100 <- rast(here("data/enviro/final/phys/CMEMS_SAL_100m_Jan2004_Dec2009_0.083_D.nc"))

sal_100h <- sal_100 %>% resample(template_rast)

# SST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sst_100 <- rast(here("data/enviro/final/phys/CMEMS_SST_100m_Jan2004_Dec2009_0.083_D.nc"))

sst_100h <- sst_100 %>% resample(template_rast)

# UO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uo_100 <- rast(here("data/enviro/final/phys/CMEMS_UO_100m_Jan2004_Dec2009_0.083_D.nc"))

uo_100h <- uo_100 %>% resample(template_rast)

# VO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vo_100 <- rast(here("data/enviro/final/phys/CMEMS_VO_100m_Jan2004_Dec2009_0.083_D.nc"))

vo_100h <- vo_100 %>% resample(template_rast)

# Biol vars (CHL, NPP, DO)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Add biological variables (CHL, NPP, DO)
chl_npp_100 <- rast(here("data/enviro/final/biol/CMEMS_CHL_NPP_100m_Jan2004_Dec2009_0.25_D.nc"))
do_100 <- rast(here("data/enviro/final/biol/CMEMS_DO_100m_Jan2004_Dec2009_0.25_D.nc"))

split_biol <-str_split(names(chl_npp_100), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")

chl100_pos <- which(split_biol == "chl")
npp100_pos <- which(split_biol == "nppv")

chl100_only <- chl_npp_100 %>% subset(chl100_pos) 
npp100_only <- chl_npp_100 %>% subset(npp100_pos)

chl_100h <- resample(chl100_only, template_rast) 
npp_100h <- resample(npp100_only, template_rast)
do_100h <- resample(do_100, template_rast)

#Combine all 100m vars~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#combine all harmonized surface vars ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_100h <- sds(sst_100h, sal_100h, uo_100h, vo_100h, chl_100h, npp_100h, do_100h)
names(all_100h) <- c("thetao", "so", "uo", "vo", "chl", "nppv", "o2")
longnames(all_100h) <- c("sea water potential temperature", "salinity", "eastward sea water velocity", "northward sea water velocity", "chlorophyll", "net primary productivity", "dissolved oxygen")
units(all_100h) <- c("C", "psu", "m/s", "m/s", "mg/m^3", "mg/m^3", "mmol/m^3")
all_100h

writeCDF(all_100h, filename = here("data/enviro/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_100m_Jan2004_Dec2009_0.25_D.nc"), overwrite = TRUE)
test <- rast(here("data/enviro/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_100m_Jan2004_Dec2009_0.25_D.nc"))

#### 150m vars ####
# Salinity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sal_150 <- rast(here("data/enviro/final/phys/CMEMS_SAL_150m_Jan2004_Dec2009_0.083_D.nc"))

sal_150h <- sal_150 %>% resample(template_rast)

# SST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sst_150 <- rast(here("data/enviro/final/phys/CMEMS_SST_150m_Jan2004_Dec2009_0.083_D.nc"))

sst_150h <- sst_150 %>% resample(template_rast)

# UO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uo_150 <- rast(here("data/enviro/final/phys/CMEMS_UO_150m_Jan2004_Dec2009_0.083_D.nc"))

uo_150h <- uo_150 %>% resample(template_rast)

# VO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vo_150 <- rast(here("data/enviro/final/phys/CMEMS_VO_150m_Jan2004_Dec2009_0.083_D.nc"))

vo_150h <- vo_150 %>% resample(template_rast)

# Biol vars (CHL, NPP, DO)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chl_npp_150 <- rast(here("data/enviro/final/biol/CMEMS_CHL_NPP_150m_Jan2004_Dec2009_0.25_D.nc"))
do_150 <- rast(here("data/enviro/final/biol/CMEMS_DO_150m_Jan2004_Dec2009_0.25_D.nc"))

split_biol <-str_split(names(chl_npp_150), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")

chl150_pos <- which(split_biol == "chl")
npp150_pos <- which(split_biol == "nppv")

chl150_only <- chl_npp_150 %>% subset(chl150_pos) 
npp150_only <- chl_npp_150 %>% subset(npp150_pos)

chl_150h <- resample(chl150_only, template_rast) 
npp_150h <- resample(npp150_only, template_rast)
do_150h <- resample(do_150, template_rast)

#Combine all 150m vars~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#combine all harmonized surface vars ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_150h <- sds(sst_150h, sal_150h, uo_150h, vo_150h, chl_150h, npp_150h, do_150h)
names(all_150h) <- c("thetao", "so", "uo", "vo", "chl", "nppv", "o2")
longnames(all_150h) <- c("sea water potential temperature", "salinity", "eastward sea water velocity", "northward sea water velocity", "chlorophyll", "net primary productivity", "dissolved oxygen")
units(all_150h) <- c("C", "psu", "m/s", "m/s", "mg/m^3", "mg/m^3", "mmol/m^3")
all_150h

writeCDF(all_150h, filename = here("data/enviro/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_150m_Jan2004_Dec2009_0.25_D.nc"), overwrite = TRUE)
  test <- rast(here("data/enviro/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_150m_Jan2004_Dec2009_0.25_D.nc"))

#### 200m vars ####
  # Salinity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sal_200 <- rast(here("data/enviro/final/phys/CMEMS_SAL_200m_Jan2004_Dec2009_0.083_D.nc"))
  
  sal_200h <- sal_200 %>% resample(template_rast)
  
  # SST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sst_200 <- rast(here("data/enviro/final/phys/CMEMS_SST_200m_Jan2004_Dec2009_0.083_D.nc"))
  
  sst_200h <- sst_200 %>% resample(template_rast)
  
  # UO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  uo_200 <- rast(here("data/enviro/final/phys/CMEMS_UO_200m_Jan2004_Dec2009_0.083_D.nc"))
  
  uo_200h <- uo_200 %>% resample(template_rast)
  
  # VO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vo_200 <- rast(here("data/enviro/final/phys/CMEMS_VO_200m_Jan2004_Dec2009_0.083_D.nc"))
  
  vo_200h <- vo_200 %>% resample(template_rast)
  
  # Biol vars (CHL, NPP, DO)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chl_npp_200 <- rast(here("data/enviro/final/biol/CMEMS_CHL_NPP_200m_Jan2004_Dec2009_0.25_D.nc"))
  do_200 <- rast(here("data/enviro/final/biol/CMEMS_DO_200m_Jan2004_Dec2009_0.25_D.nc"))
  
  split_biol <-str_split(names(chl_npp_200), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")
  
  chl200_pos <- which(split_biol == "chl")
  npp200_pos <- which(split_biol == "nppv")
  
  chl200_only <- chl_npp_200 %>% subset(chl200_pos) 
  npp200_only <- chl_npp_200 %>% subset(npp200_pos)
  
  chl_200h <- resample(chl200_only, template_rast) 
  npp_200h <- resample(npp200_only, template_rast)
  do_200h <- resample(do_200, template_rast)
  
  #Combine all 200m vars~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #combine all harmonized surface vars ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  all_200h <- sds(sst_200h, sal_200h, uo_200h, vo_200h, chl_200h, npp_200h, do_200h)
  names(all_200h) <- c("thetao", "so", "uo", "vo", "chl", "nppv", "o2")
  longnames(all_200h) <- c("sea water potential temperature", "salinity", "eastward sea water velocity", "northward sea water velocity", "chlorophyll", "net primary productivity", "dissolved oxygen")
  units(all_200h) <- c("C", "psu", "m/s", "m/s", "mg/m^3", "mg/m^3", "mmol/m^3")
  all_200h
  
  writeCDF(all_200h, filename = here("data/enviro/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_200m_Jan2004_Dec2009_0.25_D.nc"), overwrite = TRUE)
  test <- rast(here("data/enviro/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_200m_Jan2004_Dec2009_0.25_D.nc"))

#### 250m vars ####
  # Salinity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sal_250 <- rast(here("data/enviro/final/phys/CMEMS_SAL_250m_Jan2004_Dec2009_0.083_D.nc"))
  
  sal_250h <- sal_250 %>% resample(template_rast)
  
  # SST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sst_250 <- rast(here("data/enviro/final/phys/CMEMS_SST_250m_Jan2004_Dec2009_0.083_D.nc"))
  
  sst_250h <- sst_250 %>% resample(template_rast)
  
  # UO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  uo_250 <- rast(here("data/enviro/final/phys/CMEMS_UO_250m_Jan2004_Dec2009_0.083_D.nc"))
  
  uo_250h <- uo_250 %>% resample(template_rast)
  
  # VO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vo_250 <- rast(here("data/enviro/final/phys/CMEMS_VO_250m_Jan2004_Dec2009_0.083_D.nc"))
  
  vo_250h <- vo_250 %>% resample(template_rast)
  
  # Biol vars (CHL, NPP, DO)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chl_npp_250 <- rast(here("data/enviro/final/biol/CMEMS_CHL_NPP_250m_Jan2004_Dec2009_0.25_D.nc"))
  do_250 <- rast(here("data/enviro/final/biol/CMEMS_DO_250m_Jan2004_Dec2009_0.25_D.nc"))
  
  split_biol <-str_split(names(chl_npp_250), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")
  
  chl250_pos <- which(split_biol == "chl")
  npp250_pos <- which(split_biol == "nppv")
  
  chl250_only <- chl_npp_250 %>% subset(chl250_pos) 
  npp250_only <- chl_npp_250 %>% subset(npp250_pos)
  
  chl_250h <- resample(chl250_only, template_rast) 
  npp_250h <- resample(npp250_only, template_rast)
  do_250h <- resample(do_250, template_rast)
  
  #Combine all 250m vars~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #combine all harmonized surface vars ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  all_250h <- sds(sst_250h, sal_250h, uo_250h, vo_250h, chl_250h, npp_250h, do_250h)
  names(all_250h) <- c("thetao", "so", "uo", "vo", "chl", "nppv", "o2")
  longnames(all_250h) <- c("sea water potential temperature", "salinity", "eastward sea water velocity", "northward sea water velocity", "chlorophyll", "net primary productivity", "dissolved oxygen")
  units(all_250h) <- c("C", "psu", "m/s", "m/s", "mg/m^3", "mg/m^3", "mmol/m^3")
  all_250h
  
  writeCDF(all_250h, filename = here("data/enviro/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_250m_Jan2004_Dec2009_0.25_D.nc"), overwrite = TRUE)
  test <- rast(here("data/enviro/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_250m_Jan2004_Dec2009_0.25_D.nc"))


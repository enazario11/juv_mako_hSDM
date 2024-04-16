#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#load packages####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(sf)
library(terra)
library(ncdf4)
library(here)
library(raster)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#load the data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #aniMotum locs
dat_locs <- readRDS(here("data/presence_locs/roms_domain/pres_locs.RDS")) %>% mutate(PA = 0, rep = NA)

  #raw locs w/ avg, max, and med dive depth info by per day
dat_dep <- read.csv(here("data/tdl.csv")) %>% 
  mutate(date = as.POSIXct(strptime(date, format = "%m/%d/%Y")))

quantile(dat_dep$max_depth, na.rm = T, 0.90) #248 m is the 90% quantile
quantile(dat_dep$avg_depth, na.rm = T)

  #PA locs
pa_locs <- readRDS(here("data/PAs/aniMotum_crw_pa/PA_locs_16.RDS")) %>% mutate(PA = 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#join depth data to loc data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat_locs <- dat_locs %>% mutate(dep_id = paste(id, date, sep = " "))
dat_dep <- dat_dep %>% mutate(dep_id = paste(ptt, date, sep = " ")) %>% group_by(dep_id) %>% distinct(dep_id, .keep_all = T)

  #transfer the depth data
dat_loc_dep <- dat_locs
dat_loc_dep$med_depth <- NA
dat_loc_dep$max_depth <- NA

dep <- c("med_depth", "max_depth")
dat_loc_dep[dep] <- lapply(dep, function(x) dat_dep[[x]][match(dat_loc_dep$dep_id, dat_dep$dep_id)])

  #summarise results
sum(!is.na(dat_loc_dep$med_depth)) #2077
hist(dat_dep$max_depth) #peaks at 150m and tapers until about 400m
hist(dat_dep$avg_depth) #peaks at 30m and tapers until about 150m
dat_loc_dep %>% group_by(id) %>% summarise(totals = sum(!is.na(max_depth))) %>% print(n = 25)

  #add depth range column -- consider making 400m the maximum depth we create a layer for
dat_loc_dep <- dat_loc_dep %>% 
  mutate(dep_layer = ifelse(max_depth <= 10, "0", ifelse(max_depth > 10 & max_depth <=50, "50", ifelse(max_depth > 50 & max_depth <= 150, "150", ifelse(max_depth > 150 & max_depth <= 250, "250", ifelse(max_depth > 250, "250+", "NA"))))))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#join locs and PAs together####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat_locs_comb <- dat_locs %>% 
  subset(select = -c(geometry))
names(dat_locs_comb) <- c("tag", "date", "lon", "lat", "PA", "rep")

pa_locs_comb <- pa_locs %>%
  subset(select = -c(model, x, y, domain))
names(pa_locs_comb) <- c("tag", "rep", "date", "lon", "lat", "PA")
pa_locs_comb <- pa_locs_comb[, c(1, 3, 4, 5, 6, 2)]

all_locs <- rbind(dat_locs_comb, pa_locs_comb)

#load ROMSextract function
source(here("functions/enviro_extract_functions.R"))

#set loc df for extraction
    #locs
input_file <- all_locs
input_file$date <- as.factor(as.Date(substr(input_file$date, 1,  10))) #Ensure date format is ok for getvarROMS. 
input_file$dt <- as.POSIXct(strptime(input_file$date, format = "%Y-%m-%d"), tz = "UTC")

    # remove points outside of ROMS boundary (num of obsv. shouldn't change: 43083)
input_file <- input_file[input_file$lat>=30 & input_file$lat<=48,] 
input_file <- input_file[input_file$lon>=-134 & input_file$lon<=-115,] 

head(input_file)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extract bathy and rugosity ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#modify CRS and res of GEBCO NC file. Write New CDF
bathy <- rast(here("data/enviro/bathy/GEBCO_2023_n58.0_s4.0_w-147.0_e-97.0.nc"))
template_rast <- rast(
  crs = 'EPSG:4326',
  extent = ext(-134.5, -110, 29.5, 48.5),
  resolution = 0.25
)
bathy_mod <- resample(bathy, template_rast)
writeCDF(bathy_mod, here("data/enviro/bathy/processed/gebco_bathy_roms_domain.nc"), overwrite = TRUE)

    ### SCRATCH WORK FOR NOW
bathy_file <- list.files(here("data/enviro/bathy/processed"), pattern = "roms", full.names = TRUE)
#bathy_file_raw <- list.files(here("data/enviro/bathy"), full.names = TRUE, pattern = ".nc")
    ####
    
all_dat_bathy <- getBathy(bathy_file, input_file, 'gebco_bathy_roms_domain', 0.25)

#explore outputs
head(all_dat_bathy)
hist(all_dat_bathy$bathy, breaks = 30) #bathymetry
hist(all_dat_bathy$bathy_sd, breaks = 30) #rugosity

ggplot(all_dat_bathy, aes(bathy)) + geom_histogram(bins = 15, color = "grey") + facet_wrap(~PA, scales = "free") + theme_bw()
ggplot(all_dat_bathy, aes(bathy_sd)) + geom_histogram(bins = 15, color = "grey") + facet_wrap(~PA, scales = "free") + theme_bw()

all_dat_bathy %>% 
  group_by(PA) %>% 
  summarise(med_bathy = median(bathy, na.rm = TRUE), 
            med_rug = median(bathy_sd, na.rm = TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extract ROMS covars ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#covar files
ROMS_files <- list.files(here("data/enviro/ROMS/1980_2010/0m"), pattern = ".nc", full.names = TRUE)

# load in variables and individually confirm they look okay
dat <- nc_open(ROMS_files[1]);dat #BV
dat <- nc_open(ROMS_files[2]);dat #curl
dat <- nc_open(ROMS_files[3]);dat #ild
dat <- nc_open(ROMS_files[4]);dat #ssh
dat <- nc_open(ROMS_files[5]);dat #sss
dat <- nc_open(ROMS_files[6]);dat #sst
dat <- nc_open(ROMS_files[7]);dat #su
dat <- nc_open(ROMS_files[8]);dat #sustr
dat <- nc_open(ROMS_files[9]);dat #svstr

#using getvarROMS function for extraction 

xtracto = function(input_file, netcdf_list){
    #list remaining nc doc details here -- make sure number relates to correct variables
  input_file <- getvarROMS(nc = ROMS_files[1], varname = 'BV',inpts = input_file, desired.resolution = 0.25, FUN = mean, name = 'mean')
  input_file <- getvarROMS(nc = ROMS_files[2], varname = 'curl',inpts = input_file, desired.resolution = 0.25, FUN = mean, name = 'mean')
  input_file <- getvarROMS(nc = ROMS_files[3], varname = 'ild',inpts = input_file, desired.resolution = 0.25, FUN = mean, name = 'mean')
  input_file <- getvarROMS(nc = ROMS_files[4], varname = 'ssh',inpts = input_file, desired.resolution = 0.25, FUN = mean, name = 'mean')
  input_file <- getvarROMS(nc = ROMS_files[5], varname = 'sss',inpts = input_file, desired.resolution = 0.25, FUN = mean, name = 'mean')
  input_file <- getvarROMS(nc = ROMS_files[6], varname = 'sst',inpts = input_file, desired.resolution = 0.25, FUN = mean, name = 'mean')
  input_file <- getvarROMS(nc = ROMS_files[7], varname = 'su',inpts = input_file, desired.resolution = 0.25, FUN = mean, name = 'mean')
  input_file <- getvarROMS(nc = ROMS_files[8], varname = 'sustr',inpts = input_file, desired.resolution = 0.25, FUN = mean, name = 'mean')
  input_file <- getvarROMS(nc = ROMS_files[9], varname = 'svstr',inpts = input_file, desired.resolution = 0.25, FUN = mean, name = 'mean')
}

#extract values
input_file = all_dat_bathy
all_dat_covar <- xtracto(input_file, ROMS_files)

head(all_dat_covar)
hist(all_dat_covar$sst_mean, breaks = 30)


#########################################################################################################
############################################ SCRATCH WORK ################################################
##########################################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract data for PAs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #format loc data
  pa_test <- dat_PA%>% 
  mutate(date = as.character(date)) %>%  
  rename("Lon" = "lon_pa", "Lat" = "lat_pa")

pres_test <- dat_loc_dep %>% 
  filter(dep_layer == 50) %>% 
  subset(select = (c("id", "date", "lon_p", "lat_p"))) %>%
  mutate(date = as.POSIXct(strptime(date, format = "%Y-%m-%d")),
         date = as.factor(as.Date(substr(date, 1, 10))) ) %>%  
  rename("longitude" = "lon_p", 
         "latitude" = "lat_p") %>%
  mutate(dt = as.POSIXct(strptime(date, format = "%Y-%m-%d"), tz = "UTC"))


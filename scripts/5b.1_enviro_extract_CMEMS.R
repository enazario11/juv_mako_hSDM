#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#load packages####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(sf)
library(terra)
library(ncdf4)
library(here);here <- here::here
library(raster)

#load ROMSextract, CMEMextract, and pseudo depth functions
source(here("functions/enviro_extract_functions.R"))

set.seed(1004)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#load the data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #aniMotum locs
dat_locs <- readRDS(here("data/presence_locs/cmems_domain/pres_locs_alldat.RDS")) %>% mutate(PA = 0, rep = NA)

  #raw locs w/ avg, max, and med dive depth info by per day
dat_dep <- read.csv(here("data/presence_locs/tdl.csv")) %>% 
  mutate(date = as.POSIXct(strptime(date, format = "%m/%d/%Y")))

quantile(dat_dep$max_depth, na.rm = T, 0.90) #248 m is the 90% quantile

  #PA locs
pa_locs <- readRDS(here("data/PAs/aniMotum_cmems_pa/PA_locs_alldat_39.RDS")) %>% mutate(PA = 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#join depth data to loc data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat_locs <- dat_locs %>% mutate(dep_id = paste(id, date, sep = " "))
dat_dep <- dat_dep %>% mutate(dep_id = paste(ptt, date, sep = " ")) %>% group_by(dep_id) %>% distinct(dep_id, .keep_all = T)

  #transfer the depth data
dat_loc_dep <- dat_locs
dat_loc_dep$med_depth <- NA
dat_loc_dep$max_depth <- NA
dat_loc_dep$avg_depth <- NA

dep <- c("med_depth", "max_depth", "avg_depth")
dat_loc_dep[dep] <- lapply(dep, function(x) dat_dep[[x]][match(dat_loc_dep$dep_id, dat_dep$dep_id)])

  #summarise results
sum(!is.na(dat_loc_dep$med_depth)) #1302
hist(dat_dep$max_depth) #peaks at 150m and tapers until about 400m
hist(dat_dep$avg_depth) #peaks at 30m and tapers until about 150m
dat_loc_dep %>% group_by(id) %>% summarise(totals = sum(!is.na(max_depth))) %>% print(n = 25)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#join locs and PAs together####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat_locs_comb <- dat_locs %>% 
  subset(select = -c(geometry))
names(dat_locs_comb) <- c("tag", "date", "lon", "lat", "PA", "rep")

pa_locs_comb <- pa_locs %>%
  subset(select = -c(model, x, y, domain))
names(pa_locs_comb) <- c("tag", "rep", "date", "lon", "lat", "PA")
pa_locs_comb <- pa_locs_comb[, c(1, 3, 4, 5, 6, 2)]

all_locs <- rbind(dat_locs_comb, pa_locs_comb)

#set loc df for extraction
    #locs
input_file <- all_locs
input_file$date <- as.factor(as.Date(substr(input_file$date, 1,  10))) #Ensure date format is ok for getvarROMS. 
input_file$dt <- as.POSIXct(strptime(input_file$date, format = "%Y-%m-%d"), tz = "UTC")

    # remove points outside of ROMS boundary (num of obsv. shouldn't change: 148256)
input_file <- input_file[input_file$lat>=10 & input_file$lat<=50,] 
input_file <- input_file[input_file$lon>=-140 & input_file$lon<=-110,] 

head(input_file)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extract bathy and rugosity ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#modify CRS and res of GEBCO NC file. Write New CDF
# bathy <- rast(here("data/enviro/bathy/GEBCO_2023_n58.0_s4.0_w-147.0_e-97.0.nc"))
# template_rast <- rast(
#   crs = 'EPSG:4326',
#   extent = ext(-153, -100, -3, 55), #cmems domain
#   resolution = 0.25
# )
# bathy_mod <- resample(bathy, template_rast)
# writeCDF(bathy_mod, here("data/enviro/bathy/processed/gebco_bathy_cmems_domain.nc"), overwrite = TRUE)

bathy_file <- list.files(here("data/enviro/bathy/processed"), pattern = "cmems", full.names = TRUE)
    
all_dat_bathy_cmem <- getBathy(bathy_file, input_file, 'gebco_bathy_cmems_domain', 0.25)

#explore outputs
head(all_dat_bathy_cmem)
hist(all_dat_bathy_cmem$bathy, breaks = 30) #bathymetry
hist(all_dat_bathy_cmem$bathy_sd, breaks = 30) #rugosity

ggplot(all_dat_bathy_cmem, aes(bathy)) + geom_histogram(bins = 15, color = "grey") + facet_wrap(~PA, scales = "free") + theme_bw()
ggplot(all_dat_bathy_cmem, aes(bathy_sd)) + geom_histogram(bins = 15, color = "grey") + facet_wrap(~PA, scales = "free") + theme_bw()

all_dat_bathy_cmem %>% 
  group_by(PA) %>% 
  summarise(med_bathy = median(bathy, na.rm = TRUE), 
            med_rug = median(bathy_sd, na.rm = TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extract CMEMS covars ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0m all dat extract ####
cmem_nc0 <- nc_open(here("data/enviro/CMEMS/processed/CMEM_SST_SAL_MLD_SSH_UO_VO_CHL_NPP_DO_0m_Jan2004_Dec2009_0.25_D.nc"))

#surface extract
xtracto_cmem = function(input_file, nc_file){
  input_file <- getvarCMEM(nc_file, "thetao", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "so", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "mlotst", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "zos", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "uo", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "vo", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "chl", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "nppv", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "o2", input_file, 0.25, mean, "mean")
}

all_dat_cmem_0m <- xtracto_cmem(input_file, cmem_nc0)

#combine with bathy above

all_cmem_covar_0m <- cbind(all_dat_cmem_0m, all_dat_bathy_cmem$bathy, all_dat_bathy_cmem$bathy_sd)
all_cmem_covar_0m <- all_cmem_covar_0m %>% 
  rename("bathy" = "all_dat_bathy_cmem$bathy", 
         "bathy_sd" = "all_dat_bathy_cmem$bathy_sd")

saveRDS(all_cmem_covar_0m, here("data/locs_w_covar/cmems/cmem_locs_covar_0m.rds"))
head(all_cmem_covar_0m)

    #explore
ggplot(all_cmem_covar_0m, aes(o2_mean)) + geom_histogram(bins = 30, color = "grey") + facet_wrap(~PA, scales = "free") + theme_bw()

cmem_0m_long <- gather(all_cmem_covar_0m, covar, value, thetao_mean:bathy_sd) %>% mutate(PA = as.factor(PA))

ggplot(cmem_0m_long, aes(x = value, fill = PA)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~covar, scales = "free") + 
  theme_bw()+
  scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))

all_cmem_covar_0m %>% 
  group_by(PA) %>% 
  summarise(mean_sst = mean(thetao_mean, na.rm = TRUE), 
            mean_sal = mean(so_mean, na.rm = TRUE), 
            mean_mld = mean(mlotst_mean, na.rm = TRUE), 
            mean_ssh = mean(zos_mean, na.rm = TRUE), 
            mean_uo = mean(uo_mean, na.rm = TRUE), 
            mean_vo = mean(vo_mean, na.rm = TRUE), 
            mean_chl = mean(chl_mean, na.rm = TRUE), 
            mean_npp = mean(nppv_mean, na.rm = TRUE), 
            mean_do = mean(o2_mean, na.rm = TRUE))

#250m extract ####
# dat extract ####
cmem_nc250 <- nc_open(here("data/enviro/CMEMS/processed/CMEM_SST_SAL_UO_VO_CHL_NPP_DO_250m_Jan2004_Dec2009_0.25_D.nc"))

xtracto_cmem_depth = function(input_file, nc_file){
  input_file <- getvarCMEM(nc_file, "thetao", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "so", input_file, 0.25, mean, "mean")
  #input_file <- getvarCMEM(nc_file, "mlotst", input_file, 0.25, mean, "mean")
  #input_file <- getvarCMEM(nc_file, "zos", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "uo", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "vo", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "chl", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "nppv", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "o2", input_file, 0.25, mean, "mean")
}

all_dat_cmem_250m <- xtracto_cmem_depth(input_file, cmem_nc250)

saveRDS(all_dat_cmem_250m, here("data/locs_w_covar/cmems/cmem_locs_covar_250m.rds"))
head(all_dat_cmem_250m)

#explore
ggplot(all_dat_cmem_250m, aes(o2_mean)) + geom_histogram(bins = 30, color = "grey") + facet_wrap(~PA, scales = "free") + theme_bw()

cmem_250m_long <- gather(all_dat_cmem_250m, covar, value, thetao_mean:o2_mean) %>% mutate(PA = as.factor(PA))

ggplot(cmem_250m_long, aes(x = value, fill = PA)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~covar, scales = "free") + 
  theme_bw()+
  scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))

all_dat_cmem_250m %>% 
  group_by(PA) %>% 
  summarise(mean_sst = mean(thetao_mean, na.rm = TRUE), 
            mean_sal = mean(so_mean, na.rm = TRUE), 
            mean_uo = mean(uo_mean, na.rm = TRUE), 
            mean_vo = mean(vo_mean, na.rm = TRUE), 
            mean_chl = mean(chl_mean, na.rm = TRUE), 
            mean_npp = mean(nppv_mean, na.rm = TRUE), 
            mean_do = mean(o2_mean, na.rm = TRUE))


##### SCRATCH #######
#set bins
#observed data
dat_loc_dep <- dat_loc_dep %>% 
  mutate(max_dep_layer = ifelse(max_depth > 0 & max_depth <=50, "50", 
                                ifelse(max_depth > 50 & max_depth <= 100, "100",
                                       ifelse(max_depth > 100 & max_depth <= 150, "150", 
                                              ifelse(max_depth > 150 & max_depth <= 250, "250", 
                                                     ifelse(max_depth > 250, "250+", "NA"))))), 
         mean_dep_layer = ifelse(max_depth > 0 & max_depth <=50, "50", 
                                 ifelse(max_depth > 50 & max_depth <= 100, "100",
                                        ifelse(max_depth > 100 & max_depth <= 150, "150", 
                                               ifelse(max_depth > 150 & max_depth <= 250, "250", 
                                                      ifelse(max_depth > 250, "250+", "NA"))))))

dat_loc_dep %>% group_by(max_dep_layer) %>% summarise(totals = sum(!is.na(max_depth)))

#PA data
pa_locs_dep <- pa_locs_dep %>% 
  mutate(max_dep_layer = ifelse(max_depth <= 10, "0", 
                                ifelse(max_depth > 10 & max_depth <=50, "50", 
                                       ifelse(max_depth > 50 & max_depth <= 150, "150", 
                                              ifelse(max_depth > 150 & max_depth <= 250, "250", 
                                                     ifelse(max_depth > 250, "250+", "NA"))))))

pa_locs_dep %>% group_by(id, dep_layer) %>% summarise(totals = sum(!is.na(max_depth))) %>% print(n = 25)


#Depth layer scratch work
#for each shark, simulate depth data from their dive depth histograms
input_depths = dat_loc_dep
pa_loc_dep = NULL
for(i in 1:length(unique(pa_locs$id))){
  curr_ID <- unique(pa_locs$id)[i]
  temp_df <- pa_locs[pa_locs$id %in% curr_ID,]
  PA_locs = temp_df
  
  #save steps one and two as new DFs that will be used as inputs for function
  temp_pa <- get_pseudo_depths(input_depths, PA_locs)
  pa_loc_dep = rbind(pa_loc_dep, temp_pa)
}

head(pa_loc_dep)

#clean dat and pa depth DFs to match 
dat_loc_dep2 <- dat_loc_dep %>%
  subset(select = -c(geometry, dep_id)) %>%
  filter(med_depth != "NA" | max_depth != "NA" | avg_depth != "NA")
pa_loc_dep2 <- pa_loc_dep %>%
  subset(select = -c(model, x, y, domain))

#For each shark depth bin combo, filter the PAs so there is a 1:1 ratio
#### think of how I will do.....



#rather than setting bins to extract, have depths pair to nearest number from my sequence of depth layers (0:250 every 50m)






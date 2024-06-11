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
  #aniMotum locs and background PA points
all_locs <- read.csv(here("data/PAs/background/psat_spot_domain/mako_pres_Abs.csv")) 
all_locs <- all_locs %>% 
  subset(select = c(tag, lon, lat, date, presAbs)) %>%
  mutate(lon = lon - 360)

#set loc df for extraction
    #locs
input_file <- all_locs
input_file$date <- as.factor(as.Date(substr(input_file$date, 1,  10))) #Ensure date format is ok for getvarROMS. 
input_file$dt <- as.POSIXct(strptime(input_file$date, format = "%Y-%m-%d"), tz = "UTC")

    # remove points outside of domain (num of obsv. shouldn't change: 367776)
input_file <- input_file[input_file$lat>=1 & input_file$lat<=49,] 
input_file <- input_file[input_file$lon>=-153 & input_file$lon<=-103,] 

colnames(input_file) <- c("id", "lon", "lat", "date", "PA", "dt")
head(input_file)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extract bathy and rugosity ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bathy_file <- list.files(here("data/enviro/psat_spot_all/all_processed"), pattern = "bathy_0.25deg2", full.names = TRUE)

all_dat_bathy_cmem <- getBathy(bathy_file, input_file, 'gebco_bathy_0.25deg2', 0.25) #update varid when I delete the other bathy file

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
cmem_nc0 <- nc_open(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_CHL_Temp_SO_UO_UOSTR_VO_VOSTR_SSH_MLD_0m_Jan2003_Dec2015_0.25_D.nc"))

#surface extract
xtracto_cmem = function(input_file, nc_file){
  input_file <- getvarCMEM(nc_file, "o2", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "chl", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "votemper", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "vosaline", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "vozocrtx", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "sozotaux", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "vomecrty", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "sometauy", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "sossheig", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "somxlavt", input_file, 0.25, mean, "mean")
}

all_dat_cmem_0m <- xtracto_cmem(input_file, cmem_nc0)

#combine with bathy above
all_cmem_covar_0m <- cbind(all_dat_cmem_0m, all_dat_bathy_cmem$bathy, all_dat_bathy_cmem$bathy_sd)
all_cmem_covar_0m <- all_cmem_covar_0m %>% 
  rename("bathy" = "all_dat_bathy_cmem$bathy", 
         "bathy_sd" = "all_dat_bathy_cmem$bathy_sd")

#saveRDS(all_cmem_covar_0m, here("data/locs_w_covar/psat_spot/cmem_locs_covar_0m_back.rds"))
head(all_cmem_covar_0m)

    #explore
ggplot(all_cmem_covar_0m, aes(o2_mean)) + geom_histogram(bins = 30, color = "grey") + facet_wrap(~PA, scales = "free") + theme_bw()

cmem_0m_long <- gather(all_cmem_covar_0m, covar, value, o2_mean:bathy_sd) %>% mutate(PA = as.factor(PA))

ggplot(cmem_0m_long, aes(x = value, fill = PA)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~covar, scales = "free") + 
  theme_bw()+
  scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))

all_cmem_covar_0m %>% 
  group_by(PA) %>% 
  summarise(mean_temp = mean(votemper_mean, na.rm = TRUE), 
            mean_do = mean(o2_mean, na.rm = TRUE),
            mean_sal = mean(vosaline_mean, na.rm = TRUE), 
            mean_mld = mean(ssheig_mean, na.rm = TRUE), 
            mean_ssh = mean(somxlavt_mean, na.rm = TRUE), 
            mean_uo = mean(vozocrtx_mean, na.rm = TRUE), 
            mean_uostr = mean(sozotaux, na.rm = TRUE),
            mean_vo = mean(vomecrty_mean, na.rm = TRUE), 
            mean_vostr = mean(sometauy, na.rm = TRUE),
            mean_chl = mean(chl_mean, na.rm = TRUE))

#60m extract ####
# dat extract ###
cmem_nc60 <- nc_open(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_Temp_SO_60m_Jan2003_Dec2015_0.25_D.nc"))

xtracto_cmem_depth = function(input_file, nc_file){
  input_file <- getvarCMEM(nc_file, "o2", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "votemper", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM(nc_file, "vosaline", input_file, 0.25, mean, "mean")
}

all_dat_cmem_60m <- xtracto_cmem_depth(input_file, cmem_nc60)

#saveRDS(all_dat_cmem_60m, here("data/locs_w_covar/psat_spot/cmem_locs_covar_60m_back.rds"))
head(all_dat_cmem_60m)

#explore
ggplot(all_dat_cmem_60m, aes(o2_mean)) + geom_histogram(bins = 30, color = "grey") + facet_wrap(~PA, scales = "free") + theme_bw()

cmem_60m_long <- gather(all_dat_cmem_60m, covar, value, o2_mean:vosaline_mean) %>% mutate(PA = as.factor(PA))

ggplot(cmem_60m_long, aes(x = value, fill = PA)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~covar, scales = "free") + 
  theme_bw()+
  scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))

all_dat_cmem_60m %>% 
  group_by(PA) %>% 
  summarise(mean_temp = mean(votemper_mean, na.rm = TRUE), 
            mean_do = mean(o2_mean, na.rm = TRUE),
            mean_sal = mean(vosaline_mean, na.rm = TRUE))

#250m extract ####
# dat extract ###
cmem_nc250 <- nc_open(here("data/enviro/psat_spot_all/all_processed/CMEM_DO_Temp_SO_250m_Jan2003_Dec2015_0.25_D.nc"))

all_dat_cmem_250m <- xtracto_cmem_depth(input_file, cmem_nc250)

#saveRDS(all_dat_cmem_250m, here("data/locs_w_covar/psat_spot/cmem_locs_covar_250m_back.rds"))
head(all_dat_cmem_250m)

#explore
ggplot(all_dat_cmem_250m, aes(o2_mean)) + geom_histogram(bins = 30, color = "grey") + facet_wrap(~PA, scales = "free") + theme_bw()

cmem_250m_long <- gather(all_dat_cmem_250m, covar, value, o2_mean:vosaline_mean) %>% mutate(PA = as.factor(PA))

ggplot(cmem_250m_long, aes(x = value, fill = PA)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~covar, scales = "free") + 
  theme_bw()+
  scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))

all_dat_cmem_250m %>% 
  group_by(PA) %>% 
  summarise(mean_temp = mean(votemper_mean, na.rm = TRUE), 
            mean_do = mean(o2_mean, na.rm = TRUE),
            mean_sal = mean(vosaline_mean, na.rm = TRUE))


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






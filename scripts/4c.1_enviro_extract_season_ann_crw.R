#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#load packages####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{library(tidyverse)
library(sf)
library(terra)
library(ncdf4)
library(here);here <- here::here
library(raster)}

#load ROMSextract, CMEMextract, and pseudo depth functions
source(here("functions/enviro_extract_functions_ann_seas.R"))

set.seed(1004)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#load the data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#aniMotum locs
dat_locs <- readRDS(here("data/presence_locs/psat_spot_domain/processed/psat_spot_animotum.RDS")) %>% mutate(PA = 0, rep = NA)

#CRW PA locs
pa_locs <- readRDS(here("data/presence_locs/psat_spot_domain/processed/psat_spot_PAs.RDS")) %>% mutate(PA = 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#join locs and CRW PAs together####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat_locs_comb <- dat_locs %>% 
  subset(select = -c(geometry))
names(dat_locs_comb) <- c("tag", "date", "lon", "lat", "PA", "rep")

pa_locs_comb <- pa_locs %>%
  subset(select = -c(model, x, y, domain))
names(pa_locs_comb) <- c("tag", "rep", "date", "lon", "lat", "PA")
pa_locs_comb <- pa_locs_comb[, c(1, 3, 4, 5, 6, 2)] #reorders columns to match dat_locs_comb DF

all_locs <- rbind(dat_locs_comb, pa_locs_comb)

#set loc df for extraction
#locs
input_file <- all_locs
input_file$date <- as.factor(as.Date(substr(input_file$date, 1,  10))) #Ensure date format is ok for getvarROMS. 
input_file$dt <- as.POSIXct(strptime(input_file$date, format = "%Y-%m-%d"), tz = "UTC")

# remove points outside of domain (num of obsv. shouldn't change: 367776)
input_file <- input_file[input_file$lat>=1 & input_file$lat<=49,] 
input_file <- input_file[input_file$lon>=-153 & input_file$lon<=-103,] 

head(input_file)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create annual and seasonal input file date/times
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ann_dates <- function(InputData){
  InputData[ , 'dt_ann'] = NA
  
  for(i in 1:nrow(InputData)){
    
  if (year(InputData$dt[i]) == 2003){
    InputData$dt_ann[i] = "2003-01-01"
  } else if (year(InputData$dt[i]) == 2004){
    InputData$dt_ann[i] = "2004-01-01"
  } else if (year(InputData$dt[i]) == 2005){
    InputData$dt_ann[i] = "2005-01-01"
  } else if (year(InputData$dt[i]) == 2006){
    InputData$dt_ann[i] = "2006-01-01"
  } else if (year(InputData$dt[i]) == 2007){
    InputData$dt_ann[i] = "2007-01-01"
  } else if (year(InputData$dt[i]) == 2008){
    InputData$dt_ann[i] = "2008-01-01"
  } else if (year(InputData$dt[i]) == 2009){
    InputData$dt_ann[i] = "2009-01-01"
  } else if (year(InputData$dt[i]) == 2010){
    InputData$dt_ann[i] = "2010-01-01"
  } else if (year(InputData$dt[i]) == 2011){
    InputData$dt_ann[i] = "2011-01-01"
  } else if (year(InputData$dt[i]) == 2012){
    InputData$dt_ann[i] = "2012-01-01"
  } else if (year(InputData$dt[i]) == 2013){
    InputData$dt_ann[i] = "2013-01-01"
  } else if (year(InputData$dt[i]) == 2014){
    InputData$dt_ann[i] = "2014-01-01"
  } else if (year(InputData$dt[i]) == 2015){
    InputData$dt_ann[i] = "2015-01-01"
  }
  }
  return(InputData)
}

input_file_ann <- ann_dates(input_file)
input_file_ann <- input_file_ann %>% mutate(dt_ann = as.Date(dt_ann))

#saveRDS(input_file_ann, here("data/locs_w_covar/psat_spot/annual/input_file_ann.rds"))

seas_dates <- function(InputData){
  InputData[ , 'dt_seas'] = NA
  
  for(i in 1:nrow(InputData)){
    
    if (month(InputData$dt[i]) == 12 | month(InputData$dt[i]) == 1 | month(InputData$dt[i]) == 2){
      InputData$dt_seas[i] = "2003-12-01"
    } else if (month(InputData$dt[i]) == 3 | month(InputData$dt[i]) == 4 | month(InputData$dt[i]) == 5){
      InputData$dt_seas[i] = "2003-03-01"
    } else if (month(InputData$dt[i]) == 6 | month(InputData$dt[i]) == 7 | month(InputData$dt[i]) == 8){
      InputData$dt_seas[i] = "2003-06-01"
    } else if (month(InputData$dt[i]) == 9 | month(InputData$dt[i]) == 10 | month(InputData$dt[i]) == 11){
      InputData$dt_seas[i] = "2003-09-01"
    }
  }
  
  return(InputData)
}

input_file_seas <- seas_dates(input_file)
input_file_seas <- input_file_seas %>% mutate(dt_seas = as.Date(dt_seas))

#saveRDS(input_file_seas, here("data/locs_w_covar/psat_spot/seasonal/input_file_seas.rds"))

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
# Annual CRW CMEMS covars ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### 0m all dat extract ####
cmem_0m_ann <- nc_open(here("data/enviro/psat_spot_all/all_processed/annual_res/dat_0m_annual3.nc"))

#surface extract
xtracto_cmem = function(input_file, nc_file){
  input_file <- getvarCMEM_ann(nc_file, "somxlavt", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_ann(nc_file, "vosaline", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_ann(nc_file, "sossheig", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_ann(nc_file, "votemper", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_ann(nc_file, "vozocrtx", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_ann(nc_file, "sozotaux", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_ann(nc_file, "vomecrty", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_ann(nc_file, "sometauy", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_ann(nc_file, "o2", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_ann(nc_file, "chl", input_file, 0.25, mean, "mean")
  
}

all_dat_cmem_0m_ann <- xtracto_cmem(input_file_ann, cmem_0m_ann)

#combine with bathy above
all_cmem_covar_0m_ann <- cbind(all_dat_cmem_0m_ann, all_dat_bathy_cmem$bathy, all_dat_bathy_cmem$bathy_sd)
all_cmem_covar_0m_ann <- all_cmem_covar_0m_ann %>% 
  rename("bathy" = "all_dat_bathy_cmem$bathy", 
         "bathy_sd" = "all_dat_bathy_cmem$bathy_sd")
head(all_cmem_covar_0m_ann)

#saveRDS(all_cmem_covar_0m_ann, here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_0m_ann.rds"))

#explore
cmem_0m_long <- gather(all_cmem_covar_0m_ann, covar, value, somxlavt_mean:bathy_sd) %>% mutate(PA = as.factor(PA))

ggplot(cmem_0m_long, aes(x = value, fill = PA)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~covar, scales = "free") + 
  theme_bw()+
  scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))

##### 60m extract ####
# dat extract ###
cmem_nc60 <- nc_open(here("data/enviro/psat_spot_all/all_processed/annual_res/dat_60m_annual.nc"))

xtracto_cmem_depth = function(input_file, nc_file){
  input_file <- getvarCMEM_ann(nc_file, "vosaline", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_ann(nc_file, "votemper", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_ann(nc_file, "o2", input_file, 0.25, mean, "mean")
}

all_dat_cmem_60m_ann <- xtracto_cmem_depth(input_file_ann, cmem_nc60)
head(all_dat_cmem_60m_ann)

#saveRDS(all_dat_cmem_60m_ann, here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_60m_ann.rds"))

#explore
cmem_60m_long <- gather(all_dat_cmem_60m_ann, covar, value, vosaline_mean:o2_mean) %>% mutate(PA = as.factor(PA))

ggplot(cmem_60m_long, aes(x = value, fill = PA)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~covar, scales = "free") + 
  theme_bw()+
  scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))

##### 250m extract ####
# dat extract ###
cmem_nc250 <- nc_open(here("data/enviro/psat_spot_all/all_processed/annual_res/dat_250m_annual.nc"))

all_dat_cmem_250m_ann <- xtracto_cmem_depth(input_file_ann, cmem_nc250)
head(all_dat_cmem_250m_ann)

#saveRDS(all_dat_cmem_250m_ann, here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_250m_ann.rds"))

#explore
cmem_250m_long <- gather(all_dat_cmem_250m_ann, covar, value, vosaline_mean:o2_mean) %>% mutate(PA = as.factor(PA))

ggplot(cmem_250m_long, aes(x = value, fill = PA)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~covar, scales = "free") + 
  theme_bw()+
  scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Seasonal CRW CMEM Extract ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 0m ####
cmem_0m_seas <- nc_open(here("data/enviro/psat_spot_all/all_processed/season_res/dat_0m_season.nc"))

xtracto_cmem = function(input_file, nc_file){
  input_file <- getvarCMEM_seas(nc_file, "somxlavt", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_seas(nc_file, "vosaline", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_seas(nc_file, "sossheig", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_seas(nc_file, "votemper", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_seas(nc_file, "vozocrtx", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_seas(nc_file, "sozotaux", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_seas(nc_file, "vomecrty", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_seas(nc_file, "sometauy", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_seas(nc_file, "o2", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_seas(nc_file, "chl", input_file, 0.25, mean, "mean")
  
}

all_dat_cmem_0m_seas <- xtracto_cmem(input_file_seas, cmem_0m_seas)

#combine with bathy above
all_cmem_covar_0m_seas <- cbind(all_dat_cmem_0m_seas, all_dat_bathy_cmem$bathy, all_dat_bathy_cmem$bathy_sd)
all_cmem_covar_0m_seas <- all_cmem_covar_0m_seas %>% 
  rename("bathy" = "all_dat_bathy_cmem$bathy", 
         "bathy_sd" = "all_dat_bathy_cmem$bathy_sd")
head(all_cmem_covar_0m_seas)

#saveRDS(all_cmem_covar_0m_seas, here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_0m_seas.rds"))

#explore
cmem_0m_long <- gather(all_cmem_covar_0m_seas, covar, value, somxlavt_mean:bathy_sd) %>% mutate(PA = as.factor(PA))

ggplot(cmem_0m_long, aes(x = value, fill = PA)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~covar, scales = "free") + 
  theme_bw()+
  scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))

#### 60m extract ####
# dat extract ###
cmem_nc60_seas <- nc_open(here("data/enviro/psat_spot_all/all_processed/season_res/dat_60m_season.nc"))

xtracto_cmem_depth = function(input_file, nc_file){
  input_file <- getvarCMEM_seas(nc_file, "vosaline", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_seas(nc_file, "votemper", input_file, 0.25, mean, "mean")
  input_file <- getvarCMEM_seas(nc_file, "o2", input_file, 0.25, mean, "mean")
}

all_dat_cmem_60m_seas <- xtracto_cmem_depth(input_file_seas, cmem_nc60_seas)
head(all_dat_cmem_60m_seas)

#saveRDS(all_dat_cmem_60m_seas, here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_60m_seas.rds"))

#explore
cmem_60m_long <- gather(all_dat_cmem_60m_seas, covar, value, vosaline_mean:o2_mean) %>% mutate(PA = as.factor(PA))

ggplot(cmem_60m_long, aes(x = value, fill = PA)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~covar, scales = "free") + 
  theme_bw()+
  scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))

#### 250m extract ####
# dat extract ###
cmem_nc250_seas <- nc_open(here("data/enviro/psat_spot_all/all_processed/season_res/dat_250m_season.nc"))

all_dat_cmem_250m_seas <- xtracto_cmem_depth(input_file_seas, cmem_nc250_seas)
head(all_dat_cmem_250m_seas)

#saveRDS(all_dat_cmem_250m_seas, here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_250m_seas.rds"))

#explore
cmem_250m_long <- gather(all_dat_cmem_250m_seas, covar, value, vosaline_mean:o2_mean) %>% mutate(PA = as.factor(PA))

ggplot(cmem_250m_long, aes(x = value, fill = PA)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~covar, scales = "free") + 
  theme_bw()+
  scale_fill_manual(values = c("dodgerblue4", "darkseagreen4"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Annual Back CMEM Extract ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~







### libraries #### 
library(tidyverse)
library(terra)
library(sf)
library(here)
library(ncdf4)

### data ####
merc_do0_raw <- rast(here("data/enviro/model_validate/Mercator/CMEMS_CHL_DO_0m_JAN2003_Dec2015_0.25deg_D.nc"))
merc_do200_raw <- rast(here("data/enviro/model_validate/Mercator/CMEMS_CHL_DO_200m_JAN2003_Dec2015_0.25deg_D.nc"))
merc_temp0_raw <- list.files(here("data/enviro/model_validate/Mercator"), pattern = "Temp_0m", full.names = TRUE)
merc_temp200_raw <- list.files(here("data/enviro/model_validate/Mercator"), pattern = "Temp_200m", full.names = TRUE)

clim_do_ann <- rast(here("data/enviro/model_validate/Climatology/woa_oxygen/woa18_all_o00_01.nc"))
clim_do_mo <- list.files(here("data/enviro/model_validate/Climatology/woa_oxygen/monthly"))
clim_tempWA <- rast(here("data/enviro/model_validate/Climatology/sst.mon.mean.nc"))

calcofi <- read.csv(here("data/enviro/model_validate/CalCofi_IMECOCAL/CAlCOFI/194903-202105_Bottle.csv"), check.names = FALSE)
#imecocal

### Mercator data ####
#### oxygen #### 
##### 0m #####
merc_do0_raw

#filter to separate variables by the "=" and prep to be able to filter by variable in next step
merc_vars0 <- str_split(names(merc_do0_raw), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")

#identify location of oxygen layers
merc_pos0 <- which(merc_vars0 == "o2")

#subset to only keep oxygen layers
merc_do0 <- merc_do0_raw %>% subset(merc_pos0) 
merc_do0 #spatial res: 0.25*, temporal es: daily, crs: lon/lat WGS 84

#convert temporal resolution to monthly and annually
merc_do_M0 <- tapp(merc_do0, "months", mean)
merc_do_Y0 <- tapp(merc_do0, "years", mean)

#convert monthly resolution to seasonal 
#winter
merc_do_Wn0 <- subset(merc_do_M0, c("m_12", "m_1", "m_2")) #Dec/Jan/Feb
merc_do_Wn0 <- app(merc_do_Wn0, mean)
#spring
merc_do_Sp0 <- subset(merc_do_M0, c("m_3", "m_4", "m_5")) #Mar/Apr/May
merc_do_Sp0 <- app(merc_do_Sp0, mean)
#summer
merc_do_Su0 <- subset(merc_do_M0, c("m_6", "m_7", "m_8")) #Jun/Jul/Aug
merc_do_Su0 <- app(merc_do_Su0, mean)
#fall
merc_do_Fa0 <- subset(merc_do_M0, c("m_9", "m_10", "m_11")) #Sep/Oct/Nov
merc_do_Fa0 <- app(merc_do_Fa0, mean)

##### 200m #####
#filter to separate variables by the "=" and prep to be able to filter by variable in next step
merc_vars200 <- str_split(names(merc_do200_raw), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")

#identify location of oxygen layers
merc_pos200 <- which(merc_vars200 == "o2")

#subset to only keep oxygen layers
merc_do200 <- merc_do200_raw %>% subset(merc_pos200) 
merc_do200 #spatial res: 0.25*, temporal es: daily, crs: lon/lat WGS 84

#convert temporal resolution to monthly and annually
merc_do_M200 <- tapp(merc_do200, "months", mean)
merc_do_Y200 <- tapp(merc_do200, "years", mean)

#convert monthly resolution to seasonal 
#winter
merc_do_Wn200 <- subset(merc_do_M200, c("m_12", "m_1", "m_2")) #Dec/Jan/Feb
merc_do_Wn200 <- app(merc_do_Wn200, mean)
#spring
merc_do_Sp200 <- subset(merc_do_M200, c("m_3", "m_4", "m_5")) #Mar/Apr/May
merc_do_Sp200 <- app(merc_do_Sp200, mean)
#summer
merc_do_Su200 <- subset(merc_do_M200, c("m_6", "m_7", "m_8")) #Jun/Jul/Aug
merc_do_Su200 <- app(merc_do_Su200, mean)
#fall
merc_do_Fa200 <- subset(merc_do_M200, c("m_9", "m_10", "m_11")) #Sep/Oct/Nov
merc_do_Fa200 <- app(merc_do_Fa200, mean)

#### temperature #### 
##### 0m #####
#combine all temp rasts into two rasters separated by depth layer

merc_temp_all0 <- NULL
for(i in 1:length(merc_temp0_raw)){
  # temp_nc <- nc_open(merc_temp0_raw[[i]])
  # 
  # #get correct lat and lon info from ncdf4 package
  # mod_rast <- rast(
  #   rast_lat <- ncvar_get(temp_nc, "nav_lat"),
  #   rast_lon <- ncvar_get(temp_nc, "nav_lon")
  # )
  # 
  # temp_lat <- rast_lat[1,1:241]
  # temp_lon <- rast_lon[1,1:241]
  
  temp_rast <- rast(merc_temp0_raw[[i]])
  rast_source <- sources(temp_rast)
  rast_year <- str_extract_all(rast_source, "\\d{4}")[[1]][1]
  
  d1 <- seq(as.Date(paste0(rast_year,"-","01-01")), by = "month",length = 6)
  d2 <- seq(as.Date(paste0(rast_year,"-","07-01")), by = "month",length = 6)
  
  #add year-month info to raster files
  if(str_detect(rast_source, "JAN") == TRUE){
    time(temp_rast) <- d1
  } else {
    time(temp_rast) <- d2
  }
  
  merc_temp_all0 <- append(merc_temp_all0, temp_rast)
}

merc_temp_all0 #spatial res: 0.25*, temporal es: year-month, crs: lon/lat WGS 84

#convert temporal resolution to monthly and annually
merc_temp_M0 <- tapp(merc_temp_all0, "months", mean)
merc_temp_Y0 <- tapp(merc_temp_all0, "years", mean)

#convert monthly resolution to seasonal 
#winter
merc_temp_Wn0 <- subset(merc_temp_M0, c("m_12", "m_1", "m_2")) #Dec/Jan/Feb
merc_temp_Wn0 <- app(merc_temp_Wn0, mean)
#spring
merc_temp_Sp0 <- subset(merc_temp_M0, c("m_3", "m_4", "m_5")) #Mar/Apr/May
merc_temp_Sp0 <- app(merc_temp_Sp0, mean)
#summer
merc_temp_Su0 <- subset(merc_temp_M0, c("m_6", "m_7", "m_8")) #Jun/Jul/Aug
merc_temp_Su0 <- app(merc_temp_Su0, mean)
#fall
merc_temp_Fa0 <- subset(merc_temp_M0, c("m_9", "m_10", "m_11")) #Sep/Oct/Nov
merc_temp_Fa0 <- app(merc_temp_Fa0, mean)

##### 200m #####
#combine all temp rasts into two rasters separated by depth layer
merc_temp_all200 <- NULL
for(i in 1:length(merc_temp200_raw)){
  temp_rast <- rast(merc_temp200_raw[[i]])
  rast_source <- sources(temp_rast)
  rast_year <- str_extract_all(rast_source, "\\d{4}")[[1]][1]
  
  d1 <- seq(as.Date(paste0(rast_year,"-","01-01")), by = "month",length = 6)
  d2 <- seq(as.Date(paste0(rast_year,"-","07-01")), by = "month",length = 6)
  
  #add year-month info to raster files
  if(str_detect(rast_source, "JAN") == TRUE){
    time(temp_rast) <- d1
  } else {
    time(temp_rast) <- d2
  }
  
  merc_temp_all200 <- append(merc_temp_all200, temp_rast)
}

merc_temp_all200 #spatial res: 0.25*, temporal es: year-month, crs: lon/lat WGS 84

#convert temporal resolution to monthly and annually
merc_temp_M200 <- tapp(merc_temp_all200, "months", mean)
merc_temp_Y200 <- tapp(merc_temp_all200, "years", mean)

#convert monthly resolution to seasonal 
#winter
merc_temp_Wn200 <- subset(merc_temp_M200, c("m_12", "m_1", "m_2")) #Dec/Jan/Feb
merc_temp_Wn200 <- app(merc_temp_Wn200, mean)
#spring
merc_temp_Sp200 <- subset(merc_temp_M200, c("m_3", "m_4", "m_5")) #Mar/Apr/May
merc_temp_Sp200 <- app(merc_temp_Sp200, mean)
#summer
merc_temp_Su200 <- subset(merc_temp_M200, c("m_6", "m_7", "m_8")) #Jun/Jul/Aug
merc_temp_Su200 <- app(merc_temp_Su200, mean)
#fall
merc_temp_Fa200 <- subset(merc_temp_M200, c("m_9", "m_10", "m_11")) #Sep/Oct/Nov
merc_temp_Fa200 <- app(merc_temp_Fa200, mean)

### Climatology surface data ####
#### oxygen ####
#spatial res: 0.1*, temporal res: averaged annually, temporal range: 1960-2017
clim_do_ann0W <- clim_do_ann$`o_mn_depth=0` 
clim_do_ann200W <- clim_do_ann$`o_an_depth=200`

e <- ext(-153, -104, 1, 49)

clim_do_ann0 <- crop(clim_do_ann0W, e)
clim_do_ann200 <- crop(clim_do_ann200W, e)

#need to do monthly and seasonal files
clim_do_mo

#### temperature ####
clim_tempWA #spatial res: 0.25*, temporal res: annual, temporal range: 1981-2024
plot(clim_tempWA)

e2 <- ext(207, 256, 1, 49)
clim_tempA <- crop(clim_tempWA, e2)
plot(clim_tempA$sst_1)

clim_tempA <- subset(clim_tempA, time(clim_tempA) > as.Date("2003-01-01"))
clim_tempA <- subset(clim_tempA, time(clim_tempA) < as.Date("2015-12-31"))

#need to separate by year, month, and season

### Station data ####
#### CalCOFI ####
##### 0m #####

##### 200m #####

#### IMECOCAL ####
##### 0m #####

##### 200m #####





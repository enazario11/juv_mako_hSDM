### libraries #### 
library(tidyverse)
library(terra)
library(sf)
library(here)
library(ncdf4)
library(respR)

## NOTE ABOUT DISSOLVED OXYGEN ##
#The units that DO were downloaded as vary by source. After spatially and temporally trimming/modifying each data file to the correct spatial/temporal resolutions of interest, I then convert the units to the final unit of mmol/L.

## General notes ##
# Can do unit conversions at surface, but will ask MP about methods for converting units at depth. Otherwise unsure if I can compare dissolved oxygen between products at 200 m. Still need to include IMECOCAL station data in analysis. 

### data ####
merc_do0_raw <- rast(here("data/enviro/model_validate/Mercator/CMEMS_CHL_DO_0m_JAN2003_Dec2015_0.25deg_D.nc"))
merc_do200_raw <- rast(here("data/enviro/model_validate/Mercator/CMEMS_CHL_DO_200m_JAN2003_Dec2015_0.25deg_D.nc"))
merc_temp0_raw <- list.files(here("data/enviro/model_validate/Mercator"), pattern = "Temp_0m", full.names = TRUE)
merc_temp200_raw <- list.files(here("data/enviro/model_validate/Mercator"), pattern = "Temp_200m", full.names = TRUE)

clim_do_ann <- rast(here("data/enviro/model_validate/Climatology/woa_oxygen/woa18_all_o00_01.nc"))
clim_do_mo <- list.files(here("data/enviro/model_validate/Climatology/woa_oxygen/monthly"), full.names = TRUE)
clim_tempWA <- rast(here("data/enviro/model_validate/Climatology/sst.mon.mean.nc"))

calcofi_samp <- read.csv(here("data/enviro/model_validate/CalCofi_IMECOCAL/CAlCOFI/194903-202105_Bottle.csv"), check.names = FALSE)
calcofi_met <- read.csv(here("data/enviro/model_validate/CalCofi_IMECOCAL/CAlCOFI/194903-202105_Cast.csv"), check.names = FALSE)
#imecocal

### Mercator data ####
#### oxygen (input: mmol/m^3, now: mmol/L) #### 
##### 0m #####
merc_do0_raw

#filter to separate variables by the "=" and prep to be able to filter by variable in next step
merc_vars0 <- str_split(names(merc_do0_raw), "=") %>% map_chr(\(x) x[1]) %>% str_extract("[^_]+")

#identify location of oxygen layers
merc_pos0 <- which(merc_vars0 == "o2")

#subset to only keep oxygen layers
merc_do0 <- merc_do0_raw %>% subset(merc_pos0) 
merc_do0 #spatial res: 0.25*, temporal es: daily, crs: lon/lat WGS 84

#convert DO to mmol/L
#convert temporal resolution to monthly and annually. Convert DO values from mmol/m^3 to mmol/L.
merc_do_M0 <- tapp(merc_do0, "months", mean)
merc_do_M0 <- merc_do_M0/1000
merc_do_Y0 <- tapp(merc_do0, "years", mean)
merc_do_Y0 <- merc_do_Y0/1000

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

#convert DO to mmol/L
#convert temporal resolution to monthly and annually. Convert DO from mmol/m^3 to mmol/L.
merc_do_M200 <- tapp(merc_do200, "months", mean)
merc_do_M200 <- merc_do_M200/1000
merc_do_Y200 <- tapp(merc_do200, "years", mean)
merc_do_Y200 <- merc_do_Y200/1000

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

#### temperature (C) #### 
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

### Climatology data ####
#### oxygen (input: umol/kg, now: mmol/L) ####
#define extent to study area
e <- ext(-153, -104, 1, 49)
##### 0m #####
#decadal values for o2 conversion at surface for annual average
temp_o20 <- rast(here("data/enviro/model_validate/Climatology/woa_oxygen/temp_for_conversions/woa18_decav_t00_01.nc"))
temp_o20 <- crop(temp_o20,e)
temp_o20 <- temp_o20$`t_an_depth=0`

sal_o20 <- rast(here("data/enviro/model_validate/Climatology/woa_oxygen/sal_for_conversions/woa18_decav_s00_01.nc"))
sal_o20 <- crop(sal_o20,e)
sal_o20 <- sal_o20$`s_an_depth=0`

#spatial res: 0.1*, temporal res: averaged annually, temporal range: 1960-2017
#annual
clim_do_ann0W <- clim_do_ann$`o_mn_depth=0` 

clim_do_ann0 <- crop(clim_do_ann0W, e)
#plot(clim_do_ann0)

#convert to mmol/L
clim_doY_mmol_L <- clim_do_ann0*1.025*0.001 #1.025 kg/L and 0.001mmol in umol
#plot(clim_doY_mmol_L)

#monthly and seasonal files
clim_do_mo0 = NULL
for(i in 1:length(clim_do_mo)){
  temp_rast <- rast(clim_do_mo[[i]])
  temp_rast_mean <- temp_rast$`o_mn_depth=0`
  temp_rast_mean <- crop(temp_rast_mean, e)
  
  rast_name <- paste0("m", "_", i)
  names(temp_rast_mean) <- rast_name
  
  clim_do_mo0 <- append(clim_do_mo0, temp_rast_mean)
}

#monthly
clim_doM_mmol_L <- clim_do_mo0*1.025*0.001

#convert monthly resolution to seasonal 
#winter
clim_do_Wn0 <- subset(clim_doM_mmol_L, c("m_12", "m_1", "m_2")) #Dec/Jan/Feb
clim_do_Wn0 <- app(clim_do_Wn0, mean)
#spring
clim_do_Sp0 <- subset(clim_doM_mmol_L, c("m_3", "m_4", "m_5")) #Mar/Apr/May
clim_do_Sp0 <- app(clim_do_Sp0, mean)
#summer
clim_do_Su0 <- subset(clim_doM_mmol_L, c("m_6", "m_7", "m_8")) #Jun/Jul/Aug
clim_do_Su0 <- app(clim_do_Su0, mean)
#fall
clim_do_Fa0 <- subset(clim_doM_mmol_L, c("m_9", "m_10", "m_11")) #Sep/Oct/Nov
clim_do_Fa0 <- app(clim_do_Fa0, mean)

##### 200m #####
#spatial res: 0.1*, temporal res: averaged annually, temporal range: 1960-2017
clim_do_ann200W <- clim_do_ann$`o_mn_depth=200` 
clim_do_ann200 <- crop(clim_do_ann200W, e)

clim_doY_mmol_L200 <- clim_do_ann200*1.025*0.001
#plot(clim_doY_mmol_L200)

#need to do monthly and seasonal files
clim_do_mo200 = NULL
for(i in 1:length(clim_do_mo)){
  temp_rast <- rast(clim_do_mo[[i]])
  temp_rast_mean <- temp_rast$`o_mn_depth=200`
  temp_rast_mean <- crop(temp_rast_mean, e)
  
  rast_name <- paste0("m", "_", i)
  names(temp_rast_mean) <- rast_name
  
  clim_do_mo200 <- append(clim_do_mo200, temp_rast_mean)
}

clim_doM_mmol_L200 <- clim_do_mo200*1.025*0.001

#convert monthly resolution to seasonal 
#winter
clim_do_Wn200 <- subset(clim_doM_mmol_L200, c("m_12", "m_1", "m_2")) #Dec/Jan/Feb
clim_do_Wn200 <- app(clim_do_Wn200, mean)
global(clim_do_Wn200, "mean", na.rm=TRUE)

#spring
clim_do_Sp200 <- subset(clim_doM_mmol_L200, c("m_3", "m_4", "m_5")) #Mar/Apr/May
clim_do_Sp200 <- app(clim_do_Sp200, mean)
global(clim_do_Sp200, "mean", na.rm=TRUE)

#summer
clim_do_Su200 <- subset(clim_doM_mmol_L200, c("m_6", "m_7", "m_8")) #Jun/Jul/Aug
clim_do_Su200 <- app(clim_do_Su200, mean)
global(clim_do_Su200, "mean", na.rm=TRUE)

#fall
clim_do_Fa200 <- subset(clim_doM_mmol_L200, c("m_9", "m_10", "m_11")) #Sep/Oct/Nov
clim_do_Fa200 <- app(clim_do_Fa200, mean)
global(clim_do_Fa200, "mean", na.rm=TRUE)

#### temperature (C) ####
##### 0m #####
#clim_tempWA #spatial res: 0.25*, temporal res: annual, temporal range: 1981-2024
#plot(clim_tempWA)

e2 <- ext(207, 256, 1, 49)
clim_tempA <- crop(clim_tempWA, e2)
#plot(clim_tempA$sst_1)

clim_tempA <- subset(clim_tempA, time(clim_tempA) > as.Date("2003-01-01"))
clim_tempA <- subset(clim_tempA, time(clim_tempA) < as.Date("2015-12-31"))
#clim_tempA

#convert temporal resolution to monthly and annually
clim_temp_M0 <- tapp(clim_tempA, "months", mean)
clim_temp_Y0 <- tapp(clim_tempA, "years", mean)

#convert monthly resolution to seasonal 
#winter
clim_temp_Wn0 <- subset(clim_temp_M0, c("m_12", "m_1", "m_2")) #Dec/Jan/Feb
clim_temp_Wn0 <- app(clim_temp_Wn0, mean)
#spring
clim_temp_Sp0 <- subset(clim_temp_M0, c("m_3", "m_4", "m_5")) #Mar/Apr/May
clim_temp_Sp0 <- app(clim_temp_Sp0, mean)
#summer
clim_temp_Su0 <- subset(clim_temp_M0, c("m_6", "m_7", "m_8")) #Jun/Jul/Aug
clim_temp_Su0 <- app(clim_temp_Su0, mean)
#fall
clim_temp_Fa0 <- subset(clim_temp_M0, c("m_9", "m_10", "m_11")) #Sep/Oct/Nov
clim_temp_Fa0 <- app(clim_temp_Fa0, mean)

### Station data ####
#### CalCOFI ####
#filter to correct dates
calcofi_met_filt <- calcofi_met %>%
  filter(Year >= 2003 & Year <= 2015)

calcofi_samp_filt <- calcofi_samp[calcofi_samp$Cst_Cnt %in% calcofi_met_filt$Cst_Cnt, ]

#subset metadata to just Cst_Cnt ID, year, and month to join to sample DF
calcofi_met_sub <- calcofi_met_filt %>% 
  subset(select = c(Cst_Cnt, Year, Month))

calcofi_samp_filt1 <- calcofi_samp_filt %>%
  full_join(calcofi_met_sub, by = "Cst_Cnt")

calcofi_samp_filt1 <- calcofi_samp_filt1 %>%
  mutate(season = ifelse(Month >= 3 & Month <= 5, "Spring", 
                         ifelse(Month >= 6 & Month <= 8, "Summer", 
                                ifelse(Month >= 9 & Month <= 11, "Fall", "Winter"))))

##### 0m #####
calcofi_samp0 <- calcofi_samp_filt1 %>% 
  filter(Depthm == 0) %>%
  filter(!is.na(O2ml_L))

#convert DO to mmol/L for each row either using the average temp/sal if data isn't available, or the in situ measurements at that station/bottle collection
mean_sal = mean(calcofi_samp0$Salnty, na.rm = TRUE)
mean_temp = mean(calcofi_samp0$T_degC, na.rm = TRUE)
calcofi_samp0$do0_mmol_L = NA

for(i in 1:nrow(calcofi_samp0)){
  if(is.na(calcofi_samp0$Salnty[i]) | is.na(calcofi_samp0$T_degC[i])){
    
   calcofi_samp0$do0_mmol_L[i] = convert_DO(calcofi_samp0$O2ml_L[i], from = "mL/L", to = "mmol/L", S = mean_sal, t = mean_temp, P = 1.013253)
   
  } else if(!is.na(calcofi_samp0$Salnty[i]) & !is.na(calcofi_samp0$T_degC[i])){
    
    calcofi_samp0$do0_mmol_L[i] = convert_DO(calcofi_samp0$O2ml_L[i], from = "mL/L", to = "mmol/L", S = calcofi_samp0$Salnty[i], t = calcofi_samp0$T_degC[i], P = 1.013253)
    
  }
  #print(calcofi_samp0$do0_mmol_L[i])
  }

###### oxygen (input: ml/L, now: mmol/L) ######
#annual 
calcofi_samp0 %>% 
  group_by(Year) %>% 
  summarise(mean_do0_mmol_L = mean(do0_mmol_L, na.rm = TRUE))

#monthly
calcofi_samp0 %>% 
  group_by(Month) %>% 
  summarise(mean_do0_mmol_L = mean(do0_mmol_L, na.rm = TRUE))

#seasonal 
calcofi_samp0 %>% 
  group_by(season) %>% 
  summarise(mean_do0_mmol_L = mean(do0_mmol_L, na.rm = TRUE))

###### temperature (C) #####
#annual 
calcofi_samp0 %>% 
  group_by(Year) %>% 
  summarise(mean_temp0_C = mean(T_degC, na.rm = TRUE))

#monthly
calcofi_samp0 %>% 
  group_by(Month) %>% 
  summarise(mean_temp0_C = mean(T_degC, na.rm = TRUE))

#seasonal 
calcofi_samp0 %>% 
  group_by(season) %>% 
  summarise(mean_temp0_C = mean(T_degC, na.rm = TRUE))

##### 200m #####
calcofi_samp200 <- calcofi_samp_filt1 %>%
  filter(Depthm == 200) %>%
  filter(!is.na(O2ml_L))

#convert DO to mmol/L for each row either using the average temp/sal if data isn't available, or the in situ measurements at that station/bottle collection
mean_sal200 = mean(calcofi_samp200$Salnty, na.rm = TRUE)
mean_temp200 = mean(calcofi_samp200$T_degC, na.rm = TRUE)
calcofi_samp200$do200_mmol_L = NA 

# Constants for the calculation of the saturation concentration of O2 in sea water (from Sarmiento & Gruber (2006), Garcia & Gorden (1992)):
b1 = 2.00907
b2 = 3.22014
b3 = 4.05010
b4 = 4.94457
b5 = 0.256847
b6 = 3.88767
b7 = 0.00624523
b8 = 0.00737614
b9 = 0.010341
b10 = 0.00817083
b11 = 0.000000488682

# Other constants:
R = 8.31             # Gas constant [J/(mol*K)]
GSmv = 22.3916       # Molar volume O2 [L/mol] (from Sarmiento & Gruber (2006))
pmv = 0.000032       # Partial molar volume O2 [m3/mol] (from Deutsch et al. (2015, Science))
KC = 273.15          # Kelvin conversion [K]
Ks = 0.000086173     # Boltzmann constant [J/K]

#calc in-situ density of seawater
pressure = (1025 * 9.81 * 200) + 101325 #avg density of sea water (kg/m^3) * gravity * depth + atmospheric pressure at surface in Pa
press_200 = pressure / 1000000 #get calculated pressure into dbar

for(i in 1:nrow(calcofi_samp0)){
  if(is.na(calcofi_samp200$Salnty[i]) | is.na(calcofi_samp200$T_degC[i])){
    
    Tpot = mean_temp200 #temp in C
    T_scaled = log((298.15 - Tpot) / (KC + Tpot))
    rho = gsw::gsw_rho(SA = mean_sal200, p = press_200, CT = Tpot)
    
    #calculate saturation concentration of O2 in seawater
    l = b1+b2*T_scaled+b3*(T_scaled^2)+b4*(T_scaled^3)+b5*(T_scaled^4)+b6*(T_scaled^5)+mean_sal200*(b7+b8*T_scaled+b9*(T_scaled^2)+b10*(T_scaled^3)) + b11*mean_sal200^2
    calc_sat_o2 = (1000/GSmv) * exp(l) #mmol/m^3
    calcofi_samp0$do0_mmol_L[i] = calc_sat_o2/1000 #mmol/L
    
  } else if(!is.na(calcofi_samp200$Salnty[i]) & !is.na(calcofi_samp200$T_degC[i])){
    
    Tpot = calcofi_samp200$T_degC[i] #temp in C
    T_scaled = log((298.15 - Tpot) / (KC + Tpot))
    rho = gsw::gsw_rho(SA = calcofi_samp200$Salnty[i], p = press_200, CT = Tpot)
    
    #calculate saturation concentration of O2 in seawater
    l = b1+b2*T_scaled+b3*(T_scaled^2)+b4*(T_scaled^3)+b5*(T_scaled^4)+b6*(T_scaled^5)+calcofi_samp200$Salnty[i]*(b7+b8*T_scaled+b9*(T_scaled^2)+b10*(T_scaled^3)) + b11*calcofi_samp200$Salnty[i]^2
    calc_sat_o2 = (1000/GSmv) * exp(l) #mmol/m^3
    calcofi_samp200$do200_mmol_L[i] = calc_sat_o2/1000 #mmol/L
    
  }
  print(calcofi_samp200$do200_mmol_L[i])
}

###### oxygen (ml/L) ######
#annual 
calcofi_samp200 %>% 
  group_by(Year) %>% 
  summarise(mean_do0_ml_L = mean(O2ml_L, na.rm = TRUE))

#monthly
calcofi_samp200 %>% 
  group_by(Month) %>% 
  summarise(mean_do0_ml_L = mean(O2ml_L, na.rm = TRUE))

#seasonal 
calcofi_samp200 %>% 
  group_by(season) %>% 
  summarise(mean_do0_ml_L = mean(O2ml_L, na.rm = TRUE))

###### temperature (C) #####
#annual 
calcofi_samp200 %>% 
  group_by(Year) %>% 
  summarise(mean_temp0_C = mean(T_degC, na.rm = TRUE))

#monthly
calcofi_samp200 %>% 
  group_by(Month) %>% 
  summarise(mean_temp0_C = mean(T_degC, na.rm = TRUE))

#seasonal 
calcofi_samp200 %>% 
  group_by(season) %>% 
  summarise(mean_temp0_C = mean(T_degC, na.rm = TRUE))

#### IMECOCAL ####
##### 0m #####

##### 200m #####





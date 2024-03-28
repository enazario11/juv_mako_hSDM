library(ggplot2)
library(tidyverse)
library(ggpubr)
library(raster)
library(oce)
library(chron)
library(lubridate)
library(tidyquant)
library(scales)
library(tidync)
library(data.table)
library(ggOceanMaps)
library(RColorBrewer)
library(data.table)
library(ncdf4)
library(viridis)
library(here)
library(terra)

### Test mako file -- Nerea method###
#-----------------------------------
test <- list.files("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/WC_dat/PTT folders/72808")
setwd("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/WC_dat/PTT folders/72808")
gpeFiles <- list.files(pattern='GPE3.csv')

    #modify loc file format
track <- data.frame()
for (i in seq_along(gpeFiles)) {
  # Read in the GPE file skipping the header
  a <- read.csv(gpeFiles[i], skip=5)
  # Grab the speed from the header of the GPE file. This assumes that all GPE headers are the same. Worked for mine and one of Melanies tags
  # Read in the second line of the .csv GPE file header (it's the one that has the speed information in it)
  h <- readLines(gpeFiles[i], n=2)[2]
  # Grab the first number in the character string
  speeds <- as.numeric(str_extract(h, "//-*//d+//.*//d*"))
  track <- rbind(track, cbind(a, speed=speeds))
}
names(track)[4:5] <- c('Lat','Lon')
head(track)

lonMax <- max(track$Lon)+5
lonMin <- min(track$Lon)-5
latMax <- max(track$Lat)+5
latMin <- min(track$Lat)-5


#----------------------------------------
### Test with all points -- load the data (get min/max lat/lon)
#----------------------------------------
loc_dat <- read.csv("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/tdl_scbE.csv")

loc_dat <- loc_dat %>%
  mutate(ptt = as.factor(ptt),
         Sex = as.factor(Sex),
         posix = as.POSIXct(strptime(posix, format = "%Y-%m-%d")), 
         hour = hour(posix),
         date = substr(posix, 1, 10),
         month = str_pad(month, 2, pad = "0"),
         day = str_pad(day, 2, pad = "0"),
         mo_day = paste0(month,day), 
         mo_day = as.numeric(mo_day), 
         year = as.factor(year),
         depth_neg = med_depth*-1,
         ID = as.factor(ID), 
         region = as.factor(region)) %>%
  filter(med_depth < 120, 
         max_depth < 1000, 
         avg_depth < 120) %>%
  rename(sex = Sex)

length(unique(loc_dat$date)) #670 unique days

lonMax <- max(loc_dat$Lon)+3
lonMin <- min(loc_dat$Lon)-3
latMax <- max(loc_dat$Lat)+3
latMin <- min(loc_dat$Lat)-3

  #get season 
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

loc_dat$season <- getSeason(loc_dat$date)

###Min date of deployment: 06/28/2004
###Max date of deployment: 12/06/2013

#-------
#bathy (ggOceanMaps and gebco)
#-------
##get low resolution bathy dat from gebco
new_locs <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_0m.rds"))
new_locs <- new_locs %>% filter(PA == 0)
r = rast(here("data/enviro/bathy/GEBCO_2023_n58.0_s4.0_w-147.0_e-97.0.nc"))
res(r)
r_lowres <- aggregate(r, fact=5)

bathy_df <- as.data.frame(r_lowres,xy = TRUE)

  ##bathy map by age class -- OceanMap (runs faster but figure not as nice)
bathy_lim <- data.frame(lon = c(-140, -110), lat = c(10, 50))
basemap(data = bathy_lim, bathymetry = TRUE) +
  geom_point(data = new_locs, aes(x = lon, y = lat), color = "gray99", shape = 1, alpha = 0.8) + 
  #facet_wrap(~age_class)+
  theme_tq()+
  theme(legend.position = "right")

  ##bathy map by age class -- gebco data (takes a while)
#filter so bathy values are only positive
bathy_df <- bathy_df %>%
  filter(elevation <= 0)

save(bathy_df, file = "C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/bathy/df_bathy.RData")

#define map for figure
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="USA"
                      | north_map$region=="Mexico",]
#get enough colors for figure (manually or expand palette)
#bathy_cols <- 15
#mycolors <- colorRampPalette(brewer.pal(9, "Blues"))(bathy_cols)

df_bathy <- get(load("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/bathy/df_bathy.RData"))

mycolors <- c("#08306B","#023858", "#034B76", "#0B559F", "#045D92","#0469A6","#1379B4","#2F8BBD","#6BAED6", "#509AC6","#74A9CF","#88BEDC", "#90B4D5","#ACBFDC","#C4CBE2" )
show_col(mycolors)

ggplot(shore, aes(long, lat)) +
  coord_map("mercator", xlim=c(-140, -110), ylim=c(10,50)) +
  geom_polygon(aes(group=group), fill="grey60",lwd=1) +
  geom_contour_filled(data=bathy_df, 
                      aes(x,y,z= elevation)) +#breaks=seq(0,1,by=0.2)
  scale_fill_manual(values = mycolors)+
  geom_point(data = new_locs, aes(x = lon, y = lat), color = "goldenrod", shape = 1, alpha = 0.8)+
  theme_tq()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(title="Bathymetry (m)"))

  ##extract resolutions from raster to loc_dat df corresponding to presence data lat/lons
loc_enviro <- read.csv("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/loc_enviro.csv")
loc_enviro$bathy = raster::extract(r_lowres, loc_enviro[,c("Lon","Lat")])
write.csv(loc_enviro,"C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/loc_enviro.csv")

  ##histo of bathy by age class 
ggplot(loc_dat, aes(x = bathy))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~age_class, scales = "free")+
  xlab("Bathymetry (m)")

  ##histo of bathy be region
ggplot(loc_dat, aes(x = bathy))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~region, scales = "free")+
  xlab("Bathymetry (m)")
       
  ##histo of bathy by season (spring removed becuase not enough data)
loc_dat %>%
  filter(season != "Spring") %>%
  ggplot(aes(x = bathy))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~season, scales = "free")+
  xlab("Bathymetry (m)")

#----------------
#SST and MLD Data (monthly average)
#----------------
#SST -- data for plotting maps
nc = nc_open("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/enviro/test/phys_vars/cmems_mod_glo_phy_my_0.083_P1M-m_1676331677420.nc")
nc
ncatt_get(nc,"time","units") # to get the origin time

df_nc <- tidync("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/enviro/test/phys_vars/cmems_mod_glo_phy_my_0.083_P1M-m_1676331677420.nc")

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% data.table::setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df_sst = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 
df_sst <- df_sst %>%
  filter(latitude > 25 & latitude < 39 & longitude < -110 & longitude > -126)
save(df_sst, file = "C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/phys_vars/df_sst.RData")

df_sst
max(df_sst$time)
min(df_sst$time)
max(df_sst$time)-min(df_sst$time) #3409.5 days total with 113 unique days (bands?)?? 
length(unique(df_sst$time))
head(df_sst)
sum(is.na(df_sst$thetao))
str(df_sst)

#SST -- extracting to loc_dat at a MONTHLY resolution. Data for histos
r = raster("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/phys_vars/cmems_mod_glo_phy_my_0.083_P1M-m_1676331677420.nc")
plot(r, col=viridis(64))
nbands(r)

# extract values at animal's coordinates for every date
loc_dat$date = as.character(loc_dat$date)
loc_dat$yr_mo = substr(loc_dat$date, 1,7)

system.time({   # 1.5min 
  for (i in 1:nbands(r)) {  # 113 dates
    setwd("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/phys_vars")
    r0 = raster("cmems_mod_glo_phy_my_0.083_P1M-m_1676331677420.nc", band = i)
    date_r = as.character(substr(r0@z,1,10))
    yr_mo_r = as.character(substr(r0@z,1,7))
    
    # extract variable in each location for each date
    loc_dat$sst[loc_dat$yr_mo==yr_mo_r]=raster::extract(r0, loc_dat[loc_dat$yr_mo==yr_mo_r,c("Lon","Lat")])
  }
})

min(loc_dat$sst, na.rm = T)
max(loc_dat$sst, na.rm = T)
sum(is.na(loc_dat$sst)) #191 NAs

#MLD (must do separately, tidync does not read with other vars for some reason...) -- data for plotting for maps
nc = nc_open("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/phys_vars/CMEMS_MLD_28JUN2003_06DEC2014_PHY-030M.nc")
nc
ncatt_get(nc,"time","units") # to get the origin time

df_nc <- tidync("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/phys_vars/CMEMS_MLD_28JUN2003_06DEC2014_PHY-030M.nc")

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df_mld = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 

df_mld
max(df_mld$time)
min(df_mld$time)
max(df_mld$time)-min(df_mld$time)
length(unique(df_mld$time))
head(df_mld)
sum(is.na(df_mld$mlotst))

#MLD -- extract to loc_dat -- data for histos
r = raster("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/phys_vars/CMEMS_MLD_28JUN2003_06DEC2014_PHY-030M.nc")
plot(r, col=viridis(64))
nbands(r)

# extract values at animal's coordinates for every date
loc_dat$date = as.character(loc_dat$date)
loc_dat$yr_mo = substr(loc_dat$date, 1,7)

system.time({   # 2min
  for (i in 1:nbands(r)) {    # 137 dates
    setwd("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/phys_vars")
    r0 = raster("CMEMS_MLD_28JUN2003_06DEC2014_PHY-030M.nc", band = i) 
    date_r = as.character(substr(r0@z,1,10))
    yr_mo_r = as.character(substr(r0@z,1,7))
    
    # extract variable in each location for each date
    loc_dat$mld[loc_dat$yr_mo==yr_mo_r]=raster::extract(r0, loc_dat[loc_dat$yr_mo==yr_mo_r,c("Lon","Lat")])
  }
})

df_mld <- df_mld %>%
  filter(latitude > 25 & latitude < 39 & longitude < -110 & longitude > -126)
save(df_mld, file = "C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/phys_vars/df_mld.RData")

min(loc_dat$mld, na.rm = T) #10.07
max(loc_dat$mld, na.rm = T) #84.078
sum(is.na(loc_dat$mld)) #158 NAs

##Save final loc_dat csv with SST and MLD values
write.csv(loc_dat,"C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/loc_enviro.csv")

#--------------------
# SST and MLD Figures
#---------------------
loc_enviro <- read.csv("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/loc_enviro.csv")

loc_dat <- read.csv("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/tdl.csv")

loc_dat <- loc_dat %>%
  mutate(ptt = as.factor(ptt),
         Sex = as.factor(Sex),
         posix = as.POSIXct(strptime(posix, format = "%m/%d/%Y %H:%M")), 
         time = substr(posix, 11, 18),
         hour = hour(posix),
         date = substr(posix, 1, 10),
         month = str_pad(month, 2, pad = "0"),
         day = str_pad(day, 2, pad = "0"),
         mo_day = paste0(month,day), 
         mo_day = as.numeric(mo_day), 
         year = as.factor(year),
         depth_neg = med_depth*-1,
         age_class = as.factor(age_class), 
         ID = as.factor(ID), 
         region = as.factor(region)) %>%
  filter(med_depth < 120, 
         max_depth < 1000, 
         avg_depth < 120) %>%
  rename(sex = Sex)
loc_dat$age_class <- factor(loc_dat$age_class, levels = c("Adult", "SA", "Age-2", "YOY"))
loc_dat$yr_mo <- loc_enviro$yr_mo
loc_dat$season <- loc_enviro$season
loc_dat$sst <- loc_enviro$sst
loc_dat$mld <- loc_enviro$mld
loc_dat$DO100 <- loc_enviro$DO100
loc_dat$DO500 <- loc_enviro$DO500
loc_dat$bathy <- loc_enviro$bathy

#plot tracks vs. SST data
#define map for figure
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="USA"
                      | north_map$region=="Mexico",]

df_sst <- get(load("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/phys_vars/df_sst.RData"))

#plot map w/ tracks limited to the SCB
ggplot(shore, aes(long, lat)) +
  coord_map("mercator", xlim=c(-125, -109), ylim=c(26,38)) +
  geom_polygon(aes(group=group), fill="grey60",lwd=1) +
  geom_contour_filled(data=df_sst, 
                      aes(longitude,latitude,z=thetao), alpha = 0.7) +#breaks=seq(0,1,by=0.2)
  scale_fill_manual(values = rev(brewer.pal(9, "RdYlBu")))+
  geom_point(data = loc_dat, aes(x = Lon, y = Lat), color = "black", shape = 1)+
  theme_tq()+
  theme(legend.position = "right")

#histos by age class
ggplot(loc_dat, aes(x = sst))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~age_class, scales = "free")+
  xlab("SST (C)")

#histos by region 
ggplot(loc_dat, aes(x = sst))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~region, scales = "free")+
  xlab("SST (C)")

#histos by season -- spring removed because not enough data for histo
loc_dat %>%
  filter(season != "Spring") %>%
  ggplot(aes(x = sst))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~season, scales = "free")+
  xlab("SST (C)")

#plot tracks vs. MLD data
#north_map = map_data("world") %>% group_by(group)
#shore     = north_map[north_map$region=="Canada" 
                      #| north_map$region=="USA"
                      #| north_map$region=="Mexico",]
df_mld <- get(load("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/phys_vars/df_mld.RData"))
head(df_mld)
class(df_mld)
str(df_mld)

ggplot(shore, aes(long, lat)) +
  coord_map("mercator", xlim=c(-125, -109), ylim=c(26,38)) +
  geom_polygon(aes(group=group), fill="grey60",lwd=1) +
  geom_contour_filled(data=df_mld, 
                      aes(longitude,latitude,z=mlotst), alpha = 0.7) +#breaks=seq(0,1,by=0.2)
  scale_fill_manual(values = c("#C4CBE2","#90B4D5","#6BAED6", "#0469A6","#034B76","#023858", "#08306B")) +
  geom_point(data = loc_dat, aes(x = Lon, y = Lat), color = "gold", shape = 1)+
  theme_tq()+
  theme(legend.position = "right")

#histos by age class
ggplot(loc_dat, aes(x = mld))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~age_class, scales = "free")+
  xlab("MLD (m)")

#histos by region 
ggplot(loc_dat, aes(x = mld))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~region, scales = "free")+
  xlab("MLD (m)")

#histos by season -- spring removed because not enough data for histo
loc_dat %>%
  filter(season != "Spring") %>%
  ggplot(aes(x = mld))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~season, scales = "free")+
  xlab("MLD (m)")

#-----------------------------
#DO Data (100m and 500m depth)
#-----------------------------
#DO100 -- data for plotting maps
nc = nc_open("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/biol_vars/CMEMS_DO100_01Jan2004_01Jan2014_glo_bgc_P1M.nc")
nc
ncatt_get(nc,"time","units") # to get the origin time

df_nc <- tidync("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/biol_vars/CMEMS_DO100_01Jan2004_01Jan2014_glo_bgc_P1M.nc")

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df_do100 = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 
save(df_do100, file = "C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/biol_vars/df_do100.RData")

df_do100
max(df_do100$time)
min(df_do100$time)
length(unique(df_do100$time))
head(df_do100)
sum(is.na(df_do100$o2)) #0 NA
str(df_do100)

#DO100 -- extracting to loc_dat at a MONTHLY resolution. Data for histos
r = raster("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/biol_vars/CMEMS_DO100_01Jan2004_01Jan2014_glo_bgc_P1M.nc")
plot(r, col=viridis(64))
nbands(r)

# extract values at animal's coordinates for every date
system.time({   #1 sec  
  for (i in 1:nbands(r)) {  # 120 dates
    setwd("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/biol_vars")
    r0 = raster("CMEMS_DO100_01Jan2004_01Jan2014_glo_bgc_P1M.nc", band = i)
    yr_mo_r = as.character(substr(r0@z,1,7))
    
    # extract variable in each location for each date
    loc_dat$DO100[loc_dat$yr_mo==yr_mo_r]=raster::extract(r0, loc_dat[loc_dat$yr_mo==yr_mo_r,c("Lon","Lat")])
  }
})

min(loc_dat$DO100, na.rm = T) #3.24
max(loc_dat$DO100, na.rm = T) #258.14
sum(is.na(loc_dat$DO100)) #1443 NAs

#DO500 -- data for plotting maps
nc = nc_open("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/biol_vars/CMEMS_DO500_01Jan2004_01Jan2014_glo_bgc_P1M.nc")
nc
ncatt_get(nc,"time","units") # to get the origin time

df_nc <- tidync("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/biol_vars/CMEMS_DO500_01Jan2004_01Jan2014_glo_bgc_P1M.nc")

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df_do500 = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 
save(df_do500, file = "C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/biol_vars/df_do500.RData")

df_do500
max(df_do500$time)
min(df_do500$time)
length(unique(df_do500$time))
head(df_do500)
sum(is.na(df_do500$o2)) #0 NA
str(df_do500)

#DO500 -- extracting to loc_dat at a MONTHLY resolution. Data for histos
r = raster("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/biol_vars/CMEMS_DO500_01Jan2004_01Jan2014_glo_bgc_P1M.nc")
plot(r, col=viridis(64))
nbands(r)

# extract values at animal's coordinates for every date
system.time({   #1 sec  
  for (i in 1:nbands(r)) {  # 120 dates
    setwd("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/biol_vars")
    r0 = raster("CMEMS_DO500_01Jan2004_01Jan2014_glo_bgc_P1M.nc", band = i)
    yr_mo_r = as.character(substr(r0@z,1,7))
    
    # extract variable in each location for each date
    loc_dat$DO500[loc_dat$yr_mo==yr_mo_r]=raster::extract(r0, loc_dat[loc_dat$yr_mo==yr_mo_r,c("Lon","Lat")])
  }
})

min(loc_dat$DO500, na.rm = T) #1.00
max(loc_dat$DO500, na.rm = T) #60.65
sum(is.na(loc_dat$DO500)) #1903 NAs

#save csv file with all current enviro data (sst, mld, do100, and do500)
write.csv(loc_dat,"C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/loc_enviro.csv")

#--------------------------------
#DO Figures (100m and 500m depth)
#--------------------------------
#DO100 
#plot tracks vs. MLD data
#north_map = map_data("world") %>% group_by(group)
#shore     = north_map[north_map$region=="Canada" 
#| north_map$region=="USA"
#| north_map$region=="Mexico",]
df_do100 <- get(load("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/biol_vars/df_do100.RData"))
head(df_do100)
class(df_do100)
str(df_do100)

#map of tracks over DO values at 100m depth
ggplot(shore, aes(long, lat)) +
  coord_map("mercator", xlim=c(-125, -109), ylim=c(26,38)) +
  geom_polygon(aes(group=group), fill="grey60",lwd=1) +
  geom_contour_filled(data=df_do100, 
                      aes(longitude,latitude,z=o2), alpha = 0.7) +#breaks=seq(0,1,by=0.2)
  scale_fill_manual(values = c("#08306B","#023858", "#034B76", "#0B559F", "#045D92","#0469A6","#1379B4","#2F8BBD","#6BAED6", "#509AC6","#74A9CF","#88BEDC", "#90B4D5","#ACBFDC","#C4CBE2")) +
  geom_point(data = loc_dat, aes(x = Lon, y = Lat), color = "gold", shape = 1)+
  facet_wrap(~age_class)+
  theme_tq()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(title="DO at 100m (mmol/m^3)"))
  

#histos by age class
ggplot(loc_dat, aes(x = DO100))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~age_class, scales = "free")+
  xlab("DO at 100m (mmol/m^3)")

#histos by region 
ggplot(loc_dat, aes(x = DO100))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~region, scales = "free")+
  xlab("DO at 100m (mmol/m^3)")

#histos by season -- spring removed because not enough data for histo
loc_dat %>%
  filter(season != "Spring") %>%
  ggplot(aes(x = DO100))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~season, scales = "free")+
  xlab("DO at 100m (mmol/m^3)")

#DO500 
#plot tracks vs. MLD data
#north_map = map_data("world") %>% group_by(group)
#shore     = north_map[north_map$region=="Canada" 
#| north_map$region=="USA"
#| north_map$region=="Mexico",]
df_do500 <- get(load("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/enviro/biol_vars/df_do500.RData"))
head(df_do500)
class(df_do500)
str(df_do500)

ggplot(shore, aes(long, lat)) +
  coord_map("mercator", xlim=c(-125, -109), ylim=c(26,38)) +
  geom_polygon(aes(group=group), fill="grey60",lwd=1) +
  geom_contour_filled(data=df_do500, 
                      aes(longitude,latitude,z=o2), alpha = 0.7) +#breaks=seq(0,1,by=0.2)
  scale_fill_manual(values = c("#08306B","#023858", "#034B76", "#0B559F", "#045D92","#0469A6","#1379B4","#2F8BBD","#6BAED6", "#509AC6","#74A9CF","#88BEDC", "#90B4D5","#ACBFDC","#C4CBE2")) +
  geom_point(data = loc_dat, aes(x = Lon, y = Lat), color = "gold", shape = 1)+
  facet_wrap(~age_class)+
  theme_tq()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(title="DO at 500m (mmol/m^3)"))

#histos by age class
ggplot(loc_dat, aes(x = DO500))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~age_class, scales = "free")+
  xlab("DO at 500m (mmol/m^3)")

#histos by region 
ggplot(loc_dat, aes(x = DO500))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~region, scales = "free")+
  xlab("DO at 500m (mmol/m^3)")

#histos by season -- spring removed because not enough data for histo
loc_dat %>%
  filter(season != "Spring") %>%
  ggplot(aes(x = DO500))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~season, scales = "free")+
  xlab("DO at 500m (mmol/m^3)")

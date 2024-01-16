library(raster)
library(ncdf4)

# using raster package (not recommended!)
#------------------------------------------
r = raster("./ENV.DATA/sst_01Aug2007-31Oct2007_PHY-030.nc", band=2)
plot(r, col=viridis(64))
nbands(r)

# extract values at animal's coordinates for every date
#-------------------------------------------------------
loc$date = as.character(loc$date)
system.time({   # 40 sec
  for (i in 1:nbands(r)) {    # 92 dates
    # setwd("C:/Users/admin/Documents/POST-DOC/2018/Bowhead_Whales/PAPER3/ENV.DATA/Daily/PHY_001_024")
    r0 = raster("sst_01Aug2007-31Oct2007_PHY-030.nc",band=i) 
    date = as.character(substr(r0@z,1,10))
    
    # extract variable in each location for each date
    loc$sst[loc$date==date]=extract(r0,loc[loc$date==date,c("lon","lat")])
  }
})

# using netcdf4 or tidync
#-------------------------
#https://pjbartlein.github.io/REarthSysSci/netCDF.html
# nc = nc_open("./ENV.DATA/sst_01Aug2007-31Oct2007_PHY-030.nc")
# lon = ncvar_get(nc,"longitude")
# range(lon)
# # lon[lon > 180] <- lon[lon > 180] - 360
# lat = ncvar_get(nc,"latitude")
# 
# # get time
# time <- ncvar_get(nc,"time")
# time





##############################
# better way of opening ncdf
##############################
library(tidync)
library(data.table)
library(tidyverse)
library(tidyquant)
nc = nc_open("./ENV.DATA/sst_ssh_sss_mld_u_v_01Aug2007-31Oct2007_PHY-030.nc")
nc
ncatt_get(nc,"time","units") # to get the origin time


df_nc <- tidync("./ENV.DATA/sst_ssh_sss_mld_u_v_01Aug2007-31Oct2007_PHY-030.nc") 

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 
df
first(df$time)
last(df$time)

# plot thetao using geom_raster
#-------------------------------
ggplot(df, aes(longitude, latitude)) +
  geom_raster(aes(fill=thetao))
  geom_tile(aes(fill=thetao))

# plot using geom_contour_filled + coastline
#-------------------------------------------
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="USA"
                      | north_map$region=="Mexico",]
  #
ggplot(shore, aes(long, lat)) +
  coord_map("mercator", xlim=c(-126, -110), ylim=c(25,39)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_contour_filled(data=df, 
                      aes(longitude,latitude,z=uo)) +#breaks=seq(0,1,by=0.2)
  scale_fill_brewer(palette="BuPu") +
  theme_tq()

ggplot(shore, aes(long, lat)) +
  geom_raster(data=df, aes(longitude,latitude,z=thetao)) +
  scale_fill_brewer(palette="BuPu") 




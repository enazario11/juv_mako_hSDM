#libs
library(tidyverse)
library(here)
library(stars)

#merc temp data at 0m
merc_temp0_raw <- list.files(here("data/enviro/model_validate/Mercator"), pattern = "Temp_0m", full.names = TRUE)

### stars practice
#read in
rasterio = list(nXOff = 6, nYOff = 6, nXSize = 100, nYSize = 100)

test_multi <- read_stars(merc_temp0_raw, quiet = TRUE, along = "time_counter")
test_stars <- read_stars(merc_temp0_raw[1])
test_nc <- read_ncdf(merc_temp0_raw[1])

test_stars <- read_stars(merc_temp0_raw[1]) %>% 
  st_set_dimensions("time_counter", as.POSIXct(st_get_dimension_values(., "time_counter"))) %>% 
  setNames("votemper")

dim(test_stars)
dim(test_nc)
dim(test_multi)

# change spatial res 
rasterio_lowres = list(nXSize = 100, nYSize = 100,
                nBufXSize = 25, nBufYSize = 25)
test_stars_lowres <- read_stars(merc_temp0_raw[1], RasterIO = rasterio_lowres)

# warp curvilinear to regular -- errors
dest <- st_bbox(test_stars) %>% 
  st_as_stars(dx = 0.25, dy = 0.25, crs = "EPSG:4326")
test_reg = st_warp(test_stars, dest)

#reprojecting a curvilinear raster 
  #create target grid 
test_stars %>% st_transform("+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_bbox() %>%
  st_as_stars() -> newgrid

  #warp old raster to new 
test_stars %>% st_warp(newgrid) -> nc.new



####### old method for final formatting ########
#combine all temp rasts. Fix temporal and spatial data.
resamp_geom <- rast(ext(-153, -104, 1, 49), 
                    crs = "EPSG:4326",
                    res = 0.25)

merc_temp_all0 <- NULL
for(i in 1:length(merc_temp0_raw)){
  #load temperature raster
  temp_rast <- read_ncdf(merc_temp0_raw[[i]])
  
  #adjust time information
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
  
  #resample to correct geometry
  ext(temp_rast) <- c(-153, -104, 1, 49)
  temp_resamp <- resample(temp_rast, resamp_geom, method='bilinear')
  
  #save as one raster 
  merc_temp_all0 <- append(merc_temp_all0, temp_resamp)
}

merc_temp_all0 #spatial res: 0.25*, temporal es: year-month, crs: lon/lat WGS 84


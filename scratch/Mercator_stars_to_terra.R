library(terra)
library(here)
library(ncdf4)
library(stars)
library(stringr)

# List of Mercator files
merc_temp0_raw <- list.files(here("data/enviro/model_validate/Mercator"), pattern = "Temp_0m", full.names = TRUE)
merc_temp200_raw <- list.files(here("data/enviro/model_validate/Mercator"), pattern = "Temp_200m", full.names = TRUE)

### temperature 0m ####
merc_temp_all0 <- NULL
for(i in 1:length(merc_temp0_raw)){
  # As stars object
    merc_stars <- read_stars(merc_temp0_raw[i]) %>% 
    st_set_dimensions("time_counter", as.POSIXct(st_get_dimension_values(., "time_counter"))) %>% 
    setNames("votemper")
  
  #template
  dest <- st_bbox(merc_stars) %>% 
    st_as_stars(dx = 0.25, dy = 0.25, crs = "EPSG:4326")
  
  #seq along each time step and warp each curv time layer to reg (aka template aka dest)
  #stupid st_warp doesn't work with time...maybe later check github comment
  merc_list <- lapply(seq_along(st_get_dimension_values(merc_stars, "time_counter")),
                      \(i) st_warp(merc_stars[1, , , 1, i], dest))
  
  merc_reg <- do.call(c, append(merc_list, list(along = "time_counter")))[drop = TRUE]
  
  #save as a .nc file 
  dat_year <- substr(as.character(st_get_dimension_values(merc_reg, which = "time_counter")[1]), 1, 4)
  dat_mo <- substr(as.character(st_get_dimension_values(merc_reg, which = "time_counter")[1]), 6, 7)
  write_mdim(merc_reg, paste0("data/enviro/model_validate/Mercator/temp_temp/0m/reg","_", dat_year, "_", dat_mo, ".nc"))
  
  #read with terra 
  temp_terra <- rast(paste0("data/enviro/model_validate/Mercator/temp_temp/0m/reg","_", dat_year, "_", dat_mo, ".nc"))
  
  #assign time 
  d1 <- seq(as.Date(paste0(dat_year,"-","01-01")), by = "month",length = 6)
  d2 <- seq(as.Date(paste0(dat_year,"-","07-01")), by = "month",length = 6)
  
  #add year-month info to raster files
  if(str_detect(dat_mo, "01") == TRUE){
    time(temp_terra) <- d1
  } else {
    time(temp_terra) <- d2
  }
  
  #append with terra
  merc_temp_all0 <- append(merc_temp_all0, temp_terra)
  
}

merc_temp_all0 #spatial res: 0.25*, temporal es: year-month, crs: lon/lat WGS 84
#saveRDS(merc_temp_all0, file = "data/enviro/model_validate/Mercator/processed/merc_temp_all0.rds")

### temperature 200m ####
merc_temp_all200 <- NULL
for(i in 1:length(merc_temp200_raw)){
  # As stars object
  merc_stars <- read_stars(merc_temp200_raw[i]) %>% 
    st_set_dimensions("time_counter", as.POSIXct(st_get_dimension_values(., "time_counter"))) %>% 
    setNames("votemper")
  
  #template
  dest <- st_bbox(merc_stars) %>% 
    st_as_stars(dx = 0.25, dy = 0.25, crs = "EPSG:4326")
  
  #seq along each time step and warp each curv time layer to reg (aka template aka dest)
  #stupid st_warp doesn't work with time...maybe later check github comment
  merc_list <- lapply(seq_along(st_get_dimension_values(merc_stars, "time_counter")),
                      \(i) st_warp(merc_stars[1, , , 1, i], dest))
  
  merc_reg <- do.call(c, append(merc_list, list(along = "time_counter")))[drop = TRUE]
  
  #save as a .nc file 
  dat_year <- substr(as.character(st_get_dimension_values(merc_reg, which = "time_counter")[1]), 1, 4)
  dat_mo <- substr(as.character(st_get_dimension_values(merc_reg, which = "time_counter")[1]), 6, 7)
  write_mdim(merc_reg, paste0("data/enviro/model_validate/Mercator/temp_temp/200m/reg","_", dat_year, "_", dat_mo, ".nc"))
  
  #read with terra 
  temp_terra <- rast(paste0("data/enviro/model_validate/Mercator/temp_temp/200m/reg","_", dat_year, "_", dat_mo, ".nc"))
  
  #assign time 
  d1 <- seq(as.Date(paste0(dat_year,"-","01-01")), by = "month",length = 6)
  d2 <- seq(as.Date(paste0(dat_year,"-","07-01")), by = "month",length = 6)
  
  #add year-month info to raster files
  if(str_detect(dat_mo, "01") == TRUE){
    time(temp_terra) <- d1
  } else {
    time(temp_terra) <- d2
  }
  
  #append with terra
  merc_temp_all200 <- append(merc_temp_all200, temp_terra)
  
}

merc_temp_all200 #spatial res: 0.25*, temporal es: year-month, crs: lon/lat WGS 84
#saveRDS(merc_temp_all200, file = "data/enviro/model_validate/Mercator/processed/merc_temp_all200.rds")





##### scratch work #######
# As stars object
merc_stars <- read_stars(merc_temp0_raw[1]) %>% 
  st_set_dimensions("time_counter", as.POSIXct(st_get_dimension_values(., "time_counter"))) %>% 
  setNames("votemper")

#template
dest <- st_bbox(merc_stars) %>% 
  st_as_stars(dx = 0.25, dy = 0.25, crs = "EPSG:4326")

#seq along each time step and warp each curv time layer to reg (aka template aka dest)
#stupid st_warp doesn't work with time...maybe later check github comment
merc_list <- lapply(seq_along(st_get_dimension_values(merc_stars, "time_counter")),
                    \(i) st_warp(merc_stars[1, , , 1, i], dest))

merc_reg <- do.call(c, append(merc_list, list(along = "time_counter")))[drop = TRUE]

write_mdim(merc_reg, "data/foo.nc")
test <- rast("data/foo.nc")












library(tidyverse)
votemper_df <- expand_grid(i = seq(dim(merc_vals)[1]), 
                           j = seq(dim(merc_vals)[2]),
                           # 3rd dimension, depth, is length 1
                           k = seq(dim(merc_vals)[4])) %>% 
  mutate(lon = merc_lon[cbind(i, j)],
         lat = merc_lat[cbind(i, j)],
         time = merc_time[k],
         votemper = merc_vals[cbind(i, j, 1, k)]) %>% 
  select(-c(i, j, k)) %>% 
  group_by(time) %>% 
  group_map(\(x) {
    st_as_stars
  })

foo <- filter(votemper_df, time == time[1])
ggplot(foo, aes(lon, lat, color = votemper)) +
  geom_point(size = 1) +
  theme_void()
foo_stars0 <- st_as_stars(select(foo, -time)) %>% 
  st_set_crs("EPSG:4326")
dest <- st_bbox(test_stars) %>% 
  st_as_stars(dx = 0.25, dy = 0.25, crs = "EPSG:4326")
foo_stars <- st_warp(foo_stars0, dest)

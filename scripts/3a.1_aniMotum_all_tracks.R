#load packages 
library(tidyverse)
#install.packages("aniMotum", repos = c("https://cloud.r-project.org","https://ianjonsen.r-universe.dev"),dependencies = TRUE)
library(aniMotum)
library(amt) #time now has to be in posix to work
library(sf)
library(here);here <- here::here
library(terra)
library(tmvtnorm) #for running the downloaded sim_fit function

##### load presence data ####
#spot and psat data
all_dat <- readRDS(here("data/presence_locs/psat_spot_domain/psat_Nspot_data.rds"))

#format for aniMotum
ssm_dat <- all_dat %>% 
  select(id = "ptt", date ="posix", lat = "lat",lon = "lon", lc = "lc") %>%
  na.omit() #must remove missing lat/lon/date values for ssm to work

ssm_dat2 <- ssm_dat %>% 
  filter(lc == "G" | lc == "3" | lc == "2" | lc == "1" | lc == "0" | lc == "A" | lc == "B") %>% #filter out Zlocation classes -- caused error
  filter(id != "52124" & id != "54607" & id != "60984" & id != "60986") %>% #filter out ptts with < 30 positions
  filter(id != "52122" & id != "52218" & id != "60993" & id != "68484" & id != "68509" & id != "68518" & id != "87549" & id != "96293" & id != "96364") #filter out ptts with low density data resulting in poor fitting SSMs

  #view number of locs by ptt
ssm_dat2 %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  print(n = 85)

##### CRW SSM ####
set.seed(1004)
ssm_crw <- fit_ssm(ssm_dat2,
                   model = "crw",
                   time.step = 24) #time step in hours
                    
aniMotum::map(ssm_crw, what = "f")|aniMotum::map(ssm_crw, what = "p")
c(ssm_crw$ssm[[1]]$AICc)

#crop the SSM so modeled points are within the CMEMS data domain (domain based off of raw data mins and maxs)
for (i in 1:63) {
  #print(length(ssm_crw$ssm[[i]]$data$geometry))
  temp1 <- st_transform(ssm_crw$ssm[[i]]$predicted$geometry, crs = st_crs("+proj=longlat +datum=WGS84 "))
  temp <- st_crop(temp1, xmin=-155, xmax=-99, ymin=2, ymax=52)
  temp2 <- st_transform(temp, crs = st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs "))
  
  ssm_crw$ssm[[i]]$predicted <- dplyr::filter(ssm_crw$ssm[[i]]$predicted, geometry %in% temp2)
  #print(length(temp))
  #print(length(ssm_crw$ssm[[i]]$data$geometry))
}

summary(ssm_crw)

#visually observe fitted and predicted tracks 
aniMotum::map(ssm_crw, what = "f")|aniMotum::map(ssm_crw, what = "p") #fitted | predicted
plot(ssm_crw, what = "p", type = 1, pages = 1) #2 shows map, 1 shows movement between lat lons across month

#visually inspect residuals and diagnostic plots 
resid_crw <- osar(ssm_crw)

plot(resid_crw, type = "qq", pages = 0)
plot(resid_crw, type = "acf", pages = 0)
plot(resid_crw, type = "ts", pages = 0)

##### aniMotum CRW PA generation ####
# create spatVect for CMEMS domain used to generate gradient
#get the bounding box of the two x & y coordintates, make sfc
ylims <- c(2, 52)
xlims <- c(-155, -99)
box_coords <- tibble(x = xlims, y = ylims) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(4326))

bounding_box <- st_bbox(box_coords) %>% st_as_sfc()

  #reproject to CRS that aligns with SSM output
bb_merc <- st_transform(bounding_box, crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs")
  
#read in continents polygon
land_vect <- read_sf(here("data/enviro/continents_shp/World_Continents.shp"))

land_merc = land_vect %>%
  st_as_sfc() %>%
  st_transform(crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs")

land_subset <- st_intersection(land_merc, bb_merc)
mapview::mapview(land_subset)

#crop where ROMS domain polygon and continents polygons intersect to get a final polygon of the CMEMS domain
grad_poly <- st_difference(bb_merc, land_subset)
mapview::mapview(grad_poly)

df <- data.frame(id = seq(length(grad_poly)))
df$geometry <- grad_poly
grad_sf <- st_as_sf(df)

grad_spatVect <- vect(grad_poly)

# calculate gradient between CMEMS polygon (that fits within CMEMS dataset) and template that is 3x the CMEMS domain
  #domain that is x3 area of ROMS domain
CMEMS_large_rast <- rast(
  crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs",
  extent = ext(-18033.76, -12228.67, -2824.927, 19262.71), 
  resolution =  47.19581
)

#create 2D gradient
x <- rasterize(grad_spatVect, CMEMS_large_rast, fun = "mean") 

## generate gradient rasters
dist <- distance(x)
x1 <- terrain(dist, v = "slope", unit = "radians")
y1 <- terrain(dist, v = "aspect", unit = "radians")
grad.x <- -1 * x1 * cos(0.5 * pi - y1)
grad.y <- -1 * x1 * sin(0.5 * pi - y1)
grad <- c(grad.x, grad.y)

plot(grad)

## use land subset to filter ssm_crw points that are on land 
for (i in 1:63) {
  temp <- st_difference(ssm_crw$ssm[[i]]$predicted$geometry, land_subset)
  
  ssm_crw$ssm[[i]]$predicted <- dplyr::filter(ssm_crw$ssm[[i]]$predicted, geometry %in% temp)
}

#save predicted points as RDS file -- may need to remove 4 locs that are on land for id 63984
dat_pred <- ssm_crw %>% 
  rowwise() %>% 
  mutate(prediction = list(st_transform(ssm$predicted, 4326)),
         prediction = list(subset(prediction, select = c(geometry, date)))) %>%
  ungroup()

#unnest prediction column
dat_pred2 <- dat_pred %>%
  unnest(prediction) %>%
  subset(select = c(id, geometry, date))

#add coordinates as new lat lon cols - also add WC indicator
dat_pred2$lon_p<-st_coordinates(dat_pred2$geometry)[,1] # get coordinates
dat_pred2$lat_p<-st_coordinates(dat_pred2$geometry)[,2] # get coordinates

#saveRDS(dat_pred2, file = here("data/presence_locs/psat_spot_domain/pres_aniM_dat.RDS"))

#develop pseudo crw locs using the predicted times (constrained to have same number of locs)
#load 01/10/24 updated sim_fit() function
source(here("functions/sim_fit.R"))

  #selected a beta value of -375 as this was the smallest absolute value number that resulted in min of 75% of the PAs remaining in the study area (the highest perc IDd)
pa_crw <- sim_fit(ssm_crw, grad = grad, beta = c(-375, -375), what = "predicted", reps = 250);filter_pa <- sim_filter(pa_crw, keep = 0.30);routed_pa <- route_path(filter_pa, centroids = T)
plot(routed_pa[23,], ncol = 1)

#saveRDS(routed_pa, file = here("data/presence_locs/cmems_domain/PA_routed_375beta_30perc_cmems.RDS"))

#IDing how many tracks are outside of the study domain 
    #unnest prediction column
dat_pa <- routed_pa %>%
  unnest(sims)

    #ID by simulation rep how many reps stay in study area
dat_pa2 <- dat_pa %>% 
  mutate(domain = ifelse(lon <= -140 |
                           lon >= -110 |
                           lat <= 10 | 
                           lat >= 50, "omit", "keep"))
  
dat_pa_filt <- dat_pa2 %>%
  group_by(id, rep) %>%
  filter(!any(domain == "omit")) %>%
  ungroup()

test <- dat_pa_filt %>% 
  group_by(id) %>% 
  summarise(n_reps = n_distinct(rep)) %>% #min number of reps inside domain is XXX, sampling rest down to that count
  print(n = 23) #min is 39 tracks -- sample to that

mean(test$n_reps/75*100) #mean is 79% stay in

  #randomly sample reps so that all have same number
dat_PA_39 <- NULL
for(i in 1:length(unique(dat_pa_filt$id))){
  #select current id
  curr_ID <- unique(dat_pa_filt$id)[i]
  temp_df <- dat_pa_filt[dat_pa_filt$id %in% curr_ID,]
  
  #sample 52 id's randomly
  temp_rep_ID <- sample(unique(temp_df$rep), 39, replace = FALSE)
  
  #narrow your data set
  temp_df2 <- temp_df[temp_df$rep %in% temp_rep_ID, ]
  
  #combine in a single df
  dat_PA_39 <- rbind(dat_PA_39, temp_df2)
  
}

#check if it worked 
dat_PA_39 %>% 
  group_by(id) %>% 
  summarise(n_reps = n_distinct(rep)) %>%
  print(n = 23)

#saveRDS(dat_PA_39, file = here("data/presence_locs/cmems_domain/PA_locs_alldat_39.RDS"))



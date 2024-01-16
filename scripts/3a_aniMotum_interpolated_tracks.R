#load packages 
library(tidyverse)
#install.packages("aniMotum", repos = c("https://cloud.r-project.org","https://ianjonsen.r-universe.dev"),dependencies = TRUE)
library(aniMotum)
library(amt) #time now has to be in posix to work
library(sf)
library(here)
library(terra)
library(tmvtnorm) #for running the downloaded sim_fit function
require(parallel) 
library(doParallel)

#--------------------
#load presence data 
#--------------------
loc_dat <- read.csv(here("data/presence_locs/tdl.csv"))

ssm_dat <- loc_dat %>% 
  select(id = "ptt", date ="posix", lat = "Lat",lon = "Lon") %>%
  mutate(lc = "G", 
         date = as.POSIXct(strptime(date, format = "%Y-%m-%d")))

  #view number of locs by ptt
ssm_dat %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  print(n = 25)

#--------------------------------
#test individual and AMT exploration
#--------------------------------
#first, use atm package to find a time interval to start with (mean time step between raw points 403 min or 6.7 hrs). May run with 12hr interval instead. 

#get data into amt format
amt_dat <- ssm_dat %>%
  filter(!is.na('lon') & !is.na('lat')) %>%
  select(x = 'lon', y = 'lat', t = 'date', id = 'id', -c("lc"))
amt_indiv <- amt_dat %>% filter(id == 72811)

#convert data to an amt track
amt_track <- mk_track(amt_indiv, .x = x, .y = y, .t = t, key = id, crs = 4326)

#get summary statistics from track
summarize_sampling_rate(amt_track)
stps <- track_resample(amt_track, rate = hours(24))

#fit ssm - left defaults for speed or location filtering but already completed in WC using GPE3 algorithm. Should turn off?
ssm_indiv <- ssm_dat %>% filter(id == 72811)
ssm_track <- fit_ssm(ssm_indiv, model = "rw", time.step = 24) #time step in hours

summary(ssm_track)
aniMotum::map(ssm_track, what = "f")|aniMotum::map(ssm_track, what = "p") #fitted | predicted
plot(ssm_track, what = "p", type = 1) #2 shows map, 1 shows movement between lat lons across month

resid <- osar(ssm_track)
require(patchwork)
(plot(resid, type = "ts") | plot(resid, type = "qq")) / 
  (plot(resid, type = "acf") | plot_spacer())

#simulate animal movement paths (start pseudo-track generation)
load(system.file("extdata/grad.rda", package = "aniMotum"))
pa_track <- sim_fit(ssm_track, what = "predicted", grad = grad, beta = c(-350, -350), reps = 50)
plot(pa_track, zoom = T)

  #filter to keep top XX%
filter_pa <- sim_filter(pa_track, keep = 0.2) #keeps top 20%
plot(pa_track)|plot(filter_pa) #compare unfiltered vs. filtered tracks
plot(filter_pa)

#re-route any remaining pseudo-locations that are on land back to water 
final_pa <- route_path(filter_pa, centroids = T)
plot(filter_pa)|plot(final_pa) #compares filtered vs. re-routed (final) tracks
plot(final_pa)


#------------------
# fit ssm, generate simulations (filter & reroute) for all indivduals 
#------------------
#### random walk exploration ####
ssm_rw <- fit_ssm(ssm_dat, model = "rw", time.step = 24) #time step in hours

summary(ssm_rw)

#visually observe fitted and predicted tracks
aniMotum::map(ssm_rw, what = "f")|aniMotum::map(ssm_rw, what = "p") #fitted | predicted
plot(ssm_rw, what = "p", type = 2) #2 shows map, 1 shows movement between lat lons across month

#visually inspect the diagnostic plots and residuals
resid_rw <- osar(ssm_rw)

plot(resid_rw, type = "qq", pages = 0)
plot(resid_rw, type = "acf", pages = 0)
plot(resid_rw, type = "ts", pages = 0)

c(ssm_rw$ssm[[1]]$AICc, ssm_crw$ssm[[1]]$AICc) #get AIC scores to compare


##### CRW SSM ####
set.seed(1004)
ssm_crw <- fit_ssm(ssm_dat, model = "crw", time.step = 24) #time step in hours

#crop the SSM so modeled points are within ROMS domain
for (i in 1:23) {
  #print(length(ssm_crw$ssm[[i]]$data$geometry))
  temp1 <- st_transform(ssm_crw$ssm[[i]]$predicted$geometry, crs = st_crs("+proj=longlat +datum=WGS84 "))
  temp <- st_crop(temp1, xmin=-134, xmax=-114, ymin=30, ymax=48)
  temp2 <- st_transform(temp, crs = st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs "))
  
  ssm_crw$ssm[[i]]$predicted <- dplyr::filter(ssm_crw$ssm[[i]]$predicted, geometry %in% temp2)
  #print(length(temp))
  #print(length(ssm_crw$ssm[[i]]$data$geometry))
}

ssm_crw <- ssm_crw %>% filter(id != "52227")
summary(ssm_crw)

#visually observe fitted and predicted tracks 
aniMotum::map(ssm_crw, what = "f")|aniMotum::map(ssm_crw, what = "p") #fitted | predicted
plot(ssm_crw, what = "p", type = 1) #2 shows map, 1 shows movement between lat lons across month

#visually inspect residuals and diagnostic plots 
resid_crw <- osar(ssm_crw)

plot(resid_crw, type = "qq", pages = 0)
plot(resid_crw, type = "acf", pages = 0)
plot(resid_crw, type = "ts", pages = 0)

#----------------------------
#aniMotum CRW PA generation 
#---------------------------
# create spatVect for CCS used to generate gradient
#get the bounding box of the two x & y coordintates, make sfc
ylims <- c(30, 48)
xlims <- c(-134, -115)
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

#crop where ROMS domain polygon and continents polygons intersect to get a final polygon of the CCS
grad_poly <- st_difference(bb_merc, land_subset)
mapview::mapview(grad_poly)

df <- data.frame(id = seq(length(grad_poly)))
df$geometry <- grad_poly
grad_sf <- st_as_sf(df)

grad_spatVect <- vect(grad_poly)

# calculate gradient between CCS polygon (that fits within ROMS domain) and template that is 3x the ROMS domain
  #domain that is x3 area of ROMS domain
ROMS_large_rast <- rast(
  crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs",
  extent = ext(-16252.65, -12252.34, 2722.523, 7135.563), 
  resolution =  31.74849
)

#create 2D gradient
x <- rasterize(grad_spatVect, ROMS_large_rast, fun = "mean") 

## generate gradient rasters
dist <- distance(x)
x1 <- terrain(dist, v = "slope", unit = "radians")
y1 <- terrain(dist, v = "aspect", unit = "radians")
grad.x <- -1 * x1 * cos(0.5 * pi - y1)
grad.y <- -1 * x1 * sin(0.5 * pi - y1)
grad <- c(grad.x, grad.y)

plot(grad)

## use land subset to filter ssm_crw points that are on land 
for (i in 1:22) {
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

#saveRDS(dat_pred2, file = here("data/presence_locs/processed/pres_locs.RDS"))

#develop pseudo crw locs using the predicted times (constrained to have same number of locs)
#load 01/10/24 updated sim_fit() function
source(here("functions/sim_fit.R"))

  #selected a beta value of -375 as this was the smallest absolute value number that resulted in ~90% of the PAs remaining in the study area (see output from line 241)
#run PA generation, filter to keep top 30%, and then reroute to get tracks out of bounding box region.
    #analysis testing different beat values (only tested first 5 sharks with 250 reps)
#-500, mean = 39% fit in
#-450, mean = 47% fit in
#-400, mean = 53% fit in ** tested this with whole data set, mean = 72%
#-375, mean = 53% fit in ** tested this with whole data set, mean = 74%
#-300, mean = 47% fit in 
#-100, mean = 30% fit in 
pa_crw3 <- sim_fit(ssm_crw, grad = grad, beta = c(-375, -375), what = "predicted", reps = 250);
filter_pa3 <- sim_filter(pa_crw3, keep = 0.30);
routed_pa3 <- route_path(filter_pa3, centroids = T)

plot(routed_pa3[3,], ncol = 1)

#saveRDS(routed_pa, file = here("data/presence_locs/processed/PA_routed_375beta_30perc.RDS"))

#IDing how many tracks are outside of the study domain 
    #unnest prediction column
dat_pa <- routed_pa3 %>%
  unnest(sims)

    #ID by simulation rep how many reps leave study area
dat_pa2 <- dat_pa %>% 
  mutate(domain = ifelse(lon <= -134 |
                           lon >= -115 |
                           lat <= 30 | 
                           lat >= 48, "omit", "keep"))
  
dat_pa_filt <- dat_pa2 %>%
  group_by(id, rep) %>%
  filter(!any(domain == "omit")) %>%
  ungroup()

test <- dat_pa_filt %>% 
  group_by(id) %>% 
  summarise(n_reps = n_distinct(rep)) %>% #min number of reps inside domain is XXXX, sampling rest down to that count
  print(n = 22)

mean(test$n_reps/75*100)
#saveRDS(dat_pa_filt, file = here("data/presence_locs/processed/PA_locs.RDS"))
dat_PA <- readRDS(here("data/presence_locs/processed/PA_locs.RDS"))

dat_PA %>% 
  group_by(id) %>% 
  summarise(n_reps = n_distinct(rep)) %>% #min number of reps inside domain is XXXX, sampling rest down to that count
  print(n = 22) #min is 16 PA tracks

dat_PA_16 <- NULL
for(i in 1:length(unique(dat_PA$id))){
  #select current id
  curr_ID <- unique(dat_PA$id)[i]
  temp_df <- dat_PA[dat_PA$id %in% curr_ID,]
  
  #sample 16 id's randomly
  temp_rep_ID <- sample(unique(temp_df$rep), 16, replace = FALSE)
  
  #narrow your data set
  temp_df2 <- temp_df[temp_df$rep %in% temp_rep_ID, ]
  
  #combine in a single df
  dat_PA_16 <- rbind(dat_PA_16, temp_df2)
  
}

#check if it worked 
dat_PA_16 %>% 
  group_by(id) %>% 
  summarise(n_reps = n_distinct(rep)) %>%
  print(n = 22)

#saveRDS(dat_PA_16, file = here("data/presence_locs/processed/PA_locs_16.RDS"))

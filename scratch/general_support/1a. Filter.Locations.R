####################################################################
##########   FILTER LOCATION DATA FROM THE 5 HP   ##################
####################################################################


# library(dplyr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(readr)
library(tidyquant)
library(gridExtra)
library(ncdf4)
library(raster)
library(viridis)
library(oce)
library(readxl)
library(suncalc)
library(maps)

install.packages(c("janitor", "readr", "dplyr", "tidyquant", "gridExtra", "ncdf4", "raster", "terra", "viridis", "oce", "readxl", "suncalc"))

dat <- read.csv("dat/io040620_GPS.csv")

###############################
# import dataset
###############################
setwd("C:/Users/Emily Nazario/Documents/R/Projects/Preliminary Shark SDMs/data/Mako Data")

loc <- read.csv("./io040620_GPS.csv") %>%
  as_tibble() %>%
  rename(lat = Lat, lon = Lot) #renames columns (left is new and right is old) 
loc
table(loc$id) #provides small summary of new DF contents

summary(loc)
loc[is.na(loc$Time),] #finds all the NA rows -- provides a count

loc = loc %>%
  filter(!is.na(Time)) #removes NAs -- very useful for changing dates! Use in for loop

# convert date time to POSIX
#-------------------------------
loc = loc %>% 
  mutate(date_time = paste0(Date, " ", Time))
loc$posix = as.POSIXct(strptime(loc$date_time, 
                                format = "%m/%d/%Y %H:%M:%S")) #use substr() to just grab certain values within the date
loc = loc[order(loc$posix),] #makes sure that the dates are listed chronologically

loc %>% #summarizes the durations of the deployments/animal -- see how we did this in new script
  #group_by(id) %>%
  summarise(start= first(posix),
            end = last(posix))

#add month and year columns to DF
loc = loc %>%
  mutate(month = substr(posix, 6, 7), 
         year = substr(posix, 1, 4))

#creates a map plotting the points with no coastline. Connects the points.
ggplot(loc, aes(lon, lat)) +  
  geom_point(aes(color = month)) +
  geom_path(aes(group = 1))+
  geom_point(data = loc[1,], color = "black")

#plots points with coastline in base R
plot(lat ~ lon, loc, pch = 19) 
map(add = TRUE, fill = TRUE, col = "grey")

#plots the points in ggplot with fancy coastline using the map_data() package
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="USA" #find USA map without Alaska
                      | north_map$region=="Mexico",]
ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-125, -110), ylim = c(15, 40))+
  geom_point(data=loc, aes(lon,lat,colour=as.factor(month)), size=0.2) +
  # coord_map("azequidistant", xlim=c(-60,0), ylim=c(43,78)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  theme(legend.position = "none") 



#----------------------
# correct wrong dates
#----------------------
wrong_dates = loc %>%
  filter(month == "41" | month == "42")
unique(wrong_dates$id) # "22849b" "22850b" "27262"  "27262b" "93100" 

# convert date and times from numeric
wrong_dates = wrong_dates %>%
  mutate(date  = as.numeric(dateTime),
         date  = excel_numeric_to_date(date))
unique(wrong_dates$date)

wrong_dates$posix = convert_to_datetime(
  wrong_dates$dateTime,
  character_fun=lubridate::ymd_hms, truncated=1, tz="America/Nuuk")
unique(wrong_dates$posix)

wrong_dates %>%
  group_by(id) %>%
  summarise(year_deploy = first(deploy_year),
            start       = first(posix),  # ymd
            end         = last(posix))   # ymd
wrong_dates$posix = as.POSIXct(strptime(wrong_dates$posix, # ymd
                                 format="%Y-%d-%m %H:%M:%S"), tz="America/Nuuk")

# convert date and times from initial dataset without wrong dates
loc2 = loc %>%
  filter(!(month == "41" | month == "42")) %>%
  mutate(date = substr(dateTime, 1, 10))
unique(loc2$date)

loc2$posix = convert_to_datetime(
  loc2$dateTime,
  character_fun = lubridate::mdy_hms, truncated=1, tz="America/Nuuk") # ymd
summary(loc2$posix)

loc3 = rbind(loc2, wrong_dates)
loc3 = loc3 %>%
  mutate(month       = substr(posix, 6, 7),
         deploy_year = substr(posix, 1, 4)) %>%
  group_by(id) %>%
  arrange(posix) %>%
  ungroup()
unique(loc3$month)
unique(loc3$deploy_year) # 2013 to 2015

loc3 %>%
  group_by(id) %>%
  summarise(start  = first(posix),
            end    = last(posix)) %>%
  mutate(dur = difftime(end, start, units = "days"))
rm(loc2, wrong_dates)

  
  


#-----------------------------
# remove coordinates outliers
#-----------------------------
summary(loc$lon) # -122 + 11.4
summary(loc$lat) # 47 to 69

# plot locations per ID
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Greenland"
                      | north_map$region=="Norway"
                      | north_map$region=="Sweden"
                      | north_map$region=="Denmark"
                      | north_map$region=="Finland"
                      | north_map$region=="Iceland",]
ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=as.factor(id)), size=0.2) +
  # coord_map("azequidistant", xlim=c(-60,0), ylim=c(43,78)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  theme(legend.position = "none") 

loc = loc %>%
  filter(lon >(-60) & lon < 0)
ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=as.factor(id)), size=0.2) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  theme(legend.position = "none") 


 
# check 2 years of deployment for id 27262b
#-------------------------------------------
table(loc3$deploy_year[loc3$id == "27262b"]) # 43 locs in 2015 East Greenland
ggplot(shore, aes(long, lat)) +
  geom_point(data=loc3[loc3$id == "27262b",], 
             aes(lon,lat,colour=deploy_year), size=0.2) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  labs(title="#27262b") 

table(loc3$id, loc3$deploy_year)
#   id     year_deploy start               end                 duration      
# 1 22849b 2014        2014-07-22 15:16:19 2014-12-15 13:43:53 145.93581 days
# 2 22850b 2014        2014-07-17 19:12:20 2014-12-01 13:33:26 136.76465 days
# 3 27262  2013        2013-10-02 15:53:13 2013-10-18 13:42:40  15.90934 days
# 4 27262b 2014        2014-07-22 15:26:13 2015-11-18 16:07:36 484.02874 days !!
# 5 93100  2014        2014-07-22 14:05:17 2014-12-25 21:56:44 156.32740 days !!

loc3 = loc3 %>%
  filter(!(deploy_year == "2015" & id == "27262b"))
table(loc3$id, loc3$deploy_year)



#---------------------------------------------
# check seasonal variation
#---------------------------------------------
loc3 -> loc
rm(loc3)
loc %>%
  group_by(id) %>%
  summarise(year_deploy = first(deploy_year),
            start       = first(posix),
            end         = last(posix)) %>%
  mutate(duration = difftime(end, start, units="days"))
  
  
ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=as.factor(month)), size=0.1) +
  coord_map("azequidistant", xlim=c(-60,0), ylim=c(43,78)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  facet_wrap(.~month) + 
  theme_tq() +
  theme(legend.position = "none")

# nlocs/day/id
#---------------
loc = loc %>%
  group_by(id) %>%
  mutate(date = as.Date(posix),
         day  = as.numeric(date - first(date)) + 1) 

ndaily_loc = loc %>%
  group_by(id, date) %>%
  summarise(ndaily_loc = n())
ndaily_loc %>%
  group_by(id) %>%
  summarise(mean = mean(ndaily_loc),
            sd   = sd(ndaily_loc),
            min  = min(ndaily_loc),
            max  = max(ndaily_loc))
#   id      mean    sd   min   max
# 1 22849b  22.9  6.55     8    34
# 2 22850b  24.7  5.55     6    40
# 3 27262   25.1  8.21    11    35
# 4 27262b  21.3  6.30     9    38
# 5 93100   15.2  6.95     1    31


# nlocs/month/id
#---------------
nmonthly_loc = loc %>%
  group_by(id, month) %>%
  summarise(nmonthly_loc = n()) 
nmonthly_loc %>% filter(id == "27262") # only oct (only 15 locs!)
nmonthly_loc %>%
  group_by(id) %>%
  summarise(mean = mean(nmonthly_loc),
            sd   = sd(nmonthly_loc),
            min  = min(nmonthly_loc),
            max  = max(nmonthly_loc))
#   id      mean    sd   min   max
# 1 22849b  286.  133.    75   449
# 2 22850b  289.  167.    21   424
# 3 27262   251    NA    251   251
# 4 27262b  256.  136.    73   428
# 5 93100   132.  110.    11   273





#########################################
# convert time zone to posix
#########################################
# https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
# http://rstudio-pubs-static.s3.amazonaws.com/17282_9510673f07294de7905e11c7f2b043a5.html
# loc = loc %>%
#   mutate(posix_local = format(posix, tz = "America/Nuuk"))
# loc = loc %>%
#   mutate(posix_local = as.POSIXct(posix_local, tz = "America/Nuuk", 
#                                   format = "%F %T"))
# loc %>%
#   dplyr::select(id, posix, posix_local) %>%
#   first()



#----------------------------
# distinguish day and night
#----------------------------
loc = loc %>%
  rename(posix_local = posix) %>%
  mutate(hour = as.numeric(substr(posix_local, 12, 13))) %>%
  ungroup()

sun  = getSunlightTimes(data = loc, tz="America/Nuuk", 
                        keep=c("sunrise","sunset","dusk",
                               "dawn","night","nightEnd")) 
head(sun)
sun %>%
  mutate(year = substr(date, 1, 4)) %>%
  filter(year == "2014") %>%
  summarise(earliest_sunrise = min(sunrise),
            latest_sunrise = max(sunrise),
            earliest_sunset = max(sunset),
            latest_sunset = min(sunset))

loc2 = cbind(loc, sun[,c("sunrise","sunset","dusk",
                         "dawn","night","nightEnd")]) %>%
  as_tibble()
names(loc2)
# dawn: before sunrise
# dusk : after sunset, before night

loc2 = loc2 %>%
  mutate(period = case_when(posix_local %within% interval(sunset, night) ~ 'dusk',
                            posix_local %within% interval(nightEnd, sunrise) ~ 'dawn',
                            posix_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night"))
table(loc2$period)
unique(loc2$period)
loc2 -> loc
rm(loc2)







############################
# remove duplicated rows
############################
nrow(loc[duplicated(loc$posix_local),])    # 1667
loc %>%
  group_by(id) %>%
  filter(duplicated(posix_local)) %>%
  summarise(n_duplicated = n())
#   id     n_duplicated
# 1 22849b          504
# 2 22850b          473
# 3 27262            69
# 4 27262b          397
# 5 93100           209

loc2 = loc %>%
  group_by(id) %>%
  filter(!(duplicated(posix_local)))
nrow(loc2[duplicated(loc2$posix_local),])  # 15
loc2 %>%
  group_by(id) %>%
  filter(duplicated(posix_local))          # 0 left
loc2 -> loc
rm(loc2)






######################################
# extract bathy at locations
######################################
bathy = raster("./ENV.DATA/GEBCO_Arctic_West.nc")
# plot(bathy, col=viridis(64))
bathy[bathy>0] = NA  # remove cells on land (above sea level)
system.time({ 
  loc$bathy = raster::extract(bathy, loc[,c("lon","lat")])
})
summary(loc$bathy) # mean: 147 m
loc$bathy = abs(loc$bathy)






# ######################################
# # extract env data at locations
# ######################################
# 
# # MLD #
# #------
# r = raster("./ENV.DATA/CMEMS/mld_02102013-08012015_PHY-001-030.nc")
# plot(r, col=viridis(64))
# loc$mld = raster::extract(r, loc[,c("lon","lat")])
# summary(loc$mld) # 66 NA
# 
# ggplot(data = loc, aes(x=month, y=mld, fill=mld)) +
#   geom_boxplot() +
#   facet_grid(~id)
# 
# daily = loc %>%
#   group_by(id, date) %>%
#   summarise(mld = mean(mld, na.rm=T))
# 
# ggplot(data = daily, aes(x=date, y=mld, fill=mld)) +
#   geom_bar(stat="identity") +
#   scale_y_continuous(trans = "reverse") +
#   scico::scale_fill_scico(palette = "berlin", direction=-1) +
#   labs(x = "", y = "MLD at location (m)", 
#        title="Mean daily MLD") +
#   facet_wrap(~id, scales="free_x") +
#   theme_tq() 
# 
# 
# 
# # sea ice concentration #
# #------------------------
# library(oce)
# library(maps)
# r = raster("./ENV.DATA/CMEMS/sic_02102013-08012015_PHY-001-030.nc")
# nbands(r)
# plot(r, col=oceColorsGebco(64))
# map(add=T)
# loc$sic = raster::extract(r, loc[,c("lon","lat")])
# summary(loc$sic) # no sic !
# 
# 
# # sst #
# #-------
# r = raster("./ENV.DATA/CMEMS/sst_02102013-08012015_PHY-001-030.nc")
# plot(r, col=oceColorsFreesurface(64))
# map(add=T)
# loc$sst = raster::extract(r, loc[,c("lon","lat")])
# summary(loc$sst) # 66 NA
# points(lat~lon, loc[is.na(loc$sst),], pch=19, cex=0.4, col="red")
# ggplot(data = loc, aes(x=month, y=sst, fill=sst)) +
#   geom_boxplot(aes(colour=sst)) +
#   facet_grid(~id)
# 
# # U #
# #-------
# r = raster("./ENV.DATA/CMEMS/u_02102013-08012015_PHY-001-030.nc")
# plot(r, col=oceColorsVelocity(64))
# map(add=T)
# loc$u = raster::extract(r, loc[,c("lon","lat")])
# summary(loc$u) # 66 NA
# points(lat~lon, loc[is.na(loc$u),], pch=19, cex=0.4, col="red")
# ggplot(data = loc, aes(x=month, y=u)) + # mainly currents flowing westward (offshore)
#   geom_boxplot() +
#   facet_grid(~id)
# 
# 
# # V #
# #-------
# r = raster("./ENV.DATA/CMEMS/v_02102013-08012015_PHY-001-030.nc")
# plot(r, col=oceColorsVelocity(64))
# map(add=T)
# loc$v = raster::extract(r, loc[,c("lon","lat")])
# summary(loc$v) # 66 NA
# points(lat~lon, loc[is.na(loc$v),], pch=19, cex=0.4, col="red")
# ggplot(data = loc, aes(x=month, y=v)) + 
#   geom_boxplot() +
#   facet_grid(~id)






#####################
# save dataset
#####################
loc = loc %>% ungroup()
saveRDS(loc, "./RDATA/1.locations_filtered_5HP.rds")











##################################
# exploration
##################################

#------------------------------
# bathy over time per whale
#------------------------------
daily = loc %>%
  group_by(id, date) %>%
  summarise(bathy = mean(bathy, na.rm=T))

ggplot(data = daily[!is.na(daily$bathy),], 
       aes(x=date, y=bathy, fill=bathy)) +
  # geom_line(colour="red") +
  geom_bar(stat="identity") +
  scale_y_continuous(trans = "reverse") +
  scico::scale_fill_scico(palette = "berlin", direction=-1) +
  labs(x = "", y = "Bathymetry at location (m)", 
       title="Mean daily bathy") +
  facet_wrap(~id, scales="free_x") +
  theme_tq() +
  theme(axis.title.y = element_text(size=10,color="black"),
        axis.title.x = element_text(size=10,color="black"),
        axis.text.x  = element_text(size=10,vjust=0.5,color="black"),
        axis.text.y  = element_text(size=10,color="black"),
        strip.text = element_text(colour='white',size=12,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        panel.background = element_blank(),
        text=element_text(size=10, family="serif"),
        panel.grid.major = element_line(colour="lightgrey", size=0.1),
        plot.margin = unit(c(0.2,0.2,0.2,0.1), "cm"),
        axis.line.x.top = element_line(size = 3, color = "red"))


#-------------------------------
# tracks with bathy colorcoded
#-------------------------------
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Greenland"
                      | north_map$region=="Norway"
                      | north_map$region=="Iceland",]

ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=bathy), size=0.1) +
  # scale_colour_brewer(palette = "PiYG") +
  # scale_colour_continuous(type = "viridis") +
  scico::scale_colour_scico(palette = "berlin", direction=-1) + #lajolla
  coord_map("azequidistant", xlim=c(-57,-40), ylim=c(60,70)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  facet_wrap(~id, ncol=3) + 
  labs(x="",y="") +
  theme_tq() +
  theme(legend.position = "bottom")


# plot tracking duration / ID
#------------------------------
loc %>%
  group_by(id) %>%
  summarise(start    = first(posix),
            end      = last(posix),
            dur_days = difftime(last(posix), first(posix), units="days"),
            dur_months = floor(as.numeric(difftime(last(posix), 
                                                   first(posix), 
                                                   units="days")/30))) %>%
  print(n=5)

a = ggplot(loc, aes(y=day, x=id, colour=id)) +
  geom_line(size=2) + 
  scale_y_continuous(name="Days", breaks=seq(0,max(loc$day),by=10)) +
  scale_color_tq() +
  labs(x="", y="Tracking duration (days)") +
  coord_flip() +
  theme_tq() +
  theme(legend.position="none")

b = ggplot(loc, aes(y=date, x=id, colour=id)) +
  geom_line(size=2) + 
  scale_color_tq() +
  labs(x="", y="Year of deployment") +
  coord_flip() +
  theme_tq() +
  theme(legend.position="none")

ggsave(paste0("./FIGURES/Tracks/TrackDuration_satTag_5HP.png"),
       grid.arrange(a, b, ncol=1),
       width=5,height=6,units="in",dpi=400)


# plot locations per individual
#--------------------------------
ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=as.factor(id)), size=0.1) +
  coord_map("azequidistant", xlim=c(-60,-20), ylim=c(60,72)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  facet_wrap(~id, ncol=2) + 
  theme_tq() +
  theme(legend.position = "none")
ggsave(paste0("./FIGURES/Tracks/Locations_indiv_5HP.png"),
       width=4.5,height=6,units="in",dpi=400)

# plot locations per month
#--------------------------------
ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=as.factor(id)), size=0.1) +
  coord_map("azequidistant", xlim=c(-60,-20), ylim=c(60,72)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  facet_wrap(~month, ncol=2) + 
  theme_tq() 
ggsave(paste0("./FIGURES/Tracks/Locations_mois_5HP.png"),
       width=4.5,height=6,units="in",dpi=400)

# latitude according to months
#---------------------------------
ggplot(loc, aes(y=lat, x=month, colour=id)) +
  geom_boxplot() +
  theme_tq()
ggsave(paste0("./FIGURES/Tracks/Locations_mois_indiv.png"),
       width=5,height=4,units="in",dpi=400)

ggplot(loc, aes(y=lat, x=month)) +
  geom_boxplot() +
  theme_tq()
ggsave(paste0("./FIGURES/Tracks/Locations_mois.png"),
       width=5,height=4,units="in",dpi=400)







##################################################
# match locations with dive data
# (to then extract bathy)
##################################################




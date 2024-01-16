library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tidyquant)

rm(list = ls())

files = list.files("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/Final_locs")
files

setwd("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/Final_locs")

### Change format to POSIX of combined date and time column ####
files_gls <- files[11:26] #assigns just the files that have date and time in the same column 
files_gls

shark.comb = NULL
for (i in 1:length(unique(files_gls))) {
  id = read.csv(files_gls[[i]]) %>%
    as_tibble()
  id = id %>%
    filter(!is.na(Date)) %>% #filter out NAs
    mutate(date_time = as.character(Date))
  #changes date/time to posix format so R can better interpret
  id$posix = as.POSIXct(strptime(id$Date, 
                                 format = "%m/%d/%Y %H:%M"))
  id$Time = format(as.POSIXct(id$posix), format = "%H:%M")
  id$Date = format(as.POSIXct(id$posix), format = "%m/%d/%Y")
  shark.comb = rbind(shark.comb, id) #adds extra rows at bottom to combine all shark files
}

shark.comb$Sex <- recode_factor(shark.comb$Sex, F = "Female", M = "Male" )
levels(shark.comb$Sex) <- c("Male", "Female")
table(shark.comb$PTT) #prints summary of final contents of combined shark DF

### Combine Date and Time then Change format to POSIX####
files_gps <- files[1:10] #working directory needs to be set for final_locs
files_gps

shark.sep = NULL
for (i in 1:length(unique(files_gps))) {
  id = read.csv(files_gps[[i]]) %>%
    as_tibble()
  #add shark id as a column
  id = id %>%
    filter(!is.na(Time)) %>%
    mutate(date_time = as.character(paste0(Date, " ", Time)))
  #changes date/time to posix format so R can better interpret
  id$posix = as.POSIXct(strptime(id$date_time, 
                                 format = "%m/%d/%Y %H:%M:%S"))
  id$Time = format(as.POSIXct(id$posix), format = "%H:%M")
  id = id %>%
   filter(!is.na(Time))
  id$Date = format(as.POSIXct(id$posix), format = "%m/%d/%Y")
  shark.sep = rbind(shark.sep, id) #adds extra rows at bottom to combine all shark files
}

shark.sep$Sex <- recode_factor(shark.sep$Sex, F = "Female", M = "Male" )
levels(shark.sep$Sex) <- c("Male", "Female")
table(shark.sep$PTT) #prints summary of final contents of combined shark DF

###Combine all shark data now that date is in same format####
shark.locs = NULL
shark.locs = rbind(shark.comb, shark.sep)
table(shark.locs$PTT)

#Add month and year to total shark locations DF
shark.locs = shark.locs %>%
  mutate(month = substr(posix, 6, 7), 
         year = substr(posix, 1, 4), 
         day = substr(posix, 9, 10))

#To save the CSV use below code
setwd("data/Mako Data")
write.csv(shark.locs, file = "shark_locs.csv")

#Summary of deployment/animal
dat_sum <- shark.locs %>% 
  group_by(PTT) %>%
  summarise(start= first(posix),
            end = last(posix), 
            n = n()) %>% #The number of locations for each shark 
  mutate(dur = as.numeric(difftime(end, start, units = "days")))

#Separate by sex and then assign age class based on FL. Then recombine
dat.m <- shark.locs %>%
  filter(Sex == "Male")
dat.f <- shark.locs %>%
  filter(Sex == "Female")

dat.m$Age.Class <- ifelse(dat.m$FL>=180, "Adult", 
                           ifelse(150<=dat.m$FL & dat.m$FL<=179, "SA", 
                                  ifelse(125<=dat.m$FL & dat.m$FL<=149, "Age-2", 
                                         ifelse(100<=dat.m$FL & dat.m$FL<=124, "Age-1", "YOY"))))
dat.f$Age.Class <- ifelse(dat.f$FL>=249, "Adult", 
                           ifelse(150<=dat.f$FL & dat.f$FL<=179, "SA", 
                                  ifelse(125<=dat.f$FL & dat.f$FL<=149, "Age-2", 
                                         ifelse(100<=dat.f$FL & dat.f$FL<=124, "Age-1", "YOY"))))

shark_AgeLocs <- rbind(dat.f, dat.m)
#setwd("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data")
#write.csv(shark_AgeLocs, file = "shark_AgeLocs.csv")

###Initial plotting for tracks ####

#creates a map plotting the points with no coastline by PTT. Connects the points.
no_coast <- ggplot(shark.locs, aes(Lon, Lat)) + 
  geom_point(aes(color = month)) +
  geom_path(aes(group = 1)) +
  facet_wrap(~PTT)

#plots the points in ggplot by PTT with fancy coastline using the map_data() package
north_map = map_data("world") %>% 
  group_by(group)
shore = north_map[north_map$region== "USA" #find USA map without Alaska
    | north_map$region=="Mexico",]
coast <- ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-125, -110), ylim = c(15, 40))+
  geom_point(data=shark.locs, aes(Lon,Lat,colour=month), size=0.2)+
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  theme(legend.position = "right") +
  facet_wrap(~PTT)

#same as above but by sex 
coast_s <- ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-125, -110), ylim = c(15, 40))+
  geom_point(data=shark_AgeLocs, aes(Lon,Lat,colour=month), size=0.2) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  theme(legend.position = "right") +
  facet_wrap(~Sex)

#same as above but by age class 
levels(shark_AgeLocs$Age.Class) <- c("YOY", "Age-2", "SA", "Adult")

coast_age <- ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-125, -110), ylim = c(15, 40))+
  geom_point(data=shark_AgeLocs, aes(Lon,Lat,colour= month), size=0.2) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  theme(legend.position = "right") +
  facet_wrap(~Age.Class)

#plot setting tracks by season and color coded by age class
shark_AgeLocs = shark_AgeLocs %>%
  mutate(Date = substr(posix, 1, 10))
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

shark_AgeLocs$Season <- getSeason(shark_AgeLocs$Date)
levels(shark_AgeLocs$Season) <- c("Winter", "Spring", "Summer", "Fall")

north_map = map_data("world") %>% 
  group_by(group)
shore = north_map[north_map$region== "USA" #find USA map without Alaska
                  | north_map$region=="Mexico",]
ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-125, -110), ylim = c(15, 40))+
  geom_point(data=shark_AgeLocs, aes(Lon,Lat,colour=Age.Class), size=0.4) +
  # coord_map("azequidistant", xlim=c(-60,0), ylim=c(43,78)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  labs(color = "Age Class")+
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right", 
        legend.key.size = unit(0.75,'cm'), 
        legend.text = element_text(size = 10), 
        panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        axis.text.x = element_text(angle = 45),
        legend.margin=margin(0,0,0,0),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")))+
  facet_wrap(~Season) + 
  scale_color_manual(values = c("#45818e", "#1c4587", "#EBA832", "grey30"))


#Location data tutorial from Rchival package
## install or load package
library(RchivalTag)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tidyquant)
library(pracma)
library(plyr)

## Package overview and help:
#?RchivalTag 
#help(package="RchivalTag") ## list of functions

#tutorial instructions for maximum likelihood tracks (GPE3 and SPOT)
#example scatterplot of max likelihood track by lat/lon
csv_file <- "data/Mako Data/GPE3_locs/72816-gpe3loc.csv"
dat <- read.csv(csv_file)
pos <- get_geopos(csv_file) ## show tracks as line plot
plot_geopos(pos) ## shows Maxmimum Likelihood tracks as scatter plot
plot_geopos(csv_file) #done more directly -- distorts view of axes

#add track to existing ggplot objects 
library("mapview")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
  #map
world <- rnaturalearth::ne_countries(scale = 'small', returnclass = 'sf')
world  <- st_cast(world, 'MULTILINESTRING') %>%
  st_cast('LINESTRING', do_split=TRUE) %>%
  mutate(npts = npts(geometry, by_feature = TRUE)) %>%
  st_cast('POLYGON')
world <- ne_countries(scale = "medium", returnclass = "sf")

ggobj <- ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) + ## add scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) + ## add compass
  coord_sf(xlim = c(-125,-112), ylim = c(27,40), expand = FALSE) + theme_minimal()

#add track
pos <- pos %>% #add month column
  mutate(Month = substr(date, 6, 7))

ggobj + 
  geom_point(data=pos, aes(Lon,Lat,color= Month), size=2)+
  scale_color_manual(values = c("#537380", "#81a9ad","#de9b71", "#c67b6f", "goldenrod2", "#f57946", "#516823", "#00496f"))

#ggplot gepos with base maps (i.e., bathy, SST, oxy, MLD)
  #plots points with bathymetry
bathy <- raster::raster(x = "C:/Users/Emily Nazario/Downloads/exportImage.tiff")
bathy[bathy>0] = NA  # remove cells on land (above sea level)
system.time({ 
  pos$bathy = raster::extract(bathy, pos[,c("Lon","Lat")])
})
summary(pos$bathy) # mean: -2828 m
pos$bathy = abs(pos$bathy)

ggobj + 
  geom_point(data=pos, aes(Lon,Lat,color= bathy), size=2)
  
  #SST raster layer
r = raster("C:/Users/Emily Nazario/Downloads/ncdcOisst2Agg_ca25_9bee_6117.nc")
plot(r)

SST <- as.data.frame(r, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit() %>%
  mutate(x = x-360)

#--- take a look ---#
head(SST)

ggobj + 
  geom_raster(data = SST, aes(x = x, y = y, fill = Daily.sea.surface.temperature))+
  scale_fill_viridis_c()+
  geom_point(data=pos, aes(Lon,Lat,color= Month), size=2)+
  scale_color_manual(values = c("#537380", "#81a9ad","#de9b71", "#c67b6f", "goldenrod2", "#f57946", "#516823", "#00496f"))

### Plotting GPE3 locations with script from Nerea ###
test <- read.csv("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/WC_dat/PTT folders/72808/72808-1-GPE3.csv")















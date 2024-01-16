rm(list=ls())

## 1. load libraries
library(sf)
library(raster)
require("rgdal")
require("rgeos")
require("dplyr")
library(ggExtra)
library(maptools)
library(adehabitatHR)
library(here)
library(maps)
library(glue)
library(tidyverse)
library(ggplot2)


## 3. load world data to get coordinate system
data=maps::map("world2",fill=T)
IDs <- sapply(strsplit(data$names, ":"), function(x) x[1])
wrld_simpl <- map2SpatialPolygons(data, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
wrld=SpatialPolygons(wrld_simpl@polygons,proj4string=wrld_simpl@proj4string) %>%
  gBuffer(., byid=TRUE, width=0)
plot(wrld)
## 4. load global template
# this is an empty global raster at .25 degrees that we will use to ensure pseduo absences are not generated for the same day/pixel as presences
template=raster("C:\\Users\\nereo\\Documents\\NOAA\\PROJECTS & COLLABORATIONS\\PROJECTS\\HAWAII\\pseudo_absences/template.grd")
string=seq(1:ncell(template))
template[]=string
plot(template)
## 5. generate some dummy data, comment this out before using, just here to demonstrate how functions work
# how the function expects the dataframe to be formatted ##
# column 'date' -> YYYY-MM-DD in *character* format (will likely need to be reformatted from original data)
# column 'lon' -> 0 to 360 longitude column in numeric format (will likely need to be transformed from -180 to 180 formated in original data)
# column 'lat' -> -90 to 90 latitude column in numeric format (will likely be in correct format in original data)




setwd("C:\\Users\\nereo\\Documents\\NOAA\\PROJECTS & COLLABORATIONS\\COLLABORATIONS\\EmilyNazario_UCSC\\Emily\\pseudo_absence/data")
file_list <- list.files()
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, header=TRUE, sep=",")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.table(file, header=TRUE, sep=",")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}

yft=dataset
head(yft)



yft$date=as.Date(yft$date, "%m/%d/%Y")
head(yft)


yft$tag =as.character(yft$tag)
colnames(yft)[4] <- "lat"
colnames(yft)[3] <- "lon"
#yft$lon=yft$lon+360
yft$lon[yft$lon < 0] <- yft$lon[yft$lon < 0] + 360 ## this line changes the meridian line, this is saying that any longitudes < 0, add 360 so they are also centred



head(yft)
## 6. run the function
csvdir="C:\\Users\\nereo\\Documents\\NOAA\\PROJECTS & COLLABORATIONS\\COLLABORATIONS\\EmilyNazario_UCSC\\Emily\\pseudo_absence/"
polydir="C:\\Users\\nereo\\Documents\\NOAA\\PROJECTS & COLLABORATIONS\\COLLABORATIONS\\EmilyNazario_UCSC\\Emily\\pseudo_absence/"
species="yft"


generate_pseudo_abs_fcn(dat=yft,
                        csv_outdir=csvdir,
                        poly_outdir=polydir,
                        sp_name=species)


## 7. check what happened
PA_dat=read.csv(glue("{csvdir}/{species}_presAbs.csv"))
head(PA_dat)
str(PA_dat)
poly=st_read(glue("{polydir}/{species}.shp"))

minx = min(PA_dat$lon)
maxx = max(PA_dat$lon)
miny = min(PA_dat$lat)
maxy = max(PA_dat$lat)

ggplot()+
  geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
  geom_point(data=PA_dat,aes(x=lon,y=lat,color=as.factor(presAbs),group=presAbs),size = .5, shape = 21)+
  geom_sf(data=poly,fill=NA,color="black")+
  scale_color_manual("PresAbs",values=c("1"="blue","0"="red"))+
  coord_sf(xlim = c(minx, maxx), ylim = c(miny,maxy))+
  ggtitle(glue("{species} npre={nrow(PA_dat)} (1:1 ratio)"))
ggsave("buffer_ps.png", width = 40, height = 20, units = "cm")

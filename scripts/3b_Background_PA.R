library(sf)
library(raster)
library(tidyquant)
library(RColorBrewer)
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

#----------------------------------------
###load the data (get min/max lat/lon)
#----------------------------------------
loc_dat <- readRDS(here("data/presence_locs/psat_spot_domain/processed/psat_spot_animotum.RDS"))

lonMax <- max(loc_dat$lon_p)+2
lonMin <- min(loc_dat$lon_p)-2
latMax <- max(loc_dat$lat_p)+2
latMin <- min(loc_dat$lat_p)-2

#------------------------------
#load world data and get coord sys. Create polygon object with all country shapes
#------------------------------
world_dat <- maps::map("world2", fill = T)
IDs <- sapply(strsplit(world_dat$names, ":"), function(x) x[1]) #pulls out the names of each country and splits by :

    #input is map object from map package and the IDs can either be an integer or char. string (e.g., country name). The output is a polygon object of the outlines of all the countries
world_simpl <- map2SpatialPolygons(world_dat, IDs = IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))

    #input is the polygon object created above, we specify we want to use the same CRS, and the gBuffer argument sets a buffer of 0 around the polygons
world_final <- SpatialPolygons(world_simpl@polygons, proj4string = world_simpl@proj4string) %>%
  gBuffer(., byid = TRUE, width = 0)
plot(world_final)

#---------------------------------
# Load template raster for sea level anomoly data
#--------------------------------
    #generate an empty raster at 0.25 degrees resolution that will use to ensure the PAs are not generated for the same day/pixel as presence locations. Template is for sea level anomolies
template = raster("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/PAs/background/template.gri")

    #generates a sequence that is length 1 to the number of cells in the template raster   
string = seq(1:ncell(template))
    #changes from Formal Class RasterLayer object to a Large RasterLayer object with the number of cells equal to that of 'string'
template[] = string 
plot(template)

#-------------------------------
#import the location data -- must be in below format 
#-------------------------------
# column 'date' -> YYYY-MM-DD in *character* format (will likely need to be reformatted from original data)
# column 'lon' -> 0 to 360 longitude column in numeric format (will likely need to be transformed from -180 to 180 formated in original data)
# column 'lat' -> -90 to 90 latitude column in numeric format (will likely be in correct format in original data)

mako_pa <- loc_dat %>%
  mutate(date2 = substr(date, 1, 10), 
         date = as.Date(date2)) %>%
  subset(select = c(id, lon_p, lat_p, date))

#reformating the data so the function works
colnames(mako_pa)[1] <- "tag"
colnames(mako_pa)[2] <- "lon"
colnames(mako_pa)[3] <- "lat"

mako_pa$lon[mako_pa$lon < 0] <- mako_pa$lon[mako_pa$lon < 0] + 360 # changes the meridian line, so any lon < 0, add 360 so they are also centered
min(mako_pa$lon)
max(mako_pa$lon)
min(mako_pa$lat)
max(mako_pa$lat)

head(mako_pa)

#-----------------------------
#create the background PA function within a minimum bounding polygon of tracking data 
#-----------------------------
    #function to convert -180/180 lon to 0/360 lon 
to360 <- function(x) {x %% 360}

  #PA generation function arguments 
# dat -> your presence dataframe, with lat, lon, and date columns formatted as in example (okay to have extra columns)
# csv_outdir -> where you want to save your csv with presences and pseudo-absences
# poly_outdir -> where you want to save the minimum bounding polygon that will be used to constrain where pseudo-absences are generated. We want to save this because we might want to use it later to constrain model predictions so we don't innaccurately spatially extrapolate
# sp_name -> species name, no spaces, e.g. swordfish. Note: good to decide upfront 1 common name for each species that you use everytime you save a file. This will make coding a lot easier down the line

generate_pseudo_abs_fcn = function(dat, csv_outdir, poly_outdir, sp_name){
  
  ## selecting lat lon coords 
  dat_coords = dat %>%
    dplyr::select(c(lon, lat))
  
  ## presences, creates unique identifier for each presence based on the pixel it falls in
  dat_presences = dat %>%
    mutate(presAbs = 1, 
           unique = raster::extract(template, dat_coords), 
           unique2 = glue("{date}{unique}"))
  
  ## creates a minimum bounding polygon to constrain where PAs are generated around the presences
  ylims <- c(2.98, 2.98, 47.37,  47.37, 2.98)
  xlims <- c(208.92, 254.59, 254.59, 208.92, 208.92)
  box_coords <- tibble(x = xlims, y = ylims)
  
  coords <- box_coords
  #ch <- chull(x = dat_coords$lon, y = dat_coords$lat) #chull function computes the subset of points which lie in the space of the points specified (areas of overlap)
  #coords <- dat_coords[c(ch, ch[1]), ]
  plot(dat_coords, pch = 19)
  lines(coords, col = "red")
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID = 1)))
  crs(sp_poly) = crs(world_final)
  final_poly = erase(sp_poly, world_final) #removes areas of overlap between world data and the spatial polygon with the presence coords
  
  ##generate PAs
  abs = spsample(final_poly, nrow(dat_coords)*2, type = "random") %>%
    as.data.frame() #generate more than needed (double) so can remove those in the same pixel/day as the presences 
  absence = dat_presences %>%
    rbind(dat_presences) %>%
    mutate(lon = abs$x, lat = abs$y)
  absence_coors = absence %>% dplyr:: select(c(lon, lat))
  dat_absence = absence %>%
    mutate(unique = raster::extract(template, absence_coors), 
           unique2 = glue("{date}{unique}")) %>%
    filter(!(unique2 %in% dat_presences$unique2)) %>%
    .[sample(nrow(.), replace = TRUE, nrow(dat_presences)),] %>%
    mutate(presAbs = 0)
  
  ## write out species pres/PAs dataset 
  final = rbind(dat_presences, dat_absence) %>%
    mutate(presAbs = as.character(presAbs)) %>%
    arrange(presAbs)
  write.csv(final, glue("{csv_outdir}/{sp_name}_pres_Abs.csv"))
  
  ## write out minimum bounding polygon
  final_poly_sf = st_as_sf(final_poly)
  st_write(final_poly_sf, glue("{poly_outdir}/{sp_name}.shp"), delete_layer = T)
  
}

#------------------------------
#run the PA buffer function - generated above
#-----------------------------
csvdir = "C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/PAs/background/psat_spot_domain"
polydir = "C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/PAs/background/psat_spot_domain"
species = "mako"
set.seed(1004)

generate_pseudo_abs_fcn(dat = mako_pa, 
                        csv_outdir = csvdir, 
                        poly_outdir = polydir, 
                        sp_name = species)

#------------------------------
# check the results 
#-----------------------------
PA_dat = read.csv(glue("{csvdir}/{species}_pres_Abs.csv"))
head(PA_dat)
str(PA_dat)
poly = st_read(glue("{polydir}/{species}.shp"))

ggplot()+
  geom_polygon(data = fortify(maps::map("world2", plot = F, fill = T)), aes(x = long, y = lat, group = group), color = "grey20", fill = "grey60")+ #fortify converts spatial objects to data frames for ggplot
  geom_point(data = PA_dat, aes(x = lon, y = lat, color = as.factor(presAbs), group = presAbs), size = 0.7, alpha = 0.8)+
  geom_sf(data = poly, fill = NA, color = "black") + 
  scale_color_manual("PresAbs", values = c("1" = "goldenrod2", "0" = "dodgerblue4")) + 
  coord_sf(xlim = c(minx, maxx), ylim = c(miny, maxy)) +
  ggtitle(glue("{species} npre = {nrow(PA_dat)} (1:1 ratio)"))+
  theme_tq()

ggsave("figs/buffer_ps.png", width = 40, height = 20, units = "cm")

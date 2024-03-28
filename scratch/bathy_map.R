#libraries
library(tidyverse)
library(here)
library(terra)
library(tidyquant)

#data
  # tag locations
    ## feel free to replace tag_locs df name below -- this is where I plotted the point locations for the sharks

  # bathy file (I've included the transformations I used here)
r = rast(here("data/enviro/bathy/GEBCO_2023_n58.0_s4.0_w-147.0_e-97.0.nc"))
r_lowres <- aggregate(r, fact=6) # I downsampled to make it easier to plot

bathy_df <- as.data.frame(r_lowres,xy = TRUE)
bathy_df <- bathy_df %>%  filter(elevation <= 0) 

#coast coordinates
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="USA"
                      | north_map$region=="Mexico",]


#bathy map 
mycolors <- c("#08306B","#023858", "#034B76", "#0B559F", "#045D92","#0469A6","#1379B4","#2F8BBD","#6BAED6", "#509AC6","#74A9CF","#88BEDC", "#90B4D5","#ACBFDC","#C4CBE2" )
show_col(mycolors)

ggplot(shore, aes(long, lat)) +
  coord_map("mercator", xlim=c(-140, -110), ylim=c(10,50)) +
  geom_polygon(aes(group=group), fill="grey60",lwd=1) +
  geom_contour_filled(data=bathy_df, 
                      aes(x,y,z= elevation)) +#breaks=seq(0,1,by=0.2)
  scale_fill_manual(values = mycolors)+
  geom_point(data = tag_locs, aes(x = lon, y = lat), color = "goldenrod", shape = 1, alpha = 0.8)+
  theme_tq()+
  theme(legend.position = "right")+
  guides(fill = guide_legend(title="Bathymetry (m)", reverse = TRUE))

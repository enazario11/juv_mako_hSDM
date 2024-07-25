### libraries ####
{library(tidyverse)
  library(here)
  library(MetBrewer)
  library(terra)}

### saved custom themes ####
theme_bls_map <- function(){ 
  font <- "Arial"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      plot.title = element_text(             #axis titles
        family = font,            #font family
        color = "black",
        size = 16), 
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        color = "black",
        size = 16),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        color = "black",
        size = 16),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      legend.position = "none"
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

### Figure 1: locs over study area bathymetry ####
#### presence location data ####
#shark data
ani_locs <- readRDS(here("data/presence_locs/psat_spot_domain/processed/psat_spot_animotum.RDS")) %>% 
  mutate(PA = 0, rep = NA) %>% 
  subset(select = -c(geometry))

names(ani_locs) <- c("tag", "date", "lon", "lat", "PA", "rep")

#coastline data
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="USA"
                      | north_map$region=="Mexico",]

#bathy data
r = rast(here("data/enviro/psat_spot_all/bathy_gebco/processed/gebco_bathy_0.25deg2.nc"))
bathy_df <- as.data.frame(r,xy = TRUE)
bathy_df <- bathy_df %>%
  filter(gebco_bathy_0.25deg2 <= 0)

mycolors <- c("#08306B","#023858", "#034B76", "#0B559F", "#045D92","#0469A6","#1379B4","#2F8BBD","#6BAED6", "#509AC6","#74A9CF","#88BEDC", "#90B4D5","#ACBFDC","#C4CBE2" )

ggplot(shore, aes(long, lat)) +
  #plot coastline
  coord_map("mercator", xlim = c(-153, -103), ylim = c(1, 49))+
  geom_polygon(aes(group=group), fill="grey75",lwd=1) +
  #plot bathymetry map
  geom_contour_filled(data=bathy_df, 
                      aes(x,y,z=gebco_bathy_0.25deg2)) +#breaks=seq(0,1,by=0.2)
  scale_fill_manual(values = mycolors)+
  #plot shark locations
  geom_path(data=ani_locs, aes(lon, lat, colour=as.factor(tag)), size = 0.5) +
  scale_color_manual(values = met.brewer("OKeeffe2", 73)) +
theme_bls_map() +
  xlim(-153, -103)+
  ylim(1, 49) +
  theme(legend.position = "right") + 
  guides(fill = guide_legend(title = "Bathymetry (m)", reverse = TRUE), 
         color = FALSE)

ggsave(here("figs/ms/tracks_bathy.png"), height = 7, width = 5, units = c("in"))

### Supplementary files ####
#### ST1: Shark metadata ####
md_df <- readRDS(here("data/presence_locs/psat_spot_domain/psat_spot_data.rds"))
spot <- read.csv(here("data/presence_locs/mako_spot_filtered_1_step_per_day.csv"))
spot_ptts <- unique(spot$PTT)

md_df2 <- md_df %>% 
  group_by(ptt) %>%
  arrange(date) %>%
  mutate(deploy_dur = difftime(tail(date, n = 1), head(date, n = 1), units = "days"), 
         tag_type = ifelse(ptt %in% spot_ptts, "SPOT", "SPOT + PSAT")) %>%
  distinct(ptt, .keep_all = TRUE) %>%
  subset(select = c(ptt, tag_type, FL, sex, deploy_dur, date, lat, lon))

#write.csv(md_df2, here("data/presence_locs/metadata_sum.csv"))











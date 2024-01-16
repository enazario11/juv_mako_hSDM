#load packages
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(raster)
library(oce)
library(chron)
library(lubridate)
library(tidyquant)
library(scales)
library(tidync)
library(data.table)
library(ggOceanMaps)
library(ggOceanMapsData)
library(RColorBrewer)
library(data.table)
library(ncdf4)
library(viridis)

#load the data 
shark_locs <- read.csv("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/shark_AgeLocs.csv")

shark_locs <- shark_locs %>%
  mutate(date_time = as.character(paste0(Date, " ", Time)))
shark_locs$posix = as.POSIXct(strptime(shark_locs$date_time,                               format = "%m/%d/%Y %H:%M"))
shark_locs$Time = format(as.POSIXct(shark_locs$posix), format = "%H:%M")
shark_locs$Date = format(as.POSIXct(shark_locs$posix), format = "%m/%d/%Y")

#plot map by season and age class 
shark_locs = shark_locs %>%
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

shark_locs$Season <- getSeason(shark_locs$Date)
shark_locs$Age.Class <- factor(shark_locs$Age.Class, levels = c("Adult", "SA", "Age-2", "YOY"))

north_map = map_data("world") %>% 
  group_by(group)
shore = north_map[north_map$region== "USA" #find USA map without Alaska
                  | north_map$region=="Mexico"
                  | north_map$region=="Canada",]

ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-125, -110), ylim = c(15, 40))+
  geom_point(data=shark_locs, aes(Lon,Lat,colour=Age.Class), size=0.2) +
  # coord_map("azequidistant", xlim=c(-60,0), ylim=c(43,78)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  labs(color = "Age Class")+
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right", 
        legend.key.size = unit(0.75,'cm'), 
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill='white'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'), 
        legend.box.background = element_rect(fill='transparent'),
        panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.margin=margin(0,0,0,0),
        strip.text = element_text(colour='white',size=16,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        text = element_text(color = "grey90", size = 20), 
        axis.text = element_text(color = "grey90", size = 20), 
        axis.title = element_text(color = "grey90", size = 20))+
  scale_color_manual(values = c("#45818e", "#EBA832", "#1c4587","grey30"))+
  facet_wrap(~Season)

ggsave("images/QE Figs/season_age_map.png", width = 250, height = 400, units = "mm")

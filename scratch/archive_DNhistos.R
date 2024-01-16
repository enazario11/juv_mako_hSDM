library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tidyquant)
library(chron)

#------------------------------
#Reshape archive data to one DF
#------------------------------
tagnum <- 78156 #60646,63984
setwd(paste0("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/WC_dat/PTT folders/",tagnum))
dat3 <- list.files(pattern='Archive.csv')
dat3 <- read.csv(dat)

dat1a <- dat1[c(1,2,4)]
names(dat1a)[3] = "Temp"
dat1a$ID = 60646
dat1a$age_class = "Adult"
dat1a$region = "SCB"
dat2a <- dat2[c(1,2,3)]
names(dat2a)[3] = "Temp"
dat2a$ID = 63984
dat2a$age_class = "SA"
dat2a$region = "SCB"
dat3a <- dat3[c(1,2,4)]
names(dat3a)[3] = "Temp"
dat3a$ID = 78156
dat3a$age_class = "AgeTwo"
dat3a$region = "SCB"

archive_dat <- rbind(dat1a, dat2a, dat3a)
head(archive_dat)
unique(archive_dat$ID)
str(archive_dat)

#change dates and times to better format
names(archive_dat)[1] = "Date_Time"
archive_dat$Date <- substr(archive_dat$Date_Time, 10,20)
archive_dat$Time <- substr(archive_dat$Date_Time, 1,8)

archive_dat <- archive_dat %>%
  mutate(Date = dmy(Date), 
         Time = hms(Time))


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

archive_dat$season <- getSeason(archive_dat$Date)

save(archive_dat, file = "C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/archive/df_archive.RData")

#---------------
#Plot D/N histos
#---------------
df_archive <- get(load("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/archive/df_archive.RData"))
head(df_archive)

#hist of depth (max and med) by time of day 
df_archive <- df_archive %>%
  mutate(day_per = ifelse(Time@hour > 7 & 19 > Time@hour, "Day", "Night")) %>%
  filter(day_per != "NA")

unique(df_archive$day_per)

#all shark dive depth by time period
ggplot(df_archive, aes(x = Depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Median Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = c("goldenrod2", "#81a9ad"))+
  scale_color_manual(values = c("goldenrod3", "#537380"))+
  xlim(0,150)

#back to back night and day histogram (ggplot version of RchivalTag output)
day_depth <- df_archive %>%
  filter(day_per == "Day") %>%
  ggplot(aes(x = Depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 50)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "goldenrod2")+
  scale_color_manual(values = "goldenrod3") + 
  coord_flip()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme(legend.position = "none")+
  xlim(175,0)+
  geom_vline(aes(xintercept = mean(Depth, na.rm = TRUE)), color = "black", linetype = 2)+
  theme(plot.margin = margin(t = 0,  # Top margin
                             r = 0.5,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0)) # Left margin

night_depth <- df_archive %>%
  filter(day_per == "Night") %>%
  ggplot(aes(x = Depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 50)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Median Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "#81a9ad")+
  scale_color_manual(values = "#537380")+
  coord_flip()+
  scale_x_reverse()+
  theme(legend.position = "none", 
        axis.text.y = element_blank())+
  xlab("")+
  xlim(175,0)+
  geom_vline(aes(xintercept = mean(Depth, na.rm = TRUE)), color = "black", linetype = 2)+
  theme(plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0)) # Left margin
  

DN_histos <- ggarrange(day_depth, night_depth, nrow = 1)
DN_histos

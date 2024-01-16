## load package
library(tidyverse)
library(lubridate)
library(tidyquant)
library(ggpubr)
library(chron)
library(scales)
library("mapview")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")

rm(list = ls())

dat <- read.csv("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/tdl.csv")

#reformat columns in data file. May need to change how time is logged. Can do this later if an issue.
dat <- dat %>%
  mutate(ptt = as.factor(ptt),
         Sex = as.factor(Sex),
         posix = as.POSIXct(strptime(posix, format = "%m/%d/%Y %H:%M")), 
         time = substr(posix, 11, 18),
         hour = hour(posix),
         date = substr(posix, 1, 10),
         month = str_pad(month, 2, pad = "0"),
         day = str_pad(day, 2, pad = "0"),
         mo_day = paste0(month,day), 
         mo_day = as.numeric(mo_day), 
         year = as.factor(year),
         depth_neg = med_depth*-1,
         age_class = as.factor(age_class), 
         ID = as.factor(ID), 
         region = as.factor(region)) %>%
  filter(med_depth < 120, 
         max_depth < 1000, 
         avg_depth < 120) %>%
  rename(sex = Sex)
dat$age_class <- factor(dat$age_class, levels = c("Adult", "SA", "Age-2", "YOY"))

#Figures I have from other scripts: Maps (loc.R), histos of depth and temp by year (pdt.R), all figs from this script combining loc, depth, and temp data from tags

#smooth lines of depth and temp averaged by year
#depth
dy <- ggplot(dat, aes(x = mo_day, y = depth_neg)) +
  geom_smooth(se = F, method = "gam", color = "#537380")+
  theme_tq()+
  ylab("Depth (m)")+
  xlab("")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_continuous(breaks= c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
#temp
ty <- ggplot(dat, aes(x = mo_day, y = med_temp)) +
  geom_smooth(se = F, method = "gam", color = "#537380")+
  theme_tq()+
  ylab("Temp (C)")+
  xlab("")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_continuous(breaks= c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ggarrange(dy, ty, nrow = 1, common.legend = T)

#smooth lines of depth and temp by region
#depth
dr <- ggplot(dat, aes(x = mo_day, y = depth_neg, color = region)) +
  geom_smooth(se = F)+
  theme_tq()+
  ylab("Depth (m)")+
  xlab("")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_continuous(breaks= c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_color_manual(values = c("#537380","goldenrod2"))
#temp
tr <- ggplot(dat, aes(x = mo_day, y = med_temp, color = region)) +
  geom_smooth(se = F)+
  theme_tq()+
  ylab("Temp (C)")+
  xlab("")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_continuous(breaks= c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_color_manual(values = c("#537380", "goldenrod2"))

ggarrange(dr, tr, nrow = 1, common.legend = T)

#smooth lines averaged by age class and region
#depth
dar <- ggplot(dat, aes(x = mo_day, y = depth_neg)) +
  geom_smooth(se = F, color = "#537380")+
  theme_tq()+
  ylab("Depth (m)")+
  xlab("")+ 
  facet_grid(rows = vars(region), cols = vars(age_class), scales = "free")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_continuous(breaks= c(100, 300, 500, 700, 900, 1100),
                     labels=c("Jan", "Mar", "May", "Jul", "Sep", "Nov"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#temp
tar <- ggplot(dat, aes(x = mo_day, y = med_temp)) +
  geom_smooth(se = F, color = "#537380")+
  theme_tq()+
  ylab("Temp (C)")+
  xlab("")+ 
  facet_grid(rows = vars(region), cols = vars(age_class), scales = "free")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_continuous(breaks= c(100, 300, 500, 700, 900, 1100),
                     labels=c("Jan", "Mar", "May", "Jul", "Sep", "Nov"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggarrange(dar, tar, nrow = 1, common.legend = T)


#hist of depth by region 
dr_med <- ggplot(dat, aes(x = med_depth))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~region, scales = "free")+
  xlab("Median Depth (m)")+ 
  scale_x_continuous(breaks = round(seq(min(dat$med_depth, na.rm = T), max(dat$med_depth, na.rm = T), by = 25),1))

dr_max <- ggplot(dat, aes(x = max_depth))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~region, scales = "free")+
  xlab("Max Depth (m)")+ 
  scale_x_continuous(breaks = round(seq(min(dat$max_depth, na.rm = T), max(dat$max_depth, na.rm = T), by = 50),1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggarrange(dr_med, dr_max)

#hist of depth by age class 
ac_med <- ggplot(dat, aes(x = med_depth))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_grid(rows = vars(region), cols = vars(age_class), scales = "free")+
  xlab("Median Depth (m)")+ 
  scale_x_continuous(breaks = round(seq(min(dat$med_depth, na.rm = T), max(dat$med_depth, na.rm = T), by = 25),1))

ac_max <- ggplot(dat, aes(x = max_depth))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_grid(rows = vars(region), cols = vars(age_class), scales = "free")+
  xlab("Max Depth (m)")+ 
  scale_x_continuous(breaks = round(seq(min(dat$max_depth, na.rm = T), max(dat$max_depth, na.rm = T), by = 50),1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggarrange(ac_med, ac_max)

#hist of depth (max and med) by time of day 
dat_time <- dat %>%
  mutate(time = times(time), 
         day_per = ifelse(hour > 7 & 19 > hour, "Day", "Night")) %>%
  filter(day_per != "NA")

#median
ggplot(dat_time, aes(x = med_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Median Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = c("goldenrod2", "#81a9ad"))+
  scale_color_manual(values = c("goldenrod3", "#537380"))

#max
ggplot(dat_time, aes(x = max_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Max Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = c("goldenrod2", "#81a9ad"))+
  scale_color_manual(values = c("goldenrod3", "#537380"))

#back to back night and day histogram (ggplot version of RchivalTag output)
#median depth  -- YOY
day_YOY <- dat_time %>%
  filter(day_per == "Day" & age_class == "YOY") %>%
  ggplot(aes(x = med_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Median Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "goldenrod2")+
  scale_color_manual(values = "goldenrod3") + 
  coord_flip()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme(legend.position = "none")

night_YOY <- dat_time %>%
  filter(day_per == "Night" & age_class == "YOY") %>%
  ggplot(aes(x = med_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
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
  xlab("")

YOY <- ggarrange(day_YOY, night_YOY, nrow = 1, labels = "YOY", hjust = -1.5, vjust = 2)

#median depth -- AGE TWO
day_AT <- dat_time %>%
  filter(day_per == "Day" & age_class == "Age-2") %>%
  ggplot(aes(x = med_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Median Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "goldenrod2")+
  scale_color_manual(values = "goldenrod3") + 
  coord_flip()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme(legend.position = "none")+
  xlab("")

night_AT <- dat_time %>%
  filter(day_per == "Night" & age_class == "Age-2") %>%
  ggplot(aes(x = med_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
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
  xlab("")

AT <- ggarrange(day_AT, night_AT, nrow = 1, labels = "Age Two", hjust = -0.8, vjust = 2)

#median depth -- SA
day_SA <- dat_time %>%
  filter(day_per == "Day" & age_class == "SA") %>%
  ggplot(aes(x = med_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Median Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "goldenrod2")+
  scale_color_manual(values = "goldenrod3") + 
  coord_flip()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme(legend.position = "none")

night_SA <- dat_time %>%
  filter(day_per == "Night" & age_class == "SA") %>%
  ggplot(aes(x = med_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
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
  xlab("")

SA <- ggarrange(day_SA, night_SA, nrow = 1, labels = "SA", hjust = -2, vjust = 2)

#median depth -- ADULT
day_A <- dat_time %>%
  filter(day_per == "Day" & age_class == "Adult") %>%
  ggplot(aes(x = med_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Median Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "goldenrod2")+
  scale_color_manual(values = "goldenrod3") + 
  coord_flip()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme(legend.position = "none") + 
  xlab("")

night_A <- dat_time %>%
  filter(day_per == "Night" & age_class == "Adult") %>%
  ggplot(aes(x = med_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
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
  xlab("")

A <- ggarrange(day_A, night_A, nrow = 1, labels = "Adult", hjust = -1.3, vjust = 2)

ggarrange(YOY, AT, SA, A, nrow = 2, ncol = 2)


#back to back night and day histogram (ggplot version of RchivalTag output)
#max depth -- YOY
day_YOYm <- dat_time %>%
  filter(day_per == "Day" & age_class == "YOY") %>%
  ggplot(aes(x = max_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Max Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "goldenrod2")+
  scale_color_manual(values = "goldenrod3") + 
  coord_flip()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme(legend.position = "none")

night_YOYm <- dat_time %>%
  filter(day_per == "Night" & age_class == "YOY") %>%
  ggplot(aes(x = max_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Max Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "#81a9ad")+
  scale_color_manual(values = "#537380")+
  coord_flip()+
  scale_x_reverse()+
  theme(legend.position = "none", 
        axis.text.y = element_blank())+
  xlab("")

YOYm <- ggarrange(day_YOYm, night_YOYm, nrow = 1, labels = "YOY", hjust = -1.5, vjust = 2)

#max depth -- AGE TWO
day_ATm <- dat_time %>%
  filter(day_per == "Day" & age_class == "Age-2") %>%
  ggplot(aes(x = max_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Max Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "goldenrod2")+
  scale_color_manual(values = "goldenrod3") + 
  coord_flip()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme(legend.position = "none")+
  xlab("")

night_ATm <- dat_time %>%
  filter(day_per == "Night" & age_class == "Age-2") %>%
  ggplot(aes(x = max_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Max Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "#81a9ad")+
  scale_color_manual(values = "#537380")+
  coord_flip()+
  scale_x_reverse()+
  theme(legend.position = "none", 
        axis.text.y = element_blank())+
  xlab("")

ATm <- ggarrange(day_ATm, night_ATm, nrow = 1, labels = "Age Two", hjust = -0.8, vjust = 2)

#max depth -- SA
day_SAm <- dat_time %>%
  filter(day_per == "Day" & age_class == "SA") %>%
  ggplot(aes(x = max_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Max Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "goldenrod2")+
  scale_color_manual(values = "goldenrod3") + 
  coord_flip()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme(legend.position = "none")

night_SAm <- dat_time %>%
  filter(day_per == "Night" & age_class == "SA") %>%
  ggplot(aes(x = max_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Max Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "#81a9ad")+
  scale_color_manual(values = "#537380")+
  coord_flip()+
  scale_x_reverse()+
  theme(legend.position = "none", 
        axis.text.y = element_blank())+
  xlab("")

SAm <- ggarrange(day_SAm, night_SAm, nrow = 1, labels = "SA", hjust = -2, vjust = 2)

#max depth -- ADULT
day_Am <- dat_time %>%
  filter(day_per == "Day" & age_class == "Adult") %>%
  ggplot(aes(x = max_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Max Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "goldenrod2")+
  scale_color_manual(values = "goldenrod3") + 
  coord_flip()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme(legend.position = "none") + 
  xlab("")

night_Am <- dat_time %>%
  filter(day_per == "Night" & age_class == "Adult") %>%
  ggplot(aes(x = max_depth, fill = day_per, color = day_per))+
  geom_histogram(bins = 15)+
  theme_tq()+
  #facet_grid(rows = vars(day_per), cols = vars(age_class), scales = "free")+
  xlab("Max Depth (m)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = "#81a9ad")+
  scale_color_manual(values = "#537380")+
  coord_flip()+
  scale_x_reverse()+
  theme(legend.position = "none", 
        axis.text.y = element_blank())+
  xlab("")

Am <- ggarrange(day_Am, night_Am, nrow = 1, labels = "Adult", hjust = -1.3, vjust = 2)

ggarrange(YOYm, ATm, SAm, Am, nrow = 2, ncol = 2)


#median depth (panel a) and max depth (panel b) vs date by region
dat_time <- dat_time %>%
  mutate(date = format(as.Date(date), "%m-%d"), 
         month = as.factor(month))

#median
ggplot(dat_time, aes(x = date, y = med_depth, color = month))+
  geom_point(na.rm = TRUE)+
  theme_tq()+
  facet_grid(rows = vars(region), scales = "free")+
  ylab("Median Depth (m)")+ 
  xlab("")+
  scale_color_manual(values = c("#537380", "#81a9ad","#de9b71", "#c67b6f", "goldenrod2", "#f57946", "#516823", "#00496f", "grey60"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

#max
ggplot(dat_time, aes(x = date, y = max_depth, color = month))+
  geom_point(na.rm = TRUE)+
  theme_tq()+
  facet_grid(rows = vars(region), scales = "free")+
  ylab("Maximum Depth (m)")+ 
  xlab("")+
  scale_color_manual(values = c("#537380", "#81a9ad","#de9b71", "#c67b6f", "goldenrod2", "#f57946", "#516823", "#00496f", "grey60"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

#median temp (panel a) and max temp (panel b) vs date by region
dat_time <- dat_time %>%
  mutate(date = format(as.Date(date), "%m-%d"), 
         month = as.factor(month))

#median -- consider changing to average instead of median for temp data??
ggplot(dat_time, aes(x = date, y = med_temp, color = month))+
  geom_point(na.rm = TRUE)+
  theme_tq()+
  facet_grid(rows = vars(region), scales = "free")+
  ylab("Median temperature (C)")+ 
  xlab("")+
  scale_color_manual(values = c("#537380", "#81a9ad","#de9b71", "#c67b6f", "goldenrod2", "#f57946", "#516823", "#00496f", "grey60"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

#max
ggplot(dat_time, aes(x = date, y = max_temp, color = month))+
  geom_point(na.rm = TRUE)+
  theme_tq()+
  facet_grid(rows = vars(region), scales = "free")+
  ylab("Maximum temperature (C)")+ 
  xlab("")+
  scale_color_manual(values = c("#537380", "#81a9ad","#de9b71", "#c67b6f", "goldenrod2", "#f57946", "#516823", "#00496f", "grey60"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

#median temp vs. median depth by region across all sharks grouped by month 
ggplot(dat_time, aes(x = med_depth, y = med_temp))+
  geom_point(na.rm = TRUE)+
  theme_tq()+
  facet_grid(rows = vars(region), cols = vars(month), scales = "free")+
  xlab("Median depth (m)")+
  ylab("Median temp (C)")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#maps of tracks with depth and temperature information -- grouped by month (eventually change to character month isntead of numeric)

#depth -- median 
north_map = map_data("world") %>% 
  group_by(group)
shore = north_map[north_map$region== "USA" #find USA map without Alaska
                  | north_map$region=="Mexico",]

dat_med <- dat %>%
  filter(med_depth != "NA")
map_depth_med <- ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-125, -112), ylim = c(27, 40))+
  geom_point(data=dat_med, aes(Lon,Lat,colour=med_depth), size=1)+
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  theme(legend.position = "right") +
  facet_wrap(~month) + 
  scale_color_viridis_c(option = "magma")
map_depth_med

#depth -- max
dat_max <- dat %>%
  filter(max_depth != "NA")
map_depth_max <- ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-125, -112), ylim = c(27, 40))+
  geom_point(data=dat_max, aes(Lon,Lat,colour=max_depth), size=1)+
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  theme(legend.position = "right") +
  facet_wrap(~month) + 
  scale_color_viridis_c(option = "magma")
map_depth_max

#temp -- median 
dat_medT <- dat %>%
  filter(med_temp != "NA")
map_temp_med <- ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-125, -112), ylim = c(27, 40))+
  geom_point(data=dat_medT, aes(Lon,Lat,colour=med_temp), size=1)+
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  theme(legend.position = "right") +
  facet_wrap(~month) + 
  scale_color_viridis_c(option = "magma")
map_temp_med

#temp -- max
dat_maxT <- dat %>%
  filter(max_temp != "NA")
map_temp_max <- ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-125, -112), ylim = c(27, 40))+
  geom_point(data=dat_maxT, aes(Lon,Lat,colour=max_temp), size=1)+
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  theme(legend.position = "right") +
  facet_wrap(~month) + 
  scale_color_viridis_c(option = "magma")
map_temp_max

#temp -- min
dat_minT <- dat %>%
  filter(min_temp != "NA")
map_temp_min <- ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-125, -112), ylim = c(27, 40))+
  geom_point(data=dat_minT, aes(Lon,Lat,colour=min_temp), size=1)+
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  theme(legend.position = "right") +
  facet_wrap(~month) + 
  scale_color_viridis_c(option = "magma")
map_temp_min

## install or load package
library(RchivalTag)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tidyquant)
library(pracma)
library(xts)
library(ggpubr)

rm(list = ls())

setwd("data/pdt")

### change date format across all PDT files, select matching columns, bind to single DF####
files = list.files("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/pdt")
files

pdt.comb = NULL
for (i in 1:length(unique(files))) {
  id = read.csv(files[[i]]) %>%
    as_tibble()
  id = id %>%
    filter(!is.na(Date)) %>% #filter out NA
    subset(select = c(Ptt, Date, Depth1, MaxTemp1, Depth2, MaxTemp2, Depth3, MaxTemp3, Depth4, MaxTemp4, Depth5, MaxTemp5, Depth6, MaxTemp6, Depth7, MaxTemp7, Depth8, MaxTemp8, Depth9, MaxTemp9, Depth10, MaxTemp10, Depth11, MaxTemp11, Depth12, MaxTemp12))
  #changes date/time to format R can better interpret
  id$Date = as.Date(id$Date, origin = "1900-01-01")
  pdt.comb = rbind(pdt.comb, id) #adds extra rows at bottom to combine all pdt files
}

head(pdt.comb)
table(pdt.comb$Ptt)

pdt.comb_L <- gather(pdt.comb, depth_num, depth_v, Depth1, Depth2, Depth3, Depth4, Depth5, Depth6, Depth7, Depth8, Depth9, Depth10, Depth11, Depth12)
pdt.comb_L <- pdt.comb_L %>% 
  gather(temp_type, temp_v, MaxTemp1, MaxTemp2, MaxTemp3, MaxTemp4, MaxTemp5, MaxTemp6, MaxTemp7, MaxTemp8, MaxTemp9, MaxTemp10, MaxTemp11, MaxTemp12)  %>%
  filter(depth_v < 1000 & depth_v > 0) %>%
  mutate(year = substr(Date, 1, 4), 
         cal_d = substr(Date, 1, 10)) %>%
  filter(Ptt != 77178 | 77181)

###ggplot version of RchivalTag PDT figures#### 
#depth and temp histos including zero -- CHANGE BINS TO TAG SETTINGS
#depth
dh_0 <- ggplot(pdt.comb_L, aes(x = depth_v))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  xlim(0,-400) + 
  facet_wrap(~year, scales = "free")+
  xlab("Depth (m)")
dh_0

#temp
th_0 <- ggplot(pdt.comb_L, aes(x = temp_v))+
  geom_histogram(fill = "grey", color = "black", bins = 10)+
  theme_tq()+
  facet_wrap(~year, scales = "free")+
  xlab("Temp (C)")
th_0

ggarrange(dh_0, th_0, nrow = 1)

#depth and temp histos not including zero
Pdt_no <- pdt.comb_L %>%
  filter(depth_v < -10)

dh <- ggplot(Pdt_no, aes(x = depth_v))+
  geom_histogram(fill = "grey", color = "black", bins = 7)+
  theme_tq()+
  facet_wrap(~year, scales = "free")+
  xlab("Depth (m)")+
  xlim(-10,-400)
dh

#temp
th <- ggplot(Pdt_no, aes(x = temp_v))+
  geom_histogram(fill = "grey", color = "black", bins = 5)+
  theme_tq()+
  facet_wrap(~year, scales = "free")+
  xlab("Temp (C)")
th

ggarrange(dh, th, nrow = 1)

### Depth and temp by loc data ####
#summarize temp/depth data by date (get one value for each day)
#detach("plyr") #run if below chunk not working
loc_pdt = pdt.comb_L %>%
  group_by(Ptt, cal_d) %>%
  summarise(avg_depth = mean(depth_v, na.rm = T), 
         max_depth = max(depth_v, na.rm = T),
         med_depth = median(depth_v, na.rm = T),
         avg_temp = mean(temp_v, na.rm = T),
         med_temp = median(temp_v, na.rm = T),
         max_temp = max(temp_v, na.rm = T), 
         min_temp = min(temp_v, na.rm = T)) %>%
  mutate(year = substr(cal_d, 1, 4),
         month = substr(cal_d, 6, 7), 
         day = substr(cal_d, 9, 10), 
         ID = paste0(Ptt, "_", year, month, day))

#due to how dates are stored may have to run through Location_Data script
dat_locs <- read.csv("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/shark_AgeLocs.csv")

dat_locs <- dat_locs %>%
  mutate(month = sprintf("%02d", month), 
         day = sprintf("%02d", day),
         ID = paste0(PTT, "_", year, month, day))

td_locs <- dat_locs %>% #FINAL DF WITH COMBO LOC,TEMP, DEP DATA
  left_join(loc_pdt, by=c("ID"))

td_locs <- td_locs %>%
  mutate(region = ifelse(Lat <= 32.522499, "Baja", "SCB"))

td_locs2 <- td_locs %>%
  subset(select = c(PTT, Date, Lat, Lon, Sex, FL, posix, Time, year.x, month.x, day.x, Age.Class, ID, avg_depth, max_depth, med_depth, avg_temp, med_temp, max_temp, min_temp, region))%>%
  rename(year = year.x, 
         month = month.x, 
         day = day.x, 
         time = Time, 
         ptt = PTT, 
         age_class = Age.Class, 
         date = Date) %>%
  filter(region != "NA")%>%
  mutate(ptt = as.factor(ptt))
td_locs2$age_class <- factor(td_locs2$age_class, levels = c("Adult", "SA", "Age-2", "YOY"))

#setwd("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data")
#write.csv(td_locs2, "tdl.csv")

#filter dates for SCB Ecoregion
loc_dat <- read.csv("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/tdl.csv") #may need if above hasn't been run
loc_dat <- loc_dat %>%
  filter(Lat < 35 & Lat > 25 & Lon > -123 & Lon < -113)

FL_male <- loc_dat %>%
  filter(Sex == "Male") %>%
  filter(FL < 180)
FL_female <- loc_dat %>%
  filter(Sex == "Female") %>%
  filter(FL < 249)
loc_dat2 <-rbind(FL_male, FL_female)

#write.csv(loc_dat2, "data/tdl_scbE.csv") #if re-written, check years for single digits (0007 instead of 2007)

#to remove duplicates 
#test <- td_locs2[!duplicated(td_locs2[,c('ID')]),]


###RchivalTag package figures####

#Read in data using read_histos() function from RchivalTag package
#my test shark data (adapted from tutorial and Nerea script)
pdt_file <- "C:/Users/Emily Nazario/Documents/R/Projects/Preliminary Shark SDMs/data/Mako Data/pdt/41770-PDTs.csv"  
dat <- read.csv(pdt_file)
names(dat)
View(dat)

  #Use Rchival function to transform PDT data to a meaningful DF
PDT <- read_PDT("41770-PDTs.csv",folder="pdt")
str(PDT,1)

#Visualizing the data
m <- interpolate_PDTs(PDT)
str(m,2)

  #Visualizing the daily interpolated depth/temp profiles
image_TempDepthProfiles(m[[1]])
abline(h=30,lty="dashed",col="steelblue4",lwd=3)

  #plot points on depth/temp profiles -- only works in minimized viewing window
image_TempDepthProfiles(m[[1]])
points(PDT$date-.5,PDT$Depth,pch=19, col="grey")

### Forloop through all pdt files to create depth-sst plot per indiv


### List of individuals by age class
##YOY: 72812, 77162
##AGE2: 78138, 78156, 61952
##SA: EVERYONE ELSE
##ADULT: 72813, 78155, 60646

#YOY loop
setwd("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/WC_dat/pdt_YOY")
data_files <- list.files("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/WC_dat/pdt_YOY")

for(i in 1:length(data_files)) { # Head of for-loop
    #create the depth - sst plot from the pdt data
  PDT <- read_PDT(data_files[i])
  m <- interpolate_PDTs(PDT)
    #save the file
  mypath <- file.path("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/images/YOY_depth_sst",paste0("depthsst_", substr(data_files[i], 1,5), ".png", sep = ""))
  png(file=mypath)
    #make the plot
  image_TempDepthProfiles(m[[1]], main = data_files[i])
  dev.off()
}

#Age Two loop
setwd("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/WC_dat/pdt_age_two")
data_files <- list.files("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/WC_dat/pdt_age_two")

for(i in 1:length(data_files)) { # Head of for-loop
  PDT <- read_PDT(data_files[i])
  m <- interpolate_PDTs(PDT)
  #save the file
  mypath <- file.path("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/images/AgeTwo_depth_sst",paste0("depthsst_", substr(data_files[i], 1,5), ".png", sep = ""))
  png(file=mypath)
  #make the plot
  image_TempDepthProfiles(m[[1]], main = data_files[i])
  dev.off()
}

#SA loop
setwd("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/WC_dat/pdt_SA")
data_files <- list.files("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/WC_dat/pdt_SA")

for(i in 1:length(data_files)) { # Head of for-loop
  PDT <- read_PDT(data_files[i])
  m <- interpolate_PDTs(PDT)
  #save the file
  mypath <- file.path("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/images/SA_depth_sst",paste0("depthsst_", substr(data_files[i], 1,5), ".png", sep = ""))
  png(file=mypath)
  #make the plot
  image_TempDepthProfiles(m[[1]], main = data_files[i])
  dev.off()
}

#Adult loop
setwd("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/WC_dat/pdt_A")
data_files <- list.files("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/WC_dat/pdt_A")

for(i in 1:length(data_files)) { # Head of for-loop
  PDT <- read_PDT(data_files[i])
  m <- interpolate_PDTs(PDT)
  #save the file
  mypath <- file.path("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/images/A_depth_sst",paste0("depthsst_", substr(data_files[i], 1,5), ".png", sep = ""))
  png(file=mypath)
  #make the plot
  image_TempDepthProfiles(m[[1]], main = data_files[i])
  dev.off()
}

### Revised RchivalTag w/ Robert's notes ###
test <- read.csv("C:/Users/Emily Nazario/Documents/R/Projects/juv_mako_hSDM/data/test_WC/41770-PDTs.csv")

#Use Rchival function to transform PDT data to a meaningful DF
setwd("data")
PDT <- read_PDT("54570-PDTs.csv",folder="pdt")
str(PDT,1)

#Visualizing the data
m <- interpolate_PDTs(PDT)
str(m,2)

#Visualizing the daily interpolated depth/temp profiles
image_TempDepthProfiles(m[[1]])
abline(h=30,lty="dashed",col="steelblue4",lwd=3)

#plot points on depth/temp profiles -- only works in minimized viewing window
image_TempDepthProfiles(m[[1]])
points(PDT$date-.5,PDT$Depth,pch=19, col="grey")


##Plot location points by depth ranges to see if spatially vary by range 
loc_dat <- read.csv("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/tdl_scbE.csv")

north_map = map_data("world") %>% 
  group_by(group)
shore = north_map[north_map$region== "USA" #find USA map without Alaska
                  | north_map$region=="Mexico",]

#all depths
ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-122.5, -112), ylim = c(25, 34.5))+
  geom_point(data=loc_dat, aes(Lon,Lat), size=0.4) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right", 
        legend.key.size = unit(0.75,'cm'), 
        legend.text = element_text(size = 10), 
        panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        axis.text.x = element_text(angle = 45),
        legend.margin=margin(0,0,0,0),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")))

#0-50m 
loc_dat50 <- loc_dat %>%
  filter(med_depth < 50)

ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-122.5, -112), ylim = c(25, 34.5))+
  geom_point(data=loc_dat50, aes(Lon,Lat), size=0.4) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right", 
        legend.key.size = unit(0.75,'cm'), 
        legend.text = element_text(size = 10), 
        panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        axis.text.x = element_text(angle = 45),
        legend.margin=margin(0,0,0,0),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")))

#50-100m 
loc_dat100 <- loc_dat %>%
  filter(med_depth >= 50 & med_depth < 100)

ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-122.5, -112), ylim = c(25, 34.5))+
  geom_point(data=loc_dat100, aes(Lon,Lat), size=0.4) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right", 
        legend.key.size = unit(0.75,'cm'), 
        legend.text = element_text(size = 10), 
        panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        axis.text.x = element_text(angle = 45),
        legend.margin=margin(0,0,0,0),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")))

#100-150m 
loc_dat150 <- loc_dat %>%
  filter(med_depth >= 100 & med_depth < 150)

ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim = c(-122.5, -112), ylim = c(25, 34.5))+
  geom_point(data=loc_dat150, aes(Lon,Lat), size=0.4) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right", 
        legend.key.size = unit(0.75,'cm'), 
        legend.text = element_text(size = 10), 
        panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        axis.text.x = element_text(angle = 45),
        legend.margin=margin(0,0,0,0),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")))

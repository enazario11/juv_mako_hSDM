#load libraries 
library(tidyverse)
library(here)

#data 
#spot and psat data
loc_dat2 <- read.csv(here::here("data/presence_locs/tdl.csv"))

#52126 spot and 54570 psat case
loc_dat$ptt <- replace(loc_dat$ptt, loc_dat$ptt == "52126", "54570")
loc_dat$FL[loc_dat$ptt == "54570"] <- 150

#54597 spot and 54575 psat case

#54591 spot and 54574 psat case

#spot only data
spot_dat <- read.csv(here("data/presence_locs/mako_spot_filtered_1_step_per_day.csv")) %>% subset(select = 1:19)
spot_dat$sex <- replace(spot_dat$sex, spot_dat$sex == "F", "Female")
spot_dat$sex <- replace(spot_dat$sex, spot_dat$sex == "M", "Male")

#combine datasets
psat_subset <- loc_dat %>%
  subset(select = -c(avg_depth, max_depth, med_depth, avg_temp, med_temp, max_temp, min_temp, region))
psat_subset <- psat_subset %>% 
  subset(select = c(ptt, Sex, FL, date, posix, year, month, day, time, Lat, Lon))
colnames(psat_subset) <- c("ptt", "sex", "FL", "date", "posix", "year", "month", "day", "time", "lat", "lon")
psat_subset$posix <- paste(psat_subset$date, psat_subset$time)
psat_subset$posix <- as.POSIXct(strptime(psat_subset$posix, format = "%m/%d/%Y %H:%M"))
psat_subset$date <- as.POSIXct(strptime(psat_subset$date, format = "%m/%d/%Y"))
psat_subset$lc <- "GL"

spot_subset <- spot_dat %>% 
  subset(select = -c(Species, SPOT_SEQ, date, ID, sec, time))
spot_subset$PDT.date.and.time <- as.POSIXct(strptime(spot_subset$PDT.date.and.time, format = "%m/%d/%Y %H:%M"))
spot_subset$time <- substr(spot_subset$PDT.date.and.time, 12,16)
spot_subset <- spot_subset %>%
  subset(select = c(PTT, sex, size, PDT.date, PDT.date.and.time, year, mo, day, time, lat, long, lc))
colnames(spot_subset) <- c("ptt","sex", "FL", "date", "posix", "year", "month", "day", "time", "lat", "lon", "lc")
spot_subset$date <- as.POSIXct(strptime(spot_subset$date, format = "%m/%d/%Y"))
spot_subset <- spot_subset %>%
  mutate(ptt = as.character(ptt))
  
    #remove FLs of adult animals
#Separate by sex and then assign age class based on FL. Then recombine
FL_male <- spot_subset %>%
  filter(sex == "Male") %>%
  filter(FL < 180)
FL_female <- spot_subset %>%
  filter(sex == "Female") %>%
  filter(FL < 249)

spot_comb <- rbind(FL_female, FL_male) #adds 56 ptts

#temporarily remove odd psat and spot PTTs -- removing odd "duplicate" psats because matching spot tracks have more positions
psat_subset <- psat_subset %>% filter(ptt != "54570" & ptt != "54575" & ptt != "54591")

#combine and save as RDS for SSM and PA generation
all_locs <- rbind(psat_subset, spot_comb) %>% mutate(FL = round(FL, digits = 0))
all_locs$deploy_id <- paste(all_locs$ptt, all_locs$FL, all_locs$sex, sep = "_")

all_locs %>% 
  summarise(n_deploys = length(unique(deploy_id)), #76
            n_pos = n(), #18002
            n_days = length(unique(posix)), #13509
            min_date = min(posix, na.rm = TRUE), #2003-06-25
            max_date = max(posix, na.rm = TRUE), #2013-12-16
            min_lat = min(lat, na.rm = TRUE), #2.77
            max_lat = max(lat, na.rm = TRUE), #47.375
            min_lon = min(lon, na.rm = TRUE), #-150.8
            max_lon = max(lon, na.rm = TRUE)) #-105.69


#bounds for PA develop?
min_lat = min(all_locs$lat, na.rm = TRUE) - 5 #-2.3
max_lat = max(all_locs$lat, na.rm = TRUE) + 5 #52.375
min_lon = min(all_locs$lon, na.rm = TRUE) - 5 #-155.8
max_lon = max(all_locs$lon, na.rm = TRUE) + 5 #-100.69

#depths?
#quantile values of max dives: 0% 8m, 25% 56m, 50% 96m, 75% 160m, 90% 248m

#save RDS
#saveRDS(all_locs, "data/presence_locs/psat_spot_domain/psat_Nspot_data.rds")


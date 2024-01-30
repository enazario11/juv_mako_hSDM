### load packages ####
library(tidyverse)
library(here)
library(tidyquant)

### read data ####
dat <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_0m.rds"))

### convert DO to atm ####
source(here("functions/oxy_demand_functions.R"))
dat_DO_atm <- DO_to_atm(dat, depth = 0)
hist(dat_DO_atm$pO2_0)

### static constants ####
# W = 51807.63; average mass in g for juv. makos as estimated by length-weight relationship. Used average FL of 177.7 cm (from study animals)
# d = 0.700; constant, Clarke et al., 2021
# K = 0.070; adult numbers for K (VBGP) for california animals (fishbase.org). No juv info available.
# j2 = 8.000; constant, Clarke et al., 2021
# j1 = 4.500; constant, Clarke et al., 2021
# OxyThresh = 0.05347; 10th percentile from ambient O2 from data
#Tpref = median(dat$sst, na.rm = T)
# Linf = 321; adult numbers for Linf FL for california animals (fishbase.org)
# LwA = 0.01670 #juvenile numbers from fishbase (New Zealand mixed animals-- closest in age class and ocean basin)
# LwB = 2.847 #juvenile numbers from fishbase (New Zealand mixed animals-- closest in age class and ocean basin)

    #Tested different values from different age class and populations that had the largest sample sizes (NZ and USA) for LwA and LwB and found that range of oxygen demand values at the surface were 0.75 and 0.88. Because these values were close, we chose the population that was in the Pacific ocean basin and contained a mix of age class individuals. 

### mako specific constants ####
#calculate O2 thresh -- as per Clarke et al., 2021 (10th percentile from above (in atm))
quantile(dat_DO_atm$pO2_0, probs = c(0, 0.10, 0.5, 0.75, 1), na.rm = T) 
OxyThresh = 0.1376611 

#calculate temp pref
Tpref = median(dat_DO_atm$thetao_mean, na.rm = T)

#run oxygen demand function with mako specific parameters
O2_demand_mako <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh, T_C = dat_DO_atm$thetao_mean)

  #explore outputs
hist(O2_demand_mako)
plot(dat_DO_atm$thetao_mean, O2_demand_mako) #should increase with temp

#calculate AGI
dat_DO_atm$AGI_0m <- dat_DO_atm$pO2_0/O2_demand_mako

  #explore outputs
hist(dat_DO_atm$AGI_0m)
plot(dat_DO_atm$thetao_mean, dat_DO_atm$AGI_0m)
plot(dat_DO_atm$o2_mean, dat_DO_atm$AGI_0m)
plot(dat_DO_atm$pO2_0, dat_DO_atm$AGI_0m)

quantile(dat_DO_atm$AGI_0m, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)
mean(dat_DO_atm$AGI_0m, na.rm = T)
min(dat_DO_atm$AGI_0m, na.rm = T)
max(dat_DO_atm$AGI_0m, na.rm = T)
sd(dat_DO_atm$AGI_0m, na.rm = T)

#saveRDS(dat_DO_atm, here("data/locs_w_covar/cmems/cmem_locs_covar_AGI_0m.rds"))

#calculate AGI critical value (10th percentile)
AGIcrit <- quantile(dat_DO_atm$AGI_0m, c(.10), na.rm = T) #1.56

map_DO_atm <- dat_DO_atm %>%
  filter(PA == 0) %>%
  mutate(AGI_crit = ifelse(AGI_0m > 1.56, "yes", "no")) #yes or no above AGIcrit

#coarse look at where the sharks were above the AGIcrit 
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="USA"| north_map$region=="Mexico",]

ggplot(shore, aes(long, lat)) +
  coord_map("mercator", xlim=c(-140, -110), ylim=c(10,48)) +
  geom_polygon(aes(group=group), fill="grey60",lwd=1) +
  geom_point(data = map_DO_atm, aes(x = lon, y = lat, color = AGI_crit), shape = 1)+
  scale_color_manual(values = c("red", "blue"))+
  theme_tq()+
  theme(legend.position = "right")

###### SCRATCH WORK ##########
## Visualise AGI's response to changes in DO and Temp. inputs:
o2_range <- seq(0.118, 0.176, length.out = 100)
temp_range <- seq(5.922, 30.1, length.out = 100)
Tpref = 19.19
OxyThresh = 0.1377
AGI_test <- outer(temp_range, o2_range,
                  \(t, o2) o2 / OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh, T_C = t)) 

filled.contour(z = AGI_test)


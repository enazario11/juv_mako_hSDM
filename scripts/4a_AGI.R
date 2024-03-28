### load packages ####
library(tidyverse)
library(here)
library(tidyquant)
library(respR)

### read data ####
dat0 <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_0m.rds"))
dat50 <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_50m.rds"))
dat250 <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_250m.rds"))

source(here("functions/oxy_demand_functions.R"))

### convert DO to atm ####
# 0m -- USE AS MODEL
dat0_DOatm <- DO_to_atm(dat0, depth = 0)
thresh0 <- thresh_atm(temp = median(dat0_DOatm$thetao_mean, na.rm = TRUE), so_psu = median(dat0_DOatm$so_mean, na.rm = T), depth = 0) #defualt do value is 2 mL/L from vetter et al., 2008

hist(dat0_DOatm$pO2_0, xlim = c(0, 0.20))
abline(v = thresh0, lwd = 2)

# 50m
dat50_DOatm <- DO_to_atm(dat50, depth = 50)
thresh50 <- thresh_atm(temp = median(dat50_DOatm$thetao_mean, na.rm = TRUE), so_psu = median(dat50_DOatm$so_mean, na.rm = T), depth = 50)

hist(dat50_DOatm$pO2_50)
abline(v = thresh50, lwd = 2)

  #testing respirometry package instead of respR
# test <- conv_o2(2, "ml_per_l", "mmol_per_l", temp = median(dat0_DOatm$thetao_mean, na.rm = TRUE), sal = median(dat0_DOatm$so_mean, na.rm = T), atm_pres = press_mbar)
# test2 <- test*1000 #convert l to cubic meter

#250m 
dat250_DOatm <- DO_to_atm(dat250, depth = 250)
thresh250 <- thresh_atm(temp = median(dat250_DOatm$thetao_mean, na.rm = TRUE), so_psu = median(dat250_DOatm$so_mean, na.rm = T), depth = 250)

hist(dat250_DOatm$pO2_250)
abline(v = thresh250, lwd = 2)

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
#calculate temp pref
Tpref50 = 16.45201 #50m tpref is 16.452

#run oxygen demand function with mako specific parameters
dat0_DOatm$O2_demand0 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh0, T_C = dat0_DOatm$thetao_mean)
dat_DO_atm50$O2_demand_mako50 <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh, T_C = dat_DO_atm50$thetao_mean)
dat250_DOatm$O2_demand250 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh250, T_C = dat250_DOatm$thetao_mean)


  #explore outputs
hist(dat250_DOatm$O2_demand250)
hist(dat250_DOatm$O2_demand250)
plot(dat0_DOatm$thetao_mean, dat0_DOatm$O2_demand_mako0) #should increase with temp

#calculate AGI
dat0_DOatm$AGI_0m <- dat0_DOatm$pO2_0/dat0_DOatm$O2_demand0
dat50_DOatm$AGI_50m <- dat0_DOatm$pO2_50/dat0_DOatm$O2_demand50
dat250_DOatm$AGI_250m <- dat250_DOatm$pO2_250/dat250_DOatm$O2_demand250

  #explore outputs
hist(dat0_DOatm$AGI_0m)
hist(dat250_DOatm$AGI_250m)

plot(dat250_DOatm$thetao_mean, dat250_DOatm$AGI_250m)
plot(dat0_DOatm$pO2_0, dat0_DOatm$AGI_0m)
plot(dat0_DOatm$o2_atm, dat0_DOatm$AGI_0m)

quantile(dat0_DOatm$AGI_0m, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)
mean(dat_DO_atm$AGI_250m, na.rm = T)
min(dat_DO_atm$AGI_250m, na.rm = T)
max(dat_DO_atm$AGI_250m, na.rm = T)
sd(dat_DO_atm$AGI_250m, na.rm = T)

#saveRDS(dat0_DOatm, here("data/locs_w_covar/cmems/cmem_covar_AGI_atm_0m_emp.rds"))

#calculate AGI critical value (10th percentile)
AGIcrit0 <- quantile(dat0_DOatm$AGI_0m, c(.10), na.rm = T) #2.55
AGIcrit250 <- quantile(dat250_DOatm$AGI_250m, c(.10), na.rm = T) #0.111

map_DO_atm <- dat250_DOatm %>%
  filter(PA == 0) %>%
  mutate(AGI_crit = ifelse(AGI_250m > AGIcrit250, "yes", "no")) #yes or no above AGIcrit

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



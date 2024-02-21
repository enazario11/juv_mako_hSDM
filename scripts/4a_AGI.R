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
thresh0 <- thresh_atm(temp = median(dat0$thetao_mean, na.rm = TRUE), so_psu = median(dat0$so_mean, na.rm = T), depth = 0) #defualt is 1.25 mL/L from vetter et al., 2008

hist(dat0_DOatm$pO2_0, xlim = c(0, 0.20))
abline(v = thresh0, lwd = 2)

# 50m
dat_DO_atm250 <- DO_to_atm(dat250, depth = 250)

hist(dat_DO_atm50$pO2_50)
abline(v = 0.030632248090974, lwd = 2)

#250m 
pressure = (1025 * 9.81 * 250) + 101325 #avg density of sea water (kg/m^3) * gravity * depth + atmospheric pressure at surface in Pa
press_bar = pressure / 100000 #bar
dat_DO_atm50 <- DO_to_atm(dat50, depth = 50)

hist(dat_DO_atm250$pO2_250)
abline(v = 0.030632248090974, lwd = 2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pressure = (1025 * 9.81 * 250) + 101325 #avg density of sea water (kg/m^3) * gravity * depth + atmospheric pressure at surface in Pa
press_bar = pressure / 100000 #bar

temp_pO2 <- NULL
for (i in 1:nrow(dat250)) {
  
  if(is.na(dat250$o2_mean[i]) | is.na(dat250$thetao_mean[i]) | is.na(dat250$so_mean[i])) {
    o2_atm <- "NA"
  } else { 
    o2_mmhg <- convert_DO(dat250$o2_mean[i]/1000, "mmol/L", "mmHg", t = dat250$thetao_mean[i], S = dat250$so_mean[i], P = press_bar) #divide o2 by 1000 to convert from cubic meter to liter. 
    o2_atm <-  o2_mmhg/760 #convert from mmHg to atm
  }
  
  temp_pO2 <- rbind(temp_pO2, o2_atm)
  cat("\rFinished", i, "of", nrow(dat250))
}

dat250_DOatm <- cbind(dat250, o2_atm = temp_pO2) %>% mutate(o2_atm = as.numeric(o2_atm))

thresh_mmHg <- convert_DO(1.25, "mL/L", "mmHg", t = median(dat250$thetao_mean, na.rm = TRUE), S = median(dat250$so_mean, na.rm = TRUE), P = press_bar)
thresh_atm250 <- thresh_mmHg / 760

hist(dat250_DOatm$o2_atm)
abline(v = thresh_atm250, lwd = 2)

# possible O2 threshold from vetter et al., 2008 study with makos and jumbo squid. Makos rarely encountered waters with less than 1.25 ml O2/L. Could be threshold for jumbo squid from the sharks. 
OxyThresh_emp <- rast_to_atm(do = 1250, 
                             temp = median(dat250$thetao_mean, na.rm = TRUE), 
                             so = mean(dat250$so_mean, na.rm = TRUE), 
                             depth = 250) #0.0247175021756423 atm -- could be new OxyThresh values


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
quantile(dat_DO_atm$pO2_50, probs = c(0, 0.10, 0.5, 0.75, 1), na.rm = TRUE) 
OxyThresh50 = 0.0843513526 #for values at 50m

OxyThresh250 = quantile(dat_DO_atm250$pO2_250, 0.10, na.rm = TRUE)

#calculate temp pref
Tpref50 = 16.452 #50m tpref is 16.452
Tpref250 = median(dat_DO_atm250$thetao_mean, na.rm = TRUE)

#run oxygen demand function with mako specific parameters
dat0_DOatm$O2_demand0 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh_atm0, T_C = dat0_DOatm$thetao_mean)
dat_DO_atm50$O2_demand_mako50 <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh, T_C = dat_DO_atm50$thetao_mean)
dat250_DOatm$O2_demand250 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh_atm250, T_C = dat250_DOatm$thetao_mean)


  #explore outputs
hist(dat0_DOatm$O2_demand0)
hist(dat250_DOatm$O2_demand250)
plot(dat_DO_atm250$thetao_mean, dat_DO_atm250$O2_demand_mako250) #should increase with temp

#calculate AGI
dat0_DOatm$AGI_0m <- dat0_DOatm$o2_atm/dat0_DOatm$O2_demand0
dat250_DOatm$AGI_250m <- dat_DO_atm250$pO2_250/dat250_DOatm$O2_demand250
dat250_DOatm$AGI_250m <- dat250_DOatm$o2_atm/dat250_DOatm$O2_demand250

  #explore outputs
hist(dat250_DOatm$AGI_250m)
plot(dat250_DOatm$thetao_mean, dat250_DOatm$AGI_250m)
plot(dat0_DOatm$o2_mean, dat0_DOatm$AGI_0m)
plot(dat0_DOatm$o2_atm, dat0_DOatm$AGI_0m)

quantile(dat_DO_atm$AGI_250m, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)
mean(dat_DO_atm$AGI_250m, na.rm = T)
min(dat_DO_atm$AGI_250m, na.rm = T)
max(dat_DO_atm$AGI_250m, na.rm = T)
sd(dat_DO_atm$AGI_250m, na.rm = T)

#saveRDS(dat250_DOatm, here("data/locs_w_covar/cmems/cmem_covar_AGI_atm2_250m.rds"))

#calculate AGI critical value (10th percentile)
AGIcrit0 <- quantile(dat_DO_atm0$AGI_0m, c(.10), na.rm = T) #2.55
AGIcrit250 <- quantile(dat_DO_atm250$AGI_250m, c(.10), na.rm = T) #0.111

map_DO_atm <- dat_DO_atm250 %>%
  filter(PA == 0) %>%
  mutate(AGI_crit = ifelse(AGI_250m > 0.111, "yes", "no")) #yes or no above AGIcrit

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


### Testing AGI across depth layers ####
dat0 <- readRDS(here("data/locs_w_covar/cmems/cmems_locs_covar_0m_AGI_dist.rds"))
dat250 <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_AGIwdemand_250m.rds"))

#distribution wide OxyThresh and Temp pref.
all_temp <- c(dat0$thetao_mean, dat250$thetao_mean)
Tpref = median(all_temp, na.rm = TRUE)

all_ox <- c(dat0$pO2_0, dat250$pO2_250)
quantile(all_ox, probs = c(0, 0.10, 0.5, 0.75, 1), na.rm = T) 
OxyThresh = 0.0177015670

#run oxygen demand function with mako specific parameters
dat0$O2_demand_mako0 <- OxyDemand(Tpref = Tpref, PO2_thresh = OxyThresh, T_C = dat0$thetao_mean)

#explore outputs
hist(dat0$O2_demand_mako0)
plot(dat0$thetao_mean, dat0$O2_demand_mako0) #should increase with temp

#calculate AGI
dat0$AGI_0 <- dat0$pO2_0/dat0$O2_demand_mako0

#explore outputs
hist(dat0$AGI_0)


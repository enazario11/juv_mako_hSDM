
### load packages ####
library(tidyverse)
library(here)
library(tidyquant)

### read data ####
dat <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_0m.rds"))

### convert DO to atm ####
source(here("functions/oxy_demand_functions.R"))
dat_DO_atm <- DO_to_atm(dat, depth = 0)

### static constants ####
# W = 51807.63 #average mass in g for juv. makos as estimated by length-weight relationship. Used average FL of 177.7 cm (from study animals)
# d = 0.700
# K = 0.070 #adult numbers for K (VBGP) for california animals (fishbase.org)
# j2 = 8000
# j1 = 4500
# OxyThresh = 0.05347
#Tpref = median(dat$sst, na.rm = T)
# Linf = 321 #adult numbers for Linf FL for california animals (fishbase.org)
# LwA = 0.01670 #juvenile numbers from fishbase (New Zealand mixed animals)
# LwB = 2.847 #juvenile numbers from fishbase (New Zealand mixed animals)

#PO2 supply -> Do100 variable (mol/m3)
#W -> average mass in g for juv. makos as estimated by length-weight relationship. Used average FL of 177.7 (from study animals)
#d -> constant, Clarke et al. 2021
#K -> fishbase.org estimate for growth, von bertanlaffy growth parameter
#j1 -> constant, Clarke et al., 2021
#j2 -> constant, Clarke et al., 2021
#OxyThresh -> 10 percentile of DO100 observed. See code above for calculation. AS done in Clarke et al., 2021
#Tpref -> median SST across the species distribution (Clarke et al., 2021). See code above for calculation. Following their protocol, calculated the median sst in C
#T -> SST variable from data table (K)
#Linf-> 321 cm is the asymptotic Length estimated by the VBGP (Linf) for adult sharks in CA
#LwA -> additional value needed to estimate asymptotic weight. Values were pulled from mixed individuals from New Zealand. No West Coast USA data available. 
#lwB -> additional value needed to estimate asymptotic weight. Values were pulled from mixed individuals from New Zealand. No West Coast USA data available. 

### mako specific constants ####
#calculate O2 thresh -- as per Clarke et al., 2021
quantile(dat_DO_atm$pO2_0, probs = c(0, 0.10, 0.5, 0.75, 1), na.rm = T) #10 percentile at surface is 0.1376230, used as threshold value (atm)
OxyThresh = 0.1376611
hist(dat_DO_atm$pO2_0)
Tpref = median(dat_DO_atm$thetao_mean, na.rm = T) #19.19*C

### oxy demand calc ####
#Function to estimate the oxygen demand of a species at a temperature (rTempNewST) given its temperature preference and oxygen threshold.
    #Tpref: species temperature preference
    #OxyThresh: species oxygen threshold
    #rTempNewST: Environmental temperature

O2_demand <- OxyDemand(Tpref = Tpref, OxyThresh = OxyThresh, rTempNewST = dat_DO_atm$thetao_mean, d = 0.7, j1 = 4500, j2 = 8000, K = 0.07, Linf = 321, LwA = 0.01670, LwB = 2.847, W = 51807.63)
dat_DO_atm$AGI_0m <- dat_DO_atm$pO2_0/O2_demand

#explore outputs
hist(dat_DO_atm$AGI_0m)
quantile(dat_DO_atm$AGI_0m, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)
mean(dat_DO_atm$AGI_0m, na.rm = T)
min(dat_DO_atm$AGI_0m, na.rm = T)
max(dat_DO_atm$AGI_0m, na.rm = T)
sd(dat_DO_atm$AGI_0m, na.rm = T)

hist(O2_demand)

saveRDS(dat_DO_atm, here("data/locs_w_covar/cmems/cmem_locs_covar_AGI_0m.rds"))

#calculate AGI critical value (10th percentile)
AGIcrit <- quantile(dat_DO_atm$AGI_0m, c(.10), na.rm = T) #1.29

map_DO_atm <- dat_DO_atm %>%
  filter(PA == 0) %>%
  mutate(AGI_crit = ifelse(AGI_0m > 1.29, "yes", "no")) #yes or no above AGIcrit

#coarse look at where the sharks were above the mean AGI 
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
o2_range <- seq(0, 0.26, length.out = 100)
temp_range <- seq(11.4, 26.8, length.out = 100)
O2_demand <- OxyDemand(Tpref = Tpref, OxyThresh = OxyThresh, rTempNewST = temp_range)
AGI_test <- outer(o2_range, temp_range, 
                  \(o2, t) o2 / OxyDemand(Tpref = Tpref, OxyThresh = OxyThresh, rTempNewST = t)) 
fig <- plotly::plot_ly(z = ~AGI_test)
fig <- fig %>% add_surface()
fig


expand_grid(i = 1:100, j = 1:100) %>% 
  mutate(o2 = o2_range[i], 
         t = temp_range[j],
         AGI = map2_dbl(i, j, \(r, c) AGI_test[r, c])) %>% 
  view()

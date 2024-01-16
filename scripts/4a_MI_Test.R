
### load packages ####

library(tidyverse)
library(RColorBrewer)
library(tidyquant)
library(raster)
library(here)

### read data ####

dat <- read.csv(here("data/enviro/test/loc_enviro.csv"))
dat <- dat %>%
  mutate(DO100 = DO100/1000)

### set var values for AGI ####

#calculate O2 thresh -- as per Clarke et al., 2021
quantile(dat$DO100, probs = c(0, 0.10, 0.5, 0.75, 1), na.rm = T) #10 percentile is 0.05347627, used as threshold value (mol/m^3)
mean(dat$FL) #177.7 cm

W = 51807.63 #average mass in g for juv. makos as estimated by length-weight relationship. Used average FL of 177.7 cm (from study animals)
d = 0.700
K = 0.070 #adult numbers for K (VBGP) for california animals (fishbase.org)
j2 = 8000
j1 = 4500
OxyThresh = 0.05347
Tpref = median(dat$sst, na.rm = T)
Linf = 321 #adult numbers for Linf FL for california animals (fishbase.org)
LwA = 0.01670 #juvenile numbers from fishbase (New Zealand mixed animals)
LwB = 2.847 #juvenile numbers from fishbase (New Zealand mixed animals)

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


### oxy demand function ####
#Function to estimate the oxygen demand of a species at a temperature (rTempNewST) given its temperature preference and oxygen threshold.
#Tpref: species temperature preference
#OxyThresh: species oxygen threshold
#rTempNewST: Environmental temperature

source(here("functions/oxy_demand_functions.R"))


### applying oxy demand function ####

O2_demand <- OxyDemand(Tpref = Tpref, OxyThresh = OxyThresh, rTempNewST = dat$sst)
AGI_test <- dat$DO100/O2_demand

hist(AGI_test)
quantile(AGI_test, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)
mean(AGI_test, na.rm = T)
min(AGI_test, na.rm = T)
max(AGI_test, na.rm = T)
sd(AGI_test, na.rm = T)

hist(O2_demand)

phicrit <- quantile(AGI_test, c(.10), na.rm = T) #Φcrit estimate 1.57 -- use as cutoff value for AGI values from projected data to see how much aerobic habitat they may lose

#map AGI 
AGI_df <- as.data.frame(AGI_test)
map_AGI <- as.data.frame(cbind(dat$Lon, dat$Lat, AGI_test))
map_AGI <- map_AGI %>%
  rename(AGI = AGI_test, 
         lat = V2, 
         long = V1)

map_AGI <- map_AGI %>%
  mutate(AGI_range = ifelse(AGI > 3.14, "yes", "no")) 

#coarse look at where the sharks were above the mean AGI 
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="USA"| north_map$region=="Mexico",]

ggplot(shore, aes(long, lat)) +
  coord_map("mercator", xlim=c(-134, -109), ylim=c(20,38)) +
  geom_polygon(aes(group=group), fill="grey60",lwd=1) +
  geom_point(data = map_AGI, aes(x = long, y = lat, color = AGI_range), shape = 1)+
  scale_color_manual(values = c("red", "blue"))+
  theme_tq()+
  theme(legend.position = "right")

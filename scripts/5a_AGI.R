### load packages ####
library(tidyverse)
library(here)
library(tidyquant)
library(respR)
source(here("functions/oxy_demand_functions_rev.R"))

### read data ####
#fix duplicates
dat0 <- readRDS(here("data/locs_w_covar/psat_spot/cmem_locs_covar_0m.rds")) 
# dat0<- dat0 %>% 
#     group_by(tag, PA, rep) %>%
#     distinct(dt, .keep_all = TRUE) %>%
#     ungroup()

#fix duplicates
dat250 <- readRDS(here("data/locs_w_covar/psat_spot/cmem_locs_covar_250m.rds"))
# dat250 <- dat250 %>% 
#     group_by(tag, PA, rep) %>%
#     distinct(dt, .keep_all = TRUE) %>%
#     ungroup()

# # ~0.5% of salinity values are missing. Linearly interpolate them. If edge case, replace with median (n < 30)
# dat0 <- dat0 |> 
#   group_by(tag, PA, rep) |> 
#   mutate(vosaline_mean = na.approx(vosaline_mean, na.rm = FALSE), 
#          vosaline_mean = ifelse(is.na(vosaline_mean), median(vosaline_mean, na.rm = TRUE), vosaline_mean)) |> 
#   ungroup()

# #interpolate (2%) and replace msissing salinity values. Replace edge case NAs (n = 195)
# dat250 <- dat250 |> 
#   group_by(tag, PA, rep) |> 
#   mutate(vosaline_mean = na.approx(vosaline_mean, na.rm = FALSE),
#          vosaline_mean = ifelse(is.na(vosaline_mean), median(vosaline_mean, na.rm = TRUE), vosaline_mean)) |> 
#   ungroup()

### convert DO to atm ####
# 0m
dat0_DOatm <- dat0 %>%
  mutate(pO2_0 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))

#0.07385
thresh0 <- do_to_atm(do = 2, t = median(dat0_DOatm$votemper_mean, na.rm = TRUE), s = median(dat0_DOatm$vosaline_mean, na.rm = TRUE), thresh = TRUE) #defualt do value is 2 mL/L from vetter et al., 2008

hist(dat0_DOatm$pO2_0, xlim = c(0, 0.30)) 
abline(v = thresh0, lwd = 2)

#250m 
dat250_DOatm <- dat250 %>%
  mutate(pO2_250 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))

#0.06428
thresh250 <- do_to_atm(do = 2, t = median(dat250_DOatm$votemper_mean, na.rm = TRUE), s = median(dat250_DOatm$vosaline_mean, na.rm = TRUE), thresh = TRUE)

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
dat0_DOatm$O2_demand0 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh0, T_C = dat0_DOatm$votemper_mean)
dat250_DOatm$O2_demand250 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh250, T_C = dat250_DOatm$votemper_mean)

  #explore outputs
hist(dat0_DOatm$O2_demand0)
hist(dat250_DOatm$O2_demand250)
plot(dat0_DOatm$votemper_mean, dat0_DOatm$O2_demand0) #should increase with temp

#calculate AGI
dat0_DOatm$AGI_0m <- dat0_DOatm$pO2_0/dat0_DOatm$O2_demand0
dat250_DOatm$AGI_250m <- dat250_DOatm$pO2_250/dat250_DOatm$O2_demand250

  #explore outputs
hist(dat0_DOatm$AGI_0m)
hist(dat250_DOatm$AGI_250m)

plot(dat250_DOatm$votemper_mean, dat250_DOatm$AGI_250m)
plot(dat0_DOatm$pO2_0, dat0_DOatm$AGI_0m)

quantile(dat0_DOatm$AGI_0m, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)
mean(dat250_DOatm$AGI_250m, na.rm = T)
min(dat250_DOatm$AGI_250m, na.rm = T)
max(dat250_DOatm$AGI_250m, na.rm = T)
sd(dat250_DOatm$AGI_250m, na.rm = T)

saveRDS(dat0_DOatm, here("data/locs_w_covar/psat_spot/cmem_locs_covar_AGI_0m_daily.rds"))
saveRDS(dat250_DOatm, here("data/locs_w_covar/psat_spot/cmem_locs_covar_AGI_250m_daily.rds"))

#calculate AGI critical value (10th percentile)
AGIcrit0 <- quantile(dat0_DOatm$AGI_0m, c(.10), na.rm = T) 
AGIcrit250 <- quantile(dat250_DOatm$AGI_250m, c(.10), na.rm = T) 

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



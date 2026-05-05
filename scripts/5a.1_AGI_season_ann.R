### load packages ####
library(tidyverse)
library(here)
library(tidyquant)
library(respR)
source(here("functions/oxy_demand_functions_rev.R"))

### read data ####
#annual
dat0_ann <- readRDS(here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_0m_ann.rds"))
dat250_ann <- readRDS(here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_250m_ann.rds"))

#seasonal
dat0_seas <- readRDS(here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_0m_seas.rds"))
dat250_seas <- readRDS(here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_250m_seas.rds"))

### CRW AGI calcs ####
#### Annual ####
##### convert do to atm ####
#calculate temp pref
Tpref50 = 16.45201 #50m tpref is 16.452

# 0m 
dat0_DOatm_ann <- dat0_ann %>%
  mutate(pO2_0 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))

thresh0_ann <- do_to_atm(do = 2, t = median(dat0_DOatm_ann$votemper_mean, na.rm = TRUE), s = median(dat0_DOatm_ann$vosaline_mean, na.rm = TRUE), thresh = TRUE) #defualt do value is 2 mL/L from vetter et al., 2008

hist(dat0_DOatm_ann$pO2_0, xlim = c(0, 0.3)) 
abline(v = thresh0_ann, lwd = 2)

#250m 
dat250_DOatm_ann <- dat250_ann %>%
  mutate(pO2_250 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))
thresh250_ann <- do_to_atm(do = 2, t = median(dat250_DOatm_ann$votemper_mean, na.rm = TRUE), s = median(dat250_DOatm_ann$vosaline_mean, na.rm = TRUE), thresh = TRUE)

hist(dat250_DOatm$pO2_250)
abline(v = thresh250_ann, lwd = 2)

##### oxy demand ####
dat0_DOatm_ann$O2_demand0 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh0_ann, T_C = dat0_DOatm_ann$votemper_mean)
dat250_DOatm_ann$O2_demand250 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh250_ann, T_C = dat250_DOatm_ann$votemper_mean)

  #explore outputs
hist(dat0_DOatm$O2_demand0)
hist(dat250_DOatm$O2_demand250)

##### calculate AGI ####
dat0_DOatm_ann$AGI_0m <- dat0_DOatm_ann$pO2_0/dat0_DOatm_ann$O2_demand0
dat250_DOatm_ann$AGI_250m <- dat250_DOatm_ann$pO2_250/dat250_DOatm_ann$O2_demand250

  #explore outputs
hist(dat0_DOatm_ann$AGI_0m)
hist(dat250_DOatm_ann$AGI_250m)

#calculate AGI critical value (10th percentile)
AGIcrit0 <- quantile(dat0_DOatm_ann$AGI_0m, c(.10), na.rm = T) #2.89
AGIcrit250 <- quantile(dat250_DOatm_ann$AGI_250m, c(.10), na.rm = T) #0.279

saveRDS(dat0_DOatm_ann, here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_AGI_0m_ann.rds"))
saveRDS(dat250_DOatm_ann, here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_AGI_250m_ann.rds"))

#### Seasonal ####
##### convert do to atm ####
#calculate temp pref
Tpref50 = 16.45201 #50m tpref is 16.452

# 0m
dat0_DOatm_seas <- dat0_seas %>%
  mutate(pO2_0 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))

thresh0_seas <- do_to_atm(do = 2, t = median(dat0_DOatm_seas$votemper_mean, na.rm = TRUE), s = median(dat0_DOatm_seas$vosaline_mean, na.rm = TRUE), thresh = TRUE) #defualt do value is 2 mL/L from vetter et al., 2008

hist(dat0_DOatm_seas$pO2_0, xlim = c(0, 0.3)) 
abline(v = thresh0_seas, lwd = 2)

#250m 
dat250_DOatm_seas <- dat250_seas %>%
  mutate(pO2_250 = do_to_atm(do = o2_mean, t = votemper_mean, s = vosaline_mean))
thresh250_seas <- do_to_atm(do = 2, t = median(dat250_DOatm_seas$votemper_mean, na.rm = TRUE), s = median(dat250_DOatm_seas$vosaline_mean, na.rm = TRUE), thresh = TRUE)

hist(dat250_DOatm_seas$pO2_250)
abline(v = thresh250_seas, lwd = 2)

##### oxy demand ####
dat0_DOatm_seas$O2_demand0 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh0_seas, T_C = dat0_DOatm_seas$votemper_mean)
dat250_DOatm_seas$O2_demand250 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh250_seas, T_C = dat250_DOatm_seas$votemper_mean)

#explore outputs
hist(dat0_DOatm_seas$O2_demand0)
hist(dat250_DOatm_seas$O2_demand250)

##### calculate AGI ####
dat0_DOatm_seas$AGI_0m <- dat0_DOatm_seas$pO2_0/dat0_DOatm_seas$O2_demand0
dat250_DOatm_seas$AGI_250m <- dat250_DOatm_seas$pO2_250/dat250_DOatm_seas$O2_demand250

#explore outputs
hist(dat0_DOatm_seas$AGI_0m)
hist(dat250_DOatm_seas$AGI_250m)

#calculate AGI critical value (10th percentile)
AGIcrit0 <- quantile(dat0_DOatm_seas$AGI_0m, c(.10), na.rm = T) #2.84
AGIcrit250 <- quantile(dat250_DOatm_seas$AGI_250m, c(.10), na.rm = T) #0.289

saveRDS(dat0_DOatm_seas, here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_AGI_0m_seas.rds"))
saveRDS(dat250_DOatm_seas, here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_AGI_250m_seas.rds"))

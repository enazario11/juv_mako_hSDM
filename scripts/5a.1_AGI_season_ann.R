### load packages ####
{ library(tidyverse)
library(here)
library(tidyquant)
library(respR)
}
  
source(here("functions/oxy_demand_functions.R"))

### read data ####
#CRW
dat0_ann <- readRDS(here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_0m_ann.rds"))
dat60_ann <- readRDS(here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_60m_ann.rds"))
dat250_ann <- readRDS(here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_250m_ann.rds"))

dat0_seas <- readRDS(here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_0m_seas.rds"))
dat60_seas <- readRDS(here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_60m_seas.rds"))
dat250_seas <- readRDS(here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_250m_seas.rds"))

#back
dat0_ann_back <- readRDS(here("data/locs_w_covar/psat_spot/annual/back_locs_covar_0m_ann.rds"))
dat60_ann_back <- readRDS(here("data/locs_w_covar/psat_spot/annual/back_locs_covar_60m_ann.rds"))
dat250_ann_back <- readRDS(here("data/locs_w_covar/psat_spot/annual/back_locs_covar_250m_ann.rds"))

dat0_seas_back <- readRDS(here("data/locs_w_covar/psat_spot/seasonal/back_locs_covar_0m_seas.rds"))
dat60_seas_back <- readRDS(here("data/locs_w_covar/psat_spot/seasonal/back_locs_covar_60m_seas.rds"))
dat250_seas_back <- readRDS(here("data/locs_w_covar/psat_spot/seasonal/back_locs_covar_250m_seas.rds"))

### CRW AGI calcs ####
#### Annual ####
##### convert do to atm ####
#calculate temp pref
Tpref50 = 16.45201 #50m tpref is 16.452

# 0m -- USE AS MODEL
dat0_DOatm <- DO_to_atm(dat0_ann, depth = 0)
thresh0 <- thresh_atm(temp = median(dat0_DOatm$votemper_mean, na.rm = TRUE), so_psu = median(dat0_DOatm$vosaline_mean, na.rm = TRUE), depth = 0) #defualt do value is 2 mL/L from vetter et al., 2008

hist(dat0_DOatm$pO2_0, xlim = c(0, 0.20)) 
abline(v = thresh0, lwd = 2)

# 60m
dat60_DOatm <- DO_to_atm(dat60_ann, depth = 60)
thresh60 <- thresh_atm(temp = median(dat60_DOatm$votemper_mean, na.rm = TRUE), so_psu = median(dat60_DOatm$vosaline_mean, na.rm = TRUE), depth = 60)

hist(dat60_DOatm$pO2_60)
abline(v = thresh60, lwd = 2)

#250m 
dat250_DOatm <- DO_to_atm(dat250_ann, depth = 250)
thresh250 <- thresh_atm(temp = median(dat250_DOatm$votemper_mean, na.rm = TRUE), so_psu = median(dat250_DOatm$vosaline_mean, na.rm = TRUE), depth = 250)

hist(dat250_DOatm$pO2_250)
abline(v = thresh250, lwd = 2)

##### oxy demand ####
dat0_DOatm$O2_demand0 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh0, T_C = dat0_DOatm$votemper_mean)
dat60_DOatm$O2_demand60 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh60, T_C = dat60_DOatm$votemper_mean)
dat250_DOatm$O2_demand250 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh250, T_C = dat250_DOatm$votemper_mean)

  #explore outputs
hist(dat0_DOatm$O2_demand0)
hist(dat60_DOatm$O2_demand60)
hist(dat250_DOatm$O2_demand250)

##### calculate AGI ####
dat0_DOatm$AGI_0m <- dat0_DOatm$pO2_0/dat0_DOatm$O2_demand0
dat60_DOatm$AGI_60m <- dat60_DOatm$pO2_60/dat60_DOatm$O2_demand60
dat250_DOatm$AGI_250m <- dat250_DOatm$pO2_250/dat250_DOatm$O2_demand250

  #explore outputs
hist(dat0_DOatm$AGI_0m)
hist(dat60_DOatm$AGI_60m)
hist(dat250_DOatm$AGI_250m)

#calculate AGI critical value (10th percentile)
AGIcrit0 <- quantile(dat0_DOatm$AGI_0m, c(.10), na.rm = T) #4.41
AGIcrit60 <- quantile(dat60_DOatm$AGI_60m, c(.10), na.rm = T) #2.94
AGIcrit250 <- quantile(dat250_DOatm$AGI_250m, c(.10), na.rm = T) #0.225

#saveRDS(dat0_DOatm, here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_AGI_0m_ann.rds"))
#saveRDS(dat60_DOatm, here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_AGI_60m_ann.rds"))
#saveRDS(dat250_DOatm, here("data/locs_w_covar/psat_spot/annual/cmem_locs_covar_AGI_250m_ann.rds"))

#### Seasonal ####
##### convert do to atm ####
#calculate temp pref
Tpref50 = 16.45201 #50m tpref is 16.452

# 0m -- USE AS MODEL
dat0_DOatm_seas <- DO_to_atm(dat0_seas, depth = 0)
thresh0_seas <- thresh_atm(temp = median(dat0_DOatm_seas$votemper_mean, na.rm = TRUE), so_psu = median(dat0_DOatm_seas$vosaline_mean, na.rm = TRUE), depth = 0) #defualt do value is 2 mL/L from vetter et al., 2008

hist(dat0_DOatm_seas$pO2_0, xlim = c(0, 0.20)) 
abline(v = thresh0_seas, lwd = 2)

# 60m
dat60_DOatm_seas <- DO_to_atm(dat60_seas, depth = 60)
thresh60_seas <- thresh_atm(temp = median(dat60_DOatm_seas$votemper_mean, na.rm = TRUE), so_psu = median(dat60_DOatm_seas$vosaline_mean, na.rm = TRUE), depth = 60)

hist(dat60_DOatm_seas$pO2_60)
abline(v = thresh60_seas, lwd = 2)

#250m 
dat250_DOatm_seas <- DO_to_atm(dat250_seas, depth = 250)
thresh250_seas <- thresh_atm(temp = median(dat250_DOatm_seas$votemper_mean, na.rm = TRUE), so_psu = median(dat250_DOatm_seas$vosaline_mean, na.rm = TRUE), depth = 250)

hist(dat250_DOatm_seas$pO2_250)
abline(v = thresh250_seas, lwd = 2)

##### oxy demand ####
dat0_DOatm_seas$O2_demand0 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh0_seas, T_C = dat0_DOatm_seas$votemper_mean)
dat60_DOatm_seas$O2_demand60 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh60_seas, T_C = dat60_DOatm_seas$votemper_mean)
dat250_DOatm_seas$O2_demand250 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh250_seas, T_C = dat250_DOatm_seas$votemper_mean)

#explore outputs
hist(dat0_DOatm_seas$O2_demand0)
hist(dat60_DOatm_seas$O2_demand60)
hist(dat250_DOatm_seas$O2_demand250)

##### calculate AGI ####
dat0_DOatm_seas$AGI_0m <- dat0_DOatm_seas$pO2_0/dat0_DOatm_seas$O2_demand0
dat60_DOatm_seas$AGI_60m <- dat60_DOatm_seas$pO2_60/dat60_DOatm_seas$O2_demand60
dat250_DOatm_seas$AGI_250m <- dat250_DOatm_seas$pO2_250/dat250_DOatm_seas$O2_demand250

#explore outputs
hist(dat0_DOatm_seas$AGI_0m)
hist(dat60_DOatm_seas$AGI_60m)
hist(dat250_DOatm_seas$AGI_250m)

#calculate AGI critical value (10th percentile)
AGIcrit0 <- quantile(dat0_DOatm_seas$AGI_0m, c(.10), na.rm = T) #4.32
AGIcrit60 <- quantile(dat60_DOatm_seas$AGI_60m, c(.10), na.rm = T) #3.01
AGIcrit250 <- quantile(dat250_DOatm_seas$AGI_250m, c(.10), na.rm = T) #0.234

#saveRDS(dat0_DOatm_seas, here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_AGI_0m_seas.rds"))
#saveRDS(dat60_DOatm_seas, here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_AGI_60m_seas.rds"))
#saveRDS(dat250_DOatm_seas, here("data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_AGI_250m_seas.rds"))

### Back convert DO to atm ####
#### Annual ####
##### convert do to atm ####
#calculate temp pref
Tpref50 = 16.45201 #50m tpref is 16.452

# 0m -- USE AS MODEL
dat0_DOatm_back_ann <- DO_to_atm(dat0_ann_back, depth = 0)
thresh0_back_ann <- thresh_atm(temp = median(dat0_DOatm_back_ann$votemper_mean, na.rm = TRUE), so_psu = median(dat0_DOatm_back_ann$vosaline_mean, na.rm = TRUE), depth = 0) #defualt do value is 2 mL/L from vetter et al., 2008

hist(dat0_DOatm_back_ann$pO2_0, xlim = c(0, 0.20)) 
abline(v = thresh0_back_ann, lwd = 2)

# 60m
dat60_DOatm_back_ann <- DO_to_atm(dat60_ann_back, depth = 60)
thresh60_back_ann <- thresh_atm(temp = median(dat60_DOatm_back_ann$votemper_mean, na.rm = TRUE), so_psu = median(dat60_DOatm_back_ann$vosaline_mean, na.rm = TRUE), depth = 60)

hist(dat60_DOatm_back_ann$pO2_60)
abline(v = thresh60_back_ann, lwd = 2)

#250m 
dat250_DOatm_back_ann <- DO_to_atm(dat250_ann_back, depth = 250)
thresh250_back_ann <- thresh_atm(temp = median(dat250_DOatm_back_ann$votemper_mean, na.rm = TRUE), so_psu = median(dat250_DOatm_back_ann$vosaline_mean, na.rm = TRUE), depth = 250)

hist(dat250_DOatm_back_ann$pO2_250)
abline(v = thresh250_back_ann, lwd = 2)

##### oxy demand ####
dat0_DOatm_back_ann$O2_demand0 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh0_back_ann, T_C = dat0_DOatm_back_ann$votemper_mean)
dat60_DOatm_back_ann$O2_demand60 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh60_back_ann, T_C = dat60_DOatm_back_ann$votemper_mean)
dat250_DOatm_back_ann$O2_demand250 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh250_back_ann, T_C = dat250_DOatm_back_ann$votemper_mean)

#explore outputs
hist(dat0_DOatm_back_ann$O2_demand0)
hist(dat60_DOatm_back_ann$O2_demand60)
hist(dat250_DOatm_back_ann$O2_demand250)

##### calculate AGI ####
dat0_DOatm_back_ann$AGI_0m <- dat0_DOatm_back_ann$pO2_0/dat0_DOatm_back_ann$O2_demand0
dat60_DOatm_back_ann$AGI_60m <- dat60_DOatm_back_ann$pO2_60/dat60_DOatm_back_ann$O2_demand60
dat250_DOatm_back_ann$AGI_250m <- dat250_DOatm_back_ann$pO2_250/dat250_DOatm_back_ann$O2_demand250

#explore outputs
hist(dat0_DOatm_back_ann$AGI_0m)
hist(dat60_DOatm_back_ann$AGI_60m)
hist(dat250_DOatm_back_ann$AGI_250m)

#calculate AGI critical value (10th percentile)
AGIcrit0 <- quantile(dat0_DOatm_back_ann$AGI_0m, c(.10), na.rm = T) #4.4
AGIcrit60 <- quantile(dat60_DOatm_back_ann$AGI_60m, c(.10), na.rm = T) #2.56
AGIcrit250 <- quantile(dat250_DOatm_back_ann$AGI_250m, c(.10), na.rm = T) #0.116

#saveRDS(dat0_DOatm_back_ann, here("data/locs_w_covar/psat_spot/annual/back_locs_covar_AGI_0m_ann.rds"))
#saveRDS(dat60_DOatm_back_ann, here("data/locs_w_covar/psat_spot/annual/back_locs_covar_AGI_60m_ann.rds"))
#saveRDS(dat250_DOatm_back_ann, here("data/locs_w_covar/psat_spot/annual/back_locs_covar_AGI_250m_ann.rds"))

#### Seasonal ####
##### convert do to atm ####
#calculate temp pref
Tpref50 = 16.45201 #50m tpref is 16.452

# 0m -- USE AS MODEL
dat0_DOatm_back_seas <- DO_to_atm(dat0_seas_back, depth = 0)
thresh0_back_seas <- thresh_atm(temp = median(dat0_DOatm_back_seas$votemper_mean, na.rm = TRUE), so_psu = median(dat0_DOatm_back_seas$vosaline_mean, na.rm = TRUE), depth = 0) #defualt do value is 2 mL/L from vetter et al., 2008

hist(dat0_DOatm_back_seas$pO2_0, xlim = c(0, 0.20)) 
abline(v = thresh0_back_seas, lwd = 2)

# 60m
dat60_DOatm_back_seas <- DO_to_atm(dat60_seas_back, depth = 60)
thresh60_back_seas <- thresh_atm(temp = median(dat60_DOatm_back_seas$votemper_mean, na.rm = TRUE), so_psu = median(dat60_DOatm_back_seas$vosaline_mean, na.rm = TRUE), depth = 60)

hist(dat60_DOatm_back_seas$pO2_60)
abline(v = thresh60_back_seas, lwd = 2)

#250m 
dat250_DOatm_back_seas <- DO_to_atm(dat250_seas_back, depth = 250)
thresh250_back_seas <- thresh_atm(temp = median(dat250_DOatm_back_seas$votemper_mean, na.rm = TRUE), so_psu = median(dat250_DOatm_back_seas$vosaline_mean, na.rm = TRUE), depth = 250)

hist(dat250_DOatm_back_seas$pO2_250)
abline(v = thresh250_back_seas, lwd = 2)

##### oxy demand ####
dat0_DOatm_back_seas$O2_demand0 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh0_back_seas, T_C = dat0_DOatm_back_seas$votemper_mean)
dat60_DOatm_back_seas$O2_demand60 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh60_back_seas, T_C = dat60_DOatm_back_seas$votemper_mean)
dat250_DOatm_back_seas$O2_demand250 <- OxyDemand(Tpref = Tpref50, PO2_thresh = thresh250_back_seas, T_C = dat250_DOatm_back_seas$votemper_mean)

#explore outputs
hist(dat0_DOatm_back_seas$O2_demand0)
hist(dat60_DOatm_back_seas$O2_demand60)
hist(dat250_DOatm_back_seas$O2_demand250)

##### calculate AGI ####
dat0_DOatm_back_seas$AGI_0m <- dat0_DOatm_back_seas$pO2_0/dat0_DOatm_back_seas$O2_demand0
dat60_DOatm_back_seas$AGI_60m <- dat60_DOatm_back_seas$pO2_60/dat60_DOatm_back_seas$O2_demand60
dat250_DOatm_back_seas$AGI_250m <- dat250_DOatm_back_seas$pO2_250/dat250_DOatm_back_seas$O2_demand250

#explore outputs
hist(dat0_DOatm_back_seas$AGI_0m)
hist(dat60_DOatm_back_seas$AGI_60m)
hist(dat250_DOatm_back_seas$AGI_250m)

#calculate AGI critical value (10th percentile)
AGIcrit0 <- quantile(dat0_DOatm_back_seas$AGI_0m, c(.10), na.rm = T) #4.32
AGIcrit60 <- quantile(dat60_DOatm_back_seas$AGI_60m, c(.10), na.rm = T) #2.68
AGIcrit250 <- quantile(dat250_DOatm_back_seas$AGI_250m, c(.10), na.rm = T) #0.134

#saveRDS(dat0_DOatm_back_seas, here("data/locs_w_covar/psat_spot/seasonal/back_locs_covar_AGI_0m_seas.rds"))
#saveRDS(dat60_DOatm_back_seas, here("data/locs_w_covar/psat_spot/seasonal/back_locs_covar_AGI_60m_seas.rds"))
#saveRDS(dat250_DOatm_back_seas, here("data/locs_w_covar/psat_spot/seasonal/back_locs_covar_AGI_250m_seas.rds"))


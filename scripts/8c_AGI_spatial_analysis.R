### libraries ####
{library(tidyverse)
  library(lme4)
  library(mgcv)
  library(here)
  set.seed(1004)}

### data ####
# CRW daily data 
dat_agi_d <- readRDS(here("data/locs_brts/crw_pas/dat_agi.rds")) %>% mutate(tag = as.factor(tag))

# CRW seasonal data 
dat_agi_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_agi_seas.rds")) %>% mutate(tag = as.factor(tag))

# CRW annual data
dat_agi_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_agi_ann.rds")) %>% mutate(tag = as.factor(tag))

# Add seasonal and annual data to daily data df
dat_agi_all <- cbind(dat_agi_d, dat_agi_s$AGI_0m, dat_agi_s$AGI_60m, dat_agi_s$AGI_250m, dat_agi_a$AGI_0m, dat_agi_a$AGI_60m, dat_agi_a$AGI_250m)
dat_agi_all <- dat_agi_all %>%
  dplyr::rename("AGI_0m_seas" = "dat_agi_s$AGI_0m",
         "AGI_60m_seas" = "dat_agi_s$AGI_60m", 
         "AGI_250m_seas" = "dat_agi_s$AGI_250m", 
         "AGI_0m_ann" = "dat_agi_a$AGI_0m", 
         "AGI_60m_ann" = "dat_agi_a$AGI_60m", 
         "AGI_250m_ann" = "dat_agi_a$AGI_250m")

### glmm ####
wilcox.test(dat_agi_all$AGI_0m) #2e-16, use generalized LMs

### gamm ####
#AGI 0m, daily
gamm_lat_0m_daily <- gam(AGI_0m ~ s(lat) + s(dist_coast) + s(tag, bs = "re"), 
                          data = dat_agi_all, 
                          family = Gamma(link = "log"))


summary(gamm_lat_0m_daily)  #p-value: <2e-16 for lat and dist coast
plot(gamm_lat_0m_daily)
diag_plots_0m_daily <- mgcViz::getViz(gamm_lat_0m_daily)
mgcViz::check.gamViz(diag_plots_0m_daily) #residuals look good
gamm_lat_0m_daily$aic #-17934, more negative is the preferred model in this case, -18657 with smoothers

# AGI 0m, seasonal 
gamm_lat_0m_seas <- gam(AGI_0m_seas ~ s(lat) + s(dist_coast) + s(tag, bs = "re"), 
                         data = dat_agi_all, 
                         family = Gamma(link = "log"))


summary(gamm_lat_0m_seas) #p-value: <2e-16 for lat and dist coast
plot(gamm_lat_0m_seas)
diag_plots_0m_seas <- mgcViz::getViz(gamm_lat_0m_seas) #residuals look good
mgcViz::check.gamViz(diag_plots_0m_seas) 
gamm_lat_0m_seas$aic #-79048 

#AGI 0m, annual
gamm_lat_0m_ann <- gam(AGI_0m_ann ~ s(lat) + s(dist_coast) + s(tag, bs = "re"), 
                         data = dat_agi_all, 
                         family = Gamma(link = "log"))


summary(gamm_lat_0m_ann) #p-value: <2e-16 for lat and dist coast
plot(gamm_lat_0m_ann)
diag_plots_0m_ann <- mgcViz::getViz(gamm_lat_0m_ann) #residuals look okay
mgcViz::check.gamViz(diag_plots_0m_ann)
gamm_lat_0m_ann$aic #-106905 

# AGI 250m, daily 
gamm_lat_250m_daily <- gam(AGI_250m ~ s(lat) + s(dist_coast) + s(tag, bs = "re"), 
                         data = dat_agi_all, 
                         family = Gamma(link = "log"))


summary(gamm_lat_250m_daily) #p-value: <2e-16 for lat and dist coast
plot(gamm_lat_250m_daily)
diag_plots_250m_daily <- mgcViz::getViz(gamm_lat_250m_daily) #clear patterns in resid   
mgcViz::check.gamViz(diag_plots_250m_daily)
gamm_lat_250m_daily$aic #65370

# AGI 250m, seasonal 
gamm_lat_250m_seas <- gam(AGI_250m_seas ~ s(lat) + s(dist_coast) + s(tag, bs = "re"), 
                        data = dat_agi_all, 
                        family = Gamma(link = "log"))


summary(gamm_lat_250m_seas) #p-value: <2e-16 for lat and dist coast
plot(gamm_lat_250m_seas)
diag_plots_250m_seas <- mgcViz::getViz(gamm_lat_250m_seas) #clear patterns in resid
mgcViz::check.gamViz(diag_plots_250m_seas)
gamm_lat_250m_seas$aic #600051

#AGI 250m, annual
gamm_lat_250m_ann <- gam(AGI_250m_ann ~ s(lat) + s(dist_coast) + s(tag, bs = "re"), 
                       data = dat_agi_all, 
                       family = Gamma(link = "log"))


summary(gamm_lat_250m_ann) #p-value: <2e-16 for lat and dist coast
plot(gamm_lat_250m_ann)
diag_plots_250m_ann <- mgcViz::getViz(gamm_lat_250m_ann) #clear patterns in resid
mgcViz::check.gamViz(diag_plots_250m_ann)
gamm_lat_250m_ann$aic #63624

#convert agi 0m, agi 250m into single column, add depth as fixed effect 
agi_daily <- dat_agi_all %>% 
  pivot_longer(cols = c("AGI_0m", "AGI_250m"), 
               names_to = "daily_AGI", 
               values_to = "AGI_vals")

gamm_all_daily <- gam(AGI_vals ~ s(lat) + s(dist_coast) + daily_AGI + s(tag, bs = "re"), 
                         data = agi_daily, 
                         family = Gamma(link = "log"))

summary(gamm_all_daily) #p-value: <2e-16 for lat and dist coast
plot(gamm_all_daily)
diag_plots_all_daily <- mgcViz::getViz(gamm_all_daily) #clear patterns in resid
mgcViz::check.gamViz(diag_plots_all_daily)
gamm_all_daily$aic 




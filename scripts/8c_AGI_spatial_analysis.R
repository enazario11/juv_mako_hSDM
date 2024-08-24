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
  rename("AGI_0m_seas" = "dat_agi_s$AGI_0m",
         "AGI_60m_seas" = "dat_agi_s$AGI_60m", 
         "AGI_250m_seas" = "dat_agi_s$AGI_250m", 
         "AGI_0m_ann" = "dat_agi_a$AGI_0m", 
         "AGI_60m_ann" = "dat_agi_a$AGI_60m", 
         "AGI_250m_ann" = "dat_agi_a$AGI_250m")

### glmm ####
wilcox.test(dat_agi_all$AGI_0m) #2e-16, use generalized LMs

### gamm ####
#find way to modify number of curves in smoother
#all predictors
gamm_lat <- gam(lat ~ AGI_0m^2 + s(AGI_250m) +  AGI_0m_seas^2 + s(AGI_250m_seas) + AGI_0m_ann^2 + s(AGI_250m_ann) + s(tag, bs = "re"), 
                 data = dat_agi_all, 
                 family = gaussian(link = "log"))

summary(gamm_lat)
plot(gamm_lat, pages = 1)
diag_plots <- mgcViz::getViz(gamm_lat)
mgcViz::check.gamViz(diag_plots)

#AGI 0m, daily
gamm_lat_0m_daily <- gam(AGI_0m ~ lat + dist_coast + s(tag, bs = "re"), 
                          data = dat_agi_all, 
                          family = Gamma(link = "log"))


summary(gamm_lat_0m_daily)
plot(gamm_lat_0m_daily)
diag_plots_0m_daily <- mgcViz::getViz(gamm_lat_0m_daily)
mgcViz::check.gamViz(diag_plots_0m_daily)
gamm_lat_0m_daily$aic #-17934, more negative is the preferred model in this case

# AGI 0m, seasonal 
gamm_lat_0m_seas <- gam(AGI_0m_seas ~ lat + dist_coast + s(tag, bs = "re"), 
                         data = dat_agi_all, 
                         family = Gamma(link = "log"))


summary(gamm_lat_0m_seas)
plot(gamm_lat_0m_seas)
diag_plots_0m_seas <- mgcViz::getViz(gamm_lat_0m_seas)
mgcViz::check.gamViz(diag_plots_0m_seas)
gamm_lat_0m_seas$aic #-68826.59, 

#AGI 0m, annual
gamm_lat_0m_ann <- gam(AGI_0m_ann ~ s(lat) + s(dist_coast) + s(tag, bs = "re"), 
                         data = dat_agi_all, 
                         family = Gamma(link = "log"))


summary(gamm_lat_0m_ann)
plot(gamm_lat_0m_ann)
diag_plots_0m_ann <- mgcViz::getViz(gamm_lat_0m_ann)
mgcViz::check.gamViz(diag_plots_0m_ann)
gamm_lat_0m_ann$aic #-94792.5, -106905 w/ smoothers

# AGI 250m, daily 
gamm_lat_250m_daily <- gam(AGI_250m ~ s(lat) + s(dist_coast) + s(tag, bs = "re"), 
                         data = dat_agi_all, 
                         family = Gamma(link = "log"))


summary(gamm_lat_250m_daily)
plot(gamm_lat_250m_daily)
diag_plots_250m_daily <- mgcViz::getViz(gamm_lat_250m_daily)
mgcViz::check.gamViz(diag_plots_250m_daily)
gamm_lat_250m_daily$aic #78376.27, 65370 w/ smoothers

# AGI 250m, seasonal 
gamm_lat_250m_seas <- gam(AGI_250m_seas ~ lat + dist_coast + s(tag, bs = "re"), 
                        data = dat_agi_all, 
                        family = Gamma(link = "log"))


summary(gamm_lat_250m_seas)
plot(gamm_lat_250m_seas)
diag_plots_250m_seas <- mgcViz::getViz(gamm_lat_250m_seas)
mgcViz::check.gamViz(diag_plots_250m_seas)
gamm_lat_250m_seas$aic #74890.57, strong patterns in residual plots

#AGI 250m, annual
gamm_lat_250m_ann <- gam(AGI_250m_ann ~ lat + dist_coast + s(tag, bs = "re"), 
                       data = dat_agi_all, 
                       family = Gamma(link = "log"))


summary(gamm_lat_250m_ann)
plot(gamm_lat_250m_ann)
diag_plots_250m_ann <- mgcViz::getViz(gamm_lat_250m_ann)
mgcViz::check.gamViz(diag_plots_250m_ann)
gamm_lat_250m_ann$aic #77493, strong patterns in residual plots

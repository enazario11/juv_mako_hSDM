#libraries
{library(tidyverse)
  library(here)
  library(gbm)
  library(shapviz)
  library(treeshap)
  library(terra)
  set.seed(0904)}

# Load data ---------------------------------------------------------------
# CRW daily data 
dat_base_d <- readRDS(here("data/locs_brts/crw_pas/dat_base.rds")) %>% mutate(tag = as.factor(tag))
dat_do_d <- readRDS(here("data/locs_brts/crw_pas/dat_do.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_d <- readRDS(here("data/locs_brts/crw_pas/dat_agi.rds")) %>% mutate(tag = as.factor(tag))

#### CRW seasonal data 
dat_base_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_base_seas.rds")) %>% mutate(tag = as.factor(tag))
dat_do_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_do_seas.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_agi_seas.rds")) %>% mutate(tag = as.factor(tag))

#### CRW annual data 
dat_base_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_base_ann.rds")) %>% mutate(tag = as.factor(tag))
dat_do_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_do_ann.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_agi_ann.rds")) %>% mutate(tag = as.factor(tag))

#### Add seasonal and annual data to daily data df for DO and AGI
dat_do_all <- cbind(dat_do_d, dat_do_s$o2_mean_0m, dat_do_s$o2_mean_60m, dat_do_s$o2_mean_250m, dat_do_a$o2_mean_0m, dat_do_a$o2_mean_60m, dat_do_a$o2_mean_250m)
dat_do_all <- dat_do_all %>%
  dplyr::rename("o2_mean_0m_seas" = "dat_do_s$o2_mean_0m", 
                "o2_mean_60m_seas" = "dat_do_s$o2_mean_60m", 
                "o2_mean_250m_seas" = "dat_do_s$o2_mean_250m", 
                "o2_mean_0m_ann" = "dat_do_a$o2_mean_0m", 
                "o2_mean_60m_ann" = "dat_do_a$o2_mean_60m", 
                "o2_mean_250m_ann" = "dat_do_a$o2_mean_250m")

dat_agi_all <- cbind(dat_agi_d, dat_agi_s$AGI_0m, dat_agi_s$AGI_60m, dat_agi_s$AGI_250m, dat_agi_a$AGI_0m, dat_agi_a$AGI_60m, dat_agi_a$AGI_250m)
dat_agi_all <- dat_agi_all %>%
  dplyr::rename("AGI_0m_seas" = "dat_agi_s$AGI_0m",
                "AGI_60m_seas" = "dat_agi_s$AGI_60m", 
                "AGI_250m_seas" = "dat_agi_s$AGI_250m", 
                "AGI_0m_ann" = "dat_agi_a$AGI_0m", 
                "AGI_60m_ann" = "dat_agi_a$AGI_60m", 
                "AGI_250m_ann" = "dat_agi_a$AGI_250m")

dat_base_d$row_id <- 1:nrow(dat_base_d)
dat_do_all$row_id <- 1:nrow(dat_do_all)
dat_agi_all$row_id <- 1:nrow(dat_agi_all)

#target point for waterfall plots
target_loc <- vect(cbind(-133,14.5), crs="EPSG:4326")

#load models
brt_do <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/do/do_1.rds"))
brt_agi <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/agi/agi_1.rds"))

# DO ----------------------------------------------------------------------
# Training data
test_do <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/do/test/do_test1.rds")) 
train_do <- filter(dat_do_all, !(row_id %in% test_do$row_id)) %>% 
  select(-c(tag, date, lon, lat, rep, dt, uo_mean, uostr_mean, vo_mean, vostr_mean, dist_coast, o2_mean_60m, 
            o2_mean_60m_seas, o2_mean_60m_ann, dist_coast))

# Unify model 
unified_do <- unify(brt_do, train_do)

# Extract target location's env
do_rast <- rast("data/enviro/psat_spot_all/hsi_rasts/Jan03_Dec15/Jan03_Dec15_do_rast.nc")
names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
target_env_do <- terra::extract(do_rast, target_loc) %>% 
  as_tibble() %>% 
  select(-ID)

#calc shap values for target location
target_shap_do <- treeshap(unified_do, target_env_do)

#baseline
do_baseline <- mean(predict(brt_do, train_do))

# waterfall plot
sv_do <- shapviz(target_shap_do, baseline = do_baseline)
waterfall_do <- sv_waterfall(sv_do, fill_colors = c("#224B5E", "#83A58C"))
waterfall_do
predict(brt_do, target_env_do) #make sure matches prediction from waterfall plot
plogis(predict(brt_do, target_env_do)) #HSI prediction for this location

sv_importance(sv_do, kind = "bee") 

# AGI ---------------------------------------------------------------------
# Training data
test_agi <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/agi/test/agi_test1.rds")) 
train_agi <- filter(dat_agi_all, !(row_id %in% test_agi$row_id)) %>% 
  select(-c(tag, date, lon, lat, rep, dt, uo_mean, uostr_mean, vo_mean, vostr_mean, dist_coast, dist_coast, 
            AGI_60m, AGI_60m_seas, AGI_60m_ann))

# Unify model
unified_agi <- unify(brt_agi, train_agi)

# Extract target location's env
agi_rast <- rast("data/enviro/psat_spot_all/hsi_rasts/Jan03_Dec15/Jan03_Dec15_agi_rast.nc")
names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
target_env_agi <- terra::extract(agi_rast, target_loc) %>% 
  as_tibble() %>% 
  select(-ID)

#calc shap values for target location
target_shap_agi <- treeshap(unified_agi, target_env_agi)

#baseline
agi_baseline <- mean(predict(brt_agi, train_agi))

# waterfall plot
sv_agi <- shapviz(target_shap_agi, baseline = agi_baseline)
waterfall_agi <- sv_waterfall(sv_agi, fill_colors = c("#224B5E", "#83A58C")) + theme()
waterfall_agi
predict(brt_agi, target_env_agi) #make sure matches prediction from waterfall plot
plogis(predict(brt_agi, target_env_agi)) #HSI prediction for this location


sv_importance(sv_agi, kind = "bee")
# Visualize ---------------------------------------------------------------

# harmonize x-axes
built_do <- ggplot_build(waterfall_do)
do_rng <- built_do$layout$panel_params[[1]]$x.range
built_agi <- ggplot_build(waterfall_agi)
agi_rng <- built_agi$layout$panel_params[[1]]$x.range
waterfall_do <- waterfall_do + xlim(min(do_rng, agi_rng), max(do_rng, agi_rng))
waterfall_agi <- waterfall_agi + xlim(min(do_rng, agi_rng), max(do_rng, agi_rng))

cowplot::plot_grid(waterfall_do, waterfall_agi, ncol = 1, align = "v")
ggsave(here("figs/ms/supp_figs/shap.png"), height = 7, width = 8, units = c("in"))









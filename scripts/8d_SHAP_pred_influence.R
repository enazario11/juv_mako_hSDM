#libraries
{library(tidyverse)
  library(here)
  library(gbm)
  library(iml)
  library(terra)
  set.seed(1004)}

# SHAP -- spatiotemporal analyses to understand drivers of LN predictions
### target area
target_loc <- vect(cbind(-141,15), crs="EPSG:4326") 


### load models
brt_do <- readRDS(here("data/brt/mod_outputs/revised/brts_st/enso/do/do_1.rds"))
brt_agi <- readRDS(here("data/brt/mod_outputs/revised/brts_st/enso/agi/agi_1.rds"))

pred_fun <- function(model, newdata) {
  predict(model, newdata = newdata, type = "response")
}

hsi_rast_gen(date_start = c("2010-10-26"), date_end = c("2010-10-26"), season = "F", output_name = "LN_F_Oct26_2010")

### DO 
### Training data
test_do <- readRDS(here("data/brt/mod_outputs/revised/brts_st/enso/do/test/do_test1.rds")) 
train_do <- filter(dat_do_st, !(row_id %in% test_do$row_id)) %>% 
  dplyr::select(-c(tag, date, lon, lat, rep, dt, uo_mean, uostr_mean, vo_mean, vostr_mean, soi, st_id))

# Extract target location's env
do_rast <- rast("data/enviro/psat_spot_all/hsi_rasts/LN_F_Oct26_2010/LN_F_Oct26_2010_do_rast.nc")
names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
target_env_do <- terra::extract(do_rast, target_loc) %>% 
  as_tibble() %>%
  dplyr::select(-ID)

#plot
x = train_do[, c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")]
predictor <- Predictor$new(brt_do, data = x, y = train_do$PA, predict.function = pred_fun)
shapley <- Shapley$new(predictor, x.interest = target_env_do)

print(shapley$results)
do_shap <- plot(shapley) + tidyquant::theme_tq()
ggsave(here("figs/ms/supp_figs/shap/do_shap.png"), do_shap, width = 6, height = 5, units = c("in"))

# AGI 
# Training data
test_agi <- readRDS(here("data/brt/mod_outputs/revised/brts_st/enso/agi/test/agi_test1.rds")) 
train_agi <- filter(dat_agi_st, !(row_id %in% test_agi$row_id)) %>% 
  dplyr::select(-c(tag, date, lon, lat, rep, dt, uo_mean, uostr_mean, vo_mean, vostr_mean, soi, st_id))

# Extract target location's env
agi_rast <- rast("data/enviro/psat_spot_all/hsi_rasts/LN_F_Oct26_2010/LN_F_Oct26_2010_agi_rast.nc")
names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
target_env_agi <- terra::extract(agi_rast, target_loc) %>% 
  as_tibble() %>% 
  dplyr::select(-ID)

#plot
x = train_agi[,c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")]
predictor <- Predictor$new(brt_agi, data = x, y = train_agi$PA, predict.function = pred_fun)
shapley <- Shapley$new(predictor, x.interest = target_env_agi)

print(shapley$results)
agi_shap <- plot(shapley) + tidyquant::theme_tq()
ggsave(here("figs/ms/supp_figs/shap/agi_shap.png"), agi_shap, width = 6, height = 5, units = c("in"))


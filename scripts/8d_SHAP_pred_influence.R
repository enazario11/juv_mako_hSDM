#library
library(tidyverse)
library(here)
library(doFuture)
library(shapviz)
library(kernelshap)

#######
#To get train data, run files from the 8b_performance_metric_loop.R or 8c_spatiotemporal_performance scripts and sample out the row_ids from the "test" data
#######

#test models -- get model from overall dataset train/test (NOT stratified)
brt_base <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/base/base_1.rds"))
test_base <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/base/test/base_test1.rds"))
train_base <- subset(dat_base_d, !(row_id %in% test_base$row_id)) %>% subset(select = -c(tag, date, lon, lat, rep, dt, row_id, uo_mean, uostr_mean, vo_mean, vostr_mean, dist_coast))
#st_test_base <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/base/test/base_test1.rds"))

#trying regional specific test 
brt_do <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/do/do_1.rds"))
st_test_do <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/do/test/do_test1.rds")) 
st_train_do <- subset(dat_do_st, !(row_id %in% st_test_do$row_id)) %>% subset(select = -c(tag, date, lon, lat, rep, dt, uo_mean, uostr_mean, vo_mean, vostr_mean, dist_coast, o2_mean_60m, o2_mean_60m_seas, o2_mean_60m_ann, dist_coast))
reg_train_do <- st_train_do %>% filter(region == "nec") %>% subset(select = -c(region, soi, enso, st_id, row_id))

#trying region specific test
brt_agi <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/agi/agi_1.rds"))
st_test_agi <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/agi/test/agi_test1.rds")) 
st_train_agi <- subset(dat_agi_st, !(row_id %in% st_test_agi$row_id)) %>% subset(select = -c(tag, date, lon, lat, rep, dt, uo_mean, uostr_mean, vo_mean, vostr_mean, dist_coast, dist_coast, AGI_60m, AGI_60m_seas, AGI_60m_ann))
reg_train_agi <- st_train_agi %>% filter(region == "nec") %>% subset(select = -c(region, soi, enso, st_id, row_id))

#generate overall shap plots
#base -------------------------------------------------
plan(multisession, workers = 4)  # Windows

  #try again with type "link" as well as omitting the type argument
    #smaller background size increases speed
shap_values <- kernelshap(brt_base, X = train_base[1:1000, -1], bg_X = train_base[1:50, ], type = "response") #test w/ first 1000 rows, using first 50 rows as background data.

sv <- shapviz(shap_values)
#saveRDS(sv, here("data/brt/mod_outputs/shap/base_test.rds"))

sv_importance(sv, show_numbers = TRUE)
sv_importance(sv, kind = "bee")
sv_dependence(sv, "temp_mean")

#do ----------------------------------------------------
shap_values <- kernelshap(brt_do, X = reg_train_do[,-1], bg_X = reg_train_do[1:50, ], type = "response") #test using first 50 rows as background data.

sv_do <- shapviz(shap_values)
saveRDS(sv_do, here("data/brt/mod_outputs/shap/do_nec_test.rds"))

sv_importance(sv_do, show_numbers = TRUE)
sv_importance(sv_do, kind = "bee") + theme_minimal()
sv_dependence(sv_do, c("temp_mean", "o2_mean_0m", "o2_mean_250m_ann"))

#agi
shap_values <- kernelshap(brt_agi, X = reg_train_agi[,-1], bg_X = reg_train_agi[1:50, ], type = "response") #test using first 50 rows as background data.

sv_agi <- shapviz(shap_values)
saveRDS(sv_agi, here("data/brt/mod_outputs/shap/agi_nec_test.rds"))

sv_importance(sv_agi, show_numbers = TRUE)
sv_importance(sv_agi, kind = "bee") + theme_minimal()
sv_dependence(sv_agi, c("AGI_0m", "AGI_250m_ann"))

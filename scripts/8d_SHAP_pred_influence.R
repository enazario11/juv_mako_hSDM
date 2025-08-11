#library
library(tidyverse)
library(here)
library(doFuture)
library(foreach)
library(shapviz)
library(doParallel)
library(kernelshap)
library(gbm)
library(treeshap)

#######
#To get train data, run files from the 8c_spatiotemporal_performance scripts and sample out the row_ids from the "test" data
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
nec_train_do <- st_train_do %>% filter(region == "nec") %>% subset(select = -c(region, soi, enso, st_id, row_id))

#trying region specific test
brt_agi <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/agi/agi_1.rds"))
st_test_agi <- readRDS(here("data/brt/mod_outputs/perf_metric_iters/brts_st/agi/test/agi_test1.rds")) 
st_train_agi <- subset(dat_agi_st, !(row_id %in% st_test_agi$row_id)) %>% subset(select = -c(tag, date, lon, lat, rep, dt, uo_mean, uostr_mean, vo_mean, vostr_mean, dist_coast, dist_coast, AGI_60m, AGI_60m_seas, AGI_60m_ann))
nec_train_agi <- st_train_agi %>% filter(region == "nec") %>% subset(select = -c(region, soi, enso, st_id, row_id))

table(nec_train_agi$PA) #about the same for DO model


#generate overall shap plots -- SHAP = impact
#base -------------------------------------------------
  #try again with type "link" as well as omitting the type argument
    #smaller background size increases speed
shap_values <- kernelshap(brt_base, X = train_base[1:1000, -1], bg_X = train_base[1:50, ], type = "response") #test w/ first 1000 rows, using first 50 rows as background data.

sv <- shapviz(shap_values)
#saveRDS(sv, here("data/brt/mod_outputs/shap/base_test.rds"))

sv_importance(sv, show_numbers = TRUE)
sv_importance(sv, kind = "bee")
sv_dependence(sv, "temp_mean")

#do ----------------------------------------------------
unified_do <- unify(brt_do, nec_train_do[,-1])

#shap_values_do <- kernelshap(brt_do, X = nec_train_do[,-1], bg_X = nec_train_do[1:50, ], type = "response", parallel = TRUE, parallel_args = list(packages = "gbm")) #test using first 50 rows as background data.
shap_values_do <- treeshap(unified_do, nec_train_do[,-1])
nec_train_do$predicted <- predict(brt_do, nec_train_do, type = "response")
nec_do_abs <- which(nec_train_do$PA == 0)#save row numbers for known absence
nec_do_pres <- which(nec_train_do$PA == 1) #presences
nec_do_diffs <- which(abs(nec_train_do$PA - nec_train_do$predicted) > 0.8) #rows where actual and predicted have biggest diffs

sv_do <- shapviz(shap_values_do, type = "response")

sv_importance(sv_do, show_numbers = TRUE) + theme_bw()
sv_importance(sv_do, kind = "bee") + theme_bw()
sv_dependence(sv_do, c("temp_mean", "o2_mean_0m", "o2_mean_250m_ann"))
sv_force(sv_do, row_id = nec_do_abs, max_display = 5) #drivers for absence rows
sv_force(sv_do, row_id = nec_do_pres) #known presence rows
sv_force(sv_do, row_id = nec_do_diffs)

#agi
unified_agi <- unify(brt_agi, nec_train_agi[,-1])

#shap_values_agi <- kernelshap(brt_agi, X = nec_train_agi[,-1], bg_X = nec_train_agi[1:50, ], type = "response", parallel = TRUE, parallel_args = list(packages = "gbm")) #test using first 50 rows as background data.
shap_values_agi <- treeshap(unified_agi, nec_train_agi[,-1])
nec_train_agi$predicted <- predict(brt_agi, nec_train_agi, type = "response")
nec_agi_abs <- which(nec_train_agi$PA == 0)#save row numbers for known absence
nec_agi_pres <- which(nec_train_agi$PA == 1) #presences
nec_agi_diffs <- which(abs(nec_train_agi$PA - nec_train_agi$predicted) > 0.8) #rows with biggest diffs between pred and actual

sv_agi <- shapviz(shap_values_agi, type = "response")

sv_importance(sv_agi, show_numbers = TRUE) + theme_bw()
sv_importance(sv_agi, kind = "bee") + theme_bw()
sv_dependence(sv_agi, c("AGI_0m", "AGI_250m_ann"))
sv_force(sv_agi, row_id = 12) 
sv_force(sv_agi, row_id = nec_agi_abs, max_display = 5)
sv_force(sv_agi, row_id = nec_agi_pres)
sv_force(sv_agi, row_id = nec_agi_diffs)

### Key difference between AGI and DO SHAP plots is the different SHAP values between the top rated features. The AGI plot gives high impact values to temp and AGI0m, while the DO SHAP values are more evenly distributed across a number of variables. 
### To loop through 20 BRT iterations, find way to get DF of mean SHAP values across covariates. 


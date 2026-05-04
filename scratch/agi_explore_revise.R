#libraries#####
{library(tidyverse) 
library(here)
library(sf)
library(terra)
library(distancetocoast)
library(doParallel)
library(gbm)
library(iml)
source(here::here("functions/oxy_demand_functions_rev.R"))
source(here::here("functions/hsi_rast_functions.R"))
source(here::here("functions/avg_functions.R"))}

set.seed(1004)

#format data for brts#####
format_dat_crw_brts <- function(dat0_path, dat250_path, res = c("ann", "seas", "daily"), out_path){
  
  set.seed(1004)
  
  #combine all before PA selection and fix PA values before fitting BRTs

  if(res == "daily"){
  dat0 <- readRDS(here(dat0_path))
  dat0$bathy <- replace(dat0$bathy, dat0$bathy == "NaN", NA)
  colnames(dat0) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "o2_mean_0m", "chl_mean", "temp_mean", "sal_mean",  "uo_mean", "uostr_mean", "vo_mean", "vostr_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "pO2_atm_0m", "o2_demand_0m", "AGI_0m")
                  
  dat250 <- readRDS(here(dat250_path))
  colnames(dat250) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "o2_mean_250m", "temp_mean", "sal_mean", "pO2_atm_250m", "o2_demand_250m", "AGI_250m")
  
  dat_all_temp <- dat0 %>% cbind(dat250$o2_mean_250m, dat250$AGI_250m)
  }

  if(res == "seas"){
  dat0 <- readRDS(here(dat0_path))
  dat0$bathy <- replace(dat0$bathy, dat0$bathy == "NaN", NA)
  colnames(dat0) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_seas", "mld_mean", "sal_mean", "ssh_mean", "temp_mean", "uo_mean", "uostr_mean", "vo_mean", "vostr_mean", "o2_mean_0m", "chl_mean", "bathy_mean", "bathy_sd", "pO2_atm_0m", "o2_demand_0m", "AGI_0m")
  dat0 <- dat0 %>% select(-c("dt_seas")) 
    
  dat250 <- readRDS(here(dat250_path))
  colnames(dat250) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_seas", "sal_mean", "temp_mean", "o2_mean_250m", "pO2_atm_250m", "o2_demand_250m", "AGI_250m")
  dat250 <- dat250 %>% select(-c("dt_seas")) 

    
  dat_all_temp <- dat0 %>% cbind(dat250$o2_mean_250m, dat250$AGI_250m)
  }

  if(res == "ann"){
  dat0 <- readRDS(here(dat0_path))
  dat0$bathy <- replace(dat0$bathy, dat0$bathy == "NaN", NA)
  colnames(dat0) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_ann", "mld_mean", "sal_mean", "ssh_mean", "temp_mean", "uo_mean", "uostr_mean", "vo_mean", "vostr_mean", "o2_mean_0m", "chl_mean", "bathy_mean", "bathy_sd", "pO2_atm_0m", "o2_demand_0m", "AGI_0m")
  dat0 <- dat0 %>% select(-c("dt_ann"))
    
  dat250 <- readRDS(here(dat250_path))
  colnames(dat250) <- c("tag", "date", "lon", "lat", "PA", "rep", "dt", "dt_ann", "sal_mean", "temp_mean", "o2_mean_250m", "pO2_atm_250m", "o2_demand_250m", "AGI_250m")
  dat250 <- dat250 %>% select(-c("dt_ann"))

    
  dat_all_temp <- dat0 %>% cbind(dat250$o2_mean_250m, dat250$AGI_250m)
  }
  
  #originally, PA = 0 means a true position. Change so PA = 1 means a true position for fitting the BRT
  dat_all_temp$PA <- replace(dat_all_temp$PA, dat_all_temp$PA == 1, 2) #change PAs to temporarily equal 2
  dat_all_temp$PA <- replace(dat_all_temp$PA, dat_all_temp$PA == 0, 1) #change true positions to a 1
  dat_all_temp$PA <- replace(dat_all_temp$PA, dat_all_temp$PA == 2, 0) #change PA positions to a 0
  
  # randomly select one PA for each tag (CRW dataset only)
  #randomly select 1 PA rep for each tag
  dat_pos <- dat_all_temp %>% filter(PA == 1)
  dat_pa <- dat_all_temp %>% filter(PA == 0)

  dat_temp <- data.frame()
  for(i in 1:length(unique(dat_pa$tag))){
    #select current id
    curr_ID <- unique(dat_pa$tag)[i]
    temp_df <- dat_pa[dat_pa$tag %in% curr_ID,]

    #sample id's randomly
    temp_rep_ID <- sample(unique(temp_df$rep), 1, replace = FALSE)

    #narrow your data set
    temp_df2 <- temp_df[temp_df$rep %in% temp_rep_ID, ]

    #combine in a single df
    dat_temp <- rbind(dat_temp, temp_df2)
  }
  
  dat_all_temp2 <- rbind(dat_temp, dat_pos)
  dat_all_temp3 <- dat_all_temp2 %>% filter(tag != 96365)
  dat_all_temp_fix <- dat_all_temp2 %>% 
    filter(tag == 96365) %>%
    group_by(PA) %>%
    distinct(dt, .keep_all = TRUE) %>%
    ungroup()
  
  dat_all <- rbind(dat_all_temp3, dat_all_temp_fix) %>%
    dplyr::rename(o2_mean_250m = "dat250$o2_mean_250m", 
           AGI_250m = "dat250$AGI_250m")
  
  #combine depth specific data at 60m and 250m for DO and AGI. Erase for base model dataset. Here, I also change the PA values.
  #base model dataset
  dat_base <- dat_all %>% 
    subset(select = -c(o2_mean_0m, pO2_atm_0m, o2_demand_0m, AGI_0m, o2_mean_250m, AGI_250m))
  saveRDS(dat_base, here(paste0(out_path,"/dat_base","_", res,".rds")))
  
  #DO model dataset
  dat_do <- dat_all %>% 
    subset(select = -c(pO2_atm_0m, o2_demand_0m, AGI_0m, AGI_250m))
  saveRDS(dat_do, here(paste0(out_path,"/dat_do","_", res,".rds")))
  
  #AGI model dataset 
  dat_agi <- dat_all %>% 
    subset(select = -c(o2_mean_0m, pO2_atm_0m, o2_demand_0m, o2_mean_250m)) 
  saveRDS(dat_agi, here(paste0(out_path,"/dat_agi","_", res,".rds")))
  
  }

#Daily
dat0_path_dail <- "data/locs_w_covar/psat_spot/cmem_locs_covar_AGI_0m_daily.rds"
dat250_path_dail <- "data/locs_w_covar/psat_spot/cmem_locs_covar_AGI_250m_daily.rds"

out_path_dail <- "data/locs_brts/crw_pas_dail"

format_dat_crw_brts(dat0_path = dat0_path_dail, dat250_path = dat250_path_dail, res = "daily", out_path = out_path_dail)

#seasonal
dat0_path_seas <- "data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_AGI_0m_seas.rds"
dat250_path_seas <- "data/locs_w_covar/psat_spot/seasonal/cmem_locs_covar_AGI_250m_seas.rds"

out_path_seas <- "data/locs_brts/crw_pas_seas"

format_dat_crw_brts(dat0_path = dat0_path_seas, dat250_path = dat250_path_seas, res = "seas", out_path = out_path_seas)

#annual
dat0_path_ann <- "data/locs_w_covar/psat_spot/annual/cmem_locs_covar_AGI_0m_ann.rds"
dat250_path_ann <- "data/locs_w_covar/psat_spot/annual/cmem_locs_covar_AGI_250m_ann.rds"

out_path_ann <- "data/locs_brts/crw_pas_ann"

format_dat_crw_brts(dat0_path = dat0_path_ann, dat250_path = dat250_path_ann, res = "ann", out_path = out_path_ann)

# fit brts ##########
# CRW daily data 
dat_base_d <- readRDS(here("data/locs_brts/crw_pas_dail/dat_base_daily.rds")) %>% mutate(tag = as.factor(tag))
dat_do_d <- readRDS(here("data/locs_brts/crw_pas_dail/dat_do_daily.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_d <- readRDS(here("data/locs_brts/crw_pas_dail/dat_agi_daily.rds")) %>% mutate(tag = as.factor(tag))

# CRW seasonal data 
dat_do_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_do_seas.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_agi_seas.rds")) %>% mutate(tag = as.factor(tag))

# CRW annual data 
dat_do_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_do_ann.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_agi_ann.rds")) %>% mutate(tag = as.factor(tag))

# Add seasonal and annual data to daily data df for DO and AGI
dat_do_all <- cbind(dat_do_d, dat_do_s$o2_mean_0m, dat_do_s$o2_mean_250m, dat_do_a$o2_mean_0m, dat_do_a$o2_mean_250m)
dat_do_all <- dat_do_all %>%
  dplyr::rename("o2_mean_0m_seas" = "dat_do_s$o2_mean_0m",
         "o2_mean_250m_seas" = "dat_do_s$o2_mean_250m", 
         "o2_mean_0m_ann" = "dat_do_a$o2_mean_0m",  
         "o2_mean_250m_ann" = "dat_do_a$o2_mean_250m")

dat_agi_all <- cbind(dat_agi_d, dat_agi_s$AGI_0m, dat_agi_s$AGI_250m, dat_agi_a$AGI_0m, dat_agi_a$AGI_250m)
dat_agi_all <- dat_agi_all %>%
  dplyr::rename("AGI_0m_seas" = "dat_agi_s$AGI_0m",
         "AGI_250m_seas" = "dat_agi_s$AGI_250m", 
         "AGI_0m_ann" = "dat_agi_a$AGI_0m", 
         "AGI_250m_ann" = "dat_agi_a$AGI_250m")

# run brts
pred_vars_base = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd")
pred_vars_do = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m", "o2_mean_250m", "o2_mean_0m_seas", "o2_mean_0m_ann", "o2_mean_250m_seas", "o2_mean_250m_ann")
pred_vars_agi = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "AGI_0m", "AGI_250m", "AGI_0m_seas", "AGI_0m_ann", "AGI_250m_seas", "AGI_250m_ann")

n_cores <- detectCores()
cluster <- makeCluster(n_cores-2)
registerDoParallel(cluster)

brt_run <- function(dat_file, mod_type, save_folder, pred_vars, n_iter = 5){
   set.seed(1004)   

    #test vs train files
    dat_file$row_id <- 1:nrow(dat_file)
    
    foreach(i = 1:n_iter, .packages = c("here", "gbm", "dismo", "tidyverse")) %dopar% {
      set.seed(1004)

      dat_temp <- dat_file %>% 
      sample_frac(0.25)
    
    dat_test <-  subset(dat_file, row_id %in% dat_temp$row_id)
  
    #save test file
    saveRDS(dat_test, file = here(paste0(save_folder, mod_type, "/", "test/", mod_type,"_", "test", i, ".rds")))
    
    dat_train <- subset(dat_file, !(row_id %in% dat_temp$row_id))
    
    try(brt_iter <- dismo::gbm.step(
      data = dat_train, 
      gbm.x = pred_vars, 
      gbm.y = 5,
      family = "bernoulli", 
      tree.complexity = 3,
      learning.rate = 0.05, 
      bag.fraction = 0.75, 
      silent = TRUE, 
      plot.main = TRUE
    )
    )
    
  saveRDS(brt_iter, file = here(paste0(save_folder, mod_type, "/", mod_type,"_", i, ".rds"))) 
    } #end parallel    
    } #end function

# base model
brt_run(dat_file = dat_base_d, mod_type = "base", pred_vars = pred_vars_base, save_folder = "data/brt/mod_outputs/baseline/")

#do model
brt_run(dat_file = dat_do_all, mod_type = "do", pred_vars = pred_vars_do, save_folder = "data/brt/mod_outputs/baseline/")

#agi model
brt_run(dat_file = dat_agi_all, mod_type = "agi", pred_vars = pred_vars_agi, save_folder = "data/brt/mod_outputs/baseline/")

#get performance metrics
brt_perf_metric <- function(mod_files, test_files, test_type, mod_type, domain = "all"){
  
  #read in model and test file locations
  mod_files = list.files(mod_files, full.names = TRUE, pattern = ".rds")
  test_files = list.files(test_files, full.names = TRUE, pattern = ".rds")
 
  for(i in 1:length(mod_files)){
    
    #load model and test file
    mod_file <- readRDS(mod_files[i])
    test_file <- readRDS(test_files[i])
    
    if(i == 1){
      temp_df <- data.frame(matrix(ncol = 4, nrow = 20))
      cols_names <- c("iteration", "AUC", "TSS", "dev_exp")
      colnames(temp_df) <- cols_names  
    }
    
    preds <- predict.gbm(mod_file, test_file,
                         n.trees = mod_file$gbm.call$best.trees,
                         type = "response")
    observed <- test_file$PA
    
    ext.residual.deviance <- calc.deviance(obs = test_file$PA, pred=preds, family="bernoulli", calc.mean=TRUE) #get % deviance
    null.dev =  calc.deviance(test_file$PA ,rep(mean(test_file$PA),length(test_file$PA)), family="bernoulli", calc.mean=T)
    dev=(null.dev - ext.residual.deviance)/null.dev 
    
    dat_pred <- cbind(test_file$PA, preds)
    pres <- dat_pred[dat_pred[,1] == 1, 2]
    abs <- dat_pred[dat_pred[,1] == 0, 2]
    
    #evaluate (AUC, TSS, TPR)
    e = evaluate(p = pres, a = abs)
    
    #store metrics in performance metric df
    temp_df$iteration[i] <- i
    temp_df$AUC[i] <- e@auc
    temp_df$TSS[i] <- max((e@TPR + e@TNR - 1))
    temp_df$dev_exp[i] <- dev

  } #end loop per brt iteration
  
  base::saveRDS(object = temp_df, file = paste0("data/brt/mod_outputs/", test_type, "/", test_type, "_", mod_type, "_metrics.rds"))
  return(temp_df)
  
} #end function

#performance metrics entire domain and study period
#base model
base_metrics <- brt_perf_metric(mod_type = "base", test_type = "baseline", mod_files = "data/brt/mod_outputs/baseline/base", test_files = "data/brt/mod_outputs/baseline/base/test")

#do model
do_metrics <- brt_perf_metric(mod_type = "do", test_type = "baseline", mod_files = "data/brt/mod_outputs/baseline/do", test_files="data/brt/mod_outputs/baseline/do/test")

#agi model
agi_metrics <- brt_perf_metric(mod_type = "agi", test_type = "baseline",  mod_files = "data/brt/mod_outputs/baseline/agi", test_files = "data/brt/mod_outputs/baseline/agi/test")

# performance metrics overall #####
#entire domain and study period
mod_metric_files <- list.files(here("data/brt/mod_outputs/baseline"), pattern = ".rds", full.names = TRUE)

base_file <- readRDS(mod_metric_files[2])
do_file <- readRDS(mod_metric_files[3])
agi_file <- readRDS(mod_metric_files[1])

base_file$mod_type <- "Base model"
agi_file$mod_type <- "AGI model"
do_file$mod_type <- "DO model"

mod_metrics <- rbind(base_file, agi_file, do_file)
mod_metrics <- mod_metrics %>% mutate(st_id = "Overall")

#combine datasets
mod_metrics <- mod_metrics %>% mutate(dev_exp = dev_exp*100)
all_sum <- mod_metrics %>%
  group_by(mod_type) %>%
  summarise(mean_auc = mean(AUC, na.rm = TRUE), 
            sd_auc = sd(AUC, na.rm = TRUE), 
            mean_tss = mean(TSS, na.rm = TRUE), 
            sd_tss = sd(TSS, na.rm = TRUE), 
            mean_dev = mean(dev_exp, na.rm = TRUE),
            sd_dev = sd(dev_exp, na.rm = TRUE)) %>%
  ungroup() 

#overall plots
TSS_overall <- all_sum %>% mutate(mod_type = as.factor(mod_type), 
                                  mod_type = fct_relevel(mod_type, c("Base model", "AGI model", "DO model"))) %>%
  arrange(desc(mean_tss)) %>%
  ggplot(aes(x = mod_type, y=mean_tss)) +
  geom_errorbar(aes(ymin = mean_tss - 2*sd_tss, ymax = mean_tss + 2*sd_tss), color = "black", size =  1, width = 0, linewidth = 1)+
  geom_point(color = "black", size = 4)+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("TSS") + 
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16)) 

AUC_overall <- all_sum %>% mutate(mod_type = as.factor(mod_type), 
                                  mod_type = fct_relevel(mod_type, c("Base model", "AGI model", "DO model"))) %>%
  arrange(desc(mean_auc)) %>%
  ggplot(aes(x = mod_type, y=mean_auc)) +
  geom_errorbar(aes(ymin = mean_auc - 2*sd_auc, ymax = mean_auc + 2*sd_auc), color = "black", size =  1, width = 0, linewidth = 1)+
  geom_point(color = "black", size = 4)+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("AUC") + 
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16)) 

dev_overall <- all_sum %>% mutate(mod_type = as.factor(mod_type), 
                                                 mod_type = fct_relevel(mod_type, c("Base model", "AGI model", "DO model"))) %>%
  arrange(desc(mean_dev)) %>%
  ggplot(aes(x = mod_type, y=mean_dev)) +
  geom_errorbar(aes(ymin = mean_dev - 2*sd_dev, ymax = mean_dev + 2*sd_dev), color = "black", size =  1, width = 0, linewidth = 1)+
  geom_point(color = "black", size = 4)+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("% Deviance explained") + 
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16)) 

overall_metric_plots <- TSS_overall/AUC_overall/dev_overall
ggsave(here("figs/agi_explore/baseline/overall_metrics.png"), overall_metric_plots, height = 13, width = 5, units = c("in"))

# plot AGI #####
#neutral year
hsi_rast_gen(date_start = c("2013-09-01"), date_end = c("2014-01-31"), season = "FW", output_name = "neut_FW_Sept2013_Jan2014")

#La Niña - 2007/2008
hsi_rast_gen(date_start = c("2007-11-01"), date_end = c("2008-01-31"), season = "FW", output_name = "LN_FW_2007_2008")

#La Niña - 2010
hsi_rast_gen(date_start = c("2010-09-01"), date_end = c("2010-11-30"), season = "F", output_name = "LN_F_2010")

#EL Niño
hsi_rast_gen(date_start = c("2014-11-01"), date_end = c("2015-01-31"), season = "FW", output_name = "EN_FW_Nov2014_Jan2015")

agi_250m_layered <- agi_maps_layerd(rast_folder_base = here("data/enviro/psat_spot_all/hsi_rasts/agi_rasts/neut_FW_Sept2013_Jan2014"), 
                                    rast_folder_LN = here("data/enviro/psat_spot_all/hsi_rasts/agi_rasts/LN_F_2010"), 
                                    rast_folder_EN = here("data/enviro/psat_spot_all/hsi_rasts/agi_rasts/EN_FW_Nov2014_Jan2015"))

ggsave(here("figs/agi_explore/baseline/agi_250m_layered.png"), agi_250m_layered, height = 8, width = 9, units = c("in"))

# plot HSI #####
theme_ms_map <- function(){ 
  font <- "Arial"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      plot.title = element_text(             #axis titles
        family = font,            #font family
        color = "black",
        size = 16), 
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        color = "black",
        size = 16),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        color = "black",
        size = 16),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      legend.position = "none"
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

#generate raster
hsi_rast_gen(date_start = c("2003-01-01"), date_end = c("2015-12-31"), season = "SuFWSp", output_name = "Jan2003_Dec2015")

#plot HSI
all_maps_avg <- hsi_maps_avg(rast_folder = "data/enviro/psat_spot_all/hsi_rasts/Jan2003_Dec2015",
                             mod_folder = "data/brt/mod_outputs/baseline",
                             fig_folder = "figs/agi_explore/baseline",
                             ms = "Y", 
                             iter = 5)
all_maps_avg
ggsave(here("figs/agi_explore/baseline/hsi_map.png"), all_maps_avg, height = 7, width = 10, units = c("in"))

#plot spring HSI ####
study_years <- seq(from = 2003, to = 2015, by = 1)
spring_list <- rast()
for(i in 1:length(study_years)){ #represents number of study years
  year = study_years[i]
  hsi_rast_gen(date_start = c(paste0(year, "-03-01")), date_end = c(paste0(year, "-05-31")), season = "Sp", output_name = paste0("March", year, "_May", year, "_spring"))
}

spring_base <- rast()
spring_do <- rast()
spring_agi <- rast()
spring_files <- list.files(here("data/enviro/psat_spot_all/hsi_rasts"), full.names = TRUE, pattern = "spring")
for(i in 1:length(study_years)){

  base_rast_file <- list.files(here(spring_files[i]), pattern = "base", full.names = TRUE)
  do_rast_file <- list.files(here(spring_files[i]), pattern = "do", full.names = TRUE)
  agi_rast_file <- list.files(here(spring_files[i]), pattern = "agi", full.names = TRUE)

  base_rast <- rast(base_rast_file)
  do_rast <- rast(do_rast_file)
  agi_rast <- rast(agi_rast_file)
  
  spring_base <- c(spring_base, base_rast)
  spring_do <- c(spring_do, do_rast)
  spring_agi <- c(spring_agi, agi_rast)
}

idx_base = rep(1:7, times = 13) #7 variables, 13 years
spring_base_avg <- tapp(spring_base, index = idx_base, fun = mean)
writeCDF(spring_base_avg, here("data/enviro/psat_spot_all/hsi_rasts/2003_2015_spring/2003_2015_spring_base.nc"))

idx_do_agi = rep(1:13, times = 13)
spring_do_avg <- tapp(spring_do, index = idx_do_agi, fun = mean)
writeCDF(spring_do_avg, here("data/enviro/psat_spot_all/hsi_rasts/2003_2015_spring/2003_2015_spring_do.nc"))

spring_agi_avg <- tapp(spring_agi, index = idx_do_agi, fun = mean)
writeCDF(spring_agi_avg, here("data/enviro/psat_spot_all/hsi_rasts/2003_2015_spring/2003_2015_spring_agi.nc"))

hsi_maps_avg(rast_folder = "data/enviro/psat_spot_all/hsi_rasts/March2010_May2010_spring",
                             mod_folder = "data/brt/mod_outputs/baseline",
                             fig_folder = "figs/agi_explore/baseline",
                             ms = "Y", 
                             iter = 5)

#plot ENSO ####
#base year
enso_base <- hsi_maps_enso_avg(rast_folder = "data/enviro/psat_spot_all/hsi_rasts/neut_FW_Sept2013_Jan2014", test_type = "baseline", enso = "diff", iter = 5)

#LN year - 2008
enso_LN_08 <- hsi_maps_difference_enso_avg(enso_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/LN_FW_2007_2008", test_type = "baseline", neut_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/neut_FW_Sept2013_Jan2014", enso = "LN", iter = 5)

#LN year - 2010
enso_LN_10 <- hsi_maps_difference_enso_avg(enso_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/LN_F_2010", test_type = "baseline", neut_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/neut_FW_Sept2013_Jan2014", enso = "LN", iter = 5)

#EN year
enso_EN <- hsi_maps_difference_enso_avg(enso_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/EN_FW_Nov2014_Jan2015", test_type = "baseline", neut_rast_folder = "data/enviro/psat_spot_all/hsi_rasts/neut_FW_Sept2013_Jan2014", enso = "EN", iter = 5)

enso_base | enso_LN | enso_LN_08 | enso_EN
ggsave(here("figs/agi_explore/baseline/enso_maps.png"), height = 10, width = 10, units = c("in"))

#calculate % area HSI > 0.25 in NEC in strong LN year between models
calc_perc_area <- function(mod_rast, mod_type, test_type, iter){
  
  if(mod_type == "base"){  
  names(mod_rast) <- c("bathy_mean", "temp_mean", "sal_mean", "chl_mean", "ssh_mean", "bathy_sd", "mld_mean")
  }
  if(mod_type == "do"){
  names(mod_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
  }
  if(mod_type == "agi"){
  names(mod_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
  }
  
  #creating map dfs -------------------------------------------------------------------------------------------------
  mod_folder <- list.files(here(paste0("data/brt/mod_outputs/", test_type,"/", mod_type)), full.names = TRUE)
  
  bbox <- ext(-153, -103, 1, 49)

  map_list <- rast()
  #for loop to create raster for each model iteration
  for(i in 1:iter){
    #creating map dfs -------------------------------------------------------------------------------------------------
    print(i)
    
    #predict
    mod_file <- readRDS(mod_folder[i])
    map_pred <- predict(mod_rast, mod_file, type = "response", n.trees = mod_file$gbm.call$best.trees, na.rm = FALSE)
    map_pred <- crop(map_pred, bbox)
    map_list <- c(map_list, map_pred)
  }
  
  #take average of rasters produced from each model
  pred_avg <- mean(map_list)

  # mask out land
  land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  land <- vect(land)

  land <- crop(land, bbox)
  pred_avg <- mask(pred_avg, land, inverse = TRUE)
  
  #filter for just NEC and calculate percent area w hsi > 0.75
  rast_nec_filt <- pred_avg %>% filter(y <= 15)

  hsi_nec <- raster::clamp(rast_nec_filt, lower = 0.25, values = FALSE)

  hsi_area_map <- expanse(hsi_nec)
  rast_area_map <- expanse(rast_nec_filt)

  perc_area <- (hsi_area_map$area/rast_area_map$area)*100
  
  return(perc_area)
}

#Base
base_ln_rast_10 <- rast(here("data/enviro/psat_spot_all/hsi_rasts/LN_F_2010/LN_F_2010_base_rast.nc"))
base_ln_rast_10 <- base_ln_rast_10 %>% filter(y > 2) #filter out weird artefact of averaging rasters
nec_base_10 <- calc_perc_area(mod_rast = base_ln_rast_10, mod_type = "base", test_type = "baseline", iter = 5)

base_ln_rast_08 <- rast(here("data/enviro/psat_spot_all/hsi_rasts/LN_FW_2007_2008/LN_FW_2007_2008_base_rast.nc"))
base_ln_rast_08 <- base_ln_rast_08 %>% filter(y > 2) #filter out weird artefact of averaging rasters
nec_base_08 <- calc_perc_area(mod_rast = base_ln_rast_08, mod_type = "base", test_type = "baseline", iter = 5)

#DO
do_ln_rast_10 <- rast(here("data/enviro/psat_spot_all/hsi_rasts/LN_F_2010/LN_F_2010_do_rast.nc"))
do_ln_rast_10 <- do_ln_rast_10 %>% filter(y > 2) #filter out weird artefact of averaging rasters
nec_do_10 <- calc_perc_area(mod_rast = do_ln_rast_10, mod_type = "do", test_type = "baseline", iter = 5)

do_ln_rast_08 <- rast(here("data/enviro/psat_spot_all/hsi_rasts/LN_FW_2007_2008/LN_FW_2007_2008_do_rast.nc"))
do_ln_rast_08 <- do_ln_rast_08 %>% filter(y > 2) #filter out weird artefact of averaging rasters
nec_do_08 <- calc_perc_area(mod_rast = do_ln_rast_08, mod_type = "do", test_type = "baseline", iter = 5)

#AGI
agi_ln_rast_10 <- rast(here("data/enviro/psat_spot_all/hsi_rasts/LN_F_2010/LN_F_2010_agi_rast.nc"))
agi_ln_rast_10 <- agi_ln_rast_10 %>% filter(y > 2) #filter out weird artefact of averaging rasters
nec_agi_10 <- calc_perc_area(mod_rast = agi_ln_rast_10, mod_type = "agi", test_type = "baseline", iter = 5)

agi_ln_rast_08 <- rast(here("data/enviro/psat_spot_all/hsi_rasts/LN_FW_2007_2008/LN_FW_2007_2008_agi_rast.nc"))
agi_ln_rast_08 <- agi_ln_rast_08 %>% filter(y > 2) #filter out weird artefact of averaging rasters
nec_agi_08 <- calc_perc_area(mod_rast = agi_ln_rast_08, mod_type = "agi", test_type = "baseline", iter = 5)

df <- data.frame(area = rep(c("NEC_LN_2010", "NEC_LC_2008"), times = 3),
            model_type = c("base", "do", "agi", "base", "do", "agi"),
            perc_area  = c(nec_base_10, nec_base_08, nec_do_10, nec_do_08, nec_agi_10, nec_agi_08)
); df


#plot SHAP ####
### CRW daily data 
dat_base_d <- readRDS(here("data/locs_brts/crw_pas_dail/dat_base_daily.rds")) %>% mutate(tag = as.factor(tag))
dat_do_d <- readRDS(here("data/locs_brts/crw_pas_dail/dat_do_daily.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_d <- readRDS(here("data/locs_brts/crw_pas_dail/dat_agi_daily.rds")) %>% mutate(tag = as.factor(tag))

#### CRW seasonal data 
dat_do_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_do_seas.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_s <- readRDS(here("data/locs_brts/crw_pas_seas/dat_agi_seas.rds")) %>% mutate(tag = as.factor(tag))

#### CRW annual data 
dat_do_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_do_ann.rds")) %>% mutate(tag = as.factor(tag))
dat_agi_a <- readRDS(here("data/locs_brts/crw_pas_ann/dat_agi_ann.rds")) %>% mutate(tag = as.factor(tag))

#### Add seasonal and annual data to daily data df for DO and AGI
dat_do_all <- cbind(dat_do_d, dat_do_s$o2_mean_0m, dat_do_s$o2_mean_250m, dat_do_a$o2_mean_0m, dat_do_a$o2_mean_250m)
dat_do_all <- dat_do_all %>%
  dplyr::rename("o2_mean_0m_seas" = "dat_do_s$o2_mean_0m", 
                "o2_mean_250m_seas" = "dat_do_s$o2_mean_250m", 
                "o2_mean_0m_ann" = "dat_do_a$o2_mean_0m",  
                "o2_mean_250m_ann" = "dat_do_a$o2_mean_250m")

dat_agi_all <- cbind(dat_agi_d, dat_agi_s$AGI_0m, dat_agi_s$AGI_250m, dat_agi_a$AGI_0m, dat_agi_a$AGI_250m)
dat_agi_all <- dat_agi_all %>%
  dplyr::rename("AGI_0m_seas" = "dat_agi_s$AGI_0m",
                "AGI_250m_seas" = "dat_agi_s$AGI_250m", 
                "AGI_0m_ann" = "dat_agi_a$AGI_0m", 
                "AGI_250m_ann" = "dat_agi_a$AGI_250m")

dat_base_d$row_id <- 1:nrow(dat_base_d)
dat_do_all$row_id <- 1:nrow(dat_do_all)
dat_agi_all$row_id <- 1:nrow(dat_agi_all)

### target point for waterfall plots
target_loc <- vect(cbind(-133,12.5), crs="EPSG:4326")

### load models
brt_do <- readRDS(here("data/brt/mod_outputs/baseline/do/do_1.rds"))
brt_agi <- readRDS(here("data/brt/mod_outputs/baseline/agi/agi_1.rds"))

pred_fun <- function(model, newdata) {
  predict(model, newdata = newdata, type = "response")
}

### DO 
### Training data
test_do <- readRDS(here("data/brt/mod_outputs/baseline/do/test/do_test1.rds")) 
train_do <- filter(dat_do_all, !(row_id %in% test_do$row_id)) %>% 
  dplyr::select(-c(tag, date, lon, lat, rep, dt, uo_mean, uostr_mean, vo_mean, vostr_mean))

# Extract target location's env
do_rast <- rast("data/enviro/psat_spot_all/hsi_rasts/Jan2003_Dec2015/Jan2003_Dec2015_do_rast.nc")
names(do_rast) <- c("o2_mean_0m", "o2_mean_250m_ann", "o2_mean_0m_seas", "temp_mean", "o2_mean_250m_seas", "bathy_mean", "sal_mean", "chl_mean", "o2_mean_0m_ann", "o2_mean_250m", "ssh_mean", "mld_mean", "bathy_sd")
target_env_do <- terra::extract(do_rast, target_loc) %>% 
  as_tibble() %>% 
  select(-ID)

#plot
x = train_do[, c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m", "o2_mean_250m", "o2_mean_0m_seas", "o2_mean_0m_ann", "o2_mean_250m_seas", "o2_mean_250m_ann")]
predictor <- Predictor$new(brt_do, data = x, y = train_do$PA, predict.function = pred_fun)
shapley <- Shapley$new(predictor, x.interest = target_env_do)

print(shapley$results)
plot(shapley)

# AGI 
# Training data
test_agi <- readRDS(here("data/brt/mod_outputs/baseline/agi/test/agi_test1.rds")) 
train_agi <- filter(dat_agi_all, !(row_id %in% test_agi$row_id)) %>% 
  dplyr::select(-c(tag, date, lon, lat, rep, dt, uo_mean, uostr_mean, vo_mean, vostr_mean))

# Extract target location's env
agi_rast <- rast("data/enviro/psat_spot_all/hsi_rasts/Jan2003_Dec2015/Jan2003_Dec2015_agi_rast.nc")
names(agi_rast) <- c("temp_mean", "AGI_250m_ann", "AGI_0m", "bathy_mean", "AGI_0m_seas", "sal_mean", "AGI_250m_seas", "AGI_0m_ann", "chl_mean", "AGI_250m", "bathy_sd", "mld_mean", "ssh_mean")
target_env_agi <- terra::extract(agi_rast, target_loc) %>% 
  as_tibble() %>% 
  select(-ID)

#plot
x = train_agi[,c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "AGI_0m", "AGI_250m", "AGI_0m_seas", "AGI_0m_ann", "AGI_250m_seas", "AGI_250m_ann")]
predictor <- Predictor$new(brt_agi, data = x, y = train_agi$PA, predict.function = pred_fun)
shapley <- Shapley$new(predictor, x.interest = target_env_agi)

print(shapley$results)
plot(shapley)

# partial plots ####
ggPD_boot <- function (gbm.object, predictor = NULL, n.plots = length(pred.names), 
                       list.4.preds = NULL, booted.preds = NULL, nrow = NULL, ncol = NULL, 
                       col.line = "darkorange", cex.line = 0.5, type.ci = "lines", 
                       col.ci = "grey80", cex.ci = 0.3, lty.ci = 2, alpha.ci = 0.5, 
                       smooth = FALSE, col.smooth = "blue", cex.smooth = 0.3, span = 0.3, 
                       rug = FALSE, rug.pos = "t", common.scale = TRUE, type = NULL, cis = c(0.025, 
                                                                                0.975), y.label = "Fitted function", x.label = paste(var.name, "  (", 
                                                                                                                                     round(gbm.object$contributions[predictor, 2], 
                                                                                                                                           1), "%)", sep = ""), 
                       ...) 
{
  gbm.call <- gbm.object$gbm.call
  pred.names <- gbm.call$predictor.names
  ggPD_boot.plots <- function(gbm.object) {
    if (!requireNamespace("gbm")) {
      stop("you need to install the gbm package to run this function")
    }
    if (is.null(booted.preds)) {
      stop("you need to set booted.preds as the array from the bootstrap run\n           (eg testboot$function.preds using testboot<-gbm.bootstrap.functions())")
    }
    if (is.null(list.4.preds)) {
      stop("you need to set list.4.preds as the result of plot.gbm.4list()")
    }
    requireNamespace("splines")
    gbm.x <- gbm.call$gbm.x
    response.name <- gbm.call$response.name
    nt <- gbm.object$n.trees
    data <- gbm.call$dataframe
    max.vars <- length(gbm.object$contributions$var)
    if (n.plots > max.vars) {
      n.plots <- max.vars
      warning("reducing no of plotted predictors to maximum available (", 
              max.vars, ")")
    }
    predictors <- list(rep(NA, n.plots))
    responses <- list(rep(NA, n.plots))
    responses.lower <- list(rep(NA, n.plots))
    responses.upper <- list(rep(NA, n.plots))
    for (j in c(1:max.vars)) {
      k <- match(gbm.object$contributions$var[j], pred.names)
      if (is.null(x.label)) {
        var.name <- gbm.call$predictor.names[k]
      }
      else {
        var.name <- x.label
      }
      pred.data <- data[, gbm.call$gbm.x[k]]
      response.matrix <- gbm::plot.gbm(gbm.object, i.var = k, 
                                       n.trees = nt, return.grid = TRUE, ...)
      predictors[[j]] <- response.matrix[, 1]
      if (is.factor(data[, gbm.call$gbm.x[k]])) {
        predictors[[j]] <- factor(predictors[[j]], levels = levels(data[, 
                                                                        gbm.call$gbm.x[k]]))
      }
      responses[[j]] <- response.matrix[, 2] - mean(response.matrix[, 
                                                                    2])
      num.values <- nrow(response.matrix)
      temp <- apply(booted.preds[, k, ] - mean(booted.preds[, 
                                                            k, ]), 1, function(x) {
                                                              quantile(x, cis[1], na.rm = T)
                                                            })
      responses.lower[[j]] <- temp[1:num.values]
      temp <- apply(booted.preds[, k, ] - mean(booted.preds[, 
                                                            k, ]), 1, function(x) {
                                                              quantile(x, cis[2], na.rm = T)
                                                            })
      responses.upper[[j]] <- temp[1:num.values]
      if (j == 1) {
        ymin = min(responses.lower[[j]])
        ymax = max(responses.upper[[j]])
        dat <- data.frame(pred.data)
      }
      else {
        ymin = min(ymin, min(responses.lower[[j]]))
        ymax = max(ymax, max(responses.upper[[j]]))
        dat <- data.frame(dat, pred.data)
      }
    }
    if (is.null(predictor)) {
      fittedFunc <- list()
      fittedFunc.lower <- list()
      fittedFunc.upper <- list()
      fittedVal <- list()
      ribbon <- list()
      ggPD <- list()
      for (i in 1:n.plots) {
        k <- match(gbm.object$contributions$var[i], pred.names)
        var.name <- gbm.call$predictor.names[k]
        fittedFunc[[i]] <- data.frame(predictors[i], 
                                      responses[i])
        colnames(fittedFunc[[i]]) <- c("x", "y")
        fittedFunc.lower[[i]] <- data.frame(predictors[i], 
                                            responses.lower[i])
        colnames(fittedFunc.lower[[i]]) <- c("x", "y")
        fittedFunc.upper[[i]] <- data.frame(predictors[i], 
                                            responses.upper[i])
        colnames(fittedFunc.upper[[i]]) <- c("x", "y")
        fittedVal[[i]] <- data.frame(gbm.object$fitted, 
                                     dat[i])
        colnames(fittedVal[[i]]) <- c("y", "x")
        ribbon[[i]] <- data.frame(x = fittedFunc.lower[[i]]$x, 
                                  ylow = fittedFunc.lower[[i]]$y, yup = fittedFunc.upper[[i]]$y)
        if (is.factor(fittedFunc[[i]]$x)) {
          ggPD[[i]] <- ggplot(fittedFunc[[i]], aes(x = x, 
                                                   y = y)) + geom_boxplot(color = col.line, 
                                                                          size = cex.line) + geom_boxplot(data = fittedFunc.lower[[i]], 
                                                                                                          aes(x = x, y = y), color = col.ci) + geom_boxplot(data = fittedFunc.upper[[i]], 
                                                                                                                                                            aes(x = x, y = y), color = col.ci) + ylab(y.label) + 
            xlab(paste(var.name, "  (", round(gbm.object$contributions[i, 
                                                                       2], 1), "%)", sep = "")) + theme_bw() + 
            theme(panel.grid.minor = element_line(linetype = "blank"), 
                  panel.grid.major = element_line(linetype = "blank"), 
                  axis.text.x = element_text(size = 6), axis.title.x = element_text(size = 10), 
                  axis.line.y = element_line(size = 0.1), 
                  axis.line.x = element_line(size = 0.1))
          if (common.scale == T) {
            ggPD[[i]] <- ggPD[[i]] + ylim(c(ymin, ymax))
          }
        }
        if (type.ci == "lines") {
          ggPD[[i]] <- ggplot(fittedFunc[[i]], aes(x = x, 
                                                   y = y)) + geom_line(color = col.line, size = cex.line) + 
            geom_line(data = fittedFunc.lower[[i]], aes(x = x, 
                                                        y = y), size = cex.ci, color = col.ci, 
                      linetype = lty.ci) + geom_line(data = fittedFunc.upper[[i]], 
                                                     aes(x = x, y = y), size = cex.ci, color = col.ci, 
                                                     linetype = lty.ci) + ylab(y.label) + xlab(paste(var.name, 
                                                                                                     "  (", round(gbm.object$contributions[i, 
                                                                                                                                           2], 1), "%)", sep = "")) + theme_bw() + 
            theme(panel.grid.minor = element_line(linetype = "blank"), 
                  panel.grid.major = element_line(linetype = "blank"), 
                  axis.title.x = element_text(size = 10), 
                  axis.line.y = element_line(size = 0.1), 
                  axis.line.x = element_line(size = 0.1))
          if (smooth == T) {
            ggPD[[i]] <- ggPD[[i]] + geom_smooth(span = span, 
                                                 size = 0.3, color = col.smooth, se = F, 
                                                 linetype = 2)
          }
          if (rug == T) {
            ggPD[[i]] <- ggPD[[i]] + geom_rug(data = fittedVal[[i]], 
                                              aes(x = x, y = y), sides = rug.pos, position = "jitter", 
                                              color = "#EBEBEB")
          }
          if (common.scale == T) {
            ggPD[[i]] <- ggPD[[i]] + ylim(c(ymin, ymax))
          }
        }
        if (type.ci == "ribbon") {
          ggPD[[i]] <- ggplot() + geom_ribbon(data = ribbon[[i]], 
                                              aes(x = x, ymin = ylow, ymax = yup), fill = col.ci, 
                                              alpha = alpha.ci) + geom_line(data = fittedFunc[[i]], 
                                                                            aes(x = x, y = y), color = col.line, size = cex.line) + 
            ylab(y.label) + xlab(paste(var.name, "  (", 
                                       round(gbm.object$contributions[i, 2], 1), 
                                       "%)", sep = "")) + theme_bw() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                                             panel.grid.major = element_line(linetype = "blank"), 
                                                                             axis.title.x = element_text(size = 10), axis.line.y = element_line(size = 0.1), 
                                                                             axis.line.x = element_line(size = 0.1))
          if (smooth == T) {
            ggPD[[i]] <- ggPD[[i]] + geom_smooth(data = fittedFunc[[i]], 
                                                 aes(x = x, y = y), span = span, size = 0.3, 
                                                 color = col.smooth, se = F, linetype = 2)
          }
          if (rug == T) {
            ggPD[[i]] <- ggPD[[i]] + geom_rug(data = fittedVal[[i]], 
                                              aes(x = x, y = y), sides = rug.pos, position = "jitter", 
                                              color = "#EBEBEB")
          }
          if (common.scale == T) {
            ggPD[[i]] <- ggPD[[i]] + ylim(c(ymin, ymax))
          }
        }
      }
      list(ggPD = ggPD)
    }
    else {
      if (is.character(predictor)) {
        predictor <- match(predictor, gbm.object$contributions$var)
      }
      k <- match(gbm.object$contributions$var[predictor], 
                 pred.names)
      var.name <- gbm.call$predictor.names[k]
      fittedFunc <- data.frame(predictors[predictor], responses[predictor])
      colnames(fittedFunc) <- c("x", "y")
      fittedFunc.lower <- data.frame(predictors[predictor], 
                                     responses.lower[predictor])
      colnames(fittedFunc.lower) <- c("x", "y")
      fittedFunc.upper <- data.frame(predictors[predictor], 
                                     responses.upper[predictor])
      colnames(fittedFunc.upper) <- c("x", "y")
      ribbon <- data.frame(x = fittedFunc.lower$x, ylow = fittedFunc.lower$y, 
                           yup = fittedFunc.upper$y)
      fittedVal <- data.frame(gbm.object$fitted, dat[predictor])
      colnames(fittedVal) <- c("y", "x")
      if (is.factor(fittedFunc$x)) {
        ggPD <- ggplot(fittedFunc, aes(x = x, y = y)) + 
          geom_boxplot(color = col.line, size = cex.line) + 
          geom_boxplot(data = fittedFunc.lower, aes(x = x, 
                                                    y = y), color = col.ci) + geom_boxplot(data = fittedFunc.upper, 
                                                                                           aes(x = x, y = y), color = col.ci) + ylab(y.label) + 
          xlab(paste(var.name, "  (", round(gbm.object$contributions[predictor, 
                                                                     2], 1), "%)", sep = "")) + theme_bw() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                                                                                   panel.grid.major = element_line(linetype = "blank"), 
                                                                                                                   axis.text.x = element_text(size = 6), axis.title.x = element_text(size = 10), 
                                                                                                                   axis.line.y = element_line(size = 0.1), axis.line.x = element_line(size = 0.1))
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      if (type.ci == "lines") {
        ggPD <- ggplot(fittedFunc, aes(x = x, y = y)) + 
          geom_line(color = col.line, size = cex.line) + 
          geom_line(data = fittedFunc.lower, aes(x = x, 
                                                 y = y), size = cex.ci, color = col.ci, linetype = lty.ci) + 
          geom_line(data = fittedFunc.upper, aes(x = x, 
                                                 y = y), size = cex.ci, color = col.ci, linetype = lty.ci) + 
          ylab(y.label) + xlab(paste(var.name, "  (", 
                                     round(gbm.object$contributions[predictor, 2], 
                                           1), "%)", sep = "")) + theme_bw() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                                                     panel.grid.major = element_line(linetype = "blank"), 
                                                                                     axis.title.x = element_text(size = 10), axis.line.y = element_line(size = 0.1), 
                                                                                     axis.line.x = element_line(size = 0.1))
        if (smooth == T) {
          ggPD <- ggPD + geom_smooth(span = span, size = 0.3, 
                                     color = col.smooth, se = F, linetype = 2)
        }
        if (rug == T) {
          ggPD <- ggPD + geom_rug(data = fittedVal, aes(x = x, 
                                                        y = y), sides = rug.pos, position = "jitter", 
                                  color = "#EBEBEB")
        }
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      if (type.ci == "ribbon") {
        ggPD <- ggplot() + geom_ribbon(data = ribbon, 
                                       aes(x = x, ymin = ylow, ymax = yup), fill = col.ci, 
                                       alpha = alpha.ci) + geom_line(data = fittedFunc, 
                                                                     aes(x = x, y = y), color = col.line, size = cex.line) + 
          ylab(y.label) + xlab(x.label) + theme_minimal() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                                                     panel.grid.major = element_line(linetype = "blank"), 
                                                                                     axis.title.x = element_text(size = 14), axis.line.y = element_line(size = 0.1), 
                                                                                     axis.line.x = element_line(size = 0.1), 
                                                                  axis.text = element_text(size = 14, color = "black"))
        if (smooth == T) {
          ggPD <- ggPD + geom_smooth(data = fittedFunc, 
                                     aes(x = x, y = y), span = span, size = 0.3, 
                                     color = col.smooth, se = F, linetype = 2)
        }
        if (rug == T) {
          ggPD <- ggPD + geom_rug(data = fittedVal, aes(x = x, 
                                                        y = y), sides = rug.pos, position = "jitter", 
                                  color = "#EBEBEB")
        }
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      
      if (type.ci == "ribbon" & type == "do" & i != 1 & i != 10) {
        ggPD <- ggplot() + geom_ribbon(data = ribbon, 
                                       aes(x = x, ymin = ylow, ymax = yup), fill = col.ci, 
                                       alpha = alpha.ci) + geom_line(data = fittedFunc, 
                                                                     aes(x = x, y = y), color = col.line, size = cex.line) + 
          ylab(y.label) + xlab(x.label) + theme_minimal() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                             panel.grid.major = element_line(linetype = "blank"), 
                                                             axis.title.x = element_text(size = 14), axis.line.y = element_blank(), 
                                                             axis.line.x = element_line(size = 0.1), 
                                                             axis.text.x = element_text(size = 14, color = "black"))
                                                             #axis.text.y = element_blank(), 
                                                             #axis.title.y = element_blank())
        if (smooth == T) {
          ggPD <- ggPD + geom_smooth(data = fittedFunc, 
                                     aes(x = x, y = y), span = span, size = 0.3, 
                                     color = col.smooth, se = F, linetype = 2)
        }
        if (rug == T) {
          ggPD <- ggPD + geom_rug(data = fittedVal, aes(x = x, 
                                                        y = y), sides = rug.pos, position = "jitter", 
                                  color = "#EBEBEB")
        }
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      
      if (type.ci == "ribbon" & type == "agi" & i != 3 & i != 10) {
        ggPD <- ggplot() + geom_ribbon(data = ribbon, 
                                       aes(x = x, ymin = ylow, ymax = yup), fill = col.ci, 
                                       alpha = alpha.ci) + geom_line(data = fittedFunc, 
                                                                     aes(x = x, y = y), color = col.line, size = cex.line) + 
          ylab(y.label) + xlab(x.label) + theme_minimal() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                                  panel.grid.major = element_line(linetype = "blank"), 
                                                                  axis.title.x = element_text(size = 14), axis.line.y = element_blank(), 
                                                                  axis.line.x = element_line(size = 0.1), 
                                                                  axis.text.x = element_text(size = 14, color = "black"))
                                                                  #axis.text.y = element_blank(), 
                                                                  #axis.title.y = element_blank())
        if (smooth == T) {
          ggPD <- ggPD + geom_smooth(data = fittedFunc, 
                                     aes(x = x, y = y), span = span, size = 0.3, 
                                     color = col.smooth, se = F, linetype = 2)
        }
        if (rug == T) {
          ggPD <- ggPD + geom_rug(data = fittedVal, aes(x = x, 
                                                        y = y), sides = rug.pos, position = "jitter", 
                                  color = "#EBEBEB")
        }
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      
      list(ggPD = ggPD)
    }
  }
  plot <- ggPD_boot.plots(gbm.object)
  if (is.null(predictor)) {
    do.call(grid.arrange, c(plot$ggPD, list(nrow = nrow, 
                                            ncol = ncol)))
  }
  else grid.draw(plot$ggPD)
}

#base plot
# Boostrap the BRT 1000 times to build confidence intervals
brt1.prerun_base<- plot.gbm.4list(base_mod)
base_boot <- gbm.bootstrap.functions(base_mod, list.predictors=brt1.prerun, n.reps=20)

#base model
plot_list <- list()
base_names <- c("z", "temp", "sal", "chl-a", "z_sd", "SSH", "MLD")
for(i in 1:nrow(base_mod$contributions)){
  plot_temp <- ggPD_boot(base_mod, 
                         predictor = base_mod$contributions[i, 1], 
                         list.4.preds = brt1.prerun_base, 
                         booted.preds = base_boot$function.preds, 
                         type.ci = "ribbon",
                         rug = T, 
                         alpha.ci = 0.75, 
                         y.label = "Probability of presence", 
                         x.label = paste(base_names[i], "  (", 
                                         round(base_mod$contributions[i, 2], 
                                               1), "%)", sep = ""))
  
  plot_list[[i]] <- plot_temp
}

base_plots <- do.call(grid.arrange, c(plot_list, ncol = 5))
ggsave(here("figs/ms/supp_figs/par_plot_base.png"), base_plots, height = 4, width = 8.5, units = c("in"))

#do model
brt1.prerun_do<- plot.gbm.4list(do_mod_fin)
do_boot <- gbm.bootstrap.functions(do_mod_fin, list.predictors=brt1.prerun_do, n.reps=20)

plot_list <- list()
do_names <- c("DO, daily, 0m", "DO, annual, 250m", "DO, seasonal, 0m", "DO, seasonal, 250m", "temp", "sal", "z", "chl-a", "DO, annual, 0m", "DO, daily, 250m", "SSH", "MLD", "z_sd")
for(i in 1:nrow(do_mod_fin$contributions)){
  plot_temp <- ggPD_boot(do_mod_fin, 
                         predictor = do_mod_fin$contributions[i, 1], 
                         list.4.preds = brt1.prerun_do, 
                         booted.preds = do_boot$function.preds, 
                         type.ci = "ribbon",
                         col.line = "#92351e", 
                         cex.line = 1.5,
                         type = "do",
                         rug = T, 
                         alpha.ci = 0.75, 
                         y.label = "",
                         x.label = "")
  
  plot_list[[i]] <- plot_temp
}

#saveRDS(do_boot, file = here("figs/ms/fig4_par/do_ribbons.rds"))
do_plots_0m <- grid.arrange(grobs = list(plot_list[[1]], plot_list[[3]], plot_list[[9]]), ncol = 3)
ggsave(here("figs/ms/fig4_par/do_0m.png"), do_plots_0m, height = 5, width = 9, units = c("in"))

do_plots_250m <- grid.arrange(grobs = list(plot_list[[10]], plot_list[[4]], plot_list[[2]]), ncol = 3)
ggsave(here("figs/ms/fig4_par/do_250m.png"), do_plots_250m, height = 5, width = 9, units = c("in"))

#agi model
brt1.prerun_agi<- plot.gbm.4list(agi_mod)
agi_boot <- gbm.bootstrap.functions(agi_mod, list.predictors=brt1.prerun_agi, n.reps=5)

plot_list <- list()
agi_names <- c("AGI, annual, 250m", "temp", "AGI, daily, 0m", "z", "AGI, seasonal, 0m", "sal", "AGI, seasonal, 250m", "SSH", "AGI, annual, 0m", "AGI, daily, 250m", "chl-a", "z_sd", "MLD")
for(i in 1:nrow(agi_mod$contributions)){
  plot_temp <- ggPD_boot(agi_mod, 
                         predictor = agi_mod$contributions[i, 1], 
                         list.4.preds = brt1.prerun_agi, 
                         booted.preds = agi_boot$function.preds, 
                         type.ci = "ribbon",
                         rug = T, 
                         type = "agi",
                         col.line = "#92351e", 
                         cex.line = 1.5,
                         alpha.ci = 0.75, 
                         x.label = agi_mod$contributions[i,1])
  
  plot_list[[i]] <- plot_temp
}

#saveRDS(agi_boot, file = here("figs/ms/fig4_par/agi_ribbons.rds"))
agi_plots_0m <- grid.arrange(grobs = list(plot_list[[1]], plot_list[[2]]), ncol = 2)
ggsave(here("figs/ms/fig4_par/agi_0m.png"), agi_plots_0m, height = 5, width = 9, units = c("in"))
  
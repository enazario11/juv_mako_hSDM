### load libraries ####
{library(tidyverse)
  library(doParallel)
  library(here)
  library(gbm)
  library(dismo)
  set.seed(1004)}

### load data ####
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

### cols for stratified sampling ####
soi_dat <- read.csv(here("data/enviro/soi/soi.long.csv"))
soi_dat$Date <- as.Date(soi_dat$Date, "%m/%d/%Y")
soi_dat$dt_ym <- format(as.Date(soi_dat$Date), "%Y-%m")

sample_cols <- function(dat_file){
  set.seed(1004)
  #spatial columns
  dat_file$region <- "NA"
  
  for(i in 1:nrow(dat_file)){
    if(dat_file$lat[i] < 15) {
      dat_file$region[i] <- "nec"
    } else if(dat_file$lat[i] > 30 & dat_file$lat[i] < 48 & dat_file$lon[i] > -134){
      dat_file$region[i] <- "ccs"
    } else {
      dat_file$region[i] <- "nep"
    }
  } #end region loop
  
  #enso columns
  dat_file$soi <- "NA"
  for(i in 1:nrow(dat_file)){
    yr_mo_dat <- format(as.Date(dat_file$dt[i]), "%Y-%m")
    
    soi_temp <- soi_dat %>%
      filter(dt_ym == as.character(yr_mo_dat))
    
    dat_file$soi[i] <- soi_temp$SOI
  }
  
  dat_file$soi <- as.numeric(dat_file$soi)
  dat_file$enso <- "NA"
  for(i in 1:nrow(dat_file)){
    if(dat_file$soi[i] >= 1.5) {
      dat_file$enso[i] <- "en"
    } else if(dat_file$soi[i] <= -1.5){
      dat_file$enso[i] <- "ln"
    } else {
      dat_file$enso[i] <- "neut"
    }
  } #end enso loop
 
  # add unique ID
  dat_file$st_id <- "NA"
  
  for(i in 1:nrow(dat_file)){
  dat_file$st_id[i] <- paste0(dat_file$region[i], "_", dat_file$enso[i])
  }
  
  return(dat_file) 
} #end function

#### run function ####
dat_base_st <- sample_cols(dat_base_d)
dat_do_st <- sample_cols(dat_do_all)
dat_agi_st <- sample_cols(dat_agi_all)

#filter out weak ENSOs
dat_base_st <- dat_base_st %>% filter(between(soi, -0.5, 0.5) | abs(soi) >= 1.5)
dat_do_st <- dat_do_st %>% filter(between(soi, -0.5, 0.5) | abs(soi) >= 1.5)
dat_agi_st <- dat_agi_st %>% filter(between(soi, -0.5, 0.5) | abs(soi) >= 1.5)

dat_base_st %>% group_by(region) %>% summarise(count = n())
dat_base_st %>% group_by(enso) %>% summarise(count = n())

### stratified sampling w/ 20 BRT iterations ####
brt_st <- function(dat_file, n_iter = 20, mod_type, save_folder, group){
  
  #predictor vars
  if(mod_type == "base"){
    pred_vars = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd")
  }
  if(mod_type == "do"){
    pred_vars = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "o2_mean_0m", "o2_mean_250m", "o2_mean_0m_seas", "o2_mean_0m_ann", "o2_mean_250m_seas", "o2_mean_250m_ann")
  }
  if(mod_type == "agi"){
    pred_vars = c("chl_mean", "temp_mean", "sal_mean", "ssh_mean", "mld_mean", "bathy_mean", "bathy_sd", "AGI_0m", "AGI_250m", "AGI_0m_seas", "AGI_0m_ann", "AGI_250m_seas", "AGI_250m_ann")
  }
  
  #brts
  foreach(i = 1:n_iter, .packages = c("here", "gbm", "dismo", "tidyverse")) %dopar% {
    
    set.seed(1004 + i) 

    #test vs train files
    if(group == "region"){
    dat_temp <- dat_file %>% 
      group_by(region) %>%
      sample_frac(0.25)
    }

    if(group == "enso"){
      dat_temp <- dat_file %>%
        group_by(enso) %>%
        sample_frac(0.25)
    }

    dat_test <-  subset(dat_file, row_id %in% dat_temp$row_id)
    
    #save test file
    saveRDS(dat_test, file = here(paste0(save_folder, group, "/", mod_type, "/", "test/", mod_type,"_", "test", i, ".rds")))
    
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
    
    #save model
    saveRDS(brt_iter, file = here(paste0(save_folder, group, "/", mod_type, "/", mod_type,"_", i, ".rds")))  

    
  } #end parallel
} #end function

#### run brts ####
n_cores <- detectCores()
cluster <- makeCluster(n_cores-2)
registerDoParallel(cluster)

#base
dat_base_st$row_id <- 1:nrow(dat_base_st)
brt_st(dat_file = dat_base_st, mod_type = "base", group = "region", save_folder = "data/brt/mod_outputs/revised/brts_st/")
brt_st(dat_file = dat_base_st, mod_type = "base", group = "enso", save_folder = "data/brt/mod_outputs/revised/brts_st/")

#do
dat_do_st$row_id <- 1:nrow(dat_do_st)
brt_st(dat_file = dat_do_st, mod_type = "do", group = "region", save_folder = "data/brt/mod_outputs/revised/brts_st/")
brt_st(dat_file = dat_do_st, mod_type = "do", group = "enso", save_folder = "data/brt/mod_outputs/revised/brts_st/")

#agi
dat_agi_st$row_id <- 1:nrow(dat_agi_st)
brt_st(dat_file = dat_agi_st, mod_type = "agi", group = "region", save_folder = "data/brt/mod_outputs/revised/brts_st/")
brt_st(dat_file = dat_agi_st, mod_type = "agi", group = "enso", save_folder = "data/brt/mod_outputs/revised/brts_st/")

stopCluster(cl = cluster)

### performance metrics ####
metric_by_model <- function(mod_file, test_file, iter, group){
  
  #calc performance metrics for each region
  if(group == "region"){
  for(i in 1:length(unique(test_file$region))){
    if(i == 1){
    temp_df <- data.frame(matrix(ncol = 5, nrow = 9))
    cols_names <- c("iteration", "AUC", "TSS", "dev_exp", "st_id")
    colnames(temp_df) <- cols_names  
    }
    
    region_temp <- unique(test_file$region)[i]
    test_temp <- test_file %>% filter(region == region_temp)
    
    preds <- predict.gbm(mod_file, test_temp,
                         n.trees = mod_file$gbm.call$best.trees,
                         type = "response")
    observed <- test_temp$PA
    
    ext.residual.deviance <- calc.deviance(obs = test_temp$PA, pred=preds, family="bernoulli", calc.mean=TRUE) #get % deviance
    null.dev =  calc.deviance(test_temp$PA ,rep(mean(test_temp$PA),length(test_temp$PA)), family="bernoulli", calc.mean=T)
    dev=(null.dev - ext.residual.deviance)/null.dev 
    
    dat_pred <- cbind(test_temp$PA, preds)
    pres <- dat_pred[dat_pred[,1] == 1, 2]
    abs <- dat_pred[dat_pred[,1] == 0, 2]
    
    #evaluate (AUC, TSS, TPR)
    e = evaluate(p = pres, a = abs)
    
    #store metrics in performance metric df
    temp_df$iteration[i] <- iter
    temp_df$AUC[i] <- e@auc
    temp_df$TSS[i] <- max((e@TPR + e@TNR - 1))
    temp_df$dev_exp[i] <- dev
    temp_df$st_id[i] <- region_temp
  } #end metric for loop
}
  if(group == "enso"){
 for(i in 1:length(unique(test_file$enso))){
    if(i == 1){
    temp_df <- data.frame(matrix(ncol = 5, nrow = 9))
    cols_names <- c("iteration", "AUC", "TSS", "dev_exp", "st_id")
    colnames(temp_df) <- cols_names  
    }
    
    enso_temp <- unique(test_file$enso)[i]
    test_temp <- test_file %>% filter(enso == enso_temp)
    
    preds <- predict.gbm(mod_file, test_temp,
                         n.trees = mod_file$gbm.call$best.trees,
                         type = "response")
    observed <- test_temp$PA
    
    ext.residual.deviance <- calc.deviance(obs = test_temp$PA, pred=preds, family="bernoulli", calc.mean=TRUE) #get % deviance
    null.dev =  calc.deviance(test_temp$PA ,rep(mean(test_temp$PA),length(test_temp$PA)), family="bernoulli", calc.mean=T)
    dev=(null.dev - ext.residual.deviance)/null.dev 
    
    dat_pred <- cbind(test_temp$PA, preds)
    pres <- dat_pred[dat_pred[,1] == 1, 2]
    abs <- dat_pred[dat_pred[,1] == 0, 2]
    
    #evaluate (AUC, TSS, TPR)
    e = evaluate(p = pres, a = abs)
    
    #store metrics in performance metric df
    temp_df$iteration[i] <- iter
    temp_df$AUC[i] <- e@auc
    temp_df$TSS[i] <- max((e@TPR + e@TNR - 1))
    temp_df$dev_exp[i] <- dev
    temp_df$st_id[i] <- enso_temp
  } #end metric for loop
  }
return(temp_df)
}

brt_perf_metric <- function(mod_files, test_files, mod_type, domain = "all", group){
  
  #read in model and test file locations
  mod_files = list.files(mod_files, full.names = TRUE, pattern = ".rds")
  test_files = list.files(test_files, full.names = TRUE, pattern = ".rds")
  perform_df <- NULL
  
  for(i in 1:length(mod_files)){
    
    #load model and test file
    mod_file <- readRDS(mod_files[i])
    test_file <- readRDS(test_files[i])
    
    iter = i
    mod_iter_df <- metric_by_model(mod_file = mod_file, test_file = test_file, iter = iter, group = group)
    perform_df <- rbind(perform_df, mod_iter_df)
    
  } #end loop per brt iteration
  
  saveRDS(perform_df, here(paste0("data/brt/mod_outputs/revised/brts_st/", group, "/", mod_type, "_metrics.rds")))
  return(perform_df)
  
} #end function

#### get metrics ####
#base
base_metrics_st <- brt_perf_metric(mod_files = "data/brt/mod_outputs/revised/brts_st/region/base/", test_files = "data/brt/mod_outputs/revised/brts_st/region/base/test/", mod_type = "base", group = "region")
base_metrics_st <- brt_perf_metric(mod_files = "data/brt/mod_outputs/revised/brts_st/enso/base/", test_files = "data/brt/mod_outputs/revised/brts_st/enso/base/test/", mod_type = "base", group = "enso")

#do
do_metrics_st <- brt_perf_metric(mod_files = "data/brt/mod_outputs/revised/brts_st/region/do/", test_files = "data/brt/mod_outputs/revised/brts_st/region/do/test/", mod_type = "do", group = "region")
do_metrics_st <- brt_perf_metric(mod_files = "data/brt/mod_outputs/revised/brts_st/enso/do/", test_files = "data/brt/mod_outputs/revised/brts_st/enso/do/test/", mod_type = "do", group = "enso")

#agi
agi_metrics_st <- brt_perf_metric(mod_files = "data/brt/mod_outputs/revised/brts_st/region/agi/", test_files = "data/brt/mod_outputs/revised/brts_st/region/agi/test/", mod_type = "agi", group = "region")
agi_metrics_st <- brt_perf_metric(mod_files = "data/brt/mod_outputs/revised/brts_st/enso/agi/", test_files = "data/brt/mod_outputs/revised/brts_st/enso/agi/test/", mod_type = "agi", group = "enso")

#get enso mod output data
base_dat_e <- readRDS(here("data/brt/mod_outputs/revised/brts_st/enso/base_metrics.rds")) %>% drop_na() %>% mutate(mod_type = "Base model")
do_dat_e <- readRDS(here("data/brt/mod_outputs/revised/brts_st/enso/do_metrics.rds")) %>% drop_na() %>% mutate(mod_type = "DO model")
agi_dat_e <- readRDS(here("data/brt/mod_outputs/revised/brts_st/enso/agi_metrics.rds")) %>% drop_na() %>% mutate(mod_type = "AGI model")

all_enso <- rbind(base_dat_e, do_dat_e, agi_dat_e) %>%
  mutate(ENSO = NA, 
         dev_exp = dev_exp*100)

for(i in 1:nrow(all_enso)){
  if(grepl('en',all_enso$st_id[i])){
    all_enso$ENSO[i] = "El Niño"
  } 
  if(grepl('ln',all_enso$st_id[i])){
    all_enso$ENSO[i] = "La Niña"
  } 
  if(grepl('neut', all_enso$st_id[i])){
    all_enso$ENSO[i] = "Neutral"
  }
}

sum_enso <- all_enso %>%
  group_by(ENSO, mod_type) %>%
  summarise(mean_tss = mean(TSS), 
            sd_tss = sd(TSS), 
            mean_auc = mean(AUC), 
            sd_auc = sd(AUC), 
            mean_dev = mean(dev_exp), 
            sd_dev = sd(dev_exp))

#get region mod output data
base_dat_r <- readRDS(here("data/brt/mod_outputs/revised/brts_st/region/base_metrics.rds")) %>% drop_na() %>% mutate(mod_type = "Base model")
do_dat_r <- readRDS(here("data/brt/mod_outputs/revised/brts_st/region/do_metrics.rds")) %>% drop_na() %>% mutate(mod_type = "DO model")
agi_dat_r <- readRDS(here("data/brt/mod_outputs/revised/brts_st/region/agi_metrics.rds")) %>% drop_na() %>% mutate(mod_type = "AGI model")

all_reg <- rbind(base_dat_r, do_dat_r, agi_dat_r) %>%
  mutate(region = NA, 
         dev_exp = dev_exp*100)

for(i in 1:nrow(all_reg)){
  if(grepl('ccs',all_reg$st_id[i])){
    all_reg$region[i] = "CCS"
  } 
  if(grepl('nec',all_reg$st_id[i])){
    all_reg$region[i] = "NEC"
  } 
  if(grepl('nep', all_reg$st_id[i])){
    all_reg$region[i] = "NEP"
  }
}

sum_reg <- all_reg %>%
  group_by(region, mod_type) %>%
  summarise(mean_tss = mean(TSS), 
            sd_tss = sd(TSS), 
            mean_auc = mean(AUC), 
            sd_auc = sd(AUC), 
            mean_dev = mean(dev_exp), 
            sd_dev = sd(dev_exp))

#enso boxplots
TSS_neut_e <- sum_enso %>% mutate(mod_type = as.factor(mod_type), 
                              mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                              ENSO = as.factor(ENSO), 
                              ENSO = fct_relevel(ENSO, c("Neutral", "El Niño", "La Niña"))) %>%
  filter(ENSO == "Neutral") %>%
  ggplot(aes(x = ENSO, y=mean_tss)) +
  geom_point(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), shape = 22, size = 5) +
  geom_linerange(aes(ymin = mean_tss-sd_tss, ymax = mean_tss+sd_tss, color = mod_type), position = position_dodge(width = 1), size = 1) +
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("TSS") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14), 
        rect = element_rect(fill = "transparent") ) 


TSS_stress_e <- sum_enso %>% mutate(mod_type = as.factor(mod_type), 
                                  mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                                  ENSO = as.factor(ENSO), 
                                  ENSO = fct_relevel(ENSO, c("Neutral", "El Niño", "La Niña"))) %>%
  filter(ENSO != "Neutral") %>%
  ggplot(aes(x = ENSO, y=mean_tss)) +
  geom_point(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), shape = 22, size = 5) +
  geom_linerange(aes(ymin = mean_tss-sd_tss, ymax = mean_tss+sd_tss, color = mod_type), position = position_dodge(width = 1), size = 1) +
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("TSS") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14)) 


dev_neut_e <- sum_enso %>% mutate(mod_type = as.factor(mod_type), 
                                mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                                ENSO = as.factor(ENSO), 
                                ENSO = fct_relevel(ENSO, c("Neutral", "El Niño", "La Niña"))) %>%
  filter(ENSO == "Neutral") %>%
  ggplot(aes(x = ENSO, y=mean_dev)) +
  geom_point(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), shape = 22, size = 5) +
  geom_linerange(aes(ymin = mean_dev-sd_dev, ymax = mean_dev+sd_dev, color = mod_type), position = position_dodge(width = 1), size = 1) +  
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Deviance explained (%)") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14)) 


dev_stress_e <- sum_enso %>% mutate(mod_type = as.factor(mod_type), 
                                mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                                ENSO = as.factor(ENSO), 
                                ENSO = fct_relevel(ENSO, c("Neutral", "El Niño", "La Niña"))) %>%
  filter(ENSO != "Neutral") %>%
  ggplot(aes(x = ENSO, y=mean_dev)) +
  geom_point(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), shape = 22, size = 5) +
  geom_linerange(aes(ymin = mean_dev-sd_dev, ymax = mean_dev+sd_dev, color = mod_type), position = position_dodge(width = 1), size = 1) +  
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Deviance explained (%)") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14)) 

auc_stress_e <- sum_enso %>% mutate(mod_type = as.factor(mod_type), 
                                mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                                ENSO = as.factor(ENSO), 
                                ENSO = fct_relevel(ENSO, c("Neutral", "El Niño", "La Niña"))) %>%
  filter(ENSO != "Neutral") %>%
  ggplot(aes(x = ENSO, y=mean_auc)) +
  geom_point(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), shape = 22, size = 5) +
  geom_linerange(aes(ymin = mean_auc-sd_auc, ymax = mean_auc+sd_auc, color = mod_type), position = position_dodge(width = 1), size = 1) +  
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("AUC") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14))

auc_neut_e <- sum_enso %>% mutate(mod_type = as.factor(mod_type), 
                                mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                                ENSO = as.factor(ENSO), 
                                ENSO = fct_relevel(ENSO, c("Neutral", "El Niño", "La Niña"))) %>%
  filter(ENSO == "Neutral") %>%
  ggplot(aes(x = ENSO, y=mean_auc)) +
  geom_point(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), shape = 22, size = 5) +
  geom_linerange(aes(ymin = mean_auc-sd_auc, ymax = mean_auc+sd_auc, color = mod_type), position = position_dodge(width = 1), size = 1) +  
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("AUC") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14))

tss_enso <- ggarrange(TSS_neut_e, TSS_stress_e, common.legend = TRUE)
ggsave(here("figs/ms/supp_figs/st_perform/tss_enso.png"), tss_enso, width = 10, height = 5, units = c("in"))

dev_enso <- ggarrange(dev_neut_e, dev_stress_e, common.legend = TRUE)
ggsave(here("figs/ms/supp_figs/st_perform/dev_enso.png"), dev_enso, width = 10, height = 5, units = c("in"))

auc_enso <- ggarrange(auc_neut_e, auc_stress_e, common.legend = TRUE)
ggsave(here("figs/ms/supp_figs/st_perform/auc_enso.png"), auc_enso, width = 10, height = 5, units = c("in"))

#region boxplots
TSS_neut_r <- sum_reg %>% mutate(mod_type = as.factor(mod_type), 
                              mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                              region = as.factor(region), 
                              region = fct_relevel(region, c("NEP", "CCS", "NEC"))) %>%
  filter(region != "NEC") %>%
  ggplot(aes(x = region, y=mean_tss)) +
  geom_point(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), shape = 22, size = 5) +
  geom_linerange(aes(ymin = mean_tss-sd_tss, ymax = mean_tss+sd_tss, color = mod_type), position = position_dodge(width = 1), size = 1) +
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("TSS") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14),
        rect = element_rect(fill = "transparent"))

TSS_stress_r <- sum_reg %>% mutate(mod_type = as.factor(mod_type), 
                                 mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                                 region = as.factor(region), 
                                 region = fct_relevel(region, c("NEP", "CCS", "NEC"))) %>%
  filter(region == "NEC") %>%
  ggplot(aes(x = region, y=mean_tss)) +
  geom_point(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), shape = 22, size = 5) +
  geom_linerange(aes(ymin = mean_tss-sd_tss, ymax = mean_tss+sd_tss, color = mod_type), position = position_dodge(width = 1), size = 1) +  
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("TSS") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14),
        rect = element_rect(fill = "transparent") ) 

dev_neut_r <- sum_reg %>% mutate(mod_type = as.factor(mod_type), 
                               mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                               region = as.factor(region), 
                               region = fct_relevel(region, c("NEP", "CCS", "NEC"))) %>%
  filter(region != "NEC") %>%
  ggplot(aes(x = region, y=mean_dev)) +
  geom_point(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), shape = 22, size = 5) +
  geom_linerange(aes(ymin = mean_dev-sd_dev, ymax = mean_dev+sd_dev, color = mod_type), position = position_dodge(width = 1), size = 1) +
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Deviance explained (%)") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14)) 


dev_stress_r <- sum_reg %>% mutate(mod_type = as.factor(mod_type), 
                               mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                               region = as.factor(region), 
                               region = fct_relevel(region, c("NEP", "CCS", "NEC"))) %>%
  filter(region == "NEC") %>%
  ggplot(aes(x = region, y=mean_dev)) +
  geom_point(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), shape = 22, size = 5) +
  geom_linerange(aes(ymin = mean_dev-sd_dev, ymax = mean_dev+sd_dev, color = mod_type), position = position_dodge(width = 1), size = 1) +
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Deviance explained (%)") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14)) 

auc_neut_r <- sum_reg %>% mutate(mod_type = as.factor(mod_type), 
                               mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                               region = as.factor(region), 
                               region = fct_relevel(region, c("NEP", "CCS", "NEC"))) %>%
  filter(region != "NEC") %>%
  ggplot(aes(x = region, y=mean_auc)) +
  geom_point(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), shape = 22, size = 5) +
  geom_linerange(aes(ymin = mean_auc-sd_auc, ymax = mean_auc+sd_auc, color = mod_type), position = position_dodge(width = 1), size = 1) +
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("AUC") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14)) 


auc_stress_r <- sum_reg %>% mutate(mod_type = as.factor(mod_type), 
                               mod_type = fct_relevel(mod_type, c("Base model", "DO model", "AGI model")), 
                               region = as.factor(region), 
                               region = fct_relevel(region, c("NEP", "CCS", "NEC"))) %>%
  filter(region == "NEC") %>%
  ggplot(aes(x = region, y=mean_auc)) +
  geom_point(aes(fill = mod_type, color = mod_type), position = position_dodge(width = 1), shape = 22, size = 5) +
  geom_linerange(aes(ymin = mean_auc-sd_auc, ymax = mean_auc+sd_auc, color = mod_type), position = position_dodge(width = 1), size = 1) +
  theme_tq() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("AUC") + 
  scale_fill_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  scale_color_manual(values = c("#224B5E", "#527875", "#83A58C"))+
  labs(fill = "")+
  guides(color = "none")+
  #coord_cartesian(ylim = c(0.4, 0.65))+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16, color = "black"), 
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top", 
        legend.justification = "left", 
        strip.text.x = element_text(size = 14)) 


tss_reg <- ggarrange(TSS_neut_r, TSS_stress_r, common.legend = TRUE)
ggsave(here("figs/ms/supp_figs/st_perform/tss_reg.png"), tss_reg, width = 10, height = 5, units = c("in"))

dev_reg <- ggarrange(dev_neut_r, dev_stress_r, common.legend = TRUE)
ggsave(here("figs/ms/supp_figs/st_perform/dev_reg.png"), dev_reg, width = 10, height = 5, units = c("in"))

auc_reg <- ggarrange(auc_neut_r, auc_stress_r, common.legend = TRUE)
ggsave(here("figs/ms/supp_figs/st_perform/auc_reg.png"), auc_reg, width = 10, height = 5, units = c("in"))





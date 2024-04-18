## In this document...
##location summary statistics for ms##

#ssm crw output with unnested locations
ssm_dat <- readRDS(here("data/presence_locs/psat_spot_domain/processed/psat_spot_animotum.RDS"))

#observed location DF
obsv_dat <- readRDS(here("data/presence_locs/psat_spot_domain/psat_spot_data.rds"))

  #overall mean deployment duration (days)
obsv_dat %>% 
  group_by(ptt) %>% 
  arrange(ptt, date) %>%
  filter(!is.na(date)) %>%
  mutate(start_date = head(date, 1), 
         end_date = tail(date, 1), 
         deploy_dur = end_date-start_date) %>%
  summarise(mean_deploy = as.numeric(mean(deploy_dur, na.rm = TRUE), units = "days")) %>% 
  ungroup() %>%
  summarise(grand_mean = mean(mean_deploy, na.rm = TRUE), 
            min_deploy = min(mean_deploy, na.rm = TRUE), 
            max_deploy = max(mean_deploy, na.rm = TRUE))

  #overall mean number of positions
obsv_dat %>% 
  group_by(ptt) %>%
  summarise(num_pos = n()) %>%
  ungroup() %>% 
  summarise(mean_pos_n = mean(num_pos), 
            min_pos = min(num_pos, na.rm = TRUE), 
            max_pos = max(num_pos, na.rm = TRUE))
  
  #summary stats across all deployments
obsv_dat %>% 
  summarise(n_sharks = length(unique(deploy_id)), #74
            n_tracks = length(unique(ptt)),
            n_pos = n(), #17451
            n_days = length(unique(date)), #12983
            min_date = min(date, na.rm = TRUE), #2003-06-25
            max_date = max(date, na.rm = TRUE), #2013-12-16
            min_lat = min(lat, na.rm = TRUE), #2.77
            max_lat = max(lat, na.rm = TRUE), #47.375
            min_lon = min(lon, na.rm = TRUE), #-150.8
            max_lon = max(lon, na.rm = TRUE)) #-105.69


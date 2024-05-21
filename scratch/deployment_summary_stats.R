## In this document...
##location summary statistics for ms##

#ssm crw output with unnested locations
ssm_dat_sum <- readRDS(here("data/presence_locs/psat_spot_domain/processed/psat_spot_animotum.RDS"))

#observed location DF
obsv_dat <- readRDS(here("data/presence_locs/psat_spot_domain/psat_spot_data.rds"))

  #overall mean raw deployment duration (days)
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

  #overall mean number of raw positions
obsv_dat %>% 
  group_by(ptt) %>%
  summarise(num_pos = n()) %>%
  ungroup() %>% 
  summarise(mean_pos_n = mean(num_pos), 
            min_pos = min(num_pos, na.rm = TRUE), 
            max_pos = max(num_pos, na.rm = TRUE))

#overall mean ssm deployment duration (days)
ssm_dat_sum %>% 
  group_by(id) %>% 
  arrange(id, date) %>%
  filter(!is.na(date)) %>%
  mutate(start_date = head(date, 1), 
         end_date = tail(date, 1), 
         deploy_dur = end_date-start_date) %>%
  summarise(mean_deploy = as.numeric(mean(deploy_dur, na.rm = TRUE), units = "days")) %>% 
  ungroup() %>%
  summarise(grand_mean = mean(mean_deploy, na.rm = TRUE), 
            min_deploy = min(mean_deploy, na.rm = TRUE), 
            max_deploy = max(mean_deploy, na.rm = TRUE))

#overall mean number of ssm positions
ssm_dat_sum %>% 
  group_by(id) %>%
  summarise(num_pos = n()) %>%
  ungroup() %>% 
  summarise(mean_pos_n = mean(num_pos), 
            min_pos = min(num_pos, na.rm = TRUE), 
            max_pos = max(num_pos, na.rm = TRUE))

    #summary stats across all deployments (ssm data)
ssm_dat_sum %>% 
  mutate(date_chr = as.POSIXct(substr(date, 1, 10))) %>%
  summarise(n_tracks = length(unique(id)), #73
            n_pos = n(), #15542
            n_days = length(unique(date_chr)), #3774
            min_date = min(date_chr, na.rm = TRUE), #2003-06-25
            max_date = max(date_chr, na.rm = TRUE), #2015-03-23
            min_lat = min(lat_p, na.rm = TRUE), #2.99
            max_lat = max(lat_p, na.rm = TRUE), #47.37
            min_lon = min(lon_p, na.rm = TRUE), #-151.08
            max_lon = max(lon_p, na.rm = TRUE)) #-105.41

# PA summary stats 
pa_dat_sum <- readRDS(here("data/presence_locs/psat_spot_domain/processed/psat_spot_PAs.rds"))

pa_dat_sum %>% 
  mutate(date_chr = as.POSIXct(substr(date, 1, 10))) %>%
  summarise(n_tracks = length(unique(id)), #73
            n_days = length(unique(date_chr)), #3774
            min_date = min(date_chr, na.rm = TRUE), #2003-06-25
            max_date = max(date_chr, na.rm = TRUE), #2015-03-23
            min_lat = min(lat, na.rm = TRUE), #3.14
            max_lat = max(lat, na.rm = TRUE), #47.4
            min_lon = min(lon, na.rm = TRUE), #-151
            max_lon = max(lon, na.rm = TRUE)) #-105


#buffered min and max for CMEMS data download
min(ssm_locs$lon_p) -2 #-153.41
max(ssm_locs$lon_p) +2 #-103.41
min(ssm_locs$lat_p) -2 #0.98
max(ssm_locs$lat_p) +2 #49.37

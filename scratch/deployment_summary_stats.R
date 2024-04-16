## In this document...
##location summary statistics for ms##

#ssm crw output with unnested locations
dat2 <- readRDS(here("data/presence_locs/psat_spot_domain/processed/psat_spot_animotum.RDS"))

  #overall mean deployment duration
dat2 %>% 
  group_by(id) %>% 
  mutate(start_date = head(date, 1), 
         end_date = tail(date, 1), 
         deploy_dur = end_date-start_date) %>% 
  summarise(mean_deploy = mean(deploy_dur, na.rm = TRUE)) %>% 
  ungroup() %>% 
  summarise(grand_mean = mean(mean_deploy, na.rm = TRUE))

#locations with sex/size/etc. metadata
all_dat <- readRDS(here("data/presence_locs/psat_spot_domain/psat_spot_data.rds")) #75 unique deployments

#mean time step across tracks
time_step <- all_dat %>%
  group_by(ptt) %>%
  arrange(ptt, date) %>%
  mutate(diff = as.numeric(date - lag(date), units = "days")) 
  summarise(mean_diff = mean(diff, na.rm = T)) %>%
  ungroup() %>%
  summarise(all_mean = mean(mean_diff)/3600)

max_step <- all_dat %>%
     group_by(ptt) %>%
     arrange(ptt, date) %>%
     mutate(diff = date - lag(date)) %>%
     summarise(max_diff = max(diff, na.rm = T), 
               max_diff = as.numeric(max_diff, units = "days"))

all_dat <- all_dat %>% 
  filter(lc != "D") %>% #remove D otherwise get error with ssm
  filter(ptt != "52124" & ptt != "54607" & ptt != "60984" & ptt != "60986") %>% #filter out ptts with < 30 positions
  
  #number of locations by id
all_dat %>%
  group_by(ptt) %>%
  summarise(n = length(unique(date, na.rm = T))) %>%
  print(n = 85)



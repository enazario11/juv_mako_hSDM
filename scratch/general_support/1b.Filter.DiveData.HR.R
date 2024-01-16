##############################################################
#####   IMPORT and CLEAN HP HR data from 2013-2015   #########
#####                n=5 MK10 tags
##############################################################

library(dplyr)
library(janitor)
library(ggplot2)
library(readr)
library(tidyquant)
library(diveMove)
library(suncalc)
library(gridExtra)
library(magrittr)
library(data.table)


## PTT correspondence:
#----------------------
# SN 13A0471, PTT 27262
# SN 14A0375, PTT 22849
# SN 14A0376, PTT 22850
# SN 14A0380, PTT 27262
# SN 14A0384, PTT 93100




##################################
# import data for the 5 porpoises
##################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP/DATA/HP_Dive_data_2013-2016/")
files = list.files(pattern=".csv")
files # 5 files

hp = NULL
system.time({ # 77 sec
  for (i in 1:length(files)) {
    id = read_csv(files[[i]]) %>%
      as_tibble() %>%
      filter(!row_number() %in% c(1:3)) %>%  # remove 3 first rows containing headings
      mutate(id = substr(files[i], 1,7))     # add column id
    colnames(id)[1] = "dateTime"
    colnames(id)[2] = "depth"
    
    id = id %>%
      mutate(depth = as.numeric(depth),
             date  = as.numeric(dateTime),
             date  = excel_numeric_to_date(date))
    
    # convert date and times
    id$posix = convert_to_datetime(
      id$dateTime,
      character_fun=lubridate::ymd_hms, truncated=1, tz="UTC")
    
    hp = rbind(hp, id)
  }
})

hp
unique(hp$id)

# convert dates into POSIX
system.time({  # 260 sec: 4 min!
  hp$posix = as.POSIXct(strptime(hp$posix,
                                 format="%Y-%m-%d %H:%M:%S"), tz="America/Nuuk") })

# summarise data per ID
#------------------------
hp %>%
  group_by(id) %>%
  summarise(start    = first(posix),
            end      = last(posix),
            duration = difftime(last(posix), first(posix), units="days"),
            nobs     = n(),
            ndepths  = sum(is.na(.)),
            meanDep  = mean(depth, na.rm=T),
            maxDep   = max(depth, na.rm=T))
# id      start               end                 duration       meanDep maxDep
# 13A0471 2013-10-02 15:51:00 2013-10-18 12:39:25  15.86696 days    38.2   336.
# 14A0375 2014-07-22 15:15:00 2014-12-15 11:29:37 145.84348 days    27.6 32728.
# 14A0376 2014-07-17 19:11:00 2014-12-03 14:07:21 138.78913 days    30.0 32728.
# 14A0380 2014-07-22 15:24:00 2014-12-07 15:21:21 137.99816 days    25.0 32727 
# 14A0384 2014-07-27 09:15:41 2015-01-08 15:49:49 165.27370 days    29.2   348 

# plot histo of depth per ID
#-----------------------------
ggplot(data = hp, aes(x=-depth)) +
  geom_histogram() +
  # geom_density(aes(y=..scaled..,alpha=.4)) +
  xlim(-400,0) +
  coord_flip() +
  labs(y = "dive frequency", x = "Depth (m)") +
  # scale_y_continuous(scales::percent) +
  facet_wrap(~id, scales="free")



# rename ID column to match PTT from location data
#--------------------------------------------------
# SN 13A0471, PTT 27262, nloc: 251  (2013)
# SN 14A0375, PTT 22849b,nloc: 1714 (2014)
# SN 14A0376, PTT 22850b,nloc: 1732 (2014)
# SN 14A0380, PTT 27262b,nloc: 1576 (2014)
# SN 14A0384, PTT 93100, nloc: 790  (2014)
hp = hp %>%
  mutate(id = case_when(id == "13A0471" ~ "27262",
                         id == "14A0375" ~ "22849b",
                         id == "14A0376" ~ "22850b",
                         id == "14A0380" ~ "27262b",
                         id == "14A0384" ~ "93100"))
unique(hp$id)
hp = hp %>% rename(posix_local = posix) %>% select(-c(dateTime))
str(hp)








##########################################################
# Rolling join to extract coordinates from loc dataset
##########################################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP")

# import locations for the 5 HP
#----------------------------------
loc <- readRDS("./RDATA/1.locations_filtered_5HP.rds") %>% 
  dplyr::select(id, posix_local, bathy, lon, lat)  %>%
  rename(date = posix_local)
loc

hp = hp %>% rename(date2 = date, date = posix_local)

# convert into data tables
#-------------------------
library(data.table)
diveDT = setDT(hp)
setkey(diveDT,id,date)
locDT  = setDT(loc)
setkey(locDT,id,date)

# rolling join on date
dive2 = locDT[,.(date, id, lon, lat)] %>%
  .[diveDT, roll="nearest"] %>%
  .[] # to display tibble
summary(dive2$lon)
summary(dive2$lat)
names(dive2)


# check correspondence
table(loc$id)
dive2 %>% filter(id == "27262") 
id = loc %>% filter(id == "27262") %>% View()
dive2 -> hp
rm(dive2)
hp = as_tibble(hp)
names(hp)
hp = hp %>% rename(posix_local = date, date = date2) 





########################################
# distinguish day, night, dusk, dawn
########################################
sun  = getSunlightTimes(data = hp, tz="America/Nuuk",  
                        keep=c("sunrise","sunset","dusk",
                               "dawn","night","nightEnd")) 
head(sun)
sun %>%
  mutate(year = substr(date, 1, 4)) %>%
  filter(year == "2014") %>%
  summarise(earliest_sunrise = min(sunrise),
            latest_sunrise   = max(sunrise),
            earliest_sunset  = max(sunset),
            latest_sunset    = min(sunset))

hp2 = cbind(hp, sun[,c("sunrise","sunset","dusk",
                       "dawn","night","nightEnd")]) %>%
  as_tibble()
names(hp2)


# # set same time zone for all dates
# #----------------------------------
# attr(first(hp2$sunset),"tzone")
# attr(first(hp2$sunrise),"tzone")     # America/Nuuk
# attr(first(hp2$posix_local),"tzone") # GMT






# identify dusk, dawn, day, night
#------------------------------------
hp2 = hp2 %>%
  mutate(period = case_when(posix_local %within% interval(sunset, night) ~ 'dusk',
                            posix_local %within% interval(nightEnd, sunrise) ~ 'dawn',
                            posix_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night"))
table(hp2$period)
unique(hp2$period)
# unique(hp2$hour[hp2$period == "night"])
hp2 -> hp
rm(hp2)

# # round sunrise and sunsets when associated with different coordinates for same day
# #-----------------------------------------------------------------------------------
# library(lubridate)
# hp$sunrise = round_date(hp$sunrise, unit="min")
# hp$sunset  = round_date(hp$sunset, unit="min")

hp %>%
  group_by(period) %>%
  summarise(meandep   = mean(depth, na.rm=T),
            mediandep = median(depth, na.rm=T),
            maxdep    = max(depth, na.rm=T)) %>%
  ungroup()
#   period meandep mediandep maxdep
# 1 dawn      39.4      19.5 32727 
# 2 day       20.4       1.5 32728.
# 3 dusk      35.7       9.5 32728.
# 4 night     34.2      12.5 32728.
hp = hp %>%  mutate(hour = substr(posix_local, 12, 13))


# remove depth outliers
#--------------------------
summary(hp$depth)
hp = hp %>% filter(depth < 600)

# plot dive depth according to time of the day
#----------------------------------------------
# Summarize data per id: extract daily max depth
maxdep_daily = hp %>%
  group_by(id, date, hour) %>%
  summarise(period    = first(period),
            maxdep    = max(depth, na.rm=T),
            mindep    = min(depth, na.rm=T),
            meandep   = mean(depth, na.rm=T),
            mediandep = median(depth, na.rm=T))

ggplot(maxdep_daily, aes(x=as.factor(hour), 
                         y=-mediandep, colour=id)) +
  geom_boxplot() +
  facet_wrap(.~ id) +
  theme_tq() +
  theme(legend.position = "none") 



# histogram of depth per hour of the day
#-----------------------------------------
maxdep_hourly = hp %>%
  group_by(id, date, hour) %>%
  summarise(period    = first(period),
            maxdep    = max(depth, na.rm=T),
            mindep    = min(depth, na.rm=T),
            meandep   = mean(depth, na.rm=T),
            mediandep = median(depth, na.rm=T))
maxdep_hourly$hour = as.factor(maxdep_hourly$hour)

ggplot(maxdep_hourly, aes(x=hour, y=maxdep, colour = period)) +
  geom_point() +
  scale_colour_brewer(palette = "Blues") +
  scale_y_continuous(trans = "reverse") +
  labs(x="Hours of the day", y="Daily max depth (m)",
       title ="Diel pattern from max depth (daily)", colour="") +
  theme_tq() +
  theme(legend.position = "bottom")
ggsave(paste0("./FIGURES/Dive_diel_pattern/GeomPoint_DailyMaxDep_Hours.png"),
       width=5,height=4,units="in",dpi=400)

ggplot(maxdep_hourly, aes(x=hour, y=maxdep)) +
  geom_boxplot() +
  scale_y_continuous(trans = "reverse") +
  labs(x="Hours of the day", y="Daily max depth (m)",
       title ="Diel pattern from max depth (daily)", colour="") +
  theme_tq() +
  facet_wrap(.~ period) +
  theme(legend.position = "bottom")
ggsave(paste0("./FIGURES/Dive_diel_pattern/Boxplot_DailyMaxDep_Hours.png"),
       width=5,height=4,units="in",dpi=400)




# density plot of max depth (day vs night)
#---------------------------------------------
ggplot(maxdep_hourly, aes(x=maxdep, fill = period, colour = period)) +
  geom_density(aes(y=..scaled..,alpha=.4)) +
  coord_flip() +
  scale_x_continuous(trans = "reverse") +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  facet_grid(id~period) +
  labs(y="Dive density", x="Daily max depth (m)", 
       title ="Diel pattern from max depth (daily)", fill="") +
  theme_tq() +
  theme(legend.position = "none")
ggsave(paste0("./FIGURES/Dive_diel_pattern/Histo_DailyMaxDep_period.png"),
       width=6,height=5,units="in",dpi=400)

ggplot(maxdep_hourly, aes(y = maxdep, x = as.numeric(hour), colour = id)) +
  geom_smooth(method="gam", lwd = 0.5) +
  scale_y_continuous(trans = "reverse") +
  ylim(365, 0) +
  facet_grid(.~id) +
  labs(x="Hour", y="Daily max depth (m)", 
       title ="Diel pattern from max depth (daily)", fill="") +
  theme_tq()
ggsave(paste0("./FIGURES/Dive_diel_pattern/geomSmooth_DailyMaxDep_hour.png"),
       width=7,height=3,units="in",dpi=400)

ggplot(maxdep_hourly, aes(y = maxdep, x = hour, colour = id)) +
  geom_boxplot() +
  scale_y_continuous(trans = "reverse") +
  ylim(365, 0) +
  facet_wrap(.~id) +
  labs(x="Hour", y="Daily max depth (m)", 
       title ="Diel pattern from max depth (daily)", fill="") +
  theme_tq()









# ############################
# # check data gaps
# ############################
# hp = hp %>%
#   group_by(ptt) %>%
#   # select(-c(dateTime, id, posix)) %>%
#   mutate(difftime = c(NA,difftime(posix_local[2:n()], 
#                                   posix_local[1:n()-1], units="sec")))
# hp
# summary(hp$difftime) # 1 to 375 sec !
# hist(hp$difftime)
# 
# ggplot(hp, aes(x=difftime)) +
#   geom_histogram() +
#   facet_wrap(.~ptt) +
#   labs(y="Dive counts", x="Time difference between observation (s)", 
#        title ="", fill="") +
#   theme_tq() +
#   theme(legend.position = "none")










######################################
# clean dataset: check depth outliers
######################################

# summarize dataset to 1 maxDep per day (plotting purpose)
# to identify wrong depth for 75, 76 and 80
#-----------------------------------------------------------
maxdep_daily = hp %>%
  group_by(id) %>%
  mutate(day = as.numeric(date - first(date)) + 1) %>%
  group_by(id, date) %>%
  summarise(maxdep    = max(depth, na.rm=T),
            mindep    = min(depth, na.rm=T),
            meandep   = mean(depth, na.rm=T),
            mediandep = median(depth, na.rm=T))
maxdep_daily 

# need zoc!
ggplot(data = maxdep_daily, aes(x=-mindep)) +
  geom_histogram() +
  labs(y = "dive frequency", x = "min depth (m)") +
  facet_wrap(~id, scales="free")

ggplot(data = maxdep_daily, aes(x=date, y=-maxdep)) +
  geom_line() +
  ylim(-max(maxdep_daily$maxdep),0) +
  labs(x = "Time", y = "Max depth (m)") +
  facet_wrap(~id, scales="free")

hp = hp %>%
  mutate(day = as.numeric(date - first(date)) + 1)
summary(hp$depth)  # min=-40!! max: 360 m


# generate summary with 1 max depth/day and /ID
maxdep_daily = hp %>%
  group_by(id) %>%
  mutate(day = as.numeric(date - first(date)) + 1) %>%
  group_by(id, date) %>%
  summarise(maxdep    = max(depth, na.rm=T),
            mindep    = min(depth, na.rm=T),
            meandep   = mean(depth, na.rm=T),
            mediandep = median(depth, na.rm=T))
maxdep_daily 
maxdep_daily %>% 
  group_by(id) %>%
  summarise(nobs = n())

ggplot(data = maxdep_daily, aes(x=date, y=-maxdep)) +
  geom_bar(stat="identity") +
  labs(x = "Time", y = "Max depth (m)") +
  facet_wrap(~id, scales="free_x") +
  theme_tq()












########################################################
# need to reset depth above 0 (some values below 0)
########################################################
# remove negative depth (above 3 m)
nrow(hp[hp$depth < (-1.5),]) / nrow(hp )*100  # 0.4%
hp = hp %>%
  filter(depth >= (-1.5)) %>%
  mutate(depth = depth + 1.5) 

# add column day and month
# day 1 = first day for each id
#--------------------------------
hp = hp %>%
  group_by(id) %>%
  mutate(day_depart = as.numeric(date - first(date)) + 1,
         month = substr(date, 6, 7)) %>%
  ungroup()

# order dates by ID
#-----------------------
hp = hp[order(hp$id, hp$posix_local),]

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP")
saveRDS(hp, "./RDATA/1.HP_dive_HR_Not_filtered.RDS")
gc()









###################################################################################################
# ZOC and dives identification
# (D) descent, (DB) descent/bottom, (B) bottom, (BA) bottom/ascent, (A) ascent, 
# (DA) descent/ascent (occurring when no bottom phase can be detected) and (X) non-dive (surface),
###################################################################################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP")
path = paste0("./Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/",
              "ANALYSES/HP/RDATA/1.HP_dive_HR_Not_filtered.rds")
str(hp)
offset    = 0
threshold = 5  # threshold depth below which an underwater phase should be considered a dive

system.time({  
  # 14700 for 4 ids, threshold=5 m, offset=0
  # 27876 sec for offset(zoc)=0 and threshold=2 and 3 ids
  for (i in 2:length(unique(hp$id))) { 
    id = hp %>% 
      filter(id == unique(id)[i]) %>%
      rename(time = posix_local)
    ptt = unique(id$id)
    id  = id %>%
      select(depth, time)
    id = id[order(id$time),]
    id = id[!duplicated(id$time),]
      # select(-c(date, day, id, posix, period, hour))
    
    tdr   = createTDR(id$time, id$depth, file=path, speed=F)
    calib = calibrateDepth(tdr, dive.thr=threshold, 
                           zoc.method="offset", offset=offset) 
    
    saveRDS(calib, paste0("./RDATA/calib_",threshold,"m_zoc",offset,"/","calib_",
                          ptt,"_dcalib_",threshold,"m_zoc",offset,".RDS"))
  }
})











#########################################################################
# add dive number and phases to the hp dataset
#########################################################################
offset    = 0
threshold = 5 # threshold depth below which an underwater phase should be considered a dive
zoc   = paste0("calib_",threshold,"m_zoc",offset)
setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP",
             "/RDATA/",zoc))
files = list.files(pattern=paste0(zoc,".RDS"))
files 

dive = NULL
system.time({  # 60 sec
  for (i in 1:length(files)) {
    dat = readRDS(files[i])
    d   = getDAct(dat)                     # extract dive id and activity
    ptt = substr(files[i], 7, 12)          # extract ptt number
    ptt = sub("*_", "", ptt)               # retain all characters located before "_"
    
    dive2 = hp %>%
      filter(id == ptt)  %>%               # select the same id from whole dataset
      mutate(dive     = d[,1],             # extract dive number
             activity = d[,2],             # extract dive activity
             phase    = getDPhaseLab(dat)) # extract dive phase
    
    # save the 5 ids into 1 tibble
    #-----------------------------
    dive = rbind(dive, dive2)
  }
})
gc()
unique(dive$id)
rm(dive2, ptt, d, dat)



#--------------------------------------------
# proportion of dive above depth threshold?
#--------------------------------------------
dive %>%
  group_by(id) %>%
  summarise(ndive  = max(dive),
            above  = length(dive[dive==0])/3600, # in hours
            below  = length(dive[dive>0])/3600,  # in hours
            trackdur = n()/3600) %>%             # in hours
  mutate(abovepc = (above / trackdur) * 100,     # in %
         belowpc = (below / trackdur) * 100)     # in %


#----------------------------------------
# still not proper zoc starting at 0 m !
#----------------------------------------
dat = dive[1:400,]
ggplot(dat, aes(x=posix_local, y=-depth, colour=activity)) +
  geom_path(group="activity") +
  ylim(-30,0)

dive2 = dive %>%
  mutate(depth_cor = depth-1)
ggplot(dive2[400:800,], aes(x=posix_local, y=-depth_cor, colour=activity)) +
  geom_path(group="activity") +
  ylim(-30,0)
summary(dive2$depth_cor)

dive2$depth_cor[dive2$depth_cor<0] = 0
ggplot(dive2[400:800,], aes(x=posix_local, y=-depth_cor, colour=activity)) +
  geom_path(group="activity") +
  ylim(-30,0)
summary(dive2$depth_cor)

dive2 -> dive
rm(dive2)

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP")
saveRDS(dive, paste0("./RDATA/1b.dive_5HP_calib_",
                     threshold,"m_zoc",offset,".RDS"))
gc()





###############################################################################
# summarise each dive / ID
# add columns asc_rate and desc_rate
# (D) descent, (DB) descent/bottom, (B) bottom, (BA) bottom/ascent, (A) ascent
###############################################################################
ndive_summary = dive %>%
  rename(posix = posix_local) %>%
  filter(dive != 0) %>%
  group_by(dive, id) %>% 
  mutate(difftime = c(NA, diff(posix))) %>%
  summarise(dur       = n(),             # dive duration in sec
            period    = first(period),
            bot_dur   = sum(difftime[phase == "B"], na.rm=T),
            asc_dur   = sum(difftime[phase == "A"], na.rm=T),
            des_dur   = sum(difftime[phase == "D"], na.rm=T),
            diff_dep  = mean(diff(depth)),
            diff_dep_asc  = mean(diff(depth[phase == "A"])),
            diff_dep_des  = mean(diff(depth[phase == "D"])),
            diff_dep_bot  = mean(diff(depth[phase == "B"])),
            start   = first(posix),
            end     = last(posix),
            maxdep  = max(depth),
            mindep  = min(depth),
            meandep = mean(depth)) %>%
  mutate(vspeed   = abs(diff_dep / dur),     # vertical speed m/s
         asc_rate = abs(diff_dep_asc / asc_dur),
         des_rate = abs(diff_dep_des / des_dur),
         bot_rate = abs(diff_dep_bot / bot_dur))
names(ndive_summary)


# save dataset
#--------------
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP")
saveRDS(ndive_summary, paste0("./RDATA/1c.diveSummary_5HP_calib_",
                              threshold,"m_zoc",offset,".RDS"))

# explo
#------------
nrow(ndive_summary[ndive_summary$dur<60,]) / nrow(ndive_summary) * 100  # 56%
ndive_summary %>% 
  filter(dur>5) %>%
  dplyr::select(dive, id, dur, maxdep, start, end,
         bot_dur, asc_rate, des_rate, bot_rate)
hist(ndive_summary$dur)
summary(ndive_summary$dur[ndive_summary$maxdep>100]) # 184 sec

names(ndive_summary)
nrow(ndive_summary[ndive_summary$maxdep>50,]) / nrow(ndive_summary) * 100  # 26%
nrow(ndive_summary[ndive_summary$maxdep>100,]) / nrow(ndive_summary) * 100 # 13%


ndive_summary %>%
  group_by(id, period) %>%
  summarise(ndive = max(dive),
            meandep = mean(meandep),
            maxdep  = mean(maxdep),
            dur     = mean(dur)/60,          # in minutes
            des_dur = mean(des_dur,na.rm=T), # in secs
            asc_dur = mean(asc_dur,na.rm=T), # in secs
            bot_dur = mean(bot_dur,na.rm=T), # in secs
            asc_rate = mean(asc_rate,na.rm=T),
            des_rate = mean(des_rate,na.rm=T),
            bot_rate = mean(bot_rate,na.rm=T))



#--------------------------------------
# histograms of dive characteristics
#--------------------------------------
ggplot(data = ndive_summary, aes(x=dur/60, fill=period)) +
  geom_density(aes(alpha=.4, colour=period)) +
  labs(y = "Dive proba", x = "(min)", title = "Dive duration",fill="") +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() +
  facet_grid(period~id, scales="free_y") +
  theme(legend.position = "none")

ggplot(data = ndive_summary, aes(x=-maxdep, fill=period)) +
  # geom_histogram() +
  geom_density(aes(alpha=.4, colour=period)) +
  labs(y = "Dive proba", x = "(m)", title = "Max dive depth",fill="") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() +
  facet_grid(period~id, scales="free_y") +
  theme(legend.position = "none")

ggplot(data = ndive_summary[!is.na(ndive_summary$des_rate),], 
       aes(x=des_rate, fill=period)) +
  geom_density(aes(alpha=.4, colour=period)) +
  labs(y = "Dive proba", x = "(m/s)", title = "Descent rate",fill="") +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() +
  facet_grid(period~id, scales="free_y") +
  # xlim(0,2.55) +
  # ylim(0,5010) +
  theme(legend.position = "none")

ggplot(data = ndive_summary[!is.na(ndive_summary$asc_rate),], 
       aes(x=asc_rate, fill=period)) +
  geom_density(aes(alpha=.4, colour=period)) +
  labs(y = "Dive proba", x = "(m/s)", title = "Ascent rate",fill="") +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() +
  facet_grid(period~id, scales="free_y") +
  theme(legend.position = "none")

ggplot(data = ndive_summary[!is.na(ndive_summary$bot_dur),], 
       aes(x=bot_dur/60, fill=period)) +
  geom_density(aes(alpha=.4, colour=period)) +
  labs(y = "Dive porba", x = "(min)", title = "Bottom time",fill="") +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() +
  facet_grid(period~id, scales="free_y") +
  theme(legend.position = "none")

# grid.arrange(a, b, c, d, e, ncol=2)





#################################
# geom_density maxdep/dur
#################################
# all dives
#----------------
ggplot(ndive_summary, aes(y = -maxdep, x = dur/60)) +
  geom_density_2d_filled(contour_var = "ndensity",
                         aes(colour = dive)) +
  # scale_colour_distiller(palette = "Blues", direction = 1) +
  facet_wrap(vars(id)) +
  labs(x = "Dive duration (min)", y = "Dive depth (m)",
       title = "All dives", fill="n dives \nprobability") +
  theme_tq() +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="white"),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        legend.position = "bottom",
        legend.key.size = unit(0.4,"line"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_text(size=6),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        axis.text  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l
ggsave(paste0("./FIGURES/Depth_Profiles/calib_",
              threshold,"m_zoc",offset,"/MaxDep_Dur_AllDives_5HP_",
              threshold,"m_zoc",offset,".png"),
       width=4.5,height=5,units="in",dpi=400)

# deep dives > 50 m
#-----------------------
deep_dives = ndive_summary %>% filter(maxdep>=50)
table(deep_dives$id)
ggplot(deep_dives, aes(y = -maxdep, x = dur/60)) +
  geom_density_2d_filled(contour_var = "ndensity",
                         aes(colour = dive)) +
  facet_wrap(vars(id)) +
  ylim(-365,0) +
  labs(x = "Dive duration (min)", y = "Dive depth (m)",
       title = "Deep dives below 50 m", fill="n dives \nprobability") +
  theme_tq() +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="white"),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        legend.position = "bottom",
        legend.key.size = unit(0.4,"line"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_text(size=6),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        axis.text  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l
ggsave(paste0("./FIGURES/Depth_Profiles/calib_",
              threshold,"m_zoc",offset,"/MaxDep_Dur_Below50m_5HP_",
              threshold,"m_zoc",offset,".png"),
       width=4.5,height=5,units="in",dpi=400)


# deep dives > 100 m
#-----------------------
deep_dives = ndive_summary %>% filter(maxdep>=100)
table(deep_dives$id)
ggplot(deep_dives, aes(y = -maxdep, x = dur/60)) +
  geom_density_2d_filled(contour_var = "ndensity",
                         aes(colour = dive)) +
  facet_wrap(vars(id)) +
  ylim(-365,0) +
  labs(x = "Dive duration (min)", y = "Dive depth (m)",
       title = "Deep dives below 100 m", fill="n dives \nprobability") +
  theme_tq() +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="white"),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        legend.position = "bottom",
        legend.key.size = unit(0.4,"line"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_text(size=6),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        axis.text  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l
ggsave(paste0("./FIGURES/Depth_Profiles/calib_",
              threshold,"m_zoc",offset,"/MaxDep_Dur_Below100m_5HP_",
              threshold,"m_zoc",offset,".png"),
       width=4.5,height=5,units="in",dpi=400)




# #------------------------------------
# # plot depth vs time
# #--------------------------------------
# system.time({ 
#   for (ptt in unique(dive$id)) { 
#     id  = dive %>% filter(id == ptt)
#     seq = seq(1, max(id$dive), by=20)
#     
#     for (i in 1:length(seq)) { 
#       start = seq[i]
#       end   = seq[i+1]
#       d     = id %>% 
#         filter(dive >= start & dive < end)
#       
#       surf_dive = id %>% filter(posix_local >= first(d$posix_local) 
#                                 & posix_local < last(d$posix_local) & dive == 0)
#       # d = d %>%
#       #   left_join(surf_dive, by = "posix_local")
#       
#       ggplot(data = d, aes(x=posix_local, y=-depth, colour=phase)) +
#         geom_path(group="phase", size=0.6) +
#         geom_path(data = surf_dive, aes(x=posix_local, y=-depth), 
#                   group="phase", size=0.6) +
#         labs(y = "Depth (m)", x = "", fill = "",
#              title = paste0(ptt, ": ", first(d$date)), 
#              subtitle = paste0("dive N°",start," to ", end)) +
#         theme_tq() +
#         geom_hline(yintercept = 0) +
#         geom_hline(yintercept = -5, colour="blue", lty=2) +
#         scale_color_brewer(palette="Set2") +
#         theme(legend.position = "bottom")
#       
#       ggsave(paste0("./FIGURES/Depth_Profiles/calib_",
#                     threshold,"m_zoc",offset,"/",unique(id$id),
#                     "/DiveProfile_calib",threshold,
#                     "m_zoc",offset,"_",
#                     unique(id$id),"_dive",start,"to", end,".png"),
#              width=5,height=4,units="in",dpi=400)
#     }
#   }
# })
# 
# 
# 
# 
# 
# 
# 
# ###################################################
# # extract statistics for each dive
# # post_dive surface interval for each dive
# # careful: surf intervals based on threshold >5 m
# ###################################################
# i = 1
# dcalib = readRDS(files[i])
# stat   = diveStats(dcalib) %>% as_tibble()
# View(stat)
# names(stat)
# summary(stat$divetim/60)
# dim(stat)
# 
# dat = dive %>% 
#   filter(id == "27262") 
# # slice_head(n=10) 
# range(dat$dive)
# summary(stat$postdive.dur)
# 
# ggplot(stat, aes(x=divetim/60, y=postdive.dur/60)) +
#   geom_point(size = 0.2, aes(colour = maxdep)) +
#   theme_tq() +
#   scale_colour_continuous(type = "viridis", direction = -1) +
#   labs(x = "dive duration (min)", y = "post-dive duration (min)") +
#   geom_smooth(method = "gam", colour="red", se=F, lwd=0.5)
# 
# # some very long post-dive surf intervals 
# # likely due to zoc and dive threshold of 5 m
# #-----------------------------------------------------------
# # outliers = stat %>% 
# #   filter(postdive.dur > 5*50) %>% 
# #   View()
# ggplot(stat[stat$postdive.dur,], 
#        aes(x=postdive.dur/60)) +
#   geom_bar(binwidth = 2, stat = "bin",fill="grey",colour="blue")
# 
# ggplot(stat[stat$postdive.dur<(10*60),], 
#        aes(x=divetim/60, y=postdive.dur/60)) +
#   geom_point(size = 0.2, aes(colour = maxdep)) +
#   theme_tq() +
#   ylim(0,20) +
#   scale_colour_continuous(type = "viridis", direction = -1) +
#   labs(x = "dive duration (min)", y = "post-dive duration (min)") +
#   geom_smooth(method = "gam", colour="red", se=F, lwd=0.5)
# 
# ggplot(stat[stat$postdive.dur<(10*60),], 
#        aes(x=divetim/60, y=postdive.dur/60)) +
#   geom_point(size = 0.2, aes(colour = desctim)) +
#   theme_tq() +
#   ylim(0,20) +
#   scale_colour_continuous(type = "viridis", direction = -1) +
#   labs(x = "dive duration (min)", y = "post-dive duration (min)") +
#   geom_smooth(method = "gam", colour="red", se=F, lwd=0.5)
# 
# 
# ggplot(stat[stat$postdive.dur<(10*60),], 
#        aes(x=postdive.dur/60, y=-maxdep)) +
#   geom_point(size = 0.2, aes(colour = divetim)) +
#   theme_tq() +
#   scale_colour_continuous(type = "viridis", direction = -1) +
#   labs(y = "max depth (m)", x = "post-dive duration (min)") +
#   geom_smooth(method = "gam", colour="red", se=F, lwd=0.5)
#        
# dat = dive[340:718,]
# ggplot(dat, aes(x=posix_local, y=-depth_cor, colour=as.factor(dive))) +
#   geom_path(group="activity") 
# dat %>% 
#   filter(dive!=0) %>%
#   group_by(dive) %>%
#   summarise(start = first(posix_local),
#             end   = last(posix_local),
#             maxdep = max(depth)) %>%
#   mutate(dur = difftime(end, start, units = "mins")) 
# dat %>%
#   dplyr::select(id, depth, depth_cor, posix_local, dive, activity, phase) %>%
#   View()
              
              
              









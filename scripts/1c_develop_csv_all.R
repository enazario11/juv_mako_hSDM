#load the pacakges 
library(tidyverse)

rm(list = ls())

#---------------------------------------------
#import and merge raw location files from WC -- generate shark_locs.csv
#---------------------------------------------
files = list.files("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/Final_locs")
files

setwd("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/Final_locs")

# Change format to POSIX of combined date and time column 
files_gls <- files[11:26] #assigns just the files that have date and time in the same column 
files_gls

shark.comb = NULL
for (i in 1:length(unique(files_gls))) {
  id = read.csv(files_gls[[i]]) %>%
    as_tibble()
  id = id %>%
    filter(!is.na(Date)) %>% #filter out NAs
    mutate(date_time = as.character(Date))
  #changes date/time to posix format so R can better interpret
  id$posix = as.POSIXct(strptime(id$Date, 
                                 format = "%m/%d/%Y %H:%M"))
  id$Time = format(as.POSIXct(id$posix), format = "%H:%M")
  id$Date = format(as.POSIXct(id$posix), format = "%m/%d/%Y")
  shark.comb = rbind(shark.comb, id) #adds extra rows at bottom to combine all shark files
}

shark.comb$Sex <- recode_factor(shark.comb$Sex, F = "Female", M = "Male" )
levels(shark.comb$Sex) <- c("Male", "Female")
table(shark.comb$PTT) #prints summary of final contents of combined shark DF

### Combine Date and Time then Change format to POSIX
files_gps <- files[1:10] #working directory needs to be set for final_locs
files_gps

shark.sep = NULL
for (i in 1:length(unique(files_gps))) {
  id = read.csv(files_gps[[i]]) %>%
    as_tibble()
  #add shark id as a column
  id = id %>%
    filter(!is.na(Time)) %>%
    mutate(date_time = as.character(paste0(Date, " ", Time)))
  #changes date/time to posix format so R can better interpret
  id$posix = as.POSIXct(strptime(id$date_time, 
                                 format = "%m/%d/%Y %H:%M:%S"))
  id$Time = format(as.POSIXct(id$posix), format = "%H:%M")
  id = id %>%
    filter(!is.na(Time))
  id$Date = format(as.POSIXct(id$posix), format = "%m/%d/%Y")
  shark.sep = rbind(shark.sep, id) #adds extra rows at bottom to combine all shark files
}

shark.sep$Sex <- recode_factor(shark.sep$Sex, F = "Female", M = "Male" )
levels(shark.sep$Sex) <- c("Male", "Female")
table(shark.sep$PTT) #prints summary of final contents of combined shark DF

###Combine all shark data now that date is in same format
shark.locs = NULL
shark.locs = rbind(shark.comb, shark.sep)
table(shark.locs$PTT)

#Add month and year to total shark locations DF
shark.locs = shark.locs %>%
  mutate(month = substr(posix, 6, 7), 
         year = substr(posix, 1, 4), 
         day = substr(posix, 9, 10))

###fix dates for PTTs 54575 and 63984
shark.locs2 <- shark.locs %>% 
  filter(PTT == 54575 | PTT == 63984) %>%
  mutate(posix = as.POSIXct(strptime(date_time, 
                                        format = "%m/%d/%y %H:%M:%S")),
         Time = format(as.POSIXct(posix), format = "%H:%M"),
         Date = format(as.POSIXct(posix), format = "%m/%d/%Y"), 
         month = substr(posix, 6, 7), 
         year = substr(posix, 1, 4), 
         day = substr(posix, 9, 10))

shark.locs1 <- shark.locs %>%
  filter(PTT != 54575 & PTT != 63984)
shark.locs.final <- rbind(shark.locs2, shark.locs1)

#To save the CSV use below code -- shark_locs.csv generation
write.csv(shark.locs.final, file = "data/shark_locs.csv", row.names = F)



#-----------------------------
# filter by FL to remove adults -- generate shark_AgeLocs.csv
#-----------------------------
#Separate by sex and then assign age class based on FL. Then recombine
FL_male <- shark.locs.final %>%
  filter(Sex == "Male") %>%
  filter(FL < 180)
FL_female <- shark.locs.final %>%
  filter(Sex == "Female") %>%
  filter(FL < 249)

shark_AgeLocs <- rbind(FL_female, FL_male)
#write.csv(shark_AgeLocs, file = "data/shark_AgeLocs.csv", row.names = F)

#Summary of deployment/animal
dat_sum <- shark_Agelocs %>% 
  group_by(PTT) %>%
  summarise(start= first(posix),
            end = last(posix), 
            n = n()) %>% #The number of locations for each shark 
  mutate(dur = as.numeric(difftime(end, start, units = "days")))

    #mean locs per day -- range is 1-5 locations per day across all sharks
day_locs <- shark_AgeLocs %>%
  group_by(PTT, Date) %>%
  summarise(mean_n = mean(n())) %>%
  ungroup()%>%
  group_by(PTT)%>%
  summarise(loc_day = mean(mean_n))


#------------------------
#Combining PDT and location data (already filtered for FL) -- generate tdl.csv
#------------------------
setwd("data/pdt")

### change date format across all PDT files, select matching columns, bind to single DF
files = list.files("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/pdt")
files

pdt.comb = NULL
for (i in 1:length(unique(files))) {
  id = read.csv(files[[i]]) %>%
    as_tibble()
  id = id %>%
    filter(!is.na(Date)) %>% #filter out NA
    subset(select = c(Ptt, Date, Depth1, MaxTemp1, Depth2, MaxTemp2, Depth3, MaxTemp3, Depth4, MaxTemp4, Depth5, MaxTemp5, Depth6, MaxTemp6, Depth7, MaxTemp7, Depth8, MaxTemp8, Depth9, MaxTemp9, Depth10, MaxTemp10, Depth11, MaxTemp11, Depth12, MaxTemp12))
  #changes date/time to format R can better interpret
  id$Date = as.Date(id$Date, origin = "1900-01-01")
  pdt.comb = rbind(pdt.comb, id) #adds extra rows at bottom to combine all pdt files
}

head(pdt.comb)
table(pdt.comb$Ptt)

pdt.comb_L <- gather(pdt.comb, depth_num, depth_v, Depth1, Depth2, Depth3, Depth4, Depth5, Depth6, Depth7, Depth8, Depth9, Depth10, Depth11, Depth12)
pdt.comb_L <- pdt.comb_L %>% 
  gather(temp_type, temp_v, MaxTemp1, MaxTemp2, MaxTemp3, MaxTemp4, MaxTemp5, MaxTemp6, MaxTemp7, MaxTemp8, MaxTemp9, MaxTemp10, MaxTemp11, MaxTemp12)  %>%
  filter(depth_v < 1000 & depth_v > 0) %>%
  mutate(year = substr(Date, 1, 4), 
         cal_d = substr(Date, 1, 10))

### Depth and temp by loc data 
#summarize temp/depth data by date (get one value for each day)
#detach("plyr") #run if below chunk not working
loc_pdt = pdt.comb_L %>%
  group_by(Ptt, cal_d) %>%
  summarise(avg_depth = mean(depth_v, na.rm = T), 
            max_depth = max(depth_v, na.rm = T),
            med_depth = median(depth_v, na.rm = T),
            avg_temp = mean(temp_v, na.rm = T),
            med_temp = median(temp_v, na.rm = T),
            max_temp = max(temp_v, na.rm = T), 
            min_temp = min(temp_v, na.rm = T)) %>%
  mutate(year = substr(cal_d, 1, 4),
         month = substr(cal_d, 6, 7), 
         day = substr(cal_d, 9, 10), 
         ID = paste0(Ptt, "_", year, month, day))

#due to how dates are stored may have to run through Location_Data script
dat_locs <- read.csv("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/shark_AgeLocs.csv")

dat_locs <- dat_locs %>%
  mutate(month = sprintf("%02d", month), 
         day = sprintf("%02d", day),
         ID = paste0(PTT, "_", year, month, day))

td_locs <- dat_locs %>% #FINAL DF WITH COMBO LOC,TEMP, DEP DATA
  left_join(loc_pdt, by=c("ID"))

td_locs <- td_locs %>%
  mutate(region = ifelse(Lat <= 32.522499, "Baja", "SCB"))

td_locs2 <- td_locs %>%
  subset(select = c(PTT, Date, Lat, Lon, Sex, FL, posix, Time, year.x, month.x, day.x, ID, avg_depth, max_depth, med_depth, avg_temp, med_temp, max_temp, min_temp, region))%>%
  rename(year = year.x, 
         month = month.x, 
         day = day.x, 
         time = Time, 
         ptt = PTT, 
         date = Date) %>%
  filter(region != "NA")%>%
  mutate(ptt = as.factor(ptt))

write.csv(td_locs2, "data/tdl.csv", row.names = F)

#to remove duplicates 
#test <- td_locs2[!duplicated(td_locs2[,c('ID')]),]

#-------------------------------
#filter dates for SCB Ecoregion -- generate tdl_scbE.csv
#-------------------------------
loc_dat <- read.csv("C:/Users/nazar/OneDrive/Documents/R/Projects/juv_mako_hSDM/data/tdl.csv") #may need if above hasn't been run
loc_dat <- loc_dat %>%
  filter(Lat < 35 & Lat > 25 & Lon > -123 & Lon < -113)

write.csv(loc_dat, "data/tdl_scbE.csv", row.names = F)

#-----------------------------------
#combining location and enviro data
#-----------------------------------
#see points_enviro_viz script. Top part has been updated. See section on how to handle posix dates with time 00:00:00. Rest of script has not been updated since I will extract new vars from different places anyways. 




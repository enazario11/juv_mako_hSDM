rm(list=ls())
library(ncdf4)
library(magrittr)
library(raster)


#--------Historical ROMS---------
#Open historical ROMs data
ROMS_files_hist <- list.files("C:\\Users\\nereo\\Dropbox (Personal)\\Eco-ROMS\\ROMS & Bathym Data\\wcra31_daily_1980-2010",pattern=".nc", full.names=T)
#ROMS_files_hist <- list.files("~/Dropbox/WCNRT (1)/",pattern=".nc", full.names=T)



#Extraction env. variables for seanettle pre-2010.
input_file <- read.csv(file="C:\\Users\\nereo\\OneDrive\\Escritorio\\Dan/seanettle_PA_pre2010.csv")
head(input_file)
# remove "GMT" and double space in some of the dates
#input_file$t <- gsub(" GMT", "", input_file$t)
#input_file$t <- gsub("  ", " ", input_file$t)
input_file$date <- as.POSIXct(strptime(input_file$date, format = "%Y-%m-%d"))
input_file$date <- as.factor(as.Date(substr(input_file$date, 1,  10))) #Ensure date format is ok for getvarROMS.  
summary(input_file$date)
# remove points outside of ROMS boundary
input_file <- input_file[input_file$latitude>=30 & input_file$latitude<=48,] 
input_file <- input_file[input_file$longitude>=-134 & input_file$longitude<=-115,] 
input_file$dt <- as.POSIXct(strptime(input_file$date, format = "%Y-%m-%d"), tz = "UTC")
names(input_file ) <- c("date", "year", "latitude", "longitude", "catch", "PA", "dt")
names(input_file)
# to just take first X rows as a test
#input_file <- input_file[1:40000,]


###############

# load in model and see variables
#lbst_mod <- readRDS("~/Dropbox/Eco-ROMS/Model Outputs/Final EcoROMS models/LBST.res1.tc3.lr01.10models.noLat.rds")
# sst, z, ild, ssh, z_sd, bv, sst_sd, svstr, ssh_sd, curl, EKE, sv, sustr, lunar, su

# load in ncdf files and see what variables are called. 
dat <- nc_open(ROMS_files_hist[1]) #BV - Brunt-Vaisala fq over top 200 m
dat <- nc_open(ROMS_files_hist[2]) #curl - surface wind stress curl
dat <- nc_open(ROMS_files_hist[3]) #d26 - 
dat <- nc_open(ROMS_files_hist[4]) #ild - isothermal layer depth
dat <- nc_open(ROMS_files_hist[5]) #ssh - sea surface height
dat <- nc_open(ROMS_files_hist[6]) #sss 
dat <- nc_open(ROMS_files_hist[7]) #sst - sea surface temp
dat <- nc_open(ROMS_files_hist[8]) #su - surface eastward current speed
dat <- nc_open(ROMS_files_hist[9]) #sustr - surface eastward wind stress
dat <- nc_open(ROMS_files_hist[10]) #sv - surface northward current speed
dat <- nc_open(ROMS_files_hist[11]) #svstr - surface northward wind stress
dat <- nc_open(ROMS_files_hist[12]) #w 


#################  tracking only
##Load  data in which to compile ROMS info, do this seperately for each species
# blsh <- readRDS("~/Dropbox/Eco-ROMS/Species Data/Tracking Data/bstrp_lists_w_data/blsh_bstrp_list.rds") ## blsh
# blsh <- lapply(blsh,function(input_file)input_file[input_file$lat>=30 & input_file$lat<=48,]) #remove points outside of ROMS boundary
# blsh <- lapply(blsh,function(input_file)input_file[input_file$lon>=-134 & input_file$lon<=-115.5,]) #remove points outside of ROMS boundary
# #blsh <- lapply(blsh,ChangeType)  #--------------> was no longer needed after reprocessing the files (11-22-17) using bstrap_list_org.R


# lbst <- readRDS("~/Dropbox/Eco-ROMS/Species Data/Tracking Data/bstrp_lists_w_data/lbst_bstrp_list.rds") ## lbst
# lbst <- lapply(lbst,function(input_file)input_file[input_file$lat>=30 & input_file$lat<=48,]) #remove points outside of ROMS boundary
# lbst <- lapply(lbst,function(input_file)input_file[input_file$lon>=-134 & input_file$lon<=-115.5,]) #remove points outside of ROMS
# lbst <- lapply(lbst,ChangeType)


#casl <- readRDS("~/Dropbox/Eco-ROMS/Species Data/Tracking Data/bstrp_lists_w_data/casl_bstrp_list.rds") ## casl
#casl <- lapply(casl,function(input_file)input_file[input_file$lat>=30 & input_file$lat<=48,]) #remove points outside of ROMS boundary
#casl <- lapply(casl,function(input_file)input_file[input_file$lon>=-134 & input_file$lon<=-115.5,]) #remove points outside of ROMS
#casl <- lapply(casl,ChangeType)
###############

#Run Xtracto ROMS - make sure getvarROMS function is loaded (only runs when wd is native rproject directory)
#source("~/Dropbox/Tommy_working/GitHub/Eco-ROMS-private/Tommy_working/Extracto_ROMS_TC_edited.R")

############### making the historical extraction into a function
xtracto_historical=function(input_file,netcdf_list){
  #Mean 0.1
  
  input_file <- getvarROMS(ROMS_files_hist[1], 'BV', input_file, desired.resolution = 0.1, mean, 'mean')
  input_file <- getvarROMS(ROMS_files_hist[2], 'curl', input_file, desired.resolution = 0.1, mean, 'mean')
  #input_file <- getvarROMS(ROMS_files_hist[3], 'd26', input_file, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file <- getvarROMS(ROMS_files_hist[4], 'ild', input_file, desired.resolution = 0.1, mean, 'mean')
  input_file <- getvarROMS(ROMS_files_hist[5], 'ssh', input_file, desired.resolution = 0.1, mean, 'mean')
  input_file <- getvarROMS(ROMS_files_hist[7], 'sst', input_file, desired.resolution = 0.1, mean, 'mean')
  input_file <- getvarROMS(ROMS_files_hist[8], 'su', input_file, desired.resolution = 0.1, mean, 'mean')
  input_file <- getvarROMS(ROMS_files_hist[9], 'sustr', input_file, desired.resolution = 0.1, mean, 'mean')
  input_file <- getvarROMS(ROMS_files_hist[10], 'sv', input_file, desired.resolution = 0.1, mean, 'mean')
  input_file <- getvarROMS(ROMS_files_hist[11], 'svstr', input_file, desired.resolution = 0.1, mean, 'mean')
  
  # Code to get Mean over 0.25 and 0.3 degrees, and SD over 0.4 and 1 degree was removed by Steph but can be accessed historically through github.
  #The reasoning was two fold: 1) due to issues with consistency in window size between getvarROMS and focal, we decided to move to a .3 radius; 2) 0.3 SD was used to comply with Elizabth Becekr et al
  #SD0.3
  input_file <- getvarROMS(ROMS_files_hist[7], 'sst', input_file, desired.resolution = 0.7, sd, 'sd')
  input_file <- getvarROMS(ROMS_files_hist[5], 'ssh', input_file, desired.resolution = 0.7, sd, 'sd')
  
  #Now get bathymetry (z).
  #Mean 0.1 
  rastIn <- 'C:\\Users\\nereo\\Dropbox (Personal)\\Eco-ROMS\\ROMS & Bathym Data/BATHYMETRY ETOPO1/z_.1.grd'
  input_file <- getZ(input_file,rastIn,name="z")
  head(input_file)
  
  #Now get bathymetry sd, known as zsd
  #SD0.4
  rastIn <- 'C:\\Users\\nereo\\Dropbox (Personal)\\Eco-ROMS\\ROMS & Bathym Data/BATHYMETRY ETOPO1/zsd_.3.grd'
  input_file <- getZsd(input_file,rastIn,name="z_sd")
  
  #Create EKE from surface geostrophic velocity fields
  # eke = (u2 + v2) / 2
  input_file$EKE_mean <- (input_file$su_mean^2 + input_file$sv_mean^2)/2
  
  #Create log EKE
  input_file$logEKE <- log(input_file$EKE_mean)
  
  #Get lunar illumination
  library(lunar)
  col_index <- length(input_file)+1
  counter=1
  for (i in 1:nrow(input_file)){
    ill <- lunar.illumination(input_file$dt[i])
    input_file[counter,col_index] <- ill
    counter=counter+1
  }
  colnames(input_file)[col_index] <- "lunar"
  
  #Get curl & w at additional resolutions
  #input_file <- getvarROMS(ROMS_files_hist[4], 'curl', input_file, desired.resolution = 0.5, mean, 'mean_0.5')
  
  return(input_file)
}


############### extracting historical ROMS to leatherback tracking data
JELLY_ROMS_HIST <- xtracto_historical(input_file, ROMS_files_hist)
# change column names to match model
names(JELLY_ROMS_HIST)
head(JELLY_ROMS_HIST)
# "id"            "lon"           "lat"           "dt"            "flag"          "iteration"     "tag"           "PA"           
#"CRW_id"        "date"          "BV_mean"       "curl_mean"     "ild_mean"      "ssh_mean"      "sst_mean"      "su_mean"      
# "sustr_mean"    "sv_mean"       "svstr_mean"    "sst_sd"        "ssh_sd"        "z"             "z_sd"          "EKE_mean"     
# "logEKE"        "lunar"         "curl_curl_0.5"
names(JELLY_ROMS_HIST) <- c("date","year" , "latitude" , "longitude" , "catch",  "PA",  "dt",
                           "bv",  "curl", "ild" , "ssh",  "sst",  "su", "sustr", "sv",  "svstr",  "sst_sd",  "ssh_sd", "z",
                           "z_sd", "EKE", "logEKE", "lunar")
# checking nas for variables
summary(JELLY_ROMS_HIST)
# NAs for most variables are on boundaries of extent - in coastal areas and at 30 and 48 lat and 134 lon
# 11684/nrow(LBST_ROMS_HIST)*100 = 2.6%

write.csv(JELLY_ROMS_HIST,"C:\\Users\\nereo\\OneDrive\\Escritorio\\Dan/PA_seanettle_2004-2010_historicalROMSDaily.csv")



#--------New NRT ROMS---------
#Open new ROMs data
ROMS_files_newNRT <- list.files("C:\\Users\\nereo\\Dropbox (Personal)\\Eco-ROMS\\ROMS & Bathym Data/WCNRT",pattern=".nc", full.names=T)

#################  observer only
#Extraction env. variables for seanettle post-2010 -2017
input_file_newRT <- read.csv(file="C:\\Users\\nereo\\OneDrive\\Escritorio\\Dan/seanettle_PA_post2010.csv", header=T)
input_file_newRT$date <- as.POSIXct(strptime(input_file_newRT$date, format = "%Y-%m-%d"))
input_file_newRT$date <- as.factor(as.Date(substr(input_file_newRT$date, 1,  10))) #Ensure date format is ok for getvarROMS.  
summary(input_file_newRT$date)
# remove points outside of ROMS boundary
input_file_newRT <- input_file_newRT[input_file_newRT$latitude>=30 & input_file_newRT$latitude<=48,] 
input_file_newRT <- input_file_newRT[input_file_newRT$longitude>=-134 & input_file_newRT$longitude<=-115,] 
input_file_newRT$dt <- as.POSIXct(strptime(input_file_newRT$date, format = "%Y-%m-%d"), tz = "UTC")
names(input_file_newRT) <- c("date", "year", "latitude", "longitude", "catch", "PA", "dt")
head(input_file_newRT)
#input_file_newRT$dt <- as.factor(as.Date(input_file_newRT$dt, format="%m/%d/%y"))
############### 

#Run Xtracto ROMS - make sure getvarROMS function is loaded 
#source("Extracto_Scripts/Extracto_ROMS.R",chdir = TRUE)

############### making the NRT extraction into a function
xtracto_NRT=function(input_file_newRT,netcdf_list){
#Mean 0.1
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[1], 'BV', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[2], 'curl', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
#  input_file_newRT <- getvarROMS(ROMS_files_newNRT[3], 'd26', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[4], 'ild', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[5], 'ssh', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[7], 'sst', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[8], 'su', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[9], 'sustr', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[10], 'sv', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[11], 'svstr', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')

#SD0.3
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[7], 'sst', input_file_newRT, desired.resolution = 0.3, sd, 'sd_0.3')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[5], 'ssh', input_file_newRT, desired.resolution = 0.3, sd, 'sd_0.3')

#Now get bathymetry (z).
#Mean 0.1 
  rastIn <- 'C:\\Users\\nereo\\Dropbox (Personal)\\Eco-ROMS\\ROMS & Bathym Data/BATHYMETRY ETOPO1/z_.1.grd'
  input_file_newRT <- getZ(input_file_newRT,rastIn,name="z_0.1")

#Now get bathymetry sd, known as zsd
#SD0.3
  rastIn <- 'C:\\Users\\nereo\\Dropbox (Personal)\\Eco-ROMS\\ROMS & Bathym Data/BATHYMETRY ETOPO1/zsd_.3.grd'
  input_file_newRT <- getZsd(input_file_newRT,rastIn,name="zsd_0.3")

#Create EKE from surface geostrophic velocity fields
# eke = (u2 + v2) / 2
  input_file_newRT$EKE_0.1 <- (input_file_newRT$su_mean_0.1^2 + input_file_newRT$sv_mean_0.1^2)/2

#Create log EKE
  input_file_newRT$logEKE_0.1 <- log(input_file_newRT$EKE_0.1)

#Get lunar illumination
  library(lunar)
  col_index <- length(input_file_newRT)+1
  counter=1
  for (i in 1:nrow(input_file_newRT)){
    ill <- lunar.illumination(input_file_newRT$dt[i])
    input_file_newRT[counter,col_index] <- ill
    counter=counter+1
  }
  colnames(input_file_newRT)[col_index] <- "lunar"

#Get curl & w at additional resolutions
  # input_file_newRT <- getvarROMS(ROMS_files_newNRT[2], 'curl', input_file_newRT, desired.resolution = 0.5, mean, 'mean_0.5')

  return(input_file_newRT)
}

############### extracting new NRT ROMS to observer data
JELLY_ROMS_NRT <- xtracto_NRT(input_file_newRT, ROMS_files_newNRT)
#saveRDS(DGN_ROMS_NRT,'~/Dropbox/Eco-ROMS/Species Data/Observer Data/DGN_allSets_2011-2017_ROMSDaily_FinalRes.rds')
############### 
head(JELLY_ROMS_NRT)
names(JELLY_ROMS_NRT) <- c("date","year" , "latitude" , "longitude" , "catch",  "PA",  "dt",
                           "bv",  "curl", "ild" , "ssh",  "sst",  "su", "sustr", "sv",  "svstr",  "sst_sd",  "ssh_sd", "z",
                           "z_sd", "EKE", "logEKE", "lunar")

write.csv(JELLY_ROMS_NRT,"C:\\Users\\nereo\\OneDrive\\Escritorio\\Dan/PA_seanettle_2011-2017_NRTROMSDaily.csv")




#extract env. data post 2017

#--------New NRT ROMS---------
#Open new ROMs data
ROMS_files_newNRT <- list.files("C:\\Users\\nereo\\Dropbox (Personal)\\Eco-ROMS\\ROMS & Bathym Data/wcra31_daily_1980-2010/WCNRT_2017-2018",pattern=".nc", full.names=T)

#################  observer only
#Extraction env. variables for seanettle post-2010 -2017
input_file_newRT <- read.csv(file="C:\\Users\\nereo\\OneDrive\\Escritorio\\Dan/seanettle_PA_post2010.csv", header=T)
input_file_newRT$date <- as.POSIXct(strptime(input_file_newRT$date, format = "%Y-%m-%d"))
input_file_newRT$date <- as.factor(as.Date(substr(input_file_newRT$date, 1,  10))) #Ensure date format is ok for getvarROMS.  
summary(input_file_newRT$date)
# remove points outside of ROMS boundary
input_file_newRT <- input_file_newRT[input_file_newRT$latitude>=30 & input_file_newRT$latitude<=48,] 
input_file_newRT <- input_file_newRT[input_file_newRT$longitude>=-134 & input_file_newRT$longitude<=-115,] 
input_file_newRT$dt <- as.POSIXct(strptime(input_file_newRT$date, format = "%Y-%m-%d"), tz = "UTC")
names(input_file_newRT) <- c("date", "year", "latitude", "longitude", "catch", "PA", "dt")
head(input_file_newRT)
#input_file_newRT$dt <- as.factor(as.Date(input_file_newRT$dt, format="%m/%d/%y"))
############### 

#Run Xtracto ROMS - make sure getvarROMS function is loaded 
#source("Extracto_Scripts/Extracto_ROMS.R",chdir = TRUE)

############### making the NRT extraction into a function
xtracto_NRT=function(input_file_newRT,netcdf_list){
  #Mean 0.1
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[1], 'BV', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[2], 'BV', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  #  input_file_newRT <- getvarROMS(ROMS_files_newNRT[3], 'd26', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[3], 'BV', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[4], 'curl', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[5], 'curl', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[6], 'curl', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[7], 'ild', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[8], 'ild', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[9], 'ild', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[10], 'ssh', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[11], 'ssh', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[12], 'ssh', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[13], 'sst', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[14], 'sst', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[15], 'sst', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[16], 'su', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[17], 'su', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[18], 'su', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[19], 'sustr', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[20], 'sustr', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[21], 'sustr', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[22], 'sv', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[23], 'sv', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[24], 'sv', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[25], 'svstr', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[26], 'svstr', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[27], 'svstr', input_file_newRT, desired.resolution = 0.1, mean, 'mean_0.1')
  
  
  
  

  #SD0.3
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[13], 'sst', input_file_newRT, desired.resolution = 0.3, sd, 'sd_0.3')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[14], 'sst', input_file_newRT, desired.resolution = 0.3, sd, 'sd_0.3')
  input_file_newRT <- getvarROMS(ROMS_files_newNRT[15], 'sst', input_file_newRT, desired.resolution = 0.3, sd, 'sd_0.3')
  
  
   input_file_newRT <- getvarROMS(ROMS_files_newNRT[10], 'ssh', input_file_newRT, desired.resolution = 0.3, sd, 'sd_0.3')
   input_file_newRT <- getvarROMS(ROMS_files_newNRT[11], 'ssh', input_file_newRT, desired.resolution = 0.3, sd, 'sd_0.3')
   input_file_newRT <- getvarROMS(ROMS_files_newNRT[12], 'ssh', input_file_newRT, desired.resolution = 0.3, sd, 'sd_0.3')
   
  #Now get bathymetry (z).
  #Mean 0.1 
  rastIn <- 'C:\\Users\\nereo\\Dropbox (Personal)\\Eco-ROMS\\ROMS & Bathym Data/BATHYMETRY ETOPO1/z_.1.grd'
  input_file_newRT <- getZ(input_file_newRT,rastIn,name="z_0.1")
  
  #Now get bathymetry sd, known as zsd
  #SD0.3
  rastIn <- 'C:\\Users\\nereo\\Dropbox (Personal)\\Eco-ROMS\\ROMS & Bathym Data/BATHYMETRY ETOPO1/zsd_.3.grd'
  input_file_newRT <- getZsd(input_file_newRT,rastIn,name="zsd_0.3")
  
  #Create EKE from surface geostrophic velocity fields
  # eke = (u2 + v2) / 2
  input_file_newRT$EKE_0.1 <- (input_file_newRT$su_mean_0.1^2 + input_file_newRT$sv_mean_0.1^2)/2
  
  #Create log EKE
  input_file_newRT$logEKE_0.1 <- log(input_file_newRT$EKE_0.1)
  
  #Get lunar illumination
  library(lunar)
  col_index <- length(input_file_newRT)+1
  counter=1
  for (i in 1:nrow(input_file_newRT)){
    ill <- lunar.illumination(input_file_newRT$dt[i])
    input_file_newRT[counter,col_index] <- ill
    counter=counter+1
  }
  colnames(input_file_newRT)[col_index] <- "lunar"
  
  #Get curl & w at additional resolutions
  # input_file_newRT <- getvarROMS(ROMS_files_newNRT[2], 'curl', input_file_newRT, desired.resolution = 0.5, mean, 'mean_0.5')
  
  return(input_file_newRT)
}

############### extracting new NRT ROMS to observer data
JELLY_ROMS_NRT_b <- xtracto_NRT(input_file_newRT, ROMS_files_newNRT)
#saveRDS(DGN_ROMS_NRT,'~/Dropbox/Eco-ROMS/Species Data/Observer Data/DGN_allSets_2011-2017_ROMSDaily_FinalRes.rds')
############### 
head(JELLY_ROMS_NRT_b)
names(JELLY_ROMS_NRT_b) <- c("date","year" , "latitude" , "longitude" , "catch",  "PA",  "dt",
                           "bv",  "curl", "ild" , "ssh",  "sst",  "su", "sustr", "sv",  "svstr",  "sst_sd",  "ssh_sd", "z",
                           "z_sd", "EKE", "logEKE", "lunar")

write.csv(JELLY_ROMS_NRT_b,"C:\\Users\\nereo\\OneDrive\\Escritorio\\Dan/PA_seanettle_2017-2019_NRTROMSDaily.csv")



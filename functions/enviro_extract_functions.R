
#extracting enviro values from Nncdf4 file types
getvarROMS <- function(nc,varname,inpts,desired.resolution,FUN,name){
  if ((desired.resolution*10) %% 2 ==0) stop("Desired Resolution must be an odd number (e.g. 0.3 not 0.2)")
  inpts$dt <- as.POSIXct(inpts$dt, '%Y-%m-%d',tz='UTC')
  nc.data <- nc_open(nc, write=FALSE)
  
  lat <- ncvar_get(nc.data,'lat'); lat <- lat[1,]
  lon <- ncvar_get(nc.data,'lon'); lon <- lon[,1]
  nrows <- length(lat); ncols <- length(lon)
  yr <- ncvar_get(nc.data,'year'); mth <- ncvar_get(nc.data,'month'); day <- ncvar_get(nc.data,'day')
  tim <- as.POSIXct(paste(yr,mth,day,sep='-'),tz='UTC')
  
  desired.resolution = desired.resolution/2
  
  for (i in 1:nrow(inpts)){
    print(paste(varname,inpts$dt[i],sep=' '))
    if (inpts$dt[i] %in% tim){
      xdate <- which(inpts$dt[i]==tim)
      c <- which.min(abs(lon-inpts$lon[i])) #input file lat lon must have those names (e.g., NOT latitude, long, or longitude)
      c_low <- which.min(abs(lon-(inpts$lon[i]-desired.resolution))) 
      c_up <- which.min(abs(lon-(inpts$lon[i]+desired.resolution)))
      r <- which.min(abs(lat-inpts$lat[i]))
      r_low <- which.min(abs(lat-(inpts$lat[i]-desired.resolution)))
      r_up <- which.min(abs(lat-(inpts$lat[i]+desired.resolution)))
      numcols=abs(c_up-c_low); numrows=abs(r_up-r_low)
      
      if (desired.resolution!=0.1){
        data.var  <-  ncvar_get(nc.data,varname,start=c(c_low,r_low,xdate),
                                count=c(numcols,numrows,1),verbose=FALSE)
        inpts[i,paste(varname,'_',name,sep='')] <- FUN(data.var[!is.nan(data.var)])
      } else{
        data.var.point  <-  ncvar_get(nc.data,varname,start=c(c,r,xdate),
                                      count=c(1,1,1),verbose=FALSE)
        inpts[i,paste(varname,'_',name,sep='')] <- data.var.point
      }
    } #else{
    #   inpts[i,paste(varname,'_',name,sep='')] <- NA ### commenting this out so that already extracted values are not overwritted with NAs
    # }
  }
  nc_close(nc.data)
  return(inpts)
}


getvarCMEM <- function(nc, varname, inpts, desired.resolution, FUN, name) {
  if ((desired.resolution*10) %% 2 ==0) stop("Desired Resolution must be an odd number (e.g. 0.3 not 0.2)")
  inpts$dt <- as.POSIXct(inpts$dt, '%Y-%m-%d',tz='UTC')
  nc.data <- nc
  
  lat <- ncvar_get(nc.data,'latitude')
  lon <- ncvar_get(nc.data,'longitude')
  nrows <- length(lat); ncols <- length(lon)
  time_nc <- ncvar_get(nc.data,'time')
  tim <- as.POSIXct(time_nc, origin ="1970-01-01", tz='UTC')
  
  desired.resolution = desired.resolution/2
  
  #for loop to go through each row/location
  for (i in 1:nrow(inpts)){
    print(paste(varname,inpts$dt[i],sep=' '))
    
    if (inpts$dt[i] %in% tim){
      xdate <- which(inpts$dt[i]==tim)
      c <- which.min(abs(lon-inpts$lon[i])) #input file lat lon must have those names (e.g., NOT latitude, long, or longitude)
      c_low <- which.min(abs(lon-(inpts$lon[i]-desired.resolution))) 
      c_up <- which.min(abs(lon-(inpts$lon[i]+desired.resolution)))
      r <- which.min(abs(lat-inpts$lat[i]))
      r_low <- which.min(abs(lat-(inpts$lat[i]-desired.resolution)))
      r_up <- which.min(abs(lat-(inpts$lat[i]+desired.resolution)))
      numcols=abs(c_up-c_low); numrows=abs(r_up-r_low)
      
      if (desired.resolution!=0.1){
        data.var  <-  ncvar_get(nc.data,varname,start=c(c_low,r_low,xdate),
                                count=c(numcols,numrows,1),verbose=FALSE)
        inpts[i,paste(varname,'_',name,sep='')] <- FUN(data.var[!is.nan(data.var)])
      } else{
        data.var.point  <-  ncvar_get(nc.data,varname,start=c(c,r,xdate),
                                      count=c(1,1,1),verbose=FALSE)
        inpts[i,paste(varname,'_',name,sep='')] <- data.var.point
      }
  }
  
  }
  return(inpts)
}

#extract bathymetry and rugosity from ncdf4 file type
getBathy <- function(nc,inpts,bathy_inpt,desired.resolution){
  
  if ((desired.resolution*10) %% 2 == 0) stop("Desired Resolution must be an odd number (e.g. 0.3 not 0.2)")
  nc.data <- nc_open(nc, write=FALSE)
  
  lat <- ncvar_get(nc.data,'latitude') #change to lat and lon here if I don't need to change the crs and res of the nc file
  lon <- ncvar_get(nc.data,'longitude')
  nrows <- length(lat); ncols <- length(lon)
  desired.resolution = 0.25
  desired.resolution = desired.resolution/2
  
  for (i in 1:nrow(inpts)){
    print(i)
    #print(i)
    # #Compute bathymetry: point extraction
    # c <- which.min(abs(lon-data$lon[i]))
    # r <- which.min(abs(lat-data$lat[i]))
    # data.var  <-  ncvar_get(nc.data,'altitude',start=c(c,r),count=c(1,1),verbose=FALSE)
    
    #Compute Bathymetry & rugosity
    c <- which.min(abs(lon-inpts$lon[i]))
    c_low <- which.min(abs(lon-(inpts$lon[i]-desired.resolution)))
    c_up <- which.min(abs(lon-(inpts$lon[i]+desired.resolution)))
    r <- which.min(abs(lat-inpts$lat[i]))
    r_low <- which.min(abs(lat-(inpts$lat[i]-desired.resolution)))
    r_up <- which.min(abs(lat-(inpts$lat[i]+desired.resolution)))
    numcols=abs(c_up-c_low)+1; numrows=abs(r_up-r_low)+1
    #change var below to 'elevation' if I don't need to change the res and CRS of the nc file
    data.var  <-  ncvar_get(nc.data, varid = bathy_inpt, start=c(c_low,r_low),count=c(numcols,numrows),verbose=FALSE)
    data.var <- data.var[data.var < 0]
    inpts$bathy[i] <- mean(data.var, na.rm=T)
    inpts$bathy_sd[i] <- sd(data.var, na.rm=T)
  }
  nc_close(nc.data)
  return(inpts)
}


#generate pseudo dive depth data for PA locations 
get_pseudo_depths <- function(input_depths, dat_PA){
  #create hist
  x = input_depths
  samplesize = nrow(dat_PA)
  hist_med = hist(x$med_depth, freq = FALSE)
  hist_mean = hist(x$avg_depth, freq = FALSE)
  hist_max = hist(x$max_depth, freq = FALSE)
  
  #median hist samples
  #choose a bin
  bins_med = with(hist_med, sample(length(mids), samplesize, replace = TRUE, p = density))
  # sample a uniform in it
  result_med = runif(length(bins_med),hist_med$breaks[bins_med],hist_med$breaks[bins_med+1]) 
  dat_PA$med_depth_PA <- result_med
  
  #mean hist samples
  #choose a bin
  bins_mean = with(hist_mean, sample(length(mids), samplesize, replace = TRUE, p = density))
  # sample a uniform in it
  result_mean = runif(length(bins_mean),hist_mean$breaks[bins_mean],hist_mean$breaks[bins_mean+1]) 
  dat_PA$mean_depth_PA <- result_mean
  
  #max hist samples
  #choose a bin
  bins_max = with(hist_max, sample(length(mids), samplesize, replace = TRUE, p = density))
  # sample a uniform in it
  result_max = runif(length(bins_max),hist_max$breaks[bins_max],hist_max$breaks[bins_max+1]) 
  dat_PA$max_depth_PA <- result_max
  
  return(dat_PA)
  
}



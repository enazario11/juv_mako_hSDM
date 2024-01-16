
##################################################################
####### EN modified version below. Only using getvarROMS function
#################################################################


getvarROMS <- function(nc,varname,inpts,desired.resolution,FUN,name){
  if ((desired.resolution*10) %% 2 ==0) stop("Desired Resolution must be an odd number (e.g. 0.3 not 0.2)")
  inpts$dt <- as.POSIXct(inpts$dt, '%Y-%m-%d',tz='UTC')
  nc.data <- nc_open(nc, write=FALSE)
  lat <- ncvar_get(nc.data,'latitude')#; lat <- lat[1,] EN modified here as was causing error and lat info was stored as numeric vector anyways
  lon <- ncvar_get(nc.data,'longitude')#; lon <- lon[,1] EN modified here as was causing error and lon info was stored as numeric vector anyways
  nrows <- length(lat); ncols <- length(lon)
  
  #had to change how dates were imported for my nc files. See below:
  yr_mth_day <- ncvar_get(nc.data, 'time')
  yr_mth_day <- as.Date(yr_mth_day/(24), origin=as.Date("1950-01-01"))
  tim <- as.POSIXct(yr_mth_day,'%Y-%m-%d', tz='UTC') #dates should be correct at this point
  
  desired.resolution = desired.resolution/2
  for (i in 1:nrow(inpts)){
    print(paste(varname,inpts$dt[i],sep=' '))
    if (inpts$dt[i] %in% tim){
      xdate <- which(inpts$dt[i]==tim)
      c <- which.min(abs(lon-inpts$lon[i]))
      c_low <- which.min(abs(lon-(inpts$lon[i]-desired.resolution)))
      c_up <- which.min(abs(lon-(inpts$lon[i]+desired.resolution)))
      r <- which.min(abs(lat-inpts$lat[i]))
      r_low <- which.min(abs(lat-(inpts$lat[i]-desired.resolution)))
      r_up <- which.min(abs(lat-(inpts$lat[i]+desired.resolution)))
      numcols=abs(c_up-c_low); numrows=abs(r_up-r_low)
      
      ##################################################################################
      ################# Modified section below to resolve error #######################
      ##################################################################################
      
      if (desired.resolution!=0.1){
        #modified the start argument in the two calls for ncvar_get(). These now start and the min lat and lons described above, and start at the one depth and time layers. Needed to somehow reference all four dimensions
        #modified the count argument in the two calls for ncvar_get(). These now end at the number of calls and number of rows defined above. They also end at the one depth and time layer provided.
        
        data.var  <-  ncvar_get(nc.data,varname,start=c(lon = c_low,lat = r_low,depth = 1,time = xdate),
                                count=c(lon = numcols, lat = numrows, depth = 1, time = 1),verbose=FALSE) 
        inpts[i,paste(varname,'_',name,sep='')] <- FUN(data.var[!is.nan(data.var)])
      } else{
        data.var.point  <-  ncvar_get(nc.data,varname,start=c(lon = c_low,lat = r_low,depth = 1,time = xdate),
                                      count=c(lon = numcols, lat = numrows, depth = 1, time = 1),verbose=FALSE) 
        inpts[i,paste(varname,'_',name,sep='')] <- data.var.point
      }
    } #else{
    #   inpts[i,paste(varname,'_',name,sep='')] <- NA ### commenting this out so that already extracted values are not overwritted with NAs
    # }
  }
  nc_close(nc.data)
  return(inpts)
}

getvarROMS2 <- function(nc,varname,inpts,desired.resolution,FUN,name){
  inpts$dt <- as.POSIXct(inpts$dt, '%Y-%m-%d',tz='UTC')
  nc.data <- nc_open(nc, write=FALSE)
  lat <- ncvar_get(nc.data,'latitude'); #lat <- lat[1,]
  lon <- ncvar_get(nc.data,'longitude'); #lon <- lon[,1]
  nrows <- length(lat); ncols <- length(lon)
  #yr <- ncvar_get(nc.data,'year'); mth <- ncvar_get(nc.data,'month'); day <- ncvar_get(nc.data,'day')
  yr_mth_day <- ncvar_get(nc.data, 'time')
  yr_mth_day <- as.Date(yr_mth_day/(24), origin=as.Date("1950-01-01"))
  tim <- as.POSIXct(yr_mth_day,'%Y-%m-%d', tz='UTC')
  for (i in 1:nrow(inpts)){
    print(inpts$dt[i])
    desired.resolution = desired.resolution/2
    if (inpts$dt[i] %in% tim){
      browser()
      xdate <- which(inpts$dt[i]==tim)
      dep <- 
      c <- which.min(abs(lon-inpts$lon[i]))
      c_low <- which.min(abs(lon-(inpts$lon[i]-desired.resolution)))
      c_up <- which.min(abs(lon-(inpts$lon[i]+desired.resolution)))
      r <- which.min(abs(lat-inpts$lat[i]))
      r_low <- which.min(abs(lat-(inpts$lat[i]-desired.resolution)))
      r_up <- which.min(abs(lat-(inpts$lat[i]+desired.resolution)))
      numcols=abs(c_up-c_low)+1; numrows=abs(r_up-r_low)+1
      
      if(r_low<r_up){
        start_r=r_low
      }else{
        start_r=r_up
      }
      
      if(c_low<c_up){
        start_c=c_low
      }else{
        start_c=c_up
      }
      start=c(start_c,start_r,xdate)
      
      if (desired.resolution!=0.1){
        data.var  <-  ncvar_get(nc.data,varname,start=start,
                                count=c(numcols,numrows,1),verbose=FALSE)
        inpts[i,paste(varname,'_',name,sep='')] <- FUN(data.var[!is.nan(data.var)])
      } else{
        data.var.point  <-  ncvar_get(nc.data,varname,start=c(c,r,xdate),
                                      count=c(1,1,1),verbose=FALSE)
        inpts[i,paste(varname,'_',name,sep='')] <- data.var.point
      }
    } else{
      inpts[i,paste(varname,'_',name,sep='')] <- NA
    }
  }
  nc_close(nc.data)
  return(inpts)
}


getZ <- function(inpts,rastIn,name){  ## note, edits to allow new columns to be explicitly named. we will now have mulitple z's, and need unique names to keep z columns from being over written
  rastIn <- raster(rastIn)
  inpts2 <- inpts
  inpts2$longitude[inpts2$longitude<=-180]<- -180
  coordinates(inpts2) <- c('longitude','latitude')
  proj4string(inpts2) <- CRS("+proj=longlat +ellps=WGS84")
  zmean <- raster::extract(rastIn,cbind(inpts2$longitude,inpts2$latitude),method='simple',na.rm=F,cellnumbers=F)
  EOVout <- cbind(inpts,zmean)
  EOVout$zmean[EOVout$longitude<=-180] <- NA
  position=grep("zmean",names(EOVout))
  colnames(EOVout)[position]=name
  rm(inpts2,inpts); return(EOVout)
}


#Bathymetry sd
getZsd <- function(inpts,rastIn,name){ ## ## note, edits to allow new columns to be explicitly named. we will now have mulitple zsd's, and need unique names to keep zsd columns from being over written
  rastIn <- raster(rastIn)
  inpts2 <- inpts
  inpts2$longitude[inpts2$longitude<=-180]<- -180
  coordinates(inpts2) <- c('longitude','latitude')
  proj4string(inpts2) <- CRS("+proj=longlat +ellps=WGS84")
  zsdev <- raster::extract(rastIn,inpts2@coords,method='simple',na.rm=F,cellnumbers=F)
  EOVout <- cbind(inpts,zsdev)
  EOVout$zsdev[EOVout$longitude<=-180] <- NA
  position=grep("zsdev",names(EOVout))
  colnames(EOVout)[position]=name
  rm(inpts2,inpts); return(EOVout)
}

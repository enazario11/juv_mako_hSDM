rm(list = ls())

## reinstall RchivalTag
detach("package:RchivalTag", unload=TRUE)
detach("package:pracma", unload=TRUE)
remove.packages("RchivalTag")
install.packages("C:/Users/Nerea/Desktop/BFT/RchivalTag_0.1.1.tar.gz", repos = NULL, type = "source")

library("pracma")
library("RchivalTag")

#leer script para hacer histogramas de depth 
hist_file <- "C:/Users/nereo/OneDrive/Escritorio/Emily/histos.csv" 
h <- read.csv(hist_file)
names(h)
View(h)
hist_dat_1 <- read_histos(hist_file)# wildlife computers change the date format maybe for each different file or data. Lo que hace es armonozirarlo
head(hist_dat_1)
names(hist_dat_1)
class(hist_dat_1)
hist_dat_1$TAD$DeployID$41774$bin_breaks ## someone chose very bad bin_breaks
#these breaks are really bad, we are gonna change them


# option 1:
hist_dat_1$TAD$DeployID.790079_Ptt.NA$bin_breaks <- round(hist_dat_1$DeployID.790079_Ptt.NA$bin_breaks)
windows(20,20)
hist_tad(hist_dat_1)# I can't see very well the ylim
hist_tad(hist_dat_1, select_id = "790079", select_from = 'DeployID')

# get histogram data with histogram-derived average depth and temperature values
#average per day?

#hist_dat_1 <- read_histos(system.file("example_files/nerea/out-Histos.csv",min_perc=100,package="RchivalTag"))
hist_df <- hist_dat_1$TAD$DeployID.790079$df#obtenermos la media per day de profundidad
View(head(hist_df))
avg1 <- hist_df$avg 
head(hist_df)
head(hist_df$avg)
#histogram average depth by time
windows(20,20)
plot(hist_df$date, hist_df$avg, type="l", ylim=rev(range(hist_df$avg)), xlab="Date", ylab="Depth (m)")


# generate histogram data and average/sd-estimates from depth time series data of the same tag.
# attention! unlike for histogram files, the average/sd-estimates are calculated


#function para cambiar los datetime de cada 30 segundos a 10 minutos y as? 
#poder correr las funciones de Robert para hacer los plots
num2date <- function(x){
  if(class(x) != "Date"){
    out <- as.Date(x,origin="1970-01-01")
  }else{
    out <- x
  }
  return(out)
}

.fact2Date <- function(x, date_format="%Y-%m-%d", lang_format="en") {
  x0 <- x
  LOCALE <- readr::locale(lang_format)
  LOCALE$date_names$mon_ab <- gsub("\\.","",LOCALE$date_names$mon_ab)
  LOCALE$date_names$day_ab <- gsub("\\.","",LOCALE$date_names$day_ab)
  out <- readr::parse_date(x, format = date_format, locale = LOCALE)
  
  out <- readr::parse_date(x, date_format, locale = LOCALE)
  i <- is.na(out)
  if(any(i)) {
    # print(head(x0[i]))
    stop(paste("date conversion failed! Please revise current'date_format':",date_format,"to",x0[which(i)[1]]))
  }
  return(out)
}

date2datetime <- function(x,tz="",midday=T){
  sstart <- 12
  if(!midday) sstart <- 0
  strptime(paste(.fact2Date(as.character(x)),paste0(sstart,":00:00")),"%Y-%m-%d %H:%M:%S",tz = tz)
}


setwd("C:\\Users\\Nerea\\Documents\\POSDOC\\IATTC_POSDOC\\CUARTO A?O\\AZTI\\TAGGING\\TAGGING_IGOR\\BFT\\Igaratza\\1.5\\")
#este es como el archivo Series pero se llama Arch y es cada 30 segundos
tsc <- read_TS(Sys.glob("*Arch*"),date_format = "%H:%M:%S %d-%b-%Y")
head(tsc)
range(tsc$datetime)



## resample recovered time series data at lower time step (simulating transmitted data set)
df <- tsc 
tstep <- 600
ii <- which(as.numeric(df$datetime)%%tstep == 0)
full <- data.frame(datetime =seq(df$datetime[ii[1]],df$datetime[tail(ii,1)],by=tstep))
jj <- as.character(full$datetime) %in% as.character(df$datetime)
tst <- df[which(as.character(df$datetime) %in% as.character(full$datetime)),]
head(tst)

## assign temperature data 
range(tst$Recorder.Temp,na.rm = T)
range(tst$Stalk.Temp,na.rm = T)
# tst$Temperature <- tst$Recorder.Temp
tst$Temperature <- tst$Stalk.Temp # (this one looks more meaningful..)
head(tst)
#write.table(tst, file="C:/Users/Nerea/Desktop/tst.csv", sep=",")
#saio=read.csv("C:/Users/Nerea/Desktop/tst.csv", sep=",")
#head(saio)
#names(saio)
#str(saio)
#saio$newdata <- strptime(as.character(saio$Day), "%d/%m/%Y")
#saio$txtdate  <- format(saio$newdata, "%d-%m-%Y")
#head(saio)
#str(saio)
#write.table(saio, file="C:/Users/Nerea/Desktop/saio.csv", sep=",")


## check time series data to define deployment start and end
## this was already done in the tracks (see date range below)
plot_DepthTS(tst[which(tst$date %in% head(unique(tst$date)),3),])
plot_DepthTS(tst[which(tst$date %in% tail(unique(tst$date)),3),])

tad_breaks <- c(0, 2, 5, 10, 20, 50, 100, 200, 300, 400, 600, 2000)
hist_dat_2 <- ts2histos(tst, tad_breaks = tad_breaks)
head(hist_dat_2)
head(hist_dat_2$TAD$merged$df)
avg2 <- hist_dat_2$TAD$merged$df$avg 
dates <- hist_dat_2$TAD$merged$df$date
windows(20,20)
plot(dates, avg2, type="l", ylim=rev(range(avg2)), xlab="Date", ylab="Depth (m)")



## how it works!
library(plyr)
ts_stats <- ddply(tst,c("date"),function(x) c(avg=mean(x$Depth,na.rm=T), SD=sd(x$Depth,na.rm=T)))
avg2==ts_stats$avg

#hist_dat_2 tiene menos dias porque coge los 100% y los reduce de 30 segundos a 10 minutes
head(hist_dat_2$TAD$merged$df)
windows(20,20)
hist_tad(hist_dat_2,min_perc= 99)#calcula solo el hist los datos con el 100% de los datos

windows(20,20)
histos <- ts2histos(tst, tad_breaks = tad_breaks, min_perc= 99)
hist_tad(histos)


## calculate data coverage directly from transmitted time series data:
library(plyr)
sm <- ddply(tst,.(date),function(x)c(duration=nrow(x),nrec=nrow(x[which(!is.na(x$Depth)),])))
sm$nperc <- round(100*sm$nrec/sm$duration,1)
sm

tst$Lon <- 5; tst$Lat <- 43
tst <- classify_DayTime(tst)
View(tst)

#hacer el histograma day-nigth
hists$TAD$merged$split_by
View(hists$TAD$merged$df)
windows(20,20)
hists <- ts2histos(tst,tad_breaks = tad_breaks,split_by = "daytime")
hist_tad(hists,min_perc= 99)


path <- "C:\\Users\\Nerea\\Documents\\POSDOC\\IATTC_POSDOC\\CUARTO A?O\\AZTI\\TAGGING\\TAGGING_IGOR\\BFT\\Igaratza\\1.5\\\\" 
setwd(path)
PDT_raw <- read.csv("out-PDTs.csv")
View(PDT_raw)


PDT <- read_PDT("out-PDTs.csv",folder=path)
#PDT: temperature preferences of the fish. Mean temperature
View(head(PDT))


library(plyr)
ts_stats <- ddply(tst,c("date"),function(x) c(avg=mean(x$Depth,na.rm=T), SD=sd(x$Depth,na.rm=T)))
avg2==ts_stats$avg


#make plot
windows(16,5)
image_TempDepthProfiles(interpolate_PDTs(PDT)[[1]],month.line=0)
lines(ts_stats$date+.5,ts_stats$avg)#a?adir la avearge depth from my transmiter
add <- hist_dat_2$TAD$merged$df# 
lines(add$date+.5,add$avg)
axis(2,at=30,las=1)
abline(h=30,lty="dashed",col="violet",lwd=3)

m <- interpolate_PDTs(PDT)
strat <- get_thermalstrat(m, all_info = TRUE)
View(strat)

tst <- DepthTempTS

#lo mismo que arriba
M <- interpolate_TempDepthProfiles(DepthTempTS)
windows(20,20)
image_TempDepthProfiles(M[[1]],month.line=0)

M2 <- interpolate_TempDepthProfiles(bin_TempTS(DepthTempTS,res=8),"MeanPDT")
windows(20,20)
image_TempDepthProfiles(M2[[1]],month.line=0)



tst$DeployID  <- "0790300"
windows(20,20)
plot_DepthTS(tst,xlim = "2007-08-30")

range(tst$date)
tst$Lon=5; tst$Lat=43
tst=classify_DayTime(tst)
windows(15,6)
plot_DepthTempTS(tst, xlim=c("2007-08-30","2007-12-04"), plot_DayTimePeriods=T)

### plot also day night time information:
windows(15,6)
plot_DepthTS(tst,xlim = c("2007-08-30"),plot_DayTimePeriods = T)
windows(15,6)
plot_DepthTS(tst,xlim = c("2007-08-30","2007-12-04"),plot_DayTimePeriods = T)



## Part 4) tracks (model based geolocation estimates from light levels, depth and temperature)

library(oceanmap)
pos <- system.file("example_files\\nerea\\results-3-GPE3.csv",package="RchivalTag")
head(pos)
windows(20,20)
plotmap(xlim=c(-30,15), ylim=c(30,50))
plot_geopos(pos, type='l', add=T,alpha = 100)



windows(20,20)
pos <- get_geopos(pos)
plotmap(xlim=c(-30,15), ylim=c(30,50))## use keyword to derive area limits
plot_geopos(pos, add=TRUE) ## show tracks as scatter plot
plot_geopos(pos, add=FALSE, cb.date_format="%Y")
plot_geopos(pos, add=FALSE, cb.date_format="%b")
plot_geopos(pos, add=FALSE, cb.date_format="%d %b")
plot_geopos(pos, add=FALSE, cb.date_format="%b %Y")

#ahora ncd
#nc_file <- system.file("example_files/nerea//results-3-GPE3.nc",package="RchivalTag") 

#windows(20,20)
#plot_geopos(nc_file)

## alternative: load file first, then plot:
#pols_df <- get_geopos(nc_file) ## loads tracks as SpatialPolygonsDataFrame
#windows(20,20)
#plotmap(xlim=c(-30,15), ylim=c(30,50)) ## use keyword to derive area limits
#plot_geopos(pols_df,add = TRUE)
#lines(pos$Lon, pos$Lat)
#points(pos$Lon, pos$Lat,pch=19,cex=.7)



pos <- get_geopos("C:\\Users\\Nerea\\Documents\\POSDOC\\IATTC_POSDOC\\CUARTO A?O\\AZTI\\TAGGING\\TAGGING_IGOR\\BFT\\Igaratza\\2\\results-1-GPE3.csv")
head(pos)
input <- ddply(pos,.(date),function(x)c(Lon=mean(x$Lon),Lat=mean(x$Lat)))
input$datetime <- date2datetime(input$date,tz = "UTC")
out <- classify_DayTime(get_DayTimeLimits(input))
head(out)
range(out$date)
range(tst$date)



out$datetime <- c()
df <- merge(tst,out,by="date")
range(df$date) ## cut by the date range of the track! (since not merged with all=T)
range(df$datetime) #


library(oceanmap)
wz <- 7 
File <- "./nere/name.png"
if (file.exists(File)) stop(File, " already exists")
dir.create(dirname(File), showWarnings = FALSE)
png(File)

do.save <- T
head(df)
tag <- "nere"
dates <- range(df$date)
for(dn in seq(dates[1],dates[2],by=wz)){
  dd <- num2date(dn)
  date_range <- paste0(gsub("-","",c(dd,dd+wz-1)),collapse="_")
  width <- 16;height <- 5
  if(!do.save) x11(width=width,height=height)
  if(do.save) figure(paste0(tag,"_",date_range),
                     folder = tag,do.save = do.save,
                     width=width,height=height,type="jpg")
  
  # a <- plot_DepthTS(df,xlim = c(dd,dd+wz-1),ylim = c(0,200),Return = T,plot_DayTimePeriods = T)
  # if(grepl("recov",tag)) a <- plot_DepthTempTS(df,xlim = c(dd,dd+wz-1),ylim = c(0,400),Return = T,mars=c(5,4,4,7),plot_DayTimePeriods = T)
  k <- df[which(!is.na(df$Depth) & df$date > dd & df$date <= dd+wz),]
  if(nrow(k) > 0) {
    a <- plot_DepthTempTS_resampled(k,ylim = c(0,400),Return = T,mars=c(5,4,4,7),plot_DayTimePeriods = T)
    # stop()
    # a$hour <- .datetime2hour(a$datetime)
    # b <- ddply(a,.(date,hour),function(x)c(mean_Depth=mean(x$Depth,na.rm=T)))
    # b$datetime <- .fact2datetime(paste0(b$date," ",b$hour,":00:00"),tz = "UTC")
    # lines(b$datetime,b$mean_Depth,col="violet",lwd=2)
    close_fig(do.save)
    if(!do.save) stop()
  }
}


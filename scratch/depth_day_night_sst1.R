rm(list=ls())

setwd("C:/Users/nereo/OneDrive/Escritorio/Code for Rafid/Nerea_new/tags/")
library(RchivalTag)
ts_file <- "C:/Users/nereo/OneDrive/Escritorio/Code for Rafid/Nerea/tags/all.csv"
#ts_file <- "C:/Users/nereo/OneDrive/Escritorio/Code for Rafid/Nerea_news/tags/151350/all.csv"


#ts_file <- "C:/Users/nereo/OneDrive/Escritorio/Code for Rafid/Nerea/tags/all.csv"
#View(read.csv(ts_file, sep=","))
ts_df <- read_TS(ts_file)
head(ts_df)


tad_breaks <- c(0, 2, 5, 10, 20, 50, 100, 200, 300, 400, 600, 2000)
hist_dat_2 <- ts2histos(ts_df, tad_breaks = tad_breaks,min_perc=99)#transform time series data in histogram dataset

hist_dat_2$TAD$merged$bin_breaks
hist_dat_2$TAD$merged$bin_breaks <- round(hist_dat_2$TAD$merged$bin_breaks)



#calcula la media de profundidad para cada dia
library(plyr)
ts_stats <- ddply(ts_df,c("date"),function(x) c(avg=mean(x$Depth,na.rm=T), SD=sd(x$Depth,na.rm=T)))
head(ts_stats)


#te mira cuánto porcentage of the data es 100% y te hace los análisis solamente
#con los días con 100%
library(plyr)
sm <- ddply(ts_df,.(date),function(x)c(duration=nrow(x),nrec=nrow(x[which(!is.na(x$Depth)),])))
sm$nperc <- round(100*sm$nrec/sm$duration,1)
sm
#el día 8 es 9% así que hay que quitarlo. También porque ya está en superficie.

#importante! 
#los datos después del día 7 no valdrían, porque ya sale a superficie.
#Habría que quitar el último día que no vale.

#calcula solo el hist los datos con el 100% de los datos
head(hist_dat_2$TAD$merged$df)

windows(20,20)
par(oma=c(2,2,2,2))
pdf(file="202818_depth.pdf")#change for each tag
hist_tad(hist_dat_2)


dev.off()


## classify time series data by daytime:
ts_df$Lon <- 124; ts_df$Lat <- -8
ts_df <- classify_DayTime(ts_df)
#æView(ts_df)#daytime: solo dia o noche. Daytime_long: day, night, dusk and dawn

hists <- ts2histos(ts_df,tad_breaks = tad_breaks,split_by = "daytime",min_perc=99)
hists$TAD$merged$split_by
#View(hists$TAD$merged$df)


windows(20,20)
par(oma=c(2,2,2,2))
pdf(file="202818_day&night.pdf")
hist_dat_4 <- ts2histos(ts_df, tad_breaks = tad_breaks,split_by = "daytime",min_perc = 99)
hist_tad(hist_dat_4)
dev.off()

#Lineplot day-night with depth
#windows(20,20)
#ts_df$Lon=5; ts_df$Lat=43
#ts_df=classify_DayTime(ts_df)
#windows(20,20)
#plot_DepthTS(ts_df, xlim=c("2019-05-07","2019-11-04"), plot_DayTimePeriods=T)


#no funciona porque no tenemos sst. Sino, hacer esto.
ts_file <- "C:/Users/nereo/OneDrive/Escritorio/Code for Rafid/Nerea/tags/201374 (retrieved)/201374-Series.csv"
ts_file <- "C:/Users/nereo/OneDrive/Escritorio/Code for Rafid/Nerea/tags/all.csv"
#ts_file <- system.file("example_files/Nerea/196526-Series.csv",package="RchivalTag")
DepthTempTS <- read_TS(ts_file)
M <- interpolate_TempDepthProfiles(DepthTempTS)
windows(20,20)
image_TempDepthProfiles(M[[1]])

M2 <- interpolate_TempDepthProfiles(bin_TempTS(DepthTempTS,res=15),"MeanPDT")
windows(20,20)
image_TempDepthProfiles(M2[[1]])


#no podemos correr porque no tenemos sst
library(oceanmap)
data(cmap)
head(cmap)
names(ts_df)
windows(20,20)
#plot_DepthTempTS(ts_df,xlim = c("2019-05-07","2019-11-04"))
plot_DepthTempTS_resampled(ts_df,plot_DayTimePeriods = F)

ts_df$Temperature <- c()
PDT <- read_PDT("C:/Users/nereo/OneDrive/Escritorio/Code for Rafid/Nerea/tags/201374 (retrieved)/201374-PDTs.csv")
PDT <- read_PDT("C:/Users/nereo/OneDrive/Escritorio/Code for Rafid/Nerea/tags/all.csv")
plot_DepthTempTS_resampled_PDT(ts_df,PDT,plot_DayTimePeriods = F)




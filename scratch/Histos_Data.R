## install or load package
library(RchivalTag)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tidyquant)
library(pracma)
library(xts)
library(lubridate)

## Package overview and help:
?RchivalTag 
help(package="RchivalTag") ## list of functions

#Read in data using read_histos() function from RchivalTag package
#my test shark data (adapted from tutorial and Nerea script)
hist_file <- "data/Mako Data/Histos/41770-Histos.csv"  
dat <- read.csv(hist_file)
names(dat)
View(dat)
  #looking at the data
hist_dat_1 <- read_histos(hist_file, min_perc = 100) 
str(hist_dat_1,3)
#head(hist_dat_1$TAD[[1]]$df)
#tail(hist_dat_1$TAD[[1]]$df)
#str(hist_dat_1,2)
#class(hist_dat_1)
hist_df <- hist_dat_1$TAD$DeployID.41770$df #get values as a DF
avg1 <- hist_df$avg 
head(hist_df)
head(hist_df$avg)

#view bin breaks
hist_dat_1$TAT$DeployID.41770_Ptt.41770$bin_breaks
head(hist_dat_1$TAT$DeployID.41770_Ptt.41770$df,3)
hist_dat_1$TAD$DeployID.41770_Ptt.41770$bin_breaks
head(hist_dat_1$TAD$DeployID.41770_Ptt.41770$df,3)

## TAD and TAT histos -- base
par(mfrow = c(1,2))
hist_tad(hist_dat_1)
hist_tat(hist_dat_1)

par(mfrow = c(1,1))
hist_tad(hist_dat_1, select_id = "41770", select_from = 'DeployID')
  #see arguments using args()

#histogram average depth by time plotted by month
windows(20,20)
plot(hist_df$date, hist_df$avg, type="l", ylim=rev(range(hist_df$avg)), xlab="Date", ylab="Depth (m)")

## Plotting combined tag data on multiple histos
hist_file2 <- "data/Mako Data/Histos/41774-Histos.csv" 
hist_dat_2 <- read_histos(hist_file2, min_perc = 100)
str(hist_dat_2,3)
hist_dat_2$TAT$DeployID.41774_Ptt.41774$bin_breaks
hist_dat_2$TAT$DeployID.41774_Ptt.41774$bin_breaks <- round(hist_dat_2$TAT$DeployID.41774_Ptt.41774$bin_breaks)
hist_dat_2$TAD$DeployID.41774_Ptt.41774$bin_breaks

hist_dat_combined <- combine_histos(hist_dat_1, hist_dat_2)
str(hist_dat_combined,2)
par(mfrow=c(2,2))
hist_tad(hist_dat_combined, plot_ntags=F)
hist_tat(hist_dat_combined, plot_ntags=F)

## Plotting merged tag data on single histo
hist_dat_merged <- merge_histos(hist_dat_combined,force_merge = FALSE) #changing to true forces bins to align amongst tags
str(hist_dat_merged,2)
par(mfrow=c(1,2))
hist_tad(hist_dat_merged)

  #unmerge histos 
str(hist_dat_merged,2)
hist_dat_merged$TAT[[1]]$bin_breaks
hist_dat_merged$TAD[[2]]$bin_breaks
hists_unmerged <- unmerge_histos(hist_dat_merged)
str(hists_unmerged,2)
hists_unmerged$TAT[[1]]$bin_breaks
hists_unmerged$TAT[[2]]$bin_breaks

#plot histos by time period depending on tag settings
  #depth
hist_12h <- read_histos(hist_file,min_perc=0)
head(hist_12h$TAD$DeployID.41770_Ptt.41770$df,3)
df <- hist_12h$TAD$DeployID.41770_Ptt.41770$df
df <- df[which(df$tstep == 12),]
df$tperiod <- "0:00 - 12:00"
df$tperiod[grep("12:00:00",df$datetime)] <- "12:00 - 24:00"
tad_breaks <- hist_12h$TAD$DeployID.41770_Ptt.41770$bin_breaks

ggplot(df, aes(avg, fill = tperiod)) + 
  geom_histogram(bins = 10) + 
  scale_fill_manual(values = c("goldenrod2", "steelblue4"))

  #temp
df_t <- hist_12h$TAT$DeployID.41770_Ptt.41770$df
df_t <- df_t[which(df_t$tstep == 12),]
df_t$tperiod <- "0:00 - 12:00"
df_t$tperiod[grep("12:00:00",df_t$datetime)] <- "12:00 - 24:00"
tat_breaks <- hist_12h$TAT$DeployID.41770_Ptt.41770$bin_breaks

ggplot(df_t, aes(avg, fill = tperiod)) + 
  geom_histogram(bins = 10) + 
  scale_fill_manual(values = c("goldenrod2", "steelblue4"))

# get histogram data with histogram-derived average depth and temperature values
  #depth
hist_df <- hist_dat_1$TAD$DeployID.41770_Ptt.41770$df
View(head(hist_df))
avg1 <- hist_df$avg # avgerage depth of the tag infered from the histogram data

ggplot(hist_df, aes(x = date, y = avg)) + 
  geom_point() + 
  xlab("Date")+
  ylab("Depth (m)")

  #temp
hist_df <- hist_dat_1$TAT$DeployID.41770_Ptt.41770$df
View(head(hist_df))
avg2 <- hist_df$avg # avgerage temp of the tag infered from the histogram data

ggplot(hist_df, aes(x = date, y = avg)) + 
  geom_point() + 
  xlab("Date")+
  ylab("Temp (C)") + 
  ylim(15,30)

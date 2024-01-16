library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)

  #Mako shark metadata work -- could do same for CTS from same data file
dat <- read.csv("data/Shark OMZ Tag Data - Sheet1.csv")
dat.m <- dat %>%
  filter(Species == 'Mako Shark') %>%
  group_by(Locations,Sex)
dat.mm <- dat.m %>%
  filter(Sex == "Male")
dat.fm <- dat.m %>%
  filter(Sex == "Female")

dat.mm$Age.Class <- ifelse(dat.mm$FL>=180, "Adult", 
                           ifelse(150<=dat.mm$FL & dat.mm$FL<=179, "SA", 
                                  ifelse(125<=dat.mm$FL & dat.mm$FL<=149, "Age-2", 
                                         ifelse(100<=dat.mm$FL & dat.mm$FL<=124, "Age-1", "YOY"))))
dat.fm$Age.Class <- ifelse(dat.fm$FL>=249, "Adult", 
                           ifelse(150<=dat.fm$FL & dat.fm$FL<=179, "SA", 
                                  ifelse(125<=dat.fm$FL & dat.fm$FL<=149, "Age-2", 
                                         ifelse(100<=dat.fm$FL & dat.fm$FL<=124, "Age-1", "YOY"))))

dat.mm %>%
  group_by(Locations, Age.Class) %>%
  tally()
dat.fm %>%
  group_by(Locations, Age.Class) %>%
  tally()

  #Blue shark metadata work
#Baja
dat.bb <- read.csv("data/Blue Data/Blue Shark Baja.csv")
n_distinct(dat.bb$Shark_ID)
dat.bb$Date_2 <- paste(dat.bb$day, dat.bb$mo, dat.bb$year)

dat.bb$age_class <- ifelse(dat.bb$FL>=200, "Adult", 
                           ifelse(120<=dat.bb$FL & dat.bb$FL<=199, "SA", 
                                  ifelse(105<=dat.bb$FL & dat.bb$FL<=119, "Age-2", 
                                         ifelse(80<=dat.bb$FL & dat.bb$FL<=104, "Age-1", "YOY"))))

sum.bb <- dat.bb %>%
  group_by(Shark_ID, age_class, sex, FL) %>%
  summarise(count = n_distinct(Date_2))
sum.bb %>%
  group_by(age_class, sex) %>%
  tally()

#Southern California Bight
dat.bs <- read.csv("data/Blue Data/Blue Shark SCB.csv")
n_distinct(dat.bs$Shark_ID)
dat.bs$Date_2 <- paste(dat.bs$day, dat.bs$mo, dat.bs$year)

dat.bs$age_class <- ifelse(dat.bs$FL>=200, "Adult", 
                           ifelse(120<=dat.bs$FL & dat.bs$FL<=199, "SA", 
                                  ifelse(105<=dat.bs$FL & dat.bs$FL<=119, "Age-2", 
                                         ifelse(80<=dat.bs$FL & dat.bs$FL<=104, "Age-1", "YOY"))))

sum.bs <- dat.bs %>%
  group_by(Shark_ID, age_class, sex, FL) %>%
  summarise(count = n_distinct(Date_2))
sum.bs %>%
  group_by(age_class, sex) %>%
  tally()

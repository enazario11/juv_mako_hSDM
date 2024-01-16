
### load libraries ####
library(tidyverse)
library(gbm)
library(dismo)
library(here)
library(gamm4)

### load data ####
  #observed
dat_locs <- read.csv(here("data/aniMotum_crw_locs.csv")) %>% 
  subset(select = -c(geometry1, geometry2)) %>% 
  mutate(date = as.POSIXct(strptime(date, format = "%m/%d/%Y")), 
         dep_id = paste(id, date, sep = " "))

  #pseudo
pa_files <- list.files(here("data/PAs/CRW/CRW_HW"), full.names = TRUE, pattern = "all")

dat_pa = NULL
the_letters <- "[:alpha:]"
per <- '[.]'
under <- '[_]'
slash <- '[/]'
col <- '[:]'


for (file in 1:length(unique(pa_files))) {
  dat_temp <- read.csv(pa_files[[file]]) %>%
    as_tibble()
  dat_temp$id = pa_files[[file]]
  dat_pa = rbind(dat_pa, dat_temp) #adds extra rows at bottom to combine all shark files
}

#may need to remove some columns when extracting the enviro vars
dat_PA <- dat_pa %>% 
  rename(lat_pa = y, lon_pa = x, dTime = t) %>%
  mutate(id = str_remove_all(id, the_letters), 
         id = str_remove_all(id, per), 
         id = str_remove_all(id, under), 
         id = str_remove_all(id, slash), 
         id = str_remove_all(id, col),
         dTime = as.POSIXct(strptime(dTime, format = "%Y-%m-%d %H:%M:%S")),
         date = format(as.POSIXct(dTime), format = "%m/%d/%Y"))

### generate df of PAs and locs for two sharks ####
locs_sub <- dat_locs %>% filter(id == 41770| id == 78156) %>% 
  subset(select = c("id", "date", "lon_p", "lat_p")) %>%
  mutate(date = ymd(date), 
         id = as.factor(id), 
         mako = 1) %>%
  rename("lon" = "lon_p", 
         "lat" = "lat_p")
  
PA_sub <- dat_PA %>% filter(id == 41770 | id == 78156) %>% 
  filter(iteration == 10) %>%
  subset(select = c("id", "date", "lon_pa", "lat_pa")) %>%
  mutate(date = mdy(date), 
         id = as.factor(id), 
         mako = 0) %>%
  rename("lon" = "lon_pa", 
         "lat" = "lat_pa")

all_locs_sim <- rbind(locs_sub, PA_sub)

### simulate SST data ####
all_locs_sim$SST_sim <- rnorm(n=808, mean=17, sd=5)
all_locs_sim$DO_sim <- rnorm(n=808, mean=100, sd=20)
all_locs_sim$MLD_sim <- rnorm(n=808, mean=20, sd=2)
all_locs_sim$bathy_sim <- rnorm(n=808, mean=800, sd=50)

### fitting a BRT ####
#get total num of presences 
sum(all_locs_sim$mako) #404 mako presences across two sharks

#model starting point (learning rate of 0.01)
#current settings are from PA paper
try(mako.tc5.lr005_tr5000 <- dismo::gbm.fixed(data = all_locs_sim, gbm.x = 6:9, gbm.y = 5,
                            family = "bernoulli", tree.complexity = 5,
                            learning.rate = 0.005, bag.fraction = 0.75, n.trees = 5000))

gbm.plot(mako.tc5.lr01, nplots = 4, plot.layout = c(2,2), write.title = FALSE) #plots relative influence of each predictor across its range
summary(mako.tc5.lr01) #presents in a table and figure the relative influence

#glm test 
m1 <- glm(mako ~ SST_sim + bathy_sim + DO_sim, data = all_locs_sim)
summary(m1)

#gam test 
makoGAMM <- gam(mako ~ s(SST_sim) + s(DO_sim) + s(MLD_sim) + s(bathy_sim), random=~(1|id),family=binomial, data=na.omit(all_locs_sim), bs = "cs", method = "REML")
summary(makoGAMM$gam)
summary(makoGAMM$mer)

plot(makoGAMM)


# 3) review list of covariates and why decide to select. Thoughts? When I add depths, okay to have >20 predictors?
# 4a) models to make to address hypotheses
# 4b) review steps for model explore/approaches/outputs to address hypotheses (cross valid, values to note, ways to visualize)

#libraries
library(tidyverse)
library(gbm)
library(dismo)
library(here)

set.seed(1004)

#load data w/ covars
dat <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_AGI_0m.rds"))
dat$bathy <- replace(dat$bathy, dat$bathy == "NaN", NA)
head(dat)

#originally, PA = 0 means a true position. Change so PA = 1 means a true position for fitting the BRT
dat$PA <- replace(dat$PA, dat$PA == 1, 2) #change PAs to temporarily equal 2
dat$PA <- replace(dat$PA, dat$PA == 0, 1) #change true positions to a 1
dat$PA <- replace(dat$PA, dat$PA == 2, 0) #change PA positions to a 0

#glm and gam test 
m1 <- glm(PA ~ lat + thetao_mean + so_mean + mlotst_mean + lon + zos_mean + uo_mean + vo_mean + chl_mean + nppv_mean + o2_mean + bathy + bathy_sd + AGI_0m, data = dat)
summary(m1)

m2 <- gam(PA ~ s(lat) + s(thetao_mean) + s(so_mean) + s(mlotst_mean) + s(lon) + s(zos_mean) + s(uo_mean) + s(vo_mean) + s(chl_mean) + s(nppv_mean) + s(o2_mean) + s(bathy) + s(bathy_sd) + s(AGI_0m), random=~(1|tag),family=binomial, data=na.omit(dat), bs = "cs", method = "REML")
test_m <- gam(PA ~ s(AGI_0m) + s(thetao_mean), random=~(1|tag),family=binomial, data=na.omit(dat), bs = "cs", method = "REML")
test <- plot(test_m, residuals = TRUE)

#BRT starting point
dat_test <- dat %>% filter(tag == "41770" | tag == "41774" | tag == "52126")
sum(dat_test$PA) #count number of observed positions

try(test <- dismo::gbm.step(data = dat_test, gbm.x = c(3:4, 8:18, 20), gbm.y = 5,
                                              family = "bernoulli", tree.complexity = 5,
                                              learning.rate = 0.01, bag.fraction = 0.75))

#explore outputs
gbm.plot(test, nplots = 14, plot.layout = c(4,4), write.title = FALSE) #plots relative influence of each predictor across its range
summary(test) #presents in a table and figure the relative influence
gbm.plot.fits(test)

#find and plot interactions 
#ID
test_int <- gbm.interactions(test)
test_int$interactions
test_int$rank.list
#plot
gbm.perspec(test, 13, 2, theta = 30) #change first two numbers to the pair of predictors to plot

#predictions


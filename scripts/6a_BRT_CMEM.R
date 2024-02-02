
# 3) review list of covariates and why decide to select. Thoughts? When I add depths, okay to have >20 predictors?
# 4a) models to make to address hypotheses
# 4b) review steps for model explore/approaches/outputs to address hypotheses (cross valid, values to note, ways to visualize (habiata suitability pred))

#libraries
library(tidyverse)
library(gbm)
library(dismo)
library(here);here <- here::here

set.seed(1004)

#load data w/ covars
dat <- readRDS(here("data/locs_w_covar/cmems/cmem_locs_covar_AGI_0m.rds"))
dat$bathy <- replace(dat$bathy, dat$bathy == "NaN", NA)
head(dat)

#originally, PA = 0 means a true position. Change so PA = 1 means a true position for fitting the BRT
dat$PA <- replace(dat$PA, dat$PA == 1, 2) #change PAs to temporarily equal 2
dat$PA <- replace(dat$PA, dat$PA == 0, 1) #change true positions to a 1
dat$PA <- replace(dat$PA, dat$PA == 2, 0) #change PA positions to a 0

dat_pos <- dat %>% filter(PA == 1)
dat_pa <- dat %>% filter(PA == 0)

#randomly select one PA for each tag
dat2 <- NULL
for(i in 1:length(unique(dat_pa$tag))){
  #select current id
  curr_ID <- unique(dat_pa$tag)[i]
  temp_df <- dat_pa[dat_pa$tag %in% curr_ID,]
  
  #sample 52 id's randomly
  temp_rep_ID <- sample(unique(temp_df$rep), 1, replace = FALSE)
  
  #narrow your data set
  temp_df2 <- temp_df[temp_df$rep %in% temp_rep_ID, ]
  
  #combine in a single df
  dat2 <- rbind(dat2, temp_df2)
  
}

dat2 <- rbind(dat2, dat_pos)

#glm and gam test 
m1 <- glm(PA ~ lat + thetao_mean + so_mean + mlotst_mean + lon + zos_mean + uo_mean + vo_mean + chl_mean + nppv_mean + o2_mean + bathy + bathy_sd + AGI_0m, data = dat)
summary(m1)

m2 <- gam(PA ~ s(lat) + s(thetao_mean) + s(so_mean) + s(mlotst_mean) + s(lon) + s(zos_mean) + s(uo_mean) + s(vo_mean) + s(chl_mean) + s(nppv_mean) + s(o2_mean) + s(bathy) + s(bathy_sd) + s(AGI_0m), random=~(1|tag),family=binomial, data=na.omit(dat), bs = "cs", method = "REML")
test_m <- gam(PA ~ s(AGI_0m) + s(thetao_mean), random=~(1|tag),family=binomial, data=na.omit(dat), bs = "cs", method = "REML")
test <- plot(test_m, residuals = TRUE)

#BRT starting point
dat_train <- dat2 %>% sample_frac(0.75)
dat_test <- dat2 %>% sample_frac(0.25)

try(test <- dismo::gbm.step(data = dat_train, gbm.x = c(3:4, 9:15, 17, 18, 20), gbm.y = 5,
                                              family = "bernoulli", tree.complexity = 5,
                                              learning.rate = 0.01, bag.fraction = 0.75, 
                            silent = TRUE, 
                            plot.main = TRUE))

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
preds <- predict.gbm(test, Anguilla_test,
                     n.trees=angaus.tc5.lr005$gbm.call$best.trees, type="response")
calc.deviance(obs=Anguilla_test$Angaus_obs, pred=preds, calc.mean=TRUE)

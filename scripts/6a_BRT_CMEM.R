### libraries ####
library(tidyverse)
library(gbm)
library(dismo)
library(here);here <- here::here
library(ggBRT)

set.seed(1004)

### load data w/ covars ####
dat0 <- readRDS(here("data/locs_w_covar/psat_spot/cmem_locs_covar_AGI_distcoast_0m.rds"))
dat0$bathy <- replace(dat0$bathy, dat0$bathy == "NaN", NA)
head(dat0)

#originally, PA = 0 means a true position. Change so PA = 1 means a true position for fitting the BRT
dat0$PA <- replace(dat0$PA, dat0$PA == 1, 2) #change PAs to temporarily equal 2
dat0$PA <- replace(dat0$PA, dat0$PA == 0, 1) #change true positions to a 1
dat0$PA <- replace(dat0$PA, dat0$PA == 2, 0) #change PA positions to a 0

dat_pos0 <- dat0 %>% filter(PA == 1)
dat_pa0 <- dat0 %>% filter(PA == 0)

### randomly select one PA for each tag ####
dat_temp <- NULL
for(i in 1:length(unique(dat_pa0$tag))){
  #select current id
  curr_ID <- unique(dat_pa0$tag)[i]
  temp_df <- dat_pa[dat_pa0$tag %in% curr_ID,]
  
  #sample 52 id's randomly
  temp_rep_ID <- sample(unique(temp_df$rep), 1, replace = FALSE)
  
  #narrow your data set
  temp_df2 <- temp_df[temp_df$rep %in% temp_rep_ID, ]
  
  #combine in a single df
  dat_temp <- rbind(dat_temp, temp_df2)
  
}

dat_brt0 <- rbind(dat_temp, dat_pos)

### glm and gam test ####
m1 <- glm(PA ~ lat + thetao_mean + so_mean + mlotst_mean + lon + zos_mean + uo_mean + vo_mean + chl_mean + nppv_mean + o2_mean + bathy + bathy_sd + AGI_0m, data = dat)
summary(m1)

m2 <- gam(PA ~ s(lat) + s(thetao_mean) + s(so_mean) + s(mlotst_mean) + s(lon) + s(zos_mean) + s(uo_mean) + s(vo_mean) + s(chl_mean) + s(nppv_mean) + s(o2_mean) + s(bathy) + s(bathy_sd) + s(AGI_0m), random=~(1|tag),family=binomial, data=na.omit(dat), bs = "cs", method = "REML")
test_m <- gam(PA ~ s(AGI_0m) + s(thetao_mean), random=~(1|tag),family=binomial, data=na.omit(dat), bs = "cs", method = "REML")
test <- plot(test_m, residuals = TRUE)

### BRT starting point ####
dat_train0 <- dat_brt0 %>% sample_frac(0.75)
dat_test0 <- dat_brt0 %>% sample_frac(0.25)

try(test <- dismo::gbm.step(data = dat_train0, gbm.x = c(8:19, 23), gbm.y = 5,
                                              family = "bernoulli", tree.complexity = 5,
                                              learning.rate = 0.01, bag.fraction = 0.75, 
                            silent = TRUE, 
                            plot.main = TRUE))

#### explore outputs ####
ggBRT::ggPerformance(test)

gbm.plot(test, nplots = 13, plot.layout = c(3,4), write.title = FALSE) 

#relative influence of predictors
ggBRT::ggInfluence(test)

#find the 5 most important pairwise interactions 
AGI_int <- gbm.interactions(test)
AGI_int$rank.list

#plot some interactions of interest
gbm.perspec(test, 12, 1)
gbm.perspec(test, 12, 2)

#predictions
preds <- predict.gbm(test, dat_test0, 
                     n.trees = test$gbm.call$best.trees, 
                     type = "response")

calc.deviance(obs = dat_test0$PA, preds) #get % deviance

dat_pred0 <- cbind(dat_test0$PA, preds)
pres0 <- dat_pred0[dat_pred0[,1] == 1, 2]
abs0 <- dat_pred0[dat_pred0[,1] == 0, 2]

#evaluate (AUC, TSS, TPR)
e = evaluate(p = pres0, a = abs0)
plot(e, 'TPR')
plot(e, 'TNR')
plot(e, 'ROC')
boxplot(e)
density(e)
mean(e@TPR) #TPR
max(e@TPR + e@TNR -1) #TSS

dev_eval_brt=function(model_object){ #provides % deviance for model selection -- refer to PA paper for other model eval functoins for dismo pacakge model object
  null <- model_object$self.statistics$null
  res <- model_object$self.statistics$resid
  dev=((null - res)/null)*100 
  return(dev)
}


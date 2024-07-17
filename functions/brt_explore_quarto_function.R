#libraries
{library(tidyverse)
  library(gbm)
  library(dismo)
  library(here)
  library(terra)
  library(sf)
  library(tidyterra)
  library(here);here <- here::here #plyr's here function masks here::here
  
  set.seed(1004)}

source(here("functions/BRT_evaluation_functions.R"))

explore_brt <- function(mod_file_path, test_data){
  
  mod_file <- readRDS(here(mod_file_path))
  set.seed(1004)
  
  # model performance 
  print("Model performance metrics")
  perform_plot <-ggBRT::ggPerformance(mod_file)
  print(perform_plot)
  
  #relative influence of predictors
  print("Relative influence of predictor variables")
  rel_inf <- ggBRT::ggInfluence(mod_file) 
  print(rel_inf)
  
  #explore partial plots
  print("Partial plots")
  gbm.plot(mod_file, nplots = 10, plot.layout = c(3,5), write.title = FALSE) 
  
  #find the 5 most important pairwise interactions 
  print("Top most important pairwise interactions as identified by the model")
  var_int <- gbm.interactions(mod_file)
  print(var_int$rank.list)
  
  #predictive performance using test dataset 
  preds <- predict.gbm(mod_file, test_data,
                       n.trees = mod_file$gbm.call$best.trees,
                       type = "response")
  observed <- test_data$PA
  
  ext.residual.deviance <- calc.deviance(obs = test_data$PA, pred=preds, family="bernoulli", calc.mean=TRUE) #get % deviance
  null.dev =  calc.deviance(test_data$PA ,rep(mean(test_data$PA),length(test_data$PA)), family="bernoulli", calc.mean=T)
  dev=(null.dev - ext.residual.deviance)/null.dev 
  print("External percent deviance explained")
  print(dev)
  
  
  dat_pred <- cbind(test_data$PA, preds)
  pres <- dat_pred[dat_pred[,1] == 1, 2]
  abs <- dat_pred[dat_pred[,1] == 0, 2]
  
  #evaluate (AUC, TSS, TPR)
  e = evaluate(p = pres, a = abs)
  plot(e, 'TPR')
  plot(e, 'TNR')
  plot(e, 'ROC')
  
  print("TPR")
  print(mean(e@TPR)) #TPR
  
  print("TSS")
  print(max(e@TPR + e@TNR -1)) #TSS

  #provides % deviance for model selection - unsure if this is different from above calc.deviance
  # perc_dev <- dev_eval_brt(mod_file, observed = observed, pred = preds)
  # print("Percent deviance calculated by in house functions")
  # print(perc_dev)
  
  #eval 75/25
  print("Model evaluation using a 75/25 train/test data split")
  eval_metrics <- eval_7525_modified(mod_file, 
                     testInput = test_data, 
                     gbm.y = "PA")
  print(eval_metrics)
}


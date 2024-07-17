
#' Script to calculate Area Under the receiver operator Curve (AUC) and True Skill Statistic (TSS) metrics for
#' GAMM and BRT models
#'
#' Workflow:
#' 0. Setup: load libraries, assign global objects and filepaths
#' 1. Functions for calculating evaluation metrics for the following training/testing datasets:
#'    1a. Full dataset
#'    1b. k-fold
#'    1c. Leave One Year Out
#'    
#'    #may need to go back and replace 100/100 code wih OG. I replaced with gbm object. To run gbm as part of the code, will need to have inputs be data files instead.

#### Setup #####
#Load libraries
{library(mlbench)
library(dismo)
library(gbm)
library(lubridate)
library(mgcv)
library(qmap)
library(dplyr)
library(Hmisc)
library(tidyverse)}

##SAMPLE OF THE FULL DATASET
# pres <- all_data[all_data$presabs==1,]
# abs <- all_data[all_data$presabs==0,]
# 
# #absence_1 <- abs[sample(nrow(abs), nrow(pres), replace = FALSE), ]
# #presence_1 <- pres[sample(nrow(pres), 1000, replace = FALSE), ]
# #sample1 <- rbind(pres, absence_1)
# 
# absence_2 <- abs[sample(nrow(abs), nrow(pres)*2, replace = FALSE), ]
# sample2 <- rbind(pres, absence_2)
# 
# #test <- all_data[sample(nrow(all_data), 2000, replace = FALSE), ]
# 
pseudoR2.BRT <- function(x){
  d2 <- 1-(x$self.statistics$mean.resid/x$self.statistics$mean.null)
  return(d2)
}

#MODEL EVALUATION FOR PRES/ABS DATA (bernoulli)
#provides % deviance for model selection
dev_eval_brt=function(model_object, observed, pred){

  ext.residual.deviance <- calc.deviance(obs=observed, pred=preds, family="bernoulli", calc.mean=TRUE)
  null.dev =  calc.deviance(observed,rep(mean(observed),length(observed)), family="bernoulli", calc.mean=T)
  dev=(null.dev - ext.residual.deviance)/null.dev
  return(dev)

}

#Make function to 100/100  AUC test
eval_100_percent <- function(brtInput, testInput, gbm.y){
  BRTInput <- brtInput
  Evaluations_100_percent <- as.data.frame(matrix(data=0,nrow=1,ncol=6))
  colnames(Evaluations_100_percent) <- c("RMSE","Cor","C-index","PredRatio", "DevianceExplained","PseudoR2")
  DataInput_train <-  testInput
  DataInput_test <- testInput
  DataInput.kfolds <-  BRTInput
  
  show.gbm(DataInput.kfolds)
  #summary.gbm(DataInput.kfolds)
  #gbm.plot(DataInput.kfolds, write.title = FALSE)

    preds <- predict.gbm(DataInput.kfolds, DataInput_test,
                       n.trees=DataInput.kfolds$gbm.call$best.trees, type="response")

    #preds <- predict.gbm(DataInput.kfolds, DataInput_test,
    #                    n.trees=10000, type="response")
    
  #observed <- as_vector(DataInput_test %>% select(., gbm.y=gbm.y))
  
  observed <- as_vector(DataInput_test %>% pull(gbm.y))
  
  # if (min(observed) <= 0) {
  #   preds <- exp(preds)
  #   observed <- exp(observed)
  # }
  
  ###D2 = (total deviance - cross validated residual deviance)/total deviance (Leathwick et al., 2006)
  ext.residual.deviance <- calc.deviance(obs=observed, pred=preds, family="bernoulli", calc.mean=TRUE)
  null.dev =  calc.deviance(observed,rep(mean(observed),length(observed)), family="bernoulli", calc.mean=T)
  dev=(null.dev - ext.residual.deviance)/null.dev 
  
  Rsquared <- pseudoR2.BRT(DataInput.kfolds)
  Evaluations_100_percent[1,1] <-sqrt(mean((preds-observed)^2))#RMSE
  Evaluations_100_percent[1,2]<-cor(preds,observed, method="pearson") #Spearman correlation
  Evaluations_100_percent[1,3]<-rcorr.cens(preds,observed)[1] #C-index
  Evaluations_100_percent[1,4]<-sum(preds)/sum(observed) #Ratio of predictions to actual values
  Evaluations_100_percent[1,5]<-dev #Ratio of predictions to actual values
  Evaluations_100_percent[1,6]<-Rsquared #Ratio of predictions to actual values
  
  return(Evaluations_100_percent)
  }

##ALL ENVIRONMENTAL VARIABLES
# eval_100_sample2_01 <- eval_100_percent(dataInput=sample2,
#                                           gbm.x=c("adt_hist", "adt_sd_hist","bathy", "bathy_sd", "sla_hist","sla_sd_hist","sst_hist", "sst_sd_hist", "mld_hist", 
#                                              "mnkc_epi","zooc", "npp",  "l.chl_hist", 
#                                              "antic_amplitude", "antic_ndays", "antic_pos_norm", "antic_speed_rad", "antic_dist", "antic_rad_km",
#                                              "cic_amplitude", "cic_ndays", "cic_pos_norm", "cic_speed_rad", "cic_dist", "cic_rad_km",
#                                              "eke_hist", "FSLE_max", "Theta_max"),
#                                      gbm.y="presabs",
#                                      lr=0.01,
#                                      tc=3)



#Number 2: 75/25% 
eval_7525_modified <- function(brtInput, testInput, gbm.y){
  set.seed(1004)
  BRTInput <- brtInput
  Evaluations_7525 <- as.data.frame(matrix(data=0,nrow=1,ncol=6))
  colnames(Evaluations_7525) <-  c("RMSE","Cor","C-index","PredRatio","DevianceExplained","PseudoR2")
  DataInput_test<- testInput
  DataInput.kfolds <- BRTInput
  
  show.gbm(DataInput.kfolds)
  #summary.gbm(DataInput.kfolds)
  #gbm.plot(DataInput.kfolds, write.title = FALSE)
  
   preds <- predict.gbm(DataInput.kfolds, DataInput_test,
                       n.trees=DataInput.kfolds$gbm.call$best.trees, type="response")
 
   #observed <- as_vector(DataInput_test %>% select(., gbm.y=gbm.y))
  
  observed <- as_vector(DataInput_test %>% pull(gbm.y))
  
  # if (min(observed) <= 0) {
  #   preds <- exp(preds)
  #   observed <- exp(observed)
  # }
  
  ###D2 = (total deviance - cross validated residual deviance)/total deviance (Leathwick et al., 2006)
  ext.residual.deviance <- calc.deviance(obs=observed, pred=preds, family="bernoulli", calc.mean=TRUE)
  null.dev =  calc.deviance(observed,rep(mean(observed),length(observed)), family="bernoulli", calc.mean=T)
  dev=(null.dev - ext.residual.deviance)/null.dev 
  
  Rsquared <- pseudoR2.BRT(DataInput.kfolds)
  Evaluations_7525[1,1] <- sqrt(mean((preds-observed)^2))#RMSE
  Evaluations_7525[1,2] <-cor(preds,observed, method="pearson") #Spearman correlation
  Evaluations_7525[1,3]<-rcorr.cens(preds,observed)[1] #C-inde
  Evaluations_7525[1,4]<-sum(preds)/sum(observed) #Ratio of predictions to actual values
  Evaluations_7525[1,5]<-dev #External deviance explained
  Evaluations_7525[1,6]<-Rsquared #Pseudo-Rsquared

  return(Evaluations_7525)}


##ALL ENVIRONMENTAL VARIABLES
# eval_7525_sample1 <- eval_7525_modified(dataInput=sample1,
#                                         gbm.x=c("adt_hist", "adt_sd_hist","bathy", "bathy_sd", "sla_hist","sla_sd_hist","sst_hist", "sst_sd_hist", "mld_hist", 
#                                              "mnkc_epi","zooc", "npp",  "l.chl_hist", 
#                                              "antic_amplitude", "antic_ndays", "antic_pos_norm", "antic_speed_rad",
#                                              "cic_amplitude", "cic_ndays", "cic_pos_norm", "cic_speed_rad",
#                                              "eke_hist", "FSLE_max", "Theta_max"),
#                                         gbm.y="presabs",
#                                         lr=0.005,
#                                         tc=3)



#Number 3: k-folds split 

kfolds_eval_brt_modified <- function(dataInput, gbm.x, gbm.y, lr, tc, bf){
  DataInput <- dataInput
  DataInput$Kset <- kfold(DataInput,10) #randomly allocate k groups
  Evaluations_kfold <- as.data.frame(matrix(data=0,nrow=10,ncol=7))
  colnames(Evaluations_kfold) <- c("k","RMSE","Cor","C-index","PredRatio","DevianceExplained","PseudoR2")
  counter=1
  for (k in 1:10){
    print(k)
    DataInput_train <- DataInput[DataInput$Kset!=k,]
    DataInput_test <- DataInput[DataInput$Kset==k,]
    DataInput.kfolds <- gbm.step(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                                 family="bernoulli", tree.complexity=tc,
                                 learning.rate = lr, bag.fraction = 0.6)

    show.gbm(DataInput.kfolds)
    summary.gbm(DataInput.kfolds)
    gbm.plot(DataInput.kfolds, write.title = FALSE)
    
     preds <- predict.gbm(DataInput.kfolds, DataInput_test,
                         n.trees=DataInput.kfolds$gbm.call$best.trees, type="response")
    
    observed <- as_vector(DataInput_test %>% pull(gbm.y))
    
    # if (min(observed) <= 0) {
    #   preds <- exp(preds)
    #   observed <- exp(observed)
    # }
    
    ###D2 = (total deviance - cross validated residual deviance)/total deviance (Leathwick et al., 2006)
    ext.residual.deviance <- calc.deviance(obs=observed, pred=preds, family="bernoulli", calc.mean=TRUE)
    null.dev =  calc.deviance(observed,rep(mean(observed),length(observed)), family="bernoulli", calc.mean=T)
    dev=(null.dev - ext.residual.deviance)/null.dev 
    Rsquared <- pseudoR2.BRT(DataInput.kfolds)
    
    Evaluations_kfold[counter,1] <- k
    Evaluations_kfold[counter,2] <- sqrt(mean((preds-DataInput_test$presabs)^2))#RMSE
    Evaluations_kfold[counter,3] <-cor(preds,DataInput_test$presabs, method="pearson") #Spearman correlation
    Evaluations_kfold[counter,4] <-rcorr.cens(preds,DataInput_test$presabs)[1] #C-index
    Evaluations_kfold[counter,5] <-sum(preds)/sum(DataInput_test$presabs) #Ratio of predictions to actual values
    Evaluations_kfold[counter,6]<-dev #External deviance explained
    Evaluations_kfold[counter,7]<-Rsquared #Pseudo-R2 from BRT
    counter=counter+1 
  }
  return(Evaluations_kfold)
}
##ALL ENVIRONMENTAL VARIABLES
# eval_kfolds_sample1 <- kfolds_eval_brt_modified(dataInput=sample1,
#                                               gbm.x=c("adt_hist", "adt_sd_hist","bathy", "bathy_sd", "sla_hist","sla_sd_hist","sst_hist", "sst_sd_hist", "mld_hist", 
#                                                       "mnkc_epi","zooc", "npp",  "l.chl_hist", 
#                                                       "antic_amplitude", "antic_ndays", "antic_pos_norm", "antic_speed_rad",
#                                                       "cic_amplitude", "cic_ndays", "cic_pos_norm", "cic_speed_rad",
#                                                       "eke_hist", "FSLE_max", "Theta_max"),
#                                               gbm.y="presabs",
#                                               lr=0.005,
#                                               tc=3)


#Number 4: Leave one year out
LOO_eval_modified <- function(dataInput, gbm.x, gbm.y, lr=lr, tc){
  DataInput <- dataInput
  Evaluations_LOO <- as.data.frame(matrix(data=0,nrow=1,ncol=7))
  colnames(Evaluations_LOO) <-c("k","RMSE","Cor","C-index","PredRatio","DevianceExplained","PseudoR2")
  counter=1
  for (y in min(DataInput$Year):max(DataInput$Year)){
    print(y)
    DataInput_train <- DataInput[DataInput$Year!=y,]
    DataInput_test <- DataInput[DataInput$Year==y,]
    DataInput.loo <- gbm.step(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                              family="bernoulli", tree.complexity=tc,
                              learning.rate = lr, bag.fraction = 0.6)
    preds <- predict.gbm(DataInput.loo, DataInput_test,
                         n.trees=DataInput.loo$gbm.call$best.trees, type="response")
    
    observed <- as_vector(DataInput_test %>% pull(gbm.y))
    
    # if (min(observed) <= 0) {
    #   preds <- exp(preds)
    #   observed <- exp(observed)
    # }
    
    ###D2 = (total deviance - cross validated residual deviance)/total deviance (Leathwick et al., 2006)
    ext.residual.deviance <- calc.deviance(obs=observed, pred=preds, family="bernoulli", calc.mean=TRUE)
    null.dev =  calc.deviance(observed,rep(mean(observed),length(observed)), family="bernoulli", calc.mean=T)
    dev=(null.dev - ext.residual.deviance)/null.dev 
    Rsquared <- pseudoR2.BRT(DataInput.kfolds)
    
    Evaluations_LOO[counter,1] <- y
    Evaluations_LOO[counter,2] <- sqrt(mean((preds-observed)^2))#RMSE
    Evaluations_LOO[counter,3] <-cor(preds,observed, method="pearson") #Spearman correlation
    Evaluations_LOO[counter,4] <-rcorr.cens(preds,observed)[1] #C-index
    Evaluations_LOO[counter,5] <-sum(preds)/sum(observed) #Ratio of predictions to actual values
    Evaluations_LOO[counter,6] <- dev #Ratio of predictions to actual values
    #Evaluations_LOO[counter,7] <- Rsquared #Ratio of predictions to actual values
    counter=counter+1 
    
  }
  return(Evaluations_LOO)}


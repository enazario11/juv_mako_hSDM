

OxyDemand <- function(Tpref,OxyThresh,rTempNewST){
  
  library(tidyverse)
  #Tpref: species temperature preference
  #OxyThresh: species oxygen threshold
  #rTempNewST: Environmental temperature
  
  rTArrhCoef=c(j1,j2) #Arrhenius coefficients, j1 and j2 described in the methods
  
  rTWinf=LwA*Linf^LwB #Convert Linf To Winf using a and b of length weight relationship
  
  rTAnaCatCoef=numeric()
  
  rktemp1=K/(1-d)
  rTAnaCatCoef[1]=rktemp1*rTWinf**(1-d) #anabolism coefficient
  rTAnaCatCoef[1]=rTAnaCatCoef[1]/exp(rTArrhCoef[1]/(273.15+Tpref)) # Temperature dependence of the anabolism coefficient
  rTAnaCatCoef[2]=rktemp1/(exp(rTArrhCoef[2]/(273.15+Tpref)))# Temperature dependent catabolism coefficient
  
  a=rktemp1*(rTWinf**(1-d))*(exp(rTArrhCoef[2]/(273.15+Tpref)))/(OxyThresh*(exp(rTArrhCoef[1]/(273.15+Tpref))))
  
  OrigBasalMetO2Index <- rktemp1*(exp(rTArrhCoef[2]/(273.15+rTempNewST)))*W^(1-d)/(a*(exp(rTArrhCoef[1]/(273.15+rTempNewST))))
  OxyDemand <- OrigBasalMetO2Index
} 
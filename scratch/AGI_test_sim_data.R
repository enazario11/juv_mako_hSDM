set.seed(1004)
source(here("functions/oxy_demand_functions.R"))

rgbeta <- function(n, mean, var, min = 0, max = 1)
{
  dmin <- mean - min
  dmax <- max - mean
  
  if (dmin <= 0 || dmax <= 0)
  {
    stop(paste("mean must be between min =", min, "and max =", max)) 
  }
  
  if (var >= dmin * dmax)
  {
    stop(paste("var must be less than (mean - min) * (max - mean) =", dmin * dmax))
  }
  
  # mean and variance of the standard beta distributed variable
  mx <- (mean - min) / (max - min)
  vx <- var / (max - min)^2
  
  # find the corresponding alpha-beta parameterization
  a <- ((1 - mx) / vx - 1 / mx) * mx^2
  b <- a * (1 / mx - 1)
  
  # generate standard beta observations and transform
  x <- rbeta(n, a, b)
  y <- (max - min) * x + min
  
  return(y)
}

temp_test <- rgbeta(n = 100, mean = 23.7, var = 2, min = 10, max = 27) #values represent ranges from temp pref data in Clarke paper for sp
ox_test <- rgbeta(n = 100, mean = 0.200, var = 0.00001, min = 0.120, max = 0.230) #values represent ranges from oxy thresh data in Clarke paper for sp

W = (1/3)*(LwA*Linf^LwB)
d = 0.700
K = 0.9 
j2 = 8.000
j1 = 4.500
OxyThresh = 0.200
Tpref = 23.7
Linf = 20 
LwA = 0.128
LwB = 2.700 

ox_demand_test1 <- OxyDemand(Tpref = Tpref, OxyThresh = OxyThresh, W = W, d = d, K = K, j2 = j2, j1 = j1, Linf = Linf, LwA = LwA, LwB = LwB, rTempNewST = temp_test)
hist(ox_demand_test1)
plot(temp_test, ox_demand_test1)

AGI_test1 <- ox_test/ox_demand_test1
hist(AGI_test1)

ox_demand_test2 <- OxyDemand2(Tpref = Tpref, OxyThresh = OxyThresh, W = W, d = d, K = K, j2 = j2, j1 = j1, Linf = Linf, LwA = LwA, LwB = LwB, rTempNewST = temp_test)
hist(ox_demand_test2)
plot(temp_test, ox_demand_test2)

AGI_test2 <- ox_test/ox_demand_test2
hist(AGI_test2)

#compared AGI of simulated data from temp pref and oxy thresh data of figure 5 from Clarke et al., 2021 for the Atlantic Blue Crab, the true range is between 1.4 and 2.4, and the average of my calculated AGI is close to 1.4. Close to mimicking the data
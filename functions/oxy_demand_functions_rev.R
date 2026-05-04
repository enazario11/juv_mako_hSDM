
#convert DO mmol/m^3 to atm 
do_to_atm <- function(do, t, s, thresh = FALSE) {
  
  if (thresh == TRUE){
      #use the salinity and temperature conditions to get unit to mmol/L
      do_mmol_L <- respR::convert_DO(do, "mL/L", "mmol/L", S = s, t = t, P = 1.013253)
      do <- do_mmol_L/0.001 #convert L to m^3
    }

  # If input is in raster format, convert to vector
  rast_format <- inherits(do, "SpatRaster")
  if (rast_format) {
    do_rast <- do
    do <- terra::values(do)[, 1]
    t_rast <- t
    t <- terra::values(t)[, 1]
    s_rast <- s
    s <- terra::values(s)[, 1]
  }

  po2_atm <-  rep(NA_real_, length(do))

  do_mask <- !is.na(do[i]) && !is.na(t[i]) && !is.na(s[i])
  a_o2_bar <- marelac::gas_solubility(S = s[do_mask], t = t[do_mask], species = "O2")
  a_o2_atm <- a_o2_bar / 0.9869
  po2_atm[do_mask] <- do[do_mask] / a_o2_atm

  # Handle the return value if raster
  if (rast_format) {
    po2_atm_rast <- do_rast
    terra::values(po2_atm_rast) <- po2_atm
    return(po2_atm_rast)
  }

  return(po2_atm)
  }

#caclulated metabolic demand 
OxyDemand <- function(Tpref, PO2_thresh, T_C, W = 51807.63, d = 0.700, K =  0.070, j2 = 8000, j1 = 4500, 
                      Linf = 321, LwA = 0.01670, LwB = 2.847){

  # removing K/(1-d) because it cancels out in numerator and denominator
  # Convert C to K temperatues
  T_K <- T_C + 273.15 
  Tpref_K <- Tpref + 273.15
  
  #O2_demand <- PO2_thresh * ((1/3)**(1-d)) * exp( ((j2-j1)/(Tpref_K)) + ((j1-j2)/(T_K)) )

  #Convert length to weight using scaling relationship
  Winf <- LwA * Linf**LwB

  O2_demand <- ((W**(1 - d)) * exp(-j2/T_K) * PO2_thresh * exp(-j1/Tpref_K)) / 
    ((Winf**(1 - d)) * exp(-j1/T_K) * exp(-j2/Tpref_K))
  
  O2_demand
} 

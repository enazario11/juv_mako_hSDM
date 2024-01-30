#convert DO mmol/m^3 to atm 
DO_to_atm <- function(loc_enviro_dat, depth){
  for(i in 1:nrow(loc_enviro_dat)) {
    # Constants:
    # Constants for the calculation of the saturation concentration of O2 in sea water (from Sarmiento & Gruber (2006), Garcia & Gorden (1992)):
    b1 = 2.00907
    b2 = 3.22014
    b3 = 4.05010
    b4 = 4.94457
    b5 = 0.256847
    b6 = 3.88767
    b7 = 0.00624523
    b8 = 0.00737614
    b9 = 0.010341
    b10 = 0.00817083
    b11 = 0.000000488682
    
    # Other constants:
    R = 8.31             # Gas constant [J/(mol*K)]
    GSmv = 22.3916       # Molar volume O2 [L/mol] (from Sarmiento & Gruber (2006))
    pmv = 0.000032       # Partial molar volume O2 [m3/mol] (from Deutsch et al. (2015, Science))
    KC = 273.15          # Kelvin conversion [K]
    Ks = 0.000086173     # Boltzmann constant [J/K]
    
    #calc in-situ density of seawater
    pressure = (1025 * 9.81 * depth) + 101325 
    press_dbar = pressure / 1000000
    
    Tpot = loc_enviro_dat$thetao_mean[i]
    T_scaled = log((298.15 - Tpot) / (KC + Tpot))
    rho = gsw::gsw_rho(SA = loc_enviro_dat$so_mean[i], p = press_dbar, CT = Tpot)
    
    #calculate saturation concentration of O2 in seawater
    l = b1+b2*T_scaled+b3*(T_scaled^2)+b4*(T_scaled^3)+b5*(T_scaled^4)+b6*(T_scaled^5)+loc_enviro_dat$so_mean[i]*(b7+b8*T_scaled+b9*(T_scaled^2)+b10*(T_scaled^3)) + b11*loc_enviro_dat$so_mean[1]^2
    calc_sat_o2 = (1000/GSmv) * exp(l)
    calc_sat_o2_molm3 = calc_sat_o2/1000
    
    #DO solubility at surface 
    DO_sol_surf = calc_sat_o2_molm3/0.209
    
    #calc pressure correction
    pressure_corr = exp(((depth*rho*9.81*pmv)/R)/(Tpot+KC))
    
    #calc solubility at any depth 
    sol_depth = DO_sol_surf*pressure_corr
    
    #convert DO to mol/kg
    DO_mol_kg = loc_enviro_dat$o2_mean[i]/1025000 #convert from mmol/m^3 to mol/kg
    
    #calc partial pressure O2 at any loc and depth 
    pO2 = (DO_mol_kg*rho)/(sol_depth)
    
    #create column to save O2 in atm 
    loc_enviro_dat[i, paste0("pO2","_", depth)] <- pO2
  }
  
  return(loc_enviro_dat)
}

#revised

#caclulated metabolic demand
OxyDemand <- function(Tpref, PO2_thresh, T_C, W = 51807.63, d = 0.700, K = 0.070, j2 = 8, j1 = 4.5, 
                      Linf = 321, LwA = 0.01670, LwB = 2.847){

  # removing K/(1-d) because it cancels out in numerator and denominator, right?
  # Convert C to K temperatues
  T_K <- T_C + 273.15 
  Tpref_K <- Tpref + 273.15
  # Convert length to weight using scaling relationship
  Winf <- LwA * Linf**LwB

  O2_demand <- W**(1 - d) * exp(-j2 / T_K) * PO2_thresh * exp(-j1 / Tpref_K) / 
    (Winf**(1 - d) * exp(-j1 / T_K) * exp(-j2 / Tpref_K))
  
  O2_demand
} 

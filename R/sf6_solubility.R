# ==========================================================
# Function: sf6_solubility()
# Project: ttd
# Purpose: Calculate sf6 solubility using Bullister et al., 2002 constants 
# ==========================================================

sf6_solubility <- function(t, s) {
  T_k <- t + 273.15 # Convert temp to Kelvin
  sf6_Ln_F <- -82.1639+120.152*(100/T_k)+30.6372*log(T_k/100)+
    s*(0.0293201+-0.0351974*(T_k/100)+0.00740056*(T_k/100)^2)
  sf6_F <- exp(sf6_Ln_F)   # fmol per kg
  return(sf6_F)
}

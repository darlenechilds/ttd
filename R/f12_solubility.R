# ==========================================================
# Function: f12_solubility()
# Project: ttd
# Purpose: Calculate sf6 solubility using Warner and Weiss 1985 constants 
# ==========================================================

f12_solubility <- function(t, s) {
  t_k <- t + 273.15 
  f12_ln_F <- -220.2120 + 301.8695 * (100 / t_k) + 114.8533 * log(t_k / 100) + -1.39165 * (t_k / 100)^2 +
    s * (-0.147718 + 0.093175 * (t_k / 100) + -0.0157340 * (t_k / 100)^2)
  f12_F <- exp(f12_ln_F)  
  return(f12_F)
}

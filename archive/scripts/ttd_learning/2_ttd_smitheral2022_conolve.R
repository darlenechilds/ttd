rm(list = ls())
ttd <- function(t, Gamma, Delta) {
  # ifelse(t <= 0, 0,
  sqrt(Gamma^3 / (4 * pi * Delta^2 * t^3)) *
    exp(-Gamma* (t-Gamma)^2 / (4 * Delta^2 * t))
}

# calculate Xeff - effective mole fraction, in equlib with wintertime surface conc. (Raimondi, 2021)
sf6_atm <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/ttd_learning/atmos_data_sf6_comb.csv")
# need to calculate for shallow/deep layers for now lets use 80%
sf6_atm_eff <- (sf6_atm$SF6NH*80)/100
sf6_interp <- approxfun(sf6_atm$Year, sf6_atm_eff, rule=2)

## cal Co using Xeff and solubility constants from Bullister et. al., 2002
T_k <- 3.5 + 273.15  # need to further refine, as this is only an average from last 10 years from nVlsw
sal <- 34.82          #

#gravimetric - fmol per kg
sf6_Ln_F <- -82.1639+120.152*(100/T_k)+30.6372*log(T_k/100)+sal*(0.0293201+-0.0351974*(T_k/100)+0.00740056*(T_k/100)^2)
#volumetric
# sf6_Ln_F <- -80.0343+117.232*(100/T_k)+29.5817*log(T_k/100)+sal*(0.0335183+-0.0373942*(T_k/100)+0.00774862*(T_k/100)^2)

sf6_F <- exp(sf6_Ln_F)
sf6_p_atm <- sf6_atm_eff/1000000000000

#surface history
C0 <- sf6_F*sf6_p_atm*1000000000000000
years <- sf6_atm$Year
C0_fun <- approxfun(years, C0, rule = 2)  # rule=2 allows extrapolation

compute_Cxt <- function(t_today, Gamma, Delta, years, C0_fun) {
  tau <- seq(0.1, 200, by = 0.5)  # tau: age in years
  G_tau <- ttd(tau, Gamma, Delta)
  
  # Normalize the TTD (optional but good practice)
  G_tau <- G_tau / sum(G_tau * diff(c(0, tau)))
  
  # Evaluate surface history at (t_today - tau)
  t_past <- t_today - tau
  C0_vals <- C0_fun(t_past)
  
  # Numerical integration (trapezoid rule)
  integrand <- C0_vals * G_tau
  C_xt <- sum(integrand * diff(c(0, tau)))
  return(C_xt)
}


# Fill in your data
ratio <- 0.5
Gamma <- 5
Delta <- ratio*Gamma
t_today <- 2023

# Compute concentration at depth x and time t
C_xt <- compute_Cxt(t_today, Gamma, Delta, years, C0_fun)
print(C_xt)

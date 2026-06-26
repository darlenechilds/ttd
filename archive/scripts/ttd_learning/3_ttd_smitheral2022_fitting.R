rm(list = ls())

ttd <- function(t, Gamma, Delta) {
  # ifelse(t <= 0, 0,
  sqrt(Gamma^3 / (4 * pi * Delta^2 * t^3)) *
    exp(-Gamma* (t-Gamma)^2 / (4 * Delta^2 * t))
}

# calculate Xeff - effective mole fraction, in equlib with wintertime surface conc. (Raimondi, 2021)
sf6_atm <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/ttd_learning/atmos_data_sf6_comb.csv")
# need to calculate for shallow/deep convection for now lets use 80%
sf6_atm_eff <- (sf6_atm$SF6NH*80)/100
sf6_interp <- approxfun(sf6_atm$Year, sf6_atm_eff, rule=2)

## cal Co using Xeff and solubility constants from Bullister et. al., 2002
# need to further refine, as this is only an average from last 10 years from NEADW

# NEADW average
T_k <- 3.16 + 273.15  
sal <- 34.92          #

#gravimetric - fmol per kg
sf6_Ln_F <- -82.1639+120.152*(100/T_k)+30.6372*log(T_k/100)+
  sal*(0.0293201+-0.0351974*(T_k/100)+0.00740056*(T_k/100)^2)
#volumetric
# sf6_Ln_F <- -80.0343+117.232*(100/T_k)+29.5817*log(T_k/100)+sal*(0.0335183+-0.0373942*(T_k/100)+0.00774862*(T_k/100)^2)

sf6_F <- exp(sf6_Ln_F)   # fmol per kg
sf6_p_atm <- sf6_atm_eff/1000000000000    # atm but double check

#surface history
C0 <- sf6_F*sf6_p_atm*1000000000000000  # fmol per kg
years <- sf6_atm$Year
C0_fun <- approxfun(years, C0, rule = 2)  # rule=2 allows extrapolation


# Your convolution function from earlier:
compute_Cxt <- function(t_today, Gamma, Delta, C0_fun) {
  tau <- seq(0.1, 200, by = 0.5)  # time, i.e. 1-200 years
  G_tau <- ttd(tau, Gamma, Delta) # ttd based on gamma (years), delta (width, years), arb...
  G_tau <- G_tau / sum(G_tau * diff(c(0, tau)))  # normalize
  C0_vals <- C0_fun(t_today - tau)  # Evaluate surface history at (t_today - tau)
  integrand <- C0_vals * G_tau  # Numerical integration (trapezoid rule)
  C_xt <- sum(integrand * diff(c(0, tau)))
  return(C_xt)
}

# observed SF6 concentration @ depth
obs_conc <- 3.5  # example in fmol/kg
t_obs <- 2023
# year of observation

# Objective: minimize squared difference between modeled and observed
obj_fun <- function(par) {
  Gamma <- par[1]
  Delta <- par[2]
  
  # Return large penalty if Delta > Gamma or invalid values
  if (Gamma <= 0 || Delta <= 0 || Delta > Gamma) return(1e6)
  
  pred_conc <- compute_Cxt(t_today = t_obs, Gamma, Delta, C0_fun)
  return((pred_conc - obs_conc)^2)
}

fit <- optim(par = c(30, 10), fn = obj_fun, method = "L-BFGS-B",
             lower = c(1, 0.1), upper = c(200, 100))

fit$par  # best-fit Gamma and Delta
fit$value  # minimized squared error


best_pred <- compute_Cxt(t_obs, fit$par[1], fit$par[2], C0_fun)
cat("Observed =", obs_conc, ", Modeled =", best_pred, "\n")


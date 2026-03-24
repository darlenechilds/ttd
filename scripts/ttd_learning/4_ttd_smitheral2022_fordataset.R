rm(list = ls())

# Define TTD, compute_Cxt, and solubility logic first (already done in your script)
# Make sure your temperature and salinity are dynamically used in the solubility calc

ttd <- function(t, Gamma, Delta) {
  # ifelse(t <= 0, 0,
  sqrt(Gamma^3 / (4 * pi * Delta^2 * t^3)) *
    exp(-Gamma* (t-Gamma)^2 / (4 * Delta^2 * t))
}

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

#solubility logic
# calculate Xeff - effective mole fraction, in equlib with wintertime surface conc. (Raimondi, 2021)
sf6_atm <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/ttd_learning/atmos_data_sf6_comb.csv")
# need to calculate for shallow/deep layers for now lets use 80%
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


#________________________________
# Load your observation data
obs_data <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/glodap_sf6/glodap_v2_2023_tracers.csv")  # update with actual file path
obs_data <- obs_data[which(obs_data$SF6>-1),]
unique(obs_data$SF6_FLAG_W)
obs_data <- obs_data[which(obs_data$SF6_FLAG_W==2),]
names(obs_data)
# Convert the date column to year
# obs_data$year <- as.numeric(format(as.Date(obs_data$DATE, format = "%b %d %Y"), "%Y"))
obs_data$year <- as.numeric(substr(obs_data$DATE,1,4))
# Define TTD, compute_Cxt, and solubility logic first (already done in your script)
# Make sure your temperature and salinity are dynamically used in the solubility calc

obs_conc <- obs_data$SF6
t_obs <- obs_data$year
temp_C <- obs_data$CTDTMP
sal <- obs_data$CTDSAL

fit_ttd <- function(obs_conc, t_obs, temp_C, sal, C0_fun) {
  T_k <- temp_C + 273.15
  
  # Solubility constants (gravimetric)
  sf6_Ln_F <- -82.1639 + 120.152*(100/T_k) + 30.6372*log(T_k/100) +
    sal * (0.0293201 +- 0.0351974*(T_k/100) + 0.00740056*(T_k/100)^2)
  
  sf6_F <- exp(sf6_Ln_F)
  sf6_p_atm <- sf6_interp(t_obs) / 1e12  # from earlier
  C0 <- sf6_F * sf6_p_atm * 1e15         # fmol/kg
  C0_fun_local <- function(years) sf6_F * sf6_interp(years) / 1e12 * 1e15
  
  obj_fun <- function(par) {
    Gamma <- par[1]
    Delta <- par[2]
    if (Gamma <= 0 || Delta <= 0 || Delta > Gamma) return(1e6)
    pred_conc <- compute_Cxt(t_today = t_obs, Gamma, Delta, C0_fun_local)
    return((pred_conc - obs_conc)^2)
  }
  
  
  
  fit <- optim(par = c(30, 10), fn = obj_fun, method = "L-BFGS-B",
               lower = c(1, 0.1), upper = c(200, 100))
  
  list(Gamma = fit$par[1],
       Delta = fit$par[2],
       pred = compute_Cxt(t_obs, fit$par[1], fit$par[2], C0_fun_local),
       error = fit$value,
       converged = fit$convergence == 0)
}
i <- 7
results <- do.call(rbind, lapply(1:nrow(obs_data), function(i) {
  row <- obs_data[i, ]
  fit_result <- fit_ttd(obs_conc = row$SF6,
                        t_obs = row$year,
                        temp_C = row$CTDTMP,
                        sal = row$CTDSAL,
                        C0_fun = C0_fun)  # using surface history
  
  data.frame(
    EXPOCODE = row$X,
    STNNBR = row$STNNBR,
    DATE = row$DATE,
    year = row$year,
    latitude = row$LATITUDE,
    longitude = row$LONGITUDE,
    PrDM = row$CTDPRS,
    sf6_obs = row$SF6,
    Gamma = fit_result$Gamma,
    Delta = fit_result$Delta,
    sf6_model = fit_result$pred,
    error = fit_result$error,
    converged = fit_result$converged
  )
}))

# Check results
head(results)
summary(results$Gamma)
summary(results$Delta)
plot(results$sf6_obs, results$sf6_model, xlab = "Observed SF6", ylab = "Modeled SF6")
abline(0,1,col="blue")

write.csv(results,"TTD_sf6_Glodap.csv", row.names = F)
dir()

rm(list = ls())
# Update Xeff usign equation 4 from Raimondi et al., for years 2016-2026 and add to the history from Raimondi et al., 

# calculate/test equation 3 to ensure using right variables
# input = tracers atmospheric mixing ration, MLD 
# CFC = 1986-2016
# SF6 = 2012-2016
# compare results with paper

#input
mld_model <- read.csv("data/raw/Raimondi_MLD_S2.csv", header = F)
mld_iy <- read.csv("data/raw/mld_Fig4_Yashayaev2024.csv", header = T)

plot(mld_iy$x,mld_iy$y, type = "b", xlim = c(1940,2026))
points(mld_model$V1,mld_model$V2, col = "blue", type = "b")

atm_sf6 <- read.csv("data/processed/sf6_atm.csv",header = T)
atm_f12 <- read.csv("data/processed/f12_atm.csv",header = T)


obs_d <- read.csv("data/processed/OCADS_tracers_o2_ar7w.csv")
#filter/keep good data usign ocads quality control flags, 2 = good, 6 = median of replicates, 0=not flagged by ocads yet
# d <- d[d$cfc12_flag==2 | d$cfc12_flag==6 | d$cfc12_flag==0,]  
obs_d <- obs_d[obs_d$cfc12_flag==2 | obs_d$cfc12_flag==6 ,]
obs_d <- obs_d[rowSums(is.na(obs_d)) != ncol(obs_d), ]

#add theta and sigma2
obs_d$theta <- swTheta(obs_d$CTDSAL,obs_d$CTDTMP,obs_d$CTDPRS,referencePressure = 0)
obs_d$sigma2 <- swSigma2(obs_d$CTDSAL,obs_d$theta,obs_d$CTDPRS)


#interpolate data to sample year
sf6_p_atm <- approx(
  x = atm_sf6$YEAR,
  y = atm_sf6$SF6 * 1e-12,   # convert ppt to atm fraction
  xout = obs_d$year
)$y

f12_p_atm <- approx(
  x = atm_f12$YEAR,
  y = atm_f12$CFC12 * 1e-12,   # convert ppt to atm fraction
  xout = obs_d$year
)$y


# compute observed saturations
T_k <- obs_d$CTDTMP +273.15
sal <- obs_d$CTDSAL

# sf6, Bullister et. al., 2002
#gravimetric
sf6_Ln_F <- -82.1639+120.152*(100/T_k)+30.6372*log(T_k/100)+sal*(0.0293201+-0.0351974*(T_k/100)+0.00740056*(T_k/100)^2)
sf6_F <- exp(sf6_Ln_F)
sf6_C_star <- sf6_F*sf6_p_atm*1000000000000000
obs_d$sf6_sat <- obs_d$SF6/sf6_C_star*100

# f12, Warner and Weiss, 1985
#gravimetric
f12_Ln_F <- -220.2120+301.8695*(100/T_k)+114.8533*log(T_k/100)+-1.39165*(T_k/100)^2+sal*(-0.147718+0.093175*(T_k/100)+-0.0157340*(T_k/100)^2)
f12_F <- exp(f12_Ln_F)
f12_C_star <- f12_F*f12_p_atm*1000000000000
obs_d$f12_sat <- obs_d$cfc12/f12_C_star*100

#get surface saturation for LSW centre
surf <- obs_d[obs_d$LATITUDE > 56 & obs_d$LATITUDE < 60 & obs_d$CTDPRS >= 0 & obs_d$CTDPRS <= 150,]

#try LSW for 1994, sigma 2 = 36.953
lsw1994 <- obs_d[obs_d$year==1994,]
lsw1994 <- lsw1994[lsw1994$LATITUDE > 56 & lsw1994$LATITUDE < 60 & lsw1994$CTDPRS >= 200 & lsw1994$sigma2<=36.953 ,]
meanf12 <- mean(lsw1994$f12_sat)



surf <- obs_d[
  !is.na(obs_d$LATITUDE) &
    obs_d$LATITUDE > 56 &
    obs_d$LATITUDE < 60,
]

uyear <- unique(surf$year)
m_sat <- NULL
i <- uyear[1]
for (i in uyear){}
  e <- surf[surf$year==i,]
  meanf12 <- mean(e$f12_sat)
  
# MLR of observed saturations to est. sat years before f12 and sf6 were measured.  
modeled_sat <- 




C0_fun <- approxfun(years, sf6_atm$C0_fmol_kg, rule = 2)                   # rule=2 allows extrapolation




# Load a built-in dataset (e.g., mtcars)
data(mtcars)

# Build a model predicting 'mpg' using 'disp', 'hp', and 'wt'
model <- lm(mpg ~ disp + hp + wt, data = mtcars)

# View detailed results
summary(model)






















ttd <- function(t, Gamma, Delta) {
  # ifelse(t <= 0, 0,
  sqrt(Gamma^3 / (4 * pi * Delta^2 * t^3)) *
    exp(-Gamma* (t-Gamma)^2 / (4 * Delta^2 * t))
}

# convolution function
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
sf6_atm <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/ttd_learning/atmos_data_sf6_comb_wTS_wSat.csv")

sf6_atm_eff <- (sf6_atm$SF6NH*sf6_atm$sat)/100
sf6_interp <- approxfun(sf6_atm$Year, sf6_atm_eff, rule=2)  #2 = allowing extrapolation 


## compute Co using Xeff and solubility constants from Bullister et. al., 2002

## Dynamically 
# Solubiity funcion; Bulister et al., 2002, gravimetric - mol per kg per atm
sf6_solubility <- function(temp, sal) {
  T_k <- temp + 273.15 # Convert temp to Kelvin
  sf6_Ln_F <- -82.1639+120.152*(100/T_k)+30.6372*log(T_k/100)+
    sal*(0.0293201+-0.0351974*(T_k/100)+0.00740056*(T_k/100)^2)  
  sf6_F <- exp(sf6_Ln_F)   # fmol per kg
  return(sf6_F)
}

# Compute solubility
sf6_atm$T_k <- sf6_atm$MeanTemp_filled + 273.15
sf6_atm$solubility <- with(sf6_atm, sf6_solubility(temp = MeanTemp_filled , sal = MeanSal_filled))  #  (sf6_F)

sf6_atm$sf6_atm_atm <- sf6_atm$SF6NH / 1e12                                # convert ppt to atm
sf6_atm$C0_mol_kg <- sf6_atm$solubility * sf6_atm$sf6_atm_atm              # Henry’s Law  
sf6_atm$C0_fmol_kg <- sf6_atm$C0_mol_kg* 1e15                              # fmol per kg, surface history
years <- sf6_atm$Year
C0_fun <- approxfun(years, sf6_atm$C0_fmol_kg, rule = 2)                   # rule=2 allows extrapolation


#________________________________
# Load observation data
obs_data <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/glodap_sf6/glodap_v2_2023_tracers.csv")  # update with actual file path
obs_data <- obs_data[which(obs_data$SF6>-1),]       # remove -999s
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
# temp_C <- obs_data$CTDTMP
# sal <- obs_data$CTDSAL

fit_ttd <- function(obs_conc, t_obs, C0_fun) {
  
  obj_fun <- function(par) {
    Gamma <- par[1]
    Delta <- par[2]
    
    # Check constraints
    if (Gamma <= 0 || Delta <= 0 || Delta > Gamma) return(1e6)
    
    pred_conc <- tryCatch({
      compute_Cxt(t_today = t_obs, Gamma, Delta, C0_fun)
    }, error = function(e) {
      message("compute_Cxt error: ", e$message)
      return(NA_real_)
    })
    
    # Catch non-finite or missing results
    if (!is.finite(pred_conc)) {
      message("Non-finite result: Gamma = ", Gamma, ", Delta = ", Delta, 
              ", pred_conc = ", pred_conc)
      return(1e6)
    }
    
    return((pred_conc - obs_conc)^2)
  }
  
  fit <- optim(par = c(30, 10), fn = obj_fun, method = "L-BFGS-B",
               lower = c(1, 0.1), upper = c(200, 100))
  
  list(
    Gamma = fit$par[1],
    Delta = fit$par[2],
    pred = compute_Cxt(t_obs, fit$par[1], fit$par[2], C0_fun),
    error = fit$value,
    converged = fit$convergence == 0
  )
}


results <- do.call(rbind, lapply(1:nrow(obs_data), function(i) {
  row <- obs_data[i, ]
  fit_result <- fit_ttd(obs_conc = row$SF6,
                        t_obs = row$year,
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
plot(results$sf6_obs, results$sf6_model, xlab = "Observed SF6", ylab = "Modeled SF6", xlim = c(0,10))
abline(0,1,col="blue")

write.csv(results,"TTD_sf6_Glodap_3.csv", row.names = F)
dir()

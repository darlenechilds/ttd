rm(list = ls())
# Define TTD, compute_Cxt, 
# Input has been constructed using Xeff equation (ppt), ppt has been converted to fmol kg-1 using T,S and solubility constants
library(oce)


# functions 
# ttd function
ttd <- function(t, Gamma, Delta) {
  G <- sqrt(Gamma^3 / (4 * pi * Delta^2 * t^3)) *
    exp(-Gamma * (t - Gamma)^2 / (4 * Delta^2 * t))
  G[t <= 0] <- 0
  return(G)
}

compute_Cxt <- function(t_today, Gamma, Delta, C0_fun) {
  tau  <- seq(0.1, 200, by = 0.5)
  dtau <- 0.5
  # Transit time distribution
  G_tau <- ttd(tau, Gamma, Delta)
  # Normalize so integral = 1
  G_tau <- G_tau / sum(G_tau * dtau)
  # Surface boundary condition evaluated backward in time
  C0_vals <- C0_fun(t_today - tau)
  C0_vals[is.na(C0_vals)] <- 0
  # Convolution
  C_xt <- sum(C0_vals * G_tau * dtau)
  return(C_xt)
}

# sf6_C_star_xeff - input to the system  #approx function,  
cO<- read.csv("data/processed/new/recon_surf_sf6_history_new.csv")
cO_fun_xeff <- approxfun(cO$year, cO$sf6_C_star_xeff, rule = 1)  

mld <- read.csv("data/raw/mld_Fig4_Yashayaev2024.csv")
mld$year <- floor(mld$x)

# measured data
# #unsubmitted data
d <- read.csv("data/processed/UNSUB_tracers_o2.csv")
d <- d[!is.na(d$SF6) & d$SF6 > 0, ]
years <- c("HUD2015006" = 2015,
  " HUD2016006" = 2016,
  "HUD2018008" = 2018,
  " AMU2019001" = 2019,
  "AMU2020001" = 2020,
  "AT4805"     = 2022,
  "CAR2023573" = 2023,
  "CAR2024924" = 2024,
  "LAT2025146" = 2025
)
d$year <- years[d$EXPOCODE]
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta,d$CTDPRS)


# compute gamma for all the years at once...
# empty column first
d$gamma_best <- NA_real_
# years to process
yrs <- 2015:2025

for (yr in yrs) {
  # sequences
  Gamma_seq <- seq(1, 200, by = 0.5)
  ratio <- 1.8
  Delta_seq <- ratio * Gamma_seq
  # modeled SF6 for this year
  sf6_mod_xeff <- sapply(1:length(Gamma_seq), function(i) {
      compute_Cxt(t = yr,
      Gamma = Gamma_seq[i],
      Delta = Delta_seq[i],
      C0_fun = cO_fun_xeff
    )
  })
    
    # rows for this year
    ind <- d$year == yr
    # best gamma for each observation
    d$gamma_best[ind] <- sapply(d$SF6[ind], function(obs) {
        Gamma_seq[which.min(abs(sf6_mod_xeff - obs))]
  })
}
d$gamma_best <- unlist(d$gamma_best)


write.csv(d,"data/processed/new/gamma_best.csv", row.names = F)











#compute gamma for each year... 
# year
yr <- 2022
mld_cut <- mld$y[mld$year == yr]

lsw <- d[d$year == yr &
           d$LATITUDE > 56 & d$LATITUDE < 59.1 &
           d$CTDPRS > 200 &
           d$CTDPRS < mld_cut, ]

C_obs_lsw_mean <- mean(lsw$SF6, na.rm = TRUE)
c_obs_lsw_sd <- sd(lsw$SF6, na.rm = TRUE)

# NEADW
neadw <- d[d$year == yr &
             d$LATITUDE > 56 & d$LATITUDE < 59.1 &
             d$sigma2 > 36.965 & d$sigma2 < 37.04, ]

C_obs_neadw_mean <- mean(neadw$SF6, na.rm = TRUE)
c_obs_neadw_sd <- sd(neadw$SF6, na.rm = TRUE)


# solving for gamma
Gamma_seq <- seq(1, 200, by = 0.5)
ratio <- 1.8
Delta_seq <- ratio * Gamma_seq

# for each (gamma,delta) compute modeled sf6
sf6_mod_xeff <- sapply(1:length(Gamma_seq), function(i) {
    compute_Cxt(t = yr, Gamma = Gamma_seq[i], Delta = Delta_seq[i],
    C0_fun = cO_fun_xeff)})


# visualize how the Gamma is found for the observed [cfc12] pmol per kg
plot(Gamma_seq, sf6_mod_xeff, type = "l", 
     ylim = c(0.5,3),
     xlab = "Gamma (years)",
     ylab = "Modeled SF6 (fmol/kg)")
abline(h = C_obs_lsw_mean, col = "red", lty = 2, lwd = 2)
abline(h = C_obs_neadw_mean, col = "darkred", lty = 2, lwd = 2)


# find Gamma that best fits modelled data using the observed/measured data
Gamma_seq[which.min(abs(sf6_mod_xeff - C_obs_lsw_mean))]
Gamma_seq[which.min(abs(sf6_mod_xeff - C_obs_neadw_mean))]


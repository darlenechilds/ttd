rm(list = ls())
# Define TTD, compute_Cxt, 
# Input has been constructed using Xeff equation (ppt), ppt has been converted to fmol kg-1 using T,S and solubility constants
library(oce)
#lsw sigma2 limit from raimondi et al., 2021, table S3
lsw_sigma <- data.frame(
  year = c(1986, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
           2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
           2011, 2012, 2013, 2014, 2015, 2016),
  sigma2 = c(36.910, 36.943, 36.952, 36.953, 36.942, 36.931, 36.913, 36.893,
             36.870, 36.875, 36.887, 36.885, 36.887, 36.881, 36.868, 36.864,
             36.842, 36.874, 36.860, 36.817, 36.823, 36.852, 36.850, 36.860,
             36.879, 36.896))

# functions 
# ttd function
ttd <- function(t, Gamma, Delta) {
  G <- sqrt(Gamma^3 / (4 * pi * Delta^2 * t^3)) *
    exp(-Gamma * (t - Gamma)^2 / (4 * Delta^2 * t))
  G[t <= 0] <- 0
  return(G)
}

# convolution function
compute_Cxt <- function(t_today, Gamma, Delta, C0_fun) {
  tau <- seq(0.1, 80, by = 0.5)                     # time, i.e. 1-200 years
  G_tau <- ttd(tau, Gamma, Delta)                   # ttd based on gamma (years), delta (width, years)
  G_tau <- G_tau / sum(G_tau * diff(c(0, tau)))     # normalize
  C0_vals <- C0_fun(t_today - tau)                  # Evaluate surface history at (t_today - tau)
  integrand <- C0_vals * G_tau                      # Numerical integration (trapezoid rule)
  C_xt <- sum(integrand * diff(c(0, tau)))
  return(C_xt)
}

# F12_C_star - input to the system  #approx function,  
cO<- read.csv("data/processed/recon_surf_f12_history.csv")

# 
# cO_fun <- approxfun(cO$year, cO$f12_C_star, rule = 2)
cO_fun_xeff <- approxfun(cO$year, cO$f12_C_star_xeff, rule = 2)  #rule=2 allows extrapolation

# ppt - input to the system
# cO <- read.csv("data/processed/f12_sat_modeled.csv")
# cO$xeff <- (cO$CFC12*cO$f12_sat_mod)/100
# cO_fun_xeff <- approxfun(cO$year, cO$xeff, rule = 2)


# measured data
d <- read.csv("data/processed/OCADS_tracers_o2_ar7w.csv", stringsAsFactors = F)
d <- d[d$cfc12_flag==2 | d$cfc12_flag==6,]  
d <- d[!is.na(d$cfc12), ]
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta,d$CTDPRS)
# d$cfc12_ppt <- d$cfc12*  120.91  #pmol/kg * 120.91g/mol * mol/1e12 pmol * 1e9 ng/g

# year
yr <- 2006
sigma_cut <- lsw_sigma$sigma2[lsw_sigma$year == yr]

# look at different water masses
# LSW
lsw <- d[d$year == yr &
           d$LATITUDE > 56 & d$LATITUDE < 59.1 &
           d$CTDPRS > 200 &
           d$sigma2 < sigma_cut, ]

C_obs_lsw_mean <- mean(lsw$cfc12, na.rm = TRUE)
C_obs_lsw_sd <- sd(lsw$cfc12, na.rm = TRUE)

# NEADW
neadw <- d[d$year == yr &
             d$LATITUDE > 56 & d$LATITUDE < 59.1 &
             d$sigma2 > 36.965 & d$sigma2 < 37.04, ]

C_obs_neadw_mean <- mean(neadw$cfc12, na.rm = TRUE)
c_obs_neadw_sd <- sd(neadw$cfc12, na.rm = TRUE)


# solving for gamma
Gamma_seq <- seq(1, 200, by = 0.5)
ratio <- 1.8
Delta_seq <- ratio * Gamma_seq

# for each (gamma,delta) compute modeled f12
# f12_mod <- sapply(1:length(Gamma_seq), function(i) {
#   compute_Cxt(t = yr, Gamma = Gamma_seq[i], Delta = Delta_seq[i], C0_fun = cO_fun)})

f12_mod_xeff <- sapply(1:length(Gamma_seq), function(i) {
  compute_Cxt(
    t = yr,
    Gamma = Gamma_seq[i],
    Delta = Delta_seq[i],
    C0_fun = cO_fun_xeff
  )
})

# visualize how the Gamma is found for the observed [cfc12] pmol per kg
plot(Gamma_seq, f12_mod_xeff, type = "l",
     ylim = c(0.5,3),
     xlab = "Gamma (years)",
     ylab = "Modeled CFC-12 (fmol/kg)")
abline(h = C_obs_lsw_mean, col = "red", lty = 2, lwd = 2)
abline(h = C_obs_neadw_mean, col = "darkred", lty = 2, lwd = 2)


# find Gamma that best fits modelled data using the observed/measured data
Gamma_seq[which.min(abs(f12_mod_xeff - C_obs_lsw_mean))]
Gamma_seq[which.min(abs(f12_mod_xeff - C_obs_neadw_mean))]

# look at each year
e <- d[d$year==yr,]

e$gamma_best <- sapply(e$cfc12, function(obs) {
  Gamma_seq[which.min(abs(f12_mod_xeff - obs))]
})

fn <- paste("data/processed/gamma_best_",yr,".csv", sep = "")
write.csv(e,fn, row.names = F)
fn

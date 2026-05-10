# Compute_Cant
rm(list = ls())


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

# input found gamma
yr <- 2006
fn <- paste("data/processed/gamma_best_",yr,".csv", sep = "")
d <- read.csv(fn, stringsAsFactors = F)


# co2_C_star - input to the system
cO <- read.csv("data/processed/surf_pco2_history.csv")
# approx function, rule=2 allows extrapolation
cO_fun <- approxfun(cO$yr, cO$pco2_c_star, rule = 2) 

# cO_ppt - input to the system
# cO_fun <- approxfun(cO$yr, cO$y, rule = 2) 


# have gamma
Gamma <- d$gamma_best
ratio <- 1.8
Delta <- ratio * Gamma


d$Cant <- sapply(1:length(Gamma), function(i) {
  compute_Cxt(
    t = yr,
    Gamma = Gamma[i],
    Delta = Delta[i],
    C0_fun = cO_fun
  )
})

# single computation

yr <- 1996
Gamma <- 2
ratio <- 1.8
Delta <- ratio * Gamma


compute_Cxt(
    t = yr,
    Gamma = Gamma,
    Delta = Delta,
    C0_fun = cO_fun
  )


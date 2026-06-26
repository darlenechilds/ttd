#constraints on mixing conditions (selection of delta/gamma)
# used data where sf6<6ppt
# calculate gamma best for both f12 and sf6 for ratios .40 - 2.2
rm(list = ls())

# functions 
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

# f12_C_star - input to the system  #approx function,  
cO_f12 <- read.csv("data/processed/recon_surf_f12_history.csv")
cO_fun_xeff_f12 <- approxfun(cO_f12$year, cO_f12$f12_C_star_xeff, rule = 1)  

# SF6_C_star - input to the system  #approx function,  
cO_sf6 <- read.csv("data/processed/recon_surf_sf6_history.csv")
cO_fun_xeff_sf6 <- approxfun(cO_sf6$year, cO_sf6$sf6_C_star_xeff, rule = 1)  

d <- read.csv("data/processed/OCADS_tracers_o2_ar7w.csv", stringsAsFactors = F)
d <- d[d$SF6_FLAG_W==2 | d$SF6_FLAG_W==6,]  
d <- d[d$SF6<=2.082,]


Gamma_seq <- seq(1, 200, by = 0.5)
ratio <- 1.8
Delta_seq <- ratio * Gamma_seq

yr <- 2012:2016
j <- 2012
s <- NULL

for (j in yr){
  f12_mod_xeff <- sapply(1:length(Gamma_seq), function(i) {
    compute_Cxt(t = j,Gamma = Gamma_seq[i], Delta = Delta_seq[i],
      C0_fun = cO_fun_xeff_f12)})
  
  sf6_mod_xeff <- sapply(1:length(Gamma_seq), function(i) {
    compute_Cxt(t = j,Gamma = Gamma_seq[i], Delta = Delta_seq[i],
      C0_fun = cO_fun_xeff_sf6)})
  
  e <- d[d$year %in% j, ]
  
  e$gamma_best_f12 <- sapply(e$cfc12, function(obs) {
    Gamma_seq[which.min(abs(f12_mod_xeff - obs))]})
  
  e$gamma_best_sf6 <- sapply(e$SF6, function(obs) {
    Gamma_seq[which.min(abs(sf6_mod_xeff - obs))]})
  
  s <- rbind.data.frame(e,s)

  
  }

plot(s$gamma_best_sf6,s$gamma_best_f12/s$gamma_best_sf6, ylim = c(0,10), xlim = c(0, 40))

r_fit <- lm(gamma_best_f12/gamma_best_sf6  ~ gamma_best_sf6 , data = s)
summary(r_fit)
r_fit$coefficients

plot(r_fit)
plot(fitted(r_fit), residuals(r_fit))
abline(h = 0, col = "red")


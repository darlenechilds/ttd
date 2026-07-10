# ==========================================================
# Function: compute_Cxt()
# Project: ttd 
# Concentration within the ocean interiour of a dissolved 
# substance subject to time variable surface water concentrations (tracers and Cant)
# ==========================================================

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
